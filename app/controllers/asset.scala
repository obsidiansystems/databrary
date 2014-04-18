package controllers

import scala.concurrent.Future
import play.api._
import          Play.current
import          mvc._
import          data._
import          i18n.Messages
import          libs.Files.TemporaryFile
import          libs.iteratee.Enumerator
import          libs.concurrent.Execution.Implicits.defaultContext
import macros._
import dbrary._
import site._
import models._

private[controllers] sealed class AssetController extends ObjectController[Asset] {
  protected def action(i : models.Asset.Id, p : Permission.Value) =
    RequestObject.check(models.Asset.get(i)(_), p)

  protected def Action(i : models.Asset.Id, p : Permission.Value) =
    SiteAction ~> action(i, p)

  def update(o : models.Asset.Id) =
    Action(o, Permission.EDIT).async { implicit request =>
      val form = new AssetController.ChangeForm()._bind
      for {
	container <- macros.Async.map[Container.Id, Container](form.container.get,
	  Container.get(_).map(_.getOrElse(form.container.withError("Invalid container ID")._throw)))
	_ <- request.obj.change(classification = form.classification.get, name = form.name.get)
	_ <- macros.Async.foreach[Container, Unit](container,
	  request.obj.link(_, form.position.get))
      } yield (result(request.obj))
    }

  private[controllers] def assetResult(asset : BackedAsset, saveAs : Option[String] = None)(implicit request : SiteRequest[_]) : Future[SimpleResult] = {
    val tag = asset.etag
    /* Assuming assets are immutable, any if-modified-since header is good enough */
    if (HTTP.notModified(tag, new Timestamp(0)))
      macros.Async(NotModified)
    else for {
      data <- store.Asset.read(asset)
      date <- asset.source.creation
    } yield {
      val size = data.size
      val range = if (request.headers.get(IF_RANGE).forall(HTTP.unquote(_).equals(tag)))
          request.headers.get(RANGE).flatMap(HTTP.parseRange(_, size))
        else
          None
      val subdata = range.fold(data)((data.range _).tupled)
      val headers = Seq[Option[(String, String)]](
        Some(CONTENT_LENGTH -> subdata.size.toString),
        range.map(r => CONTENT_RANGE -> ("bytes " + (if (r._1 >= size) "*" else r._1.toString + "-" + r._2.toString) + "/" + size.toString)),
        Some(CONTENT_TYPE -> asset.format.mimetype),
        saveAs.map(name => CONTENT_DISPOSITION -> ("attachment; filename=" + HTTP.quote(name + asset.format.extension.fold("")("." + _)))),
        date.map(d => (LAST_MODIFIED -> HTTP.date(d))),
        Some(ETAG -> HTTP.quote(tag)),
        Some(CACHE_CONTROL -> "max-age=31556926, private") /* this needn't be private for public data */
      ).flatten
        SimpleResult(
          header = ResponseHeader(range.fold(OK)(r => if (r._1 >= size) REQUESTED_RANGE_NOT_SATISFIABLE else PARTIAL_CONTENT),
            Map(headers : _*)),
          body = subdata)
      }
  }
}

object AssetController extends AssetController {
  sealed abstract class AssetForm(action : Call)(implicit request : RequestObject.Site[_])
    extends HtmlForm[AssetForm](action,
      views.html.asset.edit(_)) {
    def actionName : String
    def formName : String = actionName + " Asset"

    val name = Field(OptionMapping(Mappings.maybeText))
    val classification = Field(OptionMapping(Mappings.enum(Classification)))
    val container = Field(Forms.optional(Forms.of[Container.Id]))
    val position = Field(Forms.optional(Forms.of[Offset]))
  }

  final class ChangeForm(implicit request : Request[_])
    extends AssetForm(routes.AssetHtml.update(request.obj.id)) {
    def actionName = "Update"
    override def formName = "Edit Asset"
    name.fill(Some(request.obj.name))
    classification.fill(Some(request.obj.classification))
    container.fill(None)
    position.fill(None)
  }

  final class UploadForm(implicit request : VolumeController.Request[_])
    extends AssetForm(routes.AssetHtml.upload(request.obj.id)) {
    classification.fill(Some(Classification.IDENTIFIED))
    def actionName = "Upload"
    val format = Field(Forms.optional(Forms.of[AssetFormat.Id]))
    val timeseries = Field(Forms.boolean)
    val localfile = Field(Forms.optional(Forms.nonEmptyText))
    val file = OptionalFile()
  }
}

object AssetHtml extends AssetController with HtmlController {
  import AssetController._

  def view(o : models.Asset.Id) = Action(o, Permission.VIEW).async { implicit request =>
    request.obj.slot.map(_.fold[SimpleResult](
      throw NotFoundException /* TODO */)(
      sa => Redirect(sa.inContainer.pageURL)))
  }

  def edit(o : models.Asset.Id) =
    Action(o, Permission.EDIT).async { implicit request =>
      new ChangeForm().Ok
    }

  def create(v : models.Volume.Id, c : Option[Container.Id], pos : Option[Offset]) =
    VolumeHtml.Action(v, Permission.CONTRIBUTE).async { implicit request =>
      val form = new UploadForm()
      form.container.fill(c)
      form.position.fill(pos)
      form.Ok
    }

  def upload(v : models.Volume.Id) =
    VolumeHtml.Action(v, Permission.CONTRIBUTE).async { implicit request =>
      val form = new UploadForm()._bind
      val adm = request.access.isAdmin
      val ifmt = form.format.get.filter(_ => adm).flatMap(AssetFormat.get(_))
      val (file, fmt, fname) =
	form.file.get.fold {
	  /* local file handling, for admin only: */
	  val file = store.Stage.file(form.localfile.get.filter(_ => adm) getOrElse
	    form.file.withError("error.required")._throw)
	  val name = file.getName
	  if (!file.isFile)
	    form.localfile.withError("File not found")._throw
	  val ffmt = ifmt orElse AssetFormat.getFilename(name) getOrElse
	    form.format.withError("file.format.unknown", "unknown")._throw
	  (store.TemporaryFileLinkOrCopy(file) : TemporaryFile, ffmt, name)
	} { file =>
	  val ffmt = ifmt orElse AssetFormat.getFilePart(file) getOrElse
	    form.file.withError("file.format.unknown", file.contentType.getOrElse("unknown"))._throw
	  (file.ref, ffmt, file.filename)
	}
      val aname = form.name.get.flatten orElse Maybe(fname).opt
      for {
	container <- macros.Async.map[Container.Id, Container](form.container.get, Container.get(_).map(_ getOrElse
	  form.container.withError("Invalid container ID")._throw))
	asset <- fmt match {
	  case fmt : TimeseriesFormat if adm && form.timeseries.get =>
	    val probe = media.AV.probe(file.file)
	    models.Asset.create(request.obj, fmt, form.classification.get.getOrElse(Classification(0)), probe.duration, aname, file)
	  case _ =>
	    models.Asset.create(request.obj, fmt, form.classification.get.getOrElse(Classification(0)), aname, file)
	}
	sa <- macros.Async.map[Container, SlotAsset](container, asset.link(_, form.position.get))
	_ = if (fmt.mimetype.startsWith("video/") && !form.timeseries.get)
	  store.Transcode.transcode(asset)
      } yield (Redirect(sa.getOrElse(asset).pageURL))
    }

  def transcode(a : models.Asset.Id) =
    (SiteAction.rootAccess(Permission.ADMIN) ~> action(a, Permission.EDIT)) { implicit request =>
      store.Transcode.transcode(request.obj)
      Ok("transcoding")
    }

  def remove(a : models.Asset.Id) = Action(a, Permission.EDIT).async { implicit request =>
    for {
      _ <- request.obj.unlink
    } yield (Redirect(request.obj.pageURL))
  }
}

object AssetApi extends AssetController with ApiController {
  def get(i : models.Asset.Id) = Action(i, Permission.VIEW).async { implicit request =>
    request.obj.json(request.apiOptions).map(Ok(_))
  }
}
