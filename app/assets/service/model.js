'use strict';

module.factory('modelService', [
  '$q', '$cacheFactory', '$play', 'routerService', 'constantService', 'Segment',
  function ($q, $cacheFactory, $play, router, constants, Segment) {

    ///////////////////////////////// Model: common base class and utils

    // just a bit more efficient than angular's
    function extend(dst, src) {
      for (var key in src)
	dst[key] = src[key];
    }

    function resData(res) {
      return res.data;
    }

    function Model(init) {
      this.init(init);
    }

    /* optional fields that are always returned (so missingness is significant) */
    Model.prototype.staticFields = [];

    Model.prototype.init = function (init) {
      extend(this, init);
    };

    Model.prototype.update = function (init) {
      if (!angular.isObject(init))
	return this;
      if ('id' in init && init.id !== this.id)
	throw new Error("update id mismatch");
      var s = this.staticFields;
      for (var i = 0; i < s.length; i ++) {
	var k = s[i];
	if (!(k in init) && k in this)
	  delete this[k];
      }
      this.init(init);
      return this;
    };

    Model.prototype.clear = function (/*f...*/) {
      for (var i = 0; i < arguments.length; i ++)
	if (arguments[i] in this)
	  delete this[arguments[i]];
    };

    /* determine whether the given object satisfies all the given dependency options already.
     * returns the missing options, or null if nothing is missing. */
    function checkOptions(obj, options) {
      var opts = {};
      var need = obj ? null : opts;
      if (Array.isArray(options)) {
	for (var i = 0; i < options.length; i ++)
	  if (!obj || !(options[i] in obj)) {
	    opts[options[i]] = '';
	    need = opts;
	  }
      }
      else if (!obj)
	return options || opts;
      else if (options)
	angular.forEach(options, function (v, o) {
	  if (v || !(o in obj)) {
	    opts[o] = v;
	    need = opts;
	  }
	});
      return need;
    }

    function modelCache(obj, name, size) {
      obj.prototype = Object.create(Model.prototype);
      obj.prototype.constructor = obj;
      obj.prototype.class = name;

      var opts = {};
      if (size)
	opts.number = size;
      obj.cache = $cacheFactory(name, opts);

      obj.clear = function (/*id...*/) {
	if (arguments.length)
	  for (var i = 0; i < arguments.length; i ++)
	    obj.cache.remove(arguments[i]);
	else
	  obj.cache.removeAll();
      };

      obj.poke = function (x) {
	return obj.cache.put(x.id, x);
      };
    }

    /* delegate the given (missing) fields on instances of obj to the sub-object sub,
     * but allow assignments to work directly as usual. */
    function delegate(obj, sub /*, field... */) {
      function descr(f) {
	return {
	  get: function () {
	    return this[sub].hasOwnProperty(f) ? this[sub][f] : undefined;
	  },
	  set: function (v) {
	    Object.defineProperty(this, f, {
	      configurable: true,
	      enumerable: true,
	      writable: true,
	      value: v
	    });
	  }
	};
      }
      for (var i = 2; i < arguments.length; i ++) {
	var f = arguments[i];
	Object.defineProperty(obj.prototype, f, descr(f));
      }
    }

    ///////////////////////////////// Party

    function Party(init) {
      Model.call(this, init);
    }

    modelCache(Party, 'party', 256);

    Party.prototype.staticFields = ['orcid', 'affiliation', 'email', 'institution', 'url'];

    Party.prototype.init = function (init) {
      Model.prototype.init.call(this, init);
      if ('volumes' in init)
	volumeMakeSubArray(this.volumes);
      if ('parents' in init)
	partyMakeSubArray(this.parents);
      if ('children' in init)
	partyMakeSubArray(this.children);
      if ('comments' in init)
	commentMakeArray(null, this.comments);
    };

    function partyPeek(id) {
      return id === Login.user.id && Login.user || Party.cache.get(id);
    }

    function partyMake(init) {
      var p = partyPeek(init.id);
      return p ? p.update(init) : Party.poke(new Party(init));
    }

    function partyMakeSubArray(l) {
      for (var i = 0; i < l.length; i ++)
	l[i].party = partyMake(l[i].party);
      return l;
    }

    function partyMakeArray(l) {
      if (l) for (var i = 0; i < l.length; i ++)
	l[i] = partyMake(l[i]);
      return l;
    }

    function partyGet(id, p, options) {
      if ((options = checkOptions(p, options)))
	return router.http(id == Login.user.id ? // may both be undefined (id may be string)
	    router.controllers.PartyApi.profile :
	    router.controllers.PartyApi.get,
	  id, options)
	  .then(function (res) {
	    return p ? p.update(res.data) : Party.poke(new Party(res.data));
	  });
      else
	return $q.successful(p);
    }

    Party.get = function (id, options) {
      return partyGet(id, partyPeek(id), options);
    };

    Party.prototype.get = function (options) {
      return partyGet(this.id, this, options);
    };

    Party.profile = function (options) {
      return Party.get(Login.user.id, options);
    };

    Party.prototype.save = function (data) {
      var p = this;
      return router.http(router.controllers.PartyApi.update, this.id, data)
	.then(function (res) {
	  return p.update(res.data);
	});
    };

    Party.query = function (data) {
      return router.http(router.controllers.PartyApi.query, data)
	.then(function (res) {
	  return res.data.map(partyMake);
	});
    };

    Object.defineProperty(Party.prototype, 'route', {
      get: function () {
	return router.party([this.id]);
      }
    });

    Object.defineProperty(Party.prototype, 'lastName', {
      get: function () {
	return this.name.substr(this.name.lastIndexOf(' ')+1);
      }
    });

    Party.prototype.editRoute = function (page) {
      var params = {};
      if (page)
	params.page = page;

      return router.partyEdit([this.id], params);
    };

    Party.prototype.avatarRoute = function (size, nonce) {
      size = size || 56;

      var params = {};
      if (nonce) {
	params.nonce = nonce;
      }

      return router.partyAvatar([this.id, size], params);
    };

    Party.prototype.authorizeSearch = function (apply, param) {
      return router.http(router.controllers.PartyApi.authorizeSearch, this.id, apply, param)
	.then(function (res) {
	  if (Array.isArray(res.data))
	    return partyMakeArray(res.data);
	  return res;
	});
    };

    Party.prototype.authorizeApply = function (target, data) {
      var p = this;
      return router.http(router.controllers.PartyApi.authorizeApply, this.id, target, data)
	.then(function (res) {
	  p.clear('parents');
	  return p.update(res.data);
	});
    };

    Party.prototype.authorizeSave = function (target, data) {
      var p = this;
      return router.http(router.controllers.PartyApi.authorizeChange, this.id, target, data)
	.then(function (res) {
	  p.clear('children');
	  return p.update(res.data);
	});
    };

    Party.prototype.authorizeDelete = function (target) {
      var p = this;
      return router.http(router.controllers.PartyApi.authorizeDelete, this.id, target)
	.then(function (res) {
	  p.clear('children');
	  return p.update(res.data);
	});
    };

    ///////////////////////////////// Login

    function Login(init) {
      Party.call(this, init);
    }

    Login.prototype = Object.create(Party.prototype);
    Login.prototype.constructor = Login;

    Login.user = new Login({id:constants.party.NOBODY});

    function loginPoke(l) {
      return (Login.user = Party.poke(new Login(l)));
    }

    loginPoke($play.user);

    function loginRes(res) {
      var l = res.data;
      if (Login.user.id === l.id && Login.user.superuser === l.superuser)
	return Login.user.update(l);
      $cacheFactory.removeAll();
      return loginPoke(l);
    }

    Login.isLoggedIn = function () {
      return Login.user.id !== constants.party.NOBODY;
    };

    Login.checkAccess = function (level) {
      return Login.user.access >= level;
    };

    Model.prototype.checkPermission = function (level) {
      return this.permission >= level || Login.user.superuser;
    };

    /* a little hacky, but to get people SUPER on themselves: */
    Login.prototype.checkPermission = function (/*level*/) {
      return this.id !== constants.party.NOBODY;
    };

    Login.isAuthorized = function () {
      return Login.isLoggedIn() && Login.checkAccess(constants.permission.PUBLIC);
    };

    angular.forEach({
      get: 'get',
      login: 'post',
      logout: 'logout',
      superuserOn: 'superuserOn',
      superuserOff: 'superuserOff'
    }, function (api, f) {
      Login[f] = function (data) {
	return router.http(router.controllers.LoginApi[api], data).then(loginRes);
      };
    });

    Login.register = function (data) {
      return router.http(router.controllers.LoginApi.register, data);
    };

    Login.issuePassword = function (data) {
      return router.http(router.controllers.TokenHtml.issuePassword, data);
    };

    Login.getToken = function (token, auth) {
      return router.http(router.controllers.TokenApi.token, token, auth)
	.then(resData);
    };

    Login.passwordToken = function (party, data) {
      return router.http(router.controllers.TokenApi.password, party, data)
	.then(loginRes);
    };

    ///////////////////////////////// Volume

    function Volume(init) {
      Model.call(this, init);
    }

    modelCache(Volume, 'volume', 8);

    Volume.prototype.staticFields = ['alias'];

    Volume.prototype.init = function (init) {
      Model.prototype.init.call(this, init);
      if ('access' in init)
	partyMakeSubArray(this.access);
      if ('containers' in init) {
	var cl = this.containers;
	var cm = {};
	for (var ci = 0; ci < cl.length; ci ++)
	  cm[cl[ci].id] = new Container(this, cl[ci]);
	this.containers = cm;
      }
      if ('top' in init) {
	if (this.containers && this.top.id in this.containers)
	  this.top = this.containers[this.top.id].update(this.top);
	else
	  this.top = new Container(this, this.top);
      }
      if ('records' in init) {
	var rl = this.records;
	var rm = {};
	for (var ri = 0; ri < rl.length; ri ++)
	  rm[rl[ri].id] = new Record(this, rl[ri]);
	this.records = rm;
      }
      if ('excerpts' in init)
	assetMakeArray(this, this.excerpts);
      if ('comments' in init)
	commentMakeArray(this, this.comments);
    };

    function volumeMake(init) {
      var v = Volume.cache.get(init.id);
      return v ? v.update(init) : Volume.poke(new Volume(init));
    }

    function volumeMakeSubArray(l) {
      for (var i = 0; i < l.length; i ++)
	l[i].volume = volumeMake(l[i].volume);
      return l;
    }

    function volumeGet(id, v, options) {
      if ((options = checkOptions(v, options)))
	return router.http(router.controllers.VolumeApi.get,
	  id, options).then(function (res) {
	    return v ? v.update(res.data) : Volume.poke(new Volume(res.data));
	  });
      else
	return $q.successful(v);
    }

    Volume.get = function (id, options) {
      return volumeGet(id, Volume.cache.get(id), options);
    };

    Volume.prototype.get = function (options) {
      return volumeGet(this.id, this, options);
    };

    Volume.prototype.save = function (data) {
      var v = this;
      return router.http(router.controllers.VolumeApi.update, this.id, data)
	.then(function (res) {
	  v.clear('citation');
	  return v.update(res.data);
	});
    };

    Volume.create = function (data, owner) {
      return router.http(router.controllers.VolumeApi.create, owner, data)
	.then(function (res) {
	  if ((owner = (owner === undefined ? Login.user : partyPeek(owner))))
	    owner.clear('volumes');
	  return volumeMake(res.data);
	});
    };
    
    Volume.query = function (data) {
      return router.http(router.controllers.VolumeApi.query, data)
	.then(function (res) {
	  return res.data.map(volumeMake);
	});
    };

    Object.defineProperty(Volume.prototype, 'type', {
      get: function () {
	if ('citation' in this)
	  return this.citation ? 'study' : 'dataset';
      }
    });

    Object.defineProperty(Volume.prototype, 'route', {
      get: function () {
	return router.volume([this.id]);
      }
    });

    Volume.prototype.editRoute = function (page) {
      var params = {};
      if (page)
	params.page = page;

      return router.volumeEdit([this.id], params);
    };

    Volume.prototype.thumbRoute = function (size) {
      return router.volumeThumb([this.id, size]);
    };

    Volume.prototype.zipRoute = function () {
      return router.volumeZip([this.id]);
    };

    Volume.prototype.accessSearch = function (param) {
      return router.http(router.controllers.VolumeApi.accessSearch, this.id, param)
	.then(function (res) {
	  return partyMakeArray(res.data);
	});
    };

    Volume.prototype.accessSave = function (target, data) {
      var v = this;
      return router.http(router.controllers.VolumeApi.accessChange, this.id, target, data)
	.then(function (res) {
	  v.clear('access');
	  return v.update(res.data);
	});
    };

    Volume.prototype.accessDelete = function (target) {
      var v = this;
      return router.http(router.controllers.VolumeApi.accessDelete, this.id, target)
	.then(function (res) {
	  v.clear('access');
	  return v.update(res.data);
	});
    };

    Volume.prototype.fundingSave = function (funder, data) {
      var v = this;
      return router.http(router.controllers.VolumeApi.fundingChange, this.id, funder, data)
	.then(function (res) {
	  v.clear('funding');
	  return v.update(res.data);
	});
    };

    Volume.prototype.fundingDelete = function (funder) {
      var v = this;
      return router.http(router.controllers.VolumeApi.fundingDelete, this.id, funder)
	.then(function (res) {
	  v.clear('funding');
	  return v.update(res.data);
	});
    };

    ///////////////////////////////// Container/Slot
    // This does not handle cross-volume inclusions

    function Slot(context, init) {
      this.container = context instanceof Volume ?
	containerPrepare(context, init.container.id) :
	context;
      if (init)
	Model.call(this, init);
    }

    Slot.prototype = Object.create(Model.prototype);
    Slot.prototype.constructor = Slot;
    Slot.prototype.class = 'slot';

    Slot.prototype.staticFields = ['consent', 'context'];

    Slot.prototype.clear = function (/*f...*/) {
      Model.prototype.clear.apply(this, arguments);
      Model.prototype.clear.apply(this.container, arguments);
    };

    function slotInit(slot, init) {
      if ('assets' in init)
	slotAssetMakeArray(slot.container, slot.assets);
      if ('comments' in init)
	commentMakeArray(slot.volume, slot.comments);
      /* records : [Record], but we shouldn't be using it */
    }

    Slot.prototype.init = function (init) {
      var c = this.container;
      Model.prototype.init.call(this, init);
      this.segment = new Segment(init.segment);
      if ('container' in init)
	this.container = c.update(init.container);
      slotInit(this, init);
    };

    delegate(Slot, 'container',
	'id', 'volume', 'top', 'date', 'name');

    delegate(Slot, 'volume',
	'permission');

    Object.defineProperty(Slot.prototype, 'displayName', {
      get: function () {
	return constants.message(this.container.top ? 'materials' : 'session') + (this.name ? ': ' + this.name : '');
      }
    });

    Slot.prototype.inContext = function () {
      if (this.segment.equals(this.context))
	return this;
      /* not type-safe for descendents:
      if (this.context === undefined)
	return this.container; */
      var s = angular.extend(Object.create(Object.getPrototypeOf(this)), this, {segment:Segment.make(this.context)});
      s.clear('format');
      return s;
    };

    function Container(volume, init) {
      this.volume = volume;
      Slot.call(this, this, init);
    }

    Container.prototype = Object.create(Slot.prototype);
    Container.prototype.constructor = Container;

    Container.prototype.staticFields = ['_PLACEHOLDER', 'name', 'date', 'top'].concat(Slot.prototype.staticFields);

    Container.prototype.init = function (init) {
      var v = this.volume;
      Model.prototype.init.call(this, init);
      if ('volume' in init)
	this.volume = v.update(init.volume);
      if ('container' in init)
	this.container = this.update(init.container);
      slotInit(this, init);
    };

    Object.defineProperty(Container.prototype, 'segment', {
      get: function () {
	return Segment.full;
      }
    });

    Container.prototype.remove = function () {
      var c = this;
      return router.http(router.controllers.SlotApi.remove, this.id)
        .then(function () {
          if ('contaiers' in c.volume)
            delete c.volume.containers[c.id];
          return true;
        }, function (res) {
          if (res.status == 409) {
            c.update(res.data);
            return false;
          }
          return $q.reject(res);
        });
    };

    function containerPrepare(volume, id) {
      if (volume.containers && id in volume.containers)
	return volume.containers[id];
      if (volume.top && volume.top.id === id)
	return volume.top;
      return new Container(volume, {id:id, _PLACEHOLDER:true});
    }

    Volume.prototype.getSlot = function (container, segment, options) {
      return containerPrepare(this, container).getSlot(segment, options);
    };

    Container.prototype.getSlot = function (segment, options) {
      var c = this;
      if (Segment.isFull(segment))
	if ((options = checkOptions(this, options)) || this._PLACEHOLDER)
	  return router.http(router.controllers.SlotApi.get,
	    this.volume.id, this.id, Segment.format(segment), options)
	    .then(function (res) {
	      return c.update(res.data);
	    });
	else return $q.successful(this);
      else return router.http(router.controllers.SlotApi.get,
	this.volume.id, this.id, Segment.format(segment), checkOptions(null, options))
	.then(function (res) {
	  return new Slot(c, res.data);
	});
    };

    Slot.prototype.save = function (data) {
      var s = this;
      return router.http(router.controllers.SlotApi.update, this.container.id, this.segment.format(), data)
	.then(function (res) {
	  return s.update(res.data);
	});
    };

    Volume.prototype.createContainer = function (data) {
      var v = this;
      return router.http(router.controllers.SlotApi.create, this.id, data)
	.then(function (res) {
	  var c = new Container(v, res.data);
	  if ('containers' in v)
	    v.containers[c.id] = c;
	  return c;
	});
    };

    function recordAdd(slot, record) {
      var r = {
        id: record.id,
        segment: slot.segment
      };
      if ('records' in slot)
        slot.records.push(r);
      if (slot.container !== slot && 'records' in slot.container)
        slot.container.records.push(r);
    }

    Slot.prototype.addRecord = function (r) {
      var s = this;
      return router.http(router.controllers.RecordApi.add, this.container.id, this.segment.format(), {record:r.id})
	.then(function (res) {
          recordAdd(s, r);
	  return r.update(res.data);
	});
    };

    Slot.prototype.newRecord = function (c) {
      var s = this;
      if (c && typeof c === 'object')
	c = c.id;
      return router.http(router.controllers.RecordApi.add, this.container.id, this.segment.format(), {category:c})
	.then(function (res) {
	  var v = s.volume;
	  var r = new Record(v, res.data);
	  if ('records' in v)
	    v.records[r.id] = r;
          recordAdd(s, r);
	  return r;
	});
    };

    Slot.prototype.removeRecord = function (r) {
      var s = this;
      return router.http(router.controllers.RecordApi.remove, this.container.id, this.segment.format(), r.id)
	.then(function (res) {
	  s.update(res.data);
	  if ('records' in s)
	    /* not quite right with segments */
	    s.records = s.records.filter(function (sr) {
	      return sr.id !== r.id;
	    });
	});
    };

    Object.defineProperty(Slot.prototype, 'route', {
      get: function () {
	return router.slot([this.volume.id, this.container.id, this.segment.format()]);
      }
    });

    Slot.prototype.editRoute = function () {
      return router.slotEdit([this.volume.id, this.container.id, this.segment.format()]);
    };

    Slot.prototype.zipRoute = function () {
      return router.slotZip([this.volume.id, this.container.id, this.segment.format()]);
    };

    ///////////////////////////////// Record

    function Record(volume, init) {
      this.volume = volume;
      Model.call(this, init);
    }

    Record.prototype = Object.create(Model.prototype);
    Record.prototype.constructor = Record;
    Record.prototype.class = 'record';

    Record.prototype.staticFields = ['category'];

    Record.prototype.init = function (init) {
      var v = this.volume;
      Model.prototype.init.call(this, init);
      if ('volume' in init)
	this.volume = v.update(init.volume);
    };

    delegate(Record, 'volume',
	'permission');

    function recordMake(volume, init) {
      if (volume.records && init.id in volume.records)
	return volume.records[init.id].update(init);
      return new Record(volume, init);
    }

    Volume.prototype.getRecord = function (record) {
      if (record instanceof Record)
	return $q.successful(record);
      if (this.records && record in this.records)
	return $q.successful(this.records[record]);
      var v = this;
      return router.http(router.controllers.RecordApi.get, record)
	.then(function (res) {
	  return new Record(v, res.data);
	});
    };

    Record.prototype.save = function (data) {
      var r = this;
      return router.http(router.controllers.RecordApi.update, this.id, data)
	.then(function (res) {
	  return r.update(res.data);
	});
    };

    Record.prototype.measureSet = function (metric, value) {
      var r = this;
      return router.http(router.controllers.RecordApi.measureUpdate, this.id, metric, {datum:value})
	.then(function (res) {
	  return r.update(res.data);
	});
    };

    Object.defineProperty(Record.prototype, 'displayName', {
      get: function () {
	var cat = constants.category[this.category];
	var idents = cat && cat.ident || [constants.metricName.ident.id];
	var ident = [];
	for (var i = 0; i < idents.length; i ++)
	  if (idents[i] in this.measures)
	    ident.push(this.measures[idents[i]]);

	return ident.length ? ident.join(', ') : '[' + this.id + ']';
      }
    });

    Object.defineProperty(Record.prototype, 'route', {
      get: function () {
	return router.record([this.id]);
      }
    });

    Record.prototype.editRoute = function () {
      return router.recordEdit([this.id]);
    };

    ///////////////////////////////// Asset

    function Asset(volume, init) {
      this.volume = volume;
      Model.call(this, init);
    }

    Asset.prototype = Object.create(Model.prototype);
    Asset.prototype.constructor = Asset;
    Asset.prototype.class = 'asset';

    Asset.prototype.staticFields = ['name', 'duration'];

    Asset.prototype.init = function (init) {
      var v = this.volume;
      Model.prototype.init.call(this, init);
      if ('segment' in init)
	this.segment = new Segment(init.segment);
      if ('volume' in init)
	this.volume = v.update(this.volume);
      if ('format' in init)
	this.format = constants.format[this.format];
      if ('revisions' in init)
	assetMakeArray(v, this.revisions);
      /* slot : Slot, but really implies SlotAsset */
    };

    function assetMake(context, init) {
      if ('id' in init)
	return new Asset('volume' in context ? context.volume : context, init);
      else
	return new SlotAsset(context, init);
    }

    function assetMakeArray(context, l) {
      if (l) for (var i = 0; i < l.length; i ++)
	l[i] = assetMake(context, l[i]);
      return l;
    }

    Asset.prototype.get = function (options) {
      var a = this;
      if ((options = checkOptions(a, options)))
	return router.http(router.controllers.AssetApi.get, a.id, options)
	  .then(function (res) {
	    return a.update(res.data);
	  });
      else
	return $q.successful(a);
    };

    ///////////////////////////////// SlotAsset

    function SlotAsset(context, init) {
      Slot.call(this, context, init);
    }

    SlotAsset.prototype = Object.create(Slot.prototype);
    SlotAsset.prototype.constructor = SlotAsset;
    SlotAsset.prototype.class = 'asset';

    SlotAsset.prototype.staticFields = ['format', 'excerpt'].concat(Slot.prototype.staticFields);

    SlotAsset.prototype.init = function (init) {
      var a = this.asset;
      Slot.prototype.init.call(this, init);
      this.asset = a ? a.update(init.asset) : new Asset(this.volume, init.asset);
      if ('format' in init)
	this.format = constants.format[this.format];
    };

    function slotAssetMakeArray(container, l) {
      if (l) for (var i = 0; i < l.length; i ++)
	l[i] = new SlotAsset(container, l[i]);
      return l;
    }

    delegate(SlotAsset, 'asset',
	'id', 'format', 'classification', 'name', 'duration');

    Object.defineProperty(SlotAsset.prototype, 'displayName', {
      get: function () {
	return this.name || this.format.name;
      }
    });

    Object.defineProperty(SlotAsset.prototype, 'route', {
      get: function () {
	return router.slot([this.volume.id, this.container.id, this.segment.format()], {asset:this.id});
      }
    });

    Object.defineProperty(SlotAsset.prototype, 'icon', {
      get: function () {
	return '/public/images/filetype/16px/' + this.format.extension + '.png';
      }
    });

    SlotAsset.prototype.save = function (data) {
      var a = this;
      return router.http(router.controllers.AssetApi.update, this.asset.id, data)
	.then(function (res) {
	  if ('excerpt' in data)
	    a.volume.clear('excerpts');
	  return 'id' in res.data ? a.asset.update(res.data) : a.update(res.data);
	});
    };

    Slot.prototype.createAsset = function (data) {
      var s = this;
      data.container = this.container.id;
      if (!('position' in data) && isFinite(this.segment.l))
	data.position = this.segment.l;
      return router.http(router.controllers.AssetApi.upload, this.volume.id, data)
	.then(function (res) {
	  s.clear('assets');
	  return assetMake(s.volume, res.data);
	});
    };

    SlotAsset.prototype.replace = function (data) {
      var s = this;
      return router.http(router.controllers.AssetApi.replace, this.asset.id, data)
	.then(function (res) {
	  var a = assetMake(s.volume, res.data);
	  if (a instanceof SlotAsset) {
	    s.clear('assets');
	    return a;
	  } else /* Asset */ {
	    s.asset = a;
	    return s;
	  }
	});
    };

    SlotAsset.prototype.remove = function () {
      var s = this;
      return router.http(router.controllers.AssetApi.remove, this.asset.id)
	.then(function (res) {
	  s.clear('assets');
	  return s.asset.update(res.data);
	});
    };

    SlotAsset.prototype.thumbRoute = function (size) {
      return router.assetThumb([this.container.id, this.segment.format(), this.asset.id, size]);
    };

    SlotAsset.prototype.downloadRoute = function (inline) {
      return router.assetDownload([this.container.id, this.segment.format(), this.asset.id, inline]);
    };

    SlotAsset.prototype.editRoute = function () {
      return router.assetEdit([this.asset.id]);
    };

    ///////////////////////////////// Comment

    function Comment(context, init) {
      Slot.call(this, context, init);
    }

    Comment.prototype = Object.create(Slot.prototype);
    Comment.prototype.constructor = Comment;
    Comment.prototype.class = 'comment';

    Comment.prototype.staticFields = ['parent'].concat(Slot.prototype.staticFields);

    Comment.prototype.init = function (init) {
      Slot.prototype.init.call(this, init);
      if ('who' in init)
	this.who = partyMake(init.who);
    };

    function commentMakeArray(volume, l) {
      if (l) for (var i = 0; i < l.length; i ++)
	l[i] = new Comment(volume || volumeMake(l[i].container.volume), l[i]);
      return l;
    }

    Slot.prototype.postComment = function (data, reply) {
      var s = this;
      if (arguments.length < 2 && this instanceof Comment)
	reply = this.id;
      return router.http(router.controllers.CommentApi.post, this.container.id, this.segment.format(), reply, data)
	.then(function (res) {
	  s.volume.clear('comments');
	  s.clear('comments');
	  return new Comment(s.container, res.data);
	});
    };

    ///////////////////////////////// Tag
    
    // no point in a model, really
    var Tag = {};

    Tag.search = function (query) {
      return router.http(router.controllers.TagApi.search, query)
	.then(function(res){
	  //do other stuff?
	  return res.data;
      });
    };

    Slot.prototype.setTag = function (tag, vote) {
      var s = this;
      return router.http(router.controllers.TagApi.update, tag, this.container.id, this.segment.format(), {vote:vote ? vote>0 : undefined})
	.then(function (res) {
	  s.volume.clear('tags');
	  s.clear('tags');
	  return res.data;
	});
    };

    /////////////////////////////////

    return {
      Segment: Segment,
      Party: Party,
      Login: Login,
      Volume: Volume,
      Container: Container,
      Slot: Slot,
      Record: Record,
      Asset: Asset,
      SlotAsset: SlotAsset,
      Comment: Comment,
      Tag: Tag,

      funder: function (query, all) {
	return router.http(router.controllers.VolumeApi.funderSearch, query, all)
	  .then(resData);
      },
      cite: function (url) {
	return router.http(router.controllers.SiteApi.cite, {url:url})
	  .then(resData);
      },
      analytic: function () {
	return router.http(router.controllers.SiteApi.void, {}, {cache:false});
      },

    };
  }
]);
