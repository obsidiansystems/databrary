{-# LANGUAGE FunctionalDependencies, TemplateHaskell #-}
module Databrary.Model.SQL.Select
  ( SelectOutput(..)
  , Selector
  , selectOutput
  , selector
  , selectColumns
  , addSelects
  , fromMap
  , crossJoin
  , joinOn
  , joinUsing
  , maybeJoinOn
  , maybeJoinUsing
  , selectJoin
  , selectMap
  , makeQuery
  , selectQuery
  , nameRef
  ) where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Monad.State (StateT(..))
import Control.Monad.Trans.Class (lift)
import Data.Char (isLetter, toLower)
import Data.List (intercalate, unfoldr)
import Data.String (IsString(..))
import Database.PostgreSQL.Typed.Query (QueryFlags, parseQueryFlags, makePGQuery)
import qualified Language.Haskell.TH as TH

import Databrary.Service.DB (useTPG)

data SelectOutput
  = OutputExpr String
  | OutputJoin !Bool TH.Name [SelectOutput]
  | OutputMap !Bool (TH.Exp -> TH.Exp) SelectOutput

instance IsString SelectOutput where
  fromString = OutputExpr

selectColumn :: String -> String -> SelectOutput
selectColumn t c = OutputExpr $ t ++ '.' : c

_outputTuple :: [SelectOutput] -> SelectOutput
_outputTuple l = OutputJoin False (TH.tupleDataName $ length l) l

outputMaybe :: SelectOutput -> SelectOutput
outputMaybe (OutputJoin False f l) = OutputJoin True f l
outputMaybe (OutputMap False f l) = OutputMap True f l
outputMaybe s = s

outputColumns :: SelectOutput -> [String]
outputColumns (OutputExpr s) = [s]
outputColumns (OutputJoin _ _ o) = concatMap outputColumns o
outputColumns (OutputMap _ _ o) = outputColumns o

outputParser :: SelectOutput -> StateT [TH.Name] TH.Q TH.Exp
outputParser (OutputExpr _) = StateT st where
  st (i:l) = return (TH.VarE i, l)
  st [] = fail "outputParser: insufficient values"
outputParser (OutputJoin mb f ol) = do
  fi <- lift $ TH.reify f
  (fe, ft) <- case fi of
    TH.ClassOpI _ t _ _ -> return (TH.VarE f, t)
    TH.DataConI _ t _ _ -> return (TH.ConE f, t)
    TH.VarI _ t _ _ -> return (TH.VarE f, t)
    _ -> die "wrong kind"
  if mb
    then do
      let am = unfoldr argMaybe ft
      (bl, ae) <- bindArgs am ol
      -- when (null bl) $ die "function with at least one non-Maybe argument required"
      return $ TH.DoE $ bl ++ [TH.NoBindS $ TH.AppE (TH.ConE 'Just) $ foldl TH.AppE fe ae]
    else foldl TH.AppE fe <$> mapM outputParser ol
  where
  bindArgs (False:m) (o:l) = do
    n <- lift $ TH.newName "cm"
    a <- outputParser (outputMaybe o)
    (bl, al) <- bindArgs m l
    return $ (TH.BindS (TH.VarP n) a : bl, TH.VarE n : al)
  bindArgs (True:m) (o:l) = do
    a <- outputParser o
    second (a:) <$> bindArgs m l
  bindArgs _ o = (,) [] <$> mapM outputParser o
  argMaybe (TH.ArrowT `TH.AppT` a `TH.AppT` r) = Just (isMaybeT a, r)
  argMaybe _ = Nothing
  isMaybeT (TH.AppT (TH.ConT m) _) = m == ''Maybe
  isMaybeT _ = False
  die s = fail $ "outputParser " ++ show f ++ ": " ++ s
outputParser (OutputMap False f o) =
  f <$> outputParser o
outputParser (OutputMap True f o) =
  f <$> outputParser (outputMaybe o) -- might want to lift f over the maybe

data Selector = Selector
  { selectOutput :: SelectOutput
  , selectSource :: String
  , selectJoined :: String
  }

selector :: String -> SelectOutput -> Selector
selector t o = Selector o t (',':t)

selectColumns :: TH.Name -> String -> [String] -> Selector
selectColumns f t c =
  selector t $ OutputJoin False f $ map (selectColumn t) c

addSelects :: TH.Name -> Selector -> [SelectOutput] -> Selector
addSelects f s c = s
  { selectOutput = OutputJoin False f (selectOutput s : c) }

fromMap :: (String -> String) -> Selector -> Selector
fromMap f sel = sel
  { selectSource = f $ selectSource sel
  , selectJoined = f $ selectJoined sel
  }

joinWith :: (String -> String) -> Selector -> Selector
joinWith j sel = sel{ selectJoined = j (selectSource sel) }

maybeJoinWith :: (String -> String) -> Selector -> Selector
maybeJoinWith j sel = sel
  { selectJoined = j (selectSource sel)
  , selectOutput = outputMaybe (selectOutput sel) }

crossJoin :: Selector -> Selector
crossJoin = joinWith (" CROSS JOIN " ++)

joinOn :: String -> Selector -> Selector
joinOn on = joinWith (\s -> " JOIN " ++ s ++ " ON " ++ on)

joinUsing :: [String] -> Selector -> Selector
joinUsing using = joinWith (\s -> " JOIN " ++ s ++ " USING (" ++ intercalate "," using ++ ")")

maybeJoinOn :: String -> Selector -> Selector
maybeJoinOn on = maybeJoinWith (\s -> " LEFT JOIN " ++ s ++ " ON " ++ on)

maybeJoinUsing :: [String] -> Selector -> Selector
maybeJoinUsing using = maybeJoinWith (\s -> " LEFT JOIN " ++ s ++ " USING (" ++ intercalate "," using ++ ")")

selectJoin :: TH.Name -> [Selector] -> Selector
selectJoin f l@(h:t) = Selector
  { selectOutput = OutputJoin False f $ map selectOutput l
  , selectSource = selectSource h ++ joins
  , selectJoined = selectJoined h ++ joins
  } where joins = concatMap selectJoined t
selectJoin _ [] = error "selectJoin: empty list"

selectMap :: (TH.Exp -> TH.Exp) -> Selector -> Selector
selectMap f s = s{ selectOutput = OutputMap False f (selectOutput s) }


takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p = fst . foldr go ([], False) where
  go x (rest, done)
    | not done && p x = (x:rest, False)
    | otherwise = (rest, True)

makeQuery :: QueryFlags -> (String -> String) -> SelectOutput -> TH.ExpQ
makeQuery flags sql output = do
  _ <- useTPG
  nl <- mapM (TH.newName . colVar) cols
  (parse, []) <- runStateT (outputParser output) nl
  TH.AppE (TH.VarE 'fmap `TH.AppE` TH.LamE [TH.TupP $ map TH.VarP nl] parse)
    <$> makePGQuery flags (sql $ intercalate "," cols)
  where
  colVar s = case takeWhileEnd isLetter s of
    [] -> "c"
    (h:l) -> toLower h : l
  cols = outputColumns output

selectQuery :: Selector -> String -> TH.ExpQ
selectQuery (Selector{ selectOutput = o, selectSource = s }) sqlf =
  makeQuery flags (\c -> "SELECT " ++ c ++ " FROM " ++ s ++ ' ':sql) o
  where (flags, sql) = parseQueryFlags sqlf

nameRef :: TH.Name -> String
nameRef n = maybe b (++ '.' : b) $ TH.nameModule n where b = TH.nameBase n