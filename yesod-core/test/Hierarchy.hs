{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Hierarchy
    ( hierarchy
    , Dispatcher (..)
    , runHandler
    , Handler2
    , App
    , toText
    , Env (..)
    , subDispatch
    ) where

import Test.Hspec
import Test.HUnit
import Yesod.Routes.Parse
import Yesod.Routes.TH
import Yesod.Routes.Class
import Language.Haskell.TH.Syntax
import Data.Text (Text, pack, unpack, append)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Set as Set

class ToText a where
    toText :: a -> Text

instance ToText Text where toText = id
instance ToText String where toText = pack

type Handler2 sub master a = a
type Handler site a = Handler2 site site a

type Request = ([Text], [(Text, Text)], ByteString) -- path info, queries, method
type App sub master = Request -> (Text, Maybe (Route master))
data Env sub master = Env
    { envToMaster :: Route sub -> Route master
    , envSub :: sub
    , envMaster :: master
    }

subDispatch
    :: (Env sub master -> App sub master)
    -> (Handler2 sub master Text -> Env sub master -> Maybe (Route sub) -> App sub master)
    -> (master -> sub)
    -> (Route sub -> Route master)
    -> Env master master
    -> App sub master
subDispatch handler _runHandler getSub toMaster env req =
    handler env' req
  where
    env' = env
        { envToMaster = envToMaster env . toMaster
        , envSub = getSub $ envMaster env
        }

class Dispatcher sub master where
    dispatcher :: Env sub master -> App sub master

runHandler
    :: ToText a
    => Handler2 sub master a
    -> Env sub master
    -> Maybe (Route sub)
    -> App sub master
runHandler h Env {..} route _ = (toText h, fmap envToMaster route)

data Hierarchy = Hierarchy

do
    let resources = [parseRoutes|
/ HomeR GET

/!#Int BackwardsR GET

/query?q=#Int QueryR GET

/captureQueries/#Int?s=#String&b=#Bool CaptureQueriesR GET

/admin/#Int AdminR:
    /            AdminRootR GET
    /login       LoginR     GET POST
    /table/#Text TableR     GET

/nest/ NestR !NestingAttr:

  /spaces      SpacedR   GET !NonNested

  /nest2 Nest2:
    /           GetPostR  GET POST
    /get        Get2      GET
    /post       Post2         POST
--    /#Int       Delete2            DELETE
  /nest3 Nest3:
    /get        Get3      GET
    /post       Post3         POST
--    /#Int       Delete3            DELETE

/afterwards AfterR !parent !key=value1:
  /             After     GET !child !key=value2

-- /trailing-nest TrailingNestR:
--  /foo TrailingFooR GET
--  /#Int TrailingIntR GET
|]

    rrinst <- mkRenderRouteInstance (ConT ''Hierarchy) $ map (fmap parseType) resources
    rainst <- mkRouteAttrsInstance (ConT ''Hierarchy) $ map (fmap parseType) resources
    prinst <- mkParseRouteInstance (ConT ''Hierarchy) $ map (fmap parseType) resources
    dispatch <- mkDispatchClause MkDispatchSettings
        { mdsRunHandler = [|runHandler|]
        , mdsSubDispatcher = [|subDispatch|]
        , mdsGetPathInfo = [|\(paths, _queries, _method) -> paths |]
        , mdsGetQueryInfo = [|\(_paths, queries, _method) -> repeat queries |]
        , mdsMethod = [|\(_paths, _queries, method) -> method|]
        , mdsSetPathInfo = [|\p (_, m) -> (p, m)|]
        , mds404 = [|pack "404"|]
        , mds405 = [|pack "405"|]
        , mdsGetHandler = defaultGetHandler
        , mdsUnwrapper = return
        } resources
    return
        $ InstanceD
            []
            (ConT ''Dispatcher
                `AppT` ConT ''Hierarchy
                `AppT` ConT ''Hierarchy)
            [FunD (mkName "dispatcher") [dispatch]]
        : prinst
        : rainst
        : rrinst

getSpacedR :: Handler site String
getSpacedR = "root-leaf"

getGet2   :: Handler site String; getGet2 = "get"
postPost2 :: Handler site String; postPost2 = "post"
deleteDelete2 :: Int -> Handler site String; deleteDelete2 = const "delete"
getGet3   :: Handler site String; getGet3 = "get"
postPost3 :: Handler site String; postPost3 = "post"
deleteDelete3   :: Int -> Handler site String; deleteDelete3 = const "delete"

getAfter   :: Handler site String; getAfter = "after"

getHomeR :: Handler site String
getHomeR = "home"

getBackwardsR :: Int -> Handler site Text
getBackwardsR _ = pack "backwards"

getAdminRootR :: Int -> Handler site Text
getAdminRootR i = pack $ "admin root: " ++ show i

getLoginR :: Int -> Handler site Text
getLoginR i = pack $ "login: " ++ show i

postLoginR :: Int -> Handler site Text
postLoginR i = pack $ "post login: " ++ show i

getTableR :: Int -> Text -> Handler site Text
getTableR _ = append "TableR "

getGetPostR :: Handler site Text
getGetPostR = pack "get"

postGetPostR :: Handler site Text
postGetPostR = pack "post"

getQueryR :: Int -> Handler site Text
getQueryR i = pack $ "query param: " ++ show i

getCaptureQueriesR :: Int -> String -> Bool -> Handler site Text
getCaptureQueriesR i s b = pack $ "capture: " ++ show i
                               ++ " query1: " ++ s
                               ++ " query2: " ++ show b

hierarchy :: Spec
hierarchy = describe "hierarchy" $ do
    it "nested with spacing" $
        renderRoute (NestR SpacedR) @?= (["nest", "spaces"], [])
    it "renders root correctly" $
        renderRoute (AdminR 5 AdminRootR) @?= (["admin", "5"], [])
    it "renders table correctly" $
        renderRoute (AdminR 6 $ TableR "foo") @?= (["admin", "6", "table", "foo"], [])
    it "renders queries correctly" $
        renderRoute (QueryR 1) @?= (["query"], [("q", "1")])
    let disp m queries ps = dispatcher
            (Env
                { envToMaster = id
                , envMaster = Hierarchy
                , envSub = Hierarchy
                })
            (map pack ps, queries, S8.pack m)

    let testGetPost route getRes postRes = do
          let routeStrs = map unpack $ fst (renderRoute route)
          disp "GET" [] routeStrs @?= (getRes, Just route)
          disp "POST" [] routeStrs @?= (postRes, Just route)

    it "dispatches routes with multiple METHODs: admin" $
        testGetPost (AdminR 1 LoginR) "login: 1" "post login: 1"

    it "dispatches routes with multiple METHODs: nesting" $
        testGetPost (NestR $ Nest2 GetPostR) "get" "post"

    it "dispatches queries correctly" $ do
        disp "GET" [("q", "1")] ["query"] @?= ("query param: 1", Just (QueryR 1))
        disp "GET" [("s", "foo"), ("b", "True")] ["captureQueries", "1"]
            @?= ("capture: 1 query1: foo query2: True", Just (CaptureQueriesR 1 "foo" True))

    it "accepts queries in any order" $ do
        disp "GET" [("b", "True"), ("s", "foo")] ["captureQueries", "1"]
            @?= ("capture: 1 query1: foo query2: True", Just (CaptureQueriesR 1 "foo" True))

    it "dispatches root correctly" $
      disp "GET" [] ["admin", "7"] @?= ("admin root: 7", Just $ AdminR 7 AdminRootR)
    it "dispatches table correctly" $
      disp "GET" [] ["admin", "8", "table", "bar"] @?= ("TableR bar", Just $ AdminR 8 $ TableR "bar")
    it "parses" $ do
        parseRoute ([], []) @?= Just HomeR
        parseRoute ([], [("foo", "bar")]) @?= Just HomeR
        parseRoute (["query"], [("q", "5")]) @?= Just (QueryR 5)
        parseRoute (["admin", "5"], []) @?= Just (AdminR 5 AdminRootR)
        parseRoute (["admin!", "5"], []) @?= (Nothing :: Maybe (Route Hierarchy))
    it "inherited attributes" $ do
        routeAttrs (NestR SpacedR) @?= Set.fromList ["NestingAttr", "NonNested"]
    it "pair attributes" $
        routeAttrs (AfterR After) @?= Set.fromList ["parent", "child", "key=value2"]
