{-# LANGUAGE TemplateHaskell #-}
module Yesod.Routes.TH.ParseRoute
    ( -- ** ParseRoute
      mkParseRouteInstance
    ) where

import Yesod.Routes.TH.Types
import Language.Haskell.TH.Syntax
import Data.Text (Text)
import Yesod.Routes.Class
import Yesod.Routes.TH.Dispatch
import Data.List (sortBy, elemIndex)
import Data.Function (on)

mkParseRouteInstance :: Type -> [ResourceTree a] -> Q Dec
mkParseRouteInstance typ ress = do
    cls <- mkDispatchClause
        MkDispatchSettings
            { mdsRunHandler = [|\_ _ x _ -> x|]
            , mds404 = [|error "mds404"|]
            , mds405 = [|error "mds405"|]
            , mdsGetPathInfo = [|\(paths, _queries) -> paths|]
            , mdsGetQueryInfo = [|\(_paths, queries) -> repeat queries|]
            , mdsMethod = [|error "mdsMethod"|]
            , mdsGetHandler = \_ _ -> [|error "mdsGetHandler"|]
            , mdsSetPathInfo = [|\p (_, q) -> (p, q)|]
            , mdsSubDispatcher = [|\_runHandler _getSub toMaster _env -> fmap toMaster . parseRoute|]
            , mdsUnwrapper = return
            }
        (map removeMethods ress)
    helper <- newName "helper"
    fixer <- [|(\f x -> f () x) :: (() -> ([Text], [(Text, Text)]) -> Maybe (Route a)) -> ([Text], [(Text, Text)]) -> Maybe (Route a)|]
    return $ InstanceD [] (ConT ''ParseRoute `AppT` typ)
        [ FunD 'parseRoute $ return $ Clause
            []
            (NormalB $ fixer `AppE` VarE helper)
            [FunD helper [cls]]
        ]
  where
    -- We do this in order to ski the unnecessary method parsing
    removeMethods (ResourceLeaf res) = ResourceLeaf $ removeMethodsLeaf res
    removeMethods (ResourceParent v w x y z) = ResourceParent v w x y $ map removeMethods z

    removeMethodsLeaf res = res { resourceDispatch = fixDispatch $ resourceDispatch res }

    fixDispatch (Methods x _) = Methods x []
    fixDispatch x = x

--  Put the query params in the order in which they are expected by the
--  handler
{-queryParamsInOrder :: [Text] -> Q Exp [(Text, Text)] -> [Text]-}
{-queryParamsInOrder q = [| \qs -> snd <$> sortBy (go `on` fst) (filter isWanted qs)-}
  {-where-}
    {-isWanted (p, _) = p `elem` q-}
    {-go x y = case (elemIndex x q, elemIndex y q) of-}
                    {-(Just ix, Just iy) -> ix `compare` iy-}
                    {-_                  -> error "impossible" filtered-}
