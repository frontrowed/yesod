{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Warning! This module is considered internal and may have breaking changes
module Yesod.Routes.TH.Types
    ( -- * Data types
      Resource (..)
    , ResourceTree (..)
    , AnyPiece (..)
    , Piece (..)
    , Query (..)
    , Dispatch (..)
    , CheckOverlap
    , FlatResource (..)
      -- ** Helper functions
    , resourceMulti
    , resourceTreePieces
    , resourceTreeName
    , flatten
    ) where

import Language.Haskell.TH.Syntax

data ResourceTree typ
    = ResourceLeaf (Resource typ)
    | ResourceParent String CheckOverlap [Piece typ] [Query typ] [ResourceTree typ]
    deriving Functor

resourceTreePieces :: ResourceTree typ -> [Piece typ]
resourceTreePieces (ResourceLeaf r) = resourcePieces r
resourceTreePieces (ResourceParent _ _ x _ _) = x

resourceTreeName :: ResourceTree typ -> String
resourceTreeName (ResourceLeaf r) = resourceName r
resourceTreeName (ResourceParent x _ _ _ _) = x

instance Lift t => Lift (ResourceTree t) where
    lift (ResourceLeaf r) = [|ResourceLeaf $(lift r)|]
    lift (ResourceParent a b c d e) = [|ResourceParent $(lift a) $(lift b) $(lift c) $(lift d) $(lift e)|]

data Resource typ = Resource
    { resourceName :: String
    , resourcePieces :: [Piece typ]
    , resourceQueries :: [Query typ]
    , resourceDispatch :: Dispatch typ
    , resourceAttrs :: [String]
    , resourceCheck :: CheckOverlap
    }
    deriving (Show, Functor)

type CheckOverlap = Bool

instance Lift t => Lift (Resource t) where
    lift (Resource a b c d e f) = [|Resource a b c d e f|]

data Piece typ = Static String | Dynamic typ
    deriving (Show, Eq, Read)

instance Functor Piece where
    fmap _ (Static s) = (Static s)
    fmap f (Dynamic t) = Dynamic (f t)

instance Lift t => Lift (Piece t) where
    lift (Static s) = [|Static $(lift s)|]
    lift (Dynamic t) = [|Dynamic $(lift t)|]

data Query typ = Query
    { paramName :: String
    , queryType :: typ
    }
    deriving (Eq, Show, Read, Functor)

instance Lift t => Lift (Query t) where
    lift (Query p t) = [|Query $(lift p) $(lift t)|]

data AnyPiece typ = UrlPiece (Piece typ) | QueryPiece (Query typ)
    deriving (Eq, Show, Read, Functor)

instance Lift t => Lift (AnyPiece t) where
    lift (UrlPiece t) = [|UrlPiece $(lift t)|]
    lift (QueryPiece t) = [|QueryPiece $(lift t)|]

data Dispatch typ =
    Methods
        { methodsMulti :: Maybe typ -- ^ type of the multi piece at the end
        , methodsMethods :: [String] -- ^ supported request methods
        }
    | Subsite
        { subsiteType :: typ
        , subsiteFunc :: String
        }
    deriving Show

instance Functor Dispatch where
    fmap f (Methods a b) = Methods (fmap f a) b
    fmap f (Subsite a b) = Subsite (f a) b

instance Lift t => Lift (Dispatch t) where
    lift (Methods Nothing b) = [|Methods Nothing $(lift b)|]
    lift (Methods (Just t) b) = [|Methods (Just $(lift t)) $(lift b)|]
    lift (Subsite t b) = [|Subsite $(lift t) $(lift b)|]

resourceMulti :: Resource typ -> Maybe typ
resourceMulti Resource { resourceDispatch = Methods (Just t) _ } = Just t
resourceMulti _ = Nothing

data FlatResource a = FlatResource
    { frParentPieces :: [(String, [Piece a], [Query a])]
    , frName :: String
    , frPieces :: [Piece a]
    , frQueries :: [Query a]
    , frDispatch :: Dispatch a
    , frCheck :: Bool
    }

flatten :: [ResourceTree a] -> [FlatResource a]
flatten =
    concatMap (go id True)
  where
    go front check' (ResourceLeaf (Resource a b c d _ check)) = [FlatResource (front []) a b c d (check' && check)]
    go front check' (ResourceParent name check pieces queries children) =
        concatMap (go (front . ((name, pieces, queries):)) (check && check')) children
