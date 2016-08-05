{-# LANGUAGE NamedFieldPuns #-}

module AST.Object
  ( Object' (..)
  , Object'' (..)
  ) where
--------------------------------------------------------------------------------
import           Location
import           Treelike
import           Type        (Type)
--------------------------------------------------------------------------------
import           Data.Monoid ((<>))
import           Data.Text   (Text, unpack)
--------------------------------------------------------------------------------

data Object'' e
  = Variable
    { name :: Text
    }
  | Member
    { inner :: Object' e
    , field :: Text
    }
  | Index
    { inner :: Object' e
    , index :: e
    }
  | Deref
    { inner :: Object' e
    }

{- The type variable in `Object' e` allows us to separate this code from
 - the code in `Expression` without creating a cycle.
 -}
data Object' e
  = Object
    { loc     :: Location
    , objType :: Type
    , obj'    :: Object'' e
    }
  | BadObject
    { loc     :: Location
    }

instance Treelike e => Treelike (Object' e) where
  toTree Object { loc, objType, obj' } = case obj' of

    Variable { name } ->
      leaf $ "Variable `" <> unpack name <> "` " <> show loc

    Member { inner, field } ->
      Node ("Member " <> show loc)
        [ toTree inner
        , leaf $ "Field `" <> unpack field <> "`"
        ]

    Index  { inner, index } ->
      Node ("Index " <> show loc)
        [ toTree inner
        , toTree index
        ]

    Deref  { inner } ->
      Node ("Deref " <> show loc)
        [ toTree inner
        ]

  toTree BadObject { loc } =
    leaf $ "Bad Object " <> show loc