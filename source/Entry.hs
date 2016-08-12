{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

module Entry
  ( Entry' (..)
  , Entry'' (..)
  -- , Value (..)
  , info
  , varType
  ) where
--------------------------------------------------------------------------------
import           AST.Expression
import           AST.Type
import           Location
import           Treelike
--------------------------------------------------------------------------------
import           Control.Lens   (makeLenses)
import           Data.Monoid    ((<>))
import           Data.Text      (Text, unpack)
--------------------------------------------------------------------------------

-- data Value = I Integer | C Char | F Double {-| S String-} | B Bool | None
--   deriving (Eq)

-- instance Show Value where
--   show (I i) = show i
--   show (C c) = show c
--   show (F f) = show f
--   show (B b) = show b
--   show None  = "None"

data Entry'' s
  = Var
    { _varType  :: Type
    , _varValue :: Maybe Expression }
  | Const
    { _constType  :: Type
    , _constValue :: Expression }
  | Argument
    { _argMode :: ArgMode
    , _argType :: Type }
  | Function
    { _funcType   :: Type
    , _funcParams :: [(Text,Type)]
    , _funcTable  :: s }
  | Procedure
    { _procParams  :: [(Text,Type)]
    , _procTable :: s }
  | AbstractTypeEntry
  | TypeEntry
  deriving (Eq)

makeLenses ''Entry''


data Entry' s
  = Entry
    { _entryName :: Text
    , _loc       :: Location
    , _info      :: Entry'' s }

makeLenses ''Entry'


instance Treelike (Entry' s) where
  toTree Entry { _entryName, _loc, _info } = case _info of

    Var { _varType, _varValue } ->
      Node ("Variable `" <> unpack _entryName <> "` " <> show _loc)
        [ leaf ("Type: " <> show _varType)
        , case _varValue of
            Nothing     -> leaf "Not initialized"
            Just value  -> Node "Initial value: " [toTree value] ]

    Const { _constType, _constValue } ->
      Node ("Constant `" <> unpack _entryName <> "` " <> show _loc)
        [ leaf $  "Type: " <> show _constType
        , Node "Value" [toTree _constValue] ]

    Argument { _argMode, _argType } ->
      Node ("Argument `" <> unpack _entryName <> "` " <> show _loc)
        [ leaf $ "Type: " <> show _argType
        , leaf $ "Mode: " <> show _argMode ]

    Function { _funcType, _funcParams, _funcTable } ->
      Node ("Function `" <> unpack _entryName <> "` " <> show _loc)
        [ leaf $ show _funcType ]

    Procedure { _procParams, _procTable } ->
      leaf ("Procedure `" <> unpack _entryName <> "` " <> show _loc)

    AbstractTypeEntry {} ->
      leaf ("Abstract Data Type `" <> unpack _entryName <> "` " <> show _loc)

    TypeEntry {} ->
      leaf ("Data Type `" <> unpack _entryName <> "` " <> show _loc)
