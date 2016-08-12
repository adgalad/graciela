{-# LANGUAGE NamedFieldPuns #-}

module AST.Expression
  ( BinaryOperator (..)
  , Conversion (..)
  , Expression (..)
  , Expression' (..)
  , Object (..)
  , QRange (..)
  , QuantOperator (..)
  , UnaryOperator (..)
  , from
  , to
  )
  where
--------------------------------------------------------------------------------
import           AST.Object    (Object')
import           Location
import           Treelike
import           Type          (Type')
--------------------------------------------------------------------------------
import           Data.Foldable (toList)
import           Data.Monoid   ((<>))
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq (Seq)
import           Data.Text     (Text, unpack)
import           Prelude       hiding (Ordering (..))
--------------------------------------------------------------------------------

type Object = Object' Expression
type Type   = Type' Expression


data Conversion = ToInt | ToDouble | ToChar
  deriving (Eq)

instance Show Conversion where
  show ToInt    = "to int"
  show ToDouble = "to double"
  show ToChar   = "to char"


data BinaryOperator
  = Plus | BMinus | Times | Div | Mod | Power | Max | Min
  | And | Or | Implies | Consequent | BEQ | BNE
  | AEQ | ANE | LT | LE | GT | GE
  | Elem | NotElem | Difference | Intersection | Union
  deriving (Eq)

instance Show BinaryOperator where
  show Plus   = "(+)"
  show BMinus = "(-)"
  show Times  = "(*)"
  show Div    = "(/)"
  show Mod    = "mod"
  show Power  = "(^)"
  show Max    = "max"
  show Min    = "min"

  show And        = "(/\\)"
  show Or         = "(\\/)"
  show Implies    = "(==>)"
  show Consequent = "(<==)"
  show BEQ        = "(===)"
  show BNE        = "(!==)"

  show AEQ = "(==)"
  show ANE = "(!=)"
  show LT  = "(<)"
  show LE  = "(<=)"
  show GT  = "(>)"
  show GE  = "(>=)"

  show Elem         = "Member of (∈)"
  show NotElem      = "Not Member of (∉)"
  show Difference   = "Difference (∖)"
  show Intersection = "Intersection (∩)"
  show Union        = "Union (∪)"


data UnaryOperator = UMinus | Not | Abs | Sqrt | Pred | Succ
  deriving (Eq)

instance Show UnaryOperator where
  show Abs    = "abs"
  show UMinus = "(-)"
  show Not    = "not"
  show Sqrt   = "sqrt"
  show Pred   = "pred"
  show Succ   = "succ"


data QuantOperator
  = ForAll    | Exists
  | Summation | Product
  | Minimum   | Maximum
  | Count
  deriving(Eq)

instance Show QuantOperator where
  show ForAll    = "Forall (∀)"
  show Exists    = "Exists (∃)"
  show Summation = "Summation (∑)"
  show Product   = "Product (∏)"
  show Minimum   = "Minimum (min)"
  show Maximum   = "Maximum (max)"
  show Count     = "Count (#)"


data QRange
  = ExpRange -- Both limits are included, i.e. low <= var <= high
    { low  :: Expression
    , high :: Expression }
  | SetRange -- ^ Works for Multiset as well
    { theSet :: Expression }
  | PointRange
    { thePoint :: Expression }
  | EmptyRange
  deriving (Eq)


instance Treelike QRange where
  toTree ExpRange { low, high } =
    Node "Exp Range"
      [ Node "From" [toTree low]
      , Node "To"   [toTree high] ]

  toTree SetRange { theSet } =
    Node "Set Range"
      [ Node "Over" [toTree theSet] ]

  toTree PointRange { thePoint } =
    Node "One-point Range"
      [ Node "At" [toTree thePoint] ]

  toTree EmptyRange =
    leaf "Empty Range"


data Expression'
  = BoolLit   { theBool :: Bool }
  | CharLit   { theChar :: Char }
  | FloatLit  { theFloat :: Double }
  | IntLit    { theInt :: Integer }
  | StringLit { theString :: String  }

  | EmptySet
  | EmptyMultiset

  | Obj       { theObj :: Object }

  | Binary
    { binOp :: BinaryOperator
    , lexpr :: Expression
    , rexpr :: Expression }

  | Unary
    { unOp  :: UnaryOperator
    , inner :: Expression }

  | FunctionCall -- ^ Llamada a funcion.
    { fname :: Text
    -- , astST :: SymbolTable  -- ?
    , args  :: [Expression] }

  | Conversion
    { toType :: Conversion
    , cExp   :: Expression }

  | Quantification
    { qOp      :: QuantOperator
    , qVar     :: Text
    , qVarType :: Type
    , qRange   :: QRange
    , qCond    :: Expression
    , qBody    :: Expression }

  | EConditional -- ^ Expresión If.
    { eguards :: Seq (Expression, Expression) }

  | ESkip
  deriving (Eq)

data Expression
  = Expression
    { loc      :: Location
    , expType  :: Type
    , constant :: Bool
    , exp'     :: Expression' }
  | BadExpression
    { loc :: Location }
  deriving (Eq)


instance Treelike Expression where
  toTree Expression { loc, expType, exp' } = case exp' of
    BoolLit   { theBool   } -> leaf $
      "Bool Literal `"   <> show theBool   <> "` " <> show loc

    CharLit   { theChar   } -> leaf $
      "Char Literal "    <> show theChar   <> " "  <> show loc

    FloatLit  { theFloat  } -> leaf $
      "Float Literal `"  <> show theFloat  <> "` " <> show loc

    IntLit    { theInt    } -> leaf $
      "Int Literal `"    <> show theInt    <> "` " <> show loc

    StringLit { theString } -> leaf $
      "String Literal `" <> show theString <> "` " <> show loc

    EmptySet -> leaf $
      "Set Literal `Empty Set` " <> show loc

    EmptyMultiset -> leaf $
      "Multiset Literal `Empty Multiset` " <> show loc

    Obj { theObj } ->
      Node ("Object " <> show loc)
        [ toTree theObj ]

    Binary { binOp, lexpr, rexpr } ->
      Node (show binOp <> " " <> show loc)
        [ toTree lexpr
        , toTree rexpr ]

    Unary { unOp, inner } ->
      Node (show unOp <> " " <> show loc)
        [ toTree inner ]

    FunctionCall { fname, {-astST,-} args } ->
      Node ("Call Func " <> unpack fname <> " " <> show loc)
        [ Node "Arguments" (toForest args) ]

    Conversion { toType, cExp } ->
      Node (show toType <> " " <> show loc)
        [ toTree cExp ]

    Quantification { qOp, qVar, qVarType, qRange, qCond, qBody } ->
      Node ("Quantification " <> show qOp <> " " <> show loc)
        [ Node "Variable"
          [ leaf $ unpack qVar
          , leaf $ "of type " <> show qVarType ]
        , Node "Range" [toTree qRange]
        , case qCond of
            Expression { exp' = ESkip } -> leaf "No Conditions"
            _ -> Node "Conditions" [ toTree qCond ]
        , Node "Body" [ toTree qBody ] ]

    EConditional { eguards } ->
      Node ("Conditional Expression " <> show loc)
        ( fmap g . toList $ eguards )

      where
        g (lhs, rhs) =
          Node "Guard"
            [ Node "If"   [toTree lhs]
            , Node "Then" [toTree rhs] ]

    ESkip ->
      leaf "Skip Expression"

  toTree BadExpression { loc } =
    leaf $ "Bad Expression " <> show loc


from :: Expression -> SourcePos
from e = let Location (f,_) = loc e in f

to :: Expression -> SourcePos
to e =   let Location (_,t) = loc e in t


instance Show Expression where
  show Expression { loc, expType, exp' } = case exp' of
    BoolLit   { theBool   } -> show theBool

    CharLit   { theChar   } -> [theChar]

    FloatLit  { theFloat  } -> show theFloat

    IntLit    { theInt    } -> show theInt

    StringLit { theString } -> show theString

    EmptySet -> "{}"

    EmptyMultiset -> "{{}}"

    Obj { theObj } -> show theObj

    Binary { binOp, lexpr, rexpr } -> 
      show lexpr <> (init . tail . show) binOp <> show rexpr

    Unary { unOp, inner } -> show unOp ++ show inner

    FunctionCall { fname, {-astST,-} args } -> 
      unpack fname <> "(" <> (concat . fmap show) args <> ")"

    Conversion { toType, cExp } -> 
      show toType <> "(" <> show cExp <> ")"

    Quantification { qOp, qVar, qVarType, qRange, qCond, qBody } -> "quantifier"

    EConditional { eguards } -> 
      "if " <> (concat . fmap showG . toList) eguards <> "fi"

      where
        showG (lhs, rhs) =
          show lhs <> " -> " <> show rhs <> "[]"

    ESkip -> "skip"

  show BadExpression {} = ""