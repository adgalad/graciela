{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.Graciela.AST.Definition where
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Declaration (Declaration)
import           Language.Graciela.AST.Expression  (Expression)
import           Language.Graciela.AST.Instruction (Instruction (..))
import           Language.Graciela.AST.Type        (ArgMode (..), Type (..))
import           Language.Graciela.Common
import           Language.Graciela.Error           (Error)
--------------------------------------------------------------------------------
import           Data.Sequence                     (Seq)
import           Data.Serialize                    (Serialize(put,get))
import           Data.Text                         (unpack)
--------------------------------------------------------------------------------

instance Serialize (Seq Type -> Either Error (Type, Text, Bool)) where
  put = undefined
  get = undefined

instance Serialize (Seq Type -> Either Error (Text, Seq ArgMode)) where
  put = undefined
  get = undefined

data Definition'
  = FunctionDef
    { funcBody      :: Expression
    , funcParams    :: Seq (Text, Type)
    , funcRetType   :: Type
    , funcDecls     :: Seq Declaration
    , funcRecursive :: Bool }
  | GracielaFunc
    { signatures :: Seq Type -> Either Error (Type, Text, Bool) -- last bool is `can abort`
    , casts      :: Seq Int }
  | GracielaProc
    { pSignatures :: Seq Type -> Either Error (Text, Seq ArgMode) }
  | ProcedureDef
    { procDecl      :: Seq (Either Declaration Instruction)
    , procBody      :: Instruction
    , procParams    :: Seq (Text, Type, ArgMode)
    , procRecursive :: Bool }
  | AbstractProcedureDef
    { abstParams :: Seq (Text, Type, ArgMode)
    , abstPDecl  :: Seq Declaration }
  | AbstractFunctionDef
    { abstFParams :: Seq (Text, Type)
    , abstFDecl   :: Seq Declaration
    , funcRetType :: Type }
  deriving (Generic, Serialize)

data Definition
  = Definition
    { defLoc   :: Location
    , isDecl   :: Bool
    , isExtern :: Bool
    , defName  :: Text
    , pre      :: Expression
    , post     :: Expression
    , bound    :: Maybe Expression
    , def'     :: Definition' }
 deriving (Generic, Serialize)

instance Treelike Definition where
  toTree Definition { defLoc, isDecl, defName, pre, post, bound, def', isExtern }
    = case def' of
      FunctionDef { funcBody, funcRetType, funcParams, funcRecursive, funcDecls } ->
        let 
          rec = if funcRecursive then "Recursive " else ""
          ext = if isExtern then "extern " else ""
        in Node (ext <> rec <> "Function " <> unpack defName <> " -> " <> show funcRetType <> " " <> show defLoc)
          [ Node "Parameters" (showFPs funcParams)
          , Node "Declarations" $ toForest funcDecls
          , Node "Precondition" [toTree pre]
          , Node "Postcondition" [toTree post]
          , boundNode
          , Node "Body" [toTree funcBody]
          , Node ("Is a Declaration: " <> show isDecl) [] ]

      GracielaFunc { } ->
        leaf $ "Graciela native function `" <> unpack defName <> "`"

      GracielaProc { } ->
        leaf $ "Graciela native Proc `" <> unpack defName <> "`"

      ProcedureDef { procDecl, procBody, procParams, procRecursive} ->
        let rec = if procRecursive then "Recursive " else ""
        in Node (rec <> "Procedure " <> unpack defName <> " " <> show defLoc)
          [ Node "Parameters" (showPs procParams)
          , Node "Declarations" $
              (\x -> case x of; Left a -> toTree a; Right b -> toTree b) <$> toList procDecl
          , Node "Precondition" [toTree pre]
          , Node "Postcondition" [toTree post]
          , boundNode
          , Node "Body" [toTree procBody]
          , Node ("Is a Declaration: " <> show isDecl) []]

      AbstractProcedureDef {abstParams} ->
        Node ("Abstract Procedure " <> unpack defName <> " " <> show defLoc)
          [ Node "Parameters" (showPs abstParams)
          , Node "Precondition" [toTree pre]
          , Node "Postcondition" [toTree post] ]

      AbstractFunctionDef {abstFParams, funcRetType} ->
        Node ("Abstract Function " <> unpack defName <> " -> " <>
               show funcRetType <> " " <> show defLoc)
          [ Node "Parameters" (showFPs abstFParams)
          , Node "Precondition" [toTree pre]
          , Node "Postcondition" [toTree post] ]

    where
      showPs :: Seq (Text, Type, ArgMode) -> [Tree String]
      showPs = fmap (\(n,t,m) -> leaf (show m <> " " <> unpack n <> " : " <> show t)) . toList
      showFPs :: Seq (Text, Type) -> [Tree String]
      showFPs = fmap (\(n,t) -> leaf (unpack n <> " : " <> show t)) . toList
      boundNode = case bound of
        Just b  -> Node "Bound" [toTree b]
        Nothing -> leaf "Not bounded"
