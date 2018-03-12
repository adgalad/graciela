{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PostfixOperators  #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE LambdaCase        #-}

module Language.Graciela.LLVM.Boolean
  ( boolean
  , wrapBoolean
  ) where
--------------------------------------------------------------------------------
import {-# SOURCE #-} Language.Graciela.LLVM.Expression (expression)
import {-# SOURCE #-} Language.Graciela.LLVM.Object     (object, objectRef)
--------------------------------------------------------------------------------
import           Language.Graciela.AST.Definition
import           Language.Graciela.AST.Expression        as Op
import           Language.Graciela.AST.Struct
import           Language.Graciela.AST.Type
import           Language.Graciela.Common
import           Language.Graciela.Error                 hiding (fArgs, fName)
import           Language.Graciela.LLVM.Abort            (abort)
import qualified Language.Graciela.LLVM.Abort            as Abort (Abort (..))
import           Language.Graciela.LLVM.Monad
import           Language.Graciela.LLVM.Quantification
import           Language.Graciela.LLVM.State
import           Language.Graciela.LLVM.Type
--------------------------------------------------------------------------------
import           Control.Lens                    (use, (&))
import           Data.Map                        as Map (lookup)
import           Data.Text                       (unpack)
import qualified LLVM.AST.CallingConvention      as CC (CallingConvention (C))
import qualified LLVM.AST.Constant               as C (Constant (..))
import           LLVM.AST.FloatingPointPredicate (FloatingPointPredicate (OEQ, OGE, OGT, OLE, OLT))
import           LLVM.AST.Instruction            (Instruction (..), Named (..),
                                                 Terminator (..))
import           LLVM.AST.IntegerPredicate       (IntegerPredicate (EQ, SGE, SGT, SLE, SLT))
import           LLVM.AST.Name                   (Name)
import           LLVM.AST.Operand                (Operand (..))
import           LLVM.AST.Type                   (ptr)
import           Prelude                         hiding (Ordering (..))
--------------------------------------------------------------------------------

boolean :: --(Expression -> LLVM Operand) -- ^ non-boolean expression code generator
        -- -> (Object -> LLVM Operand) -- ^ object code generator (both boolean and non-boolean)
        -- -> (Object -> Bool -> LLVM Operand) -- ^ object ref code generator (both boolean and non-boolean)
          Name -- ^ true destination
         -> Name -- ^ false destination
         -> Expression -- ^ boolean expression
         -> LLVM ()
boolean true false e@Expression { loc, exp' } = do
  t  <- fill $ expType e
  if t /= GBool
    then internal
      "attempted to generate non-boolean expression with `boolean` \
      \instead of `expression`"
  else case exp' of
    NullPtr -> internal "Null ptr cannot be boolean"

    Value { theValue = BoolV b } ->
      terminate Br
        { dest = if b then true else false
        , metadata' = [] }

    Value {} -> internal "Char/Int/Float V cannot be boolean"
    StringLit {} -> internal "string lit cannot be boolean"
    Collection {} -> internal "collection cannot be boolean"
    Tuple {} -> internal "tuple cannot be boolean"

    Obj { theObj } -> do
      op <- object theObj
      terminate CondBr
        { condition = op
        , trueDest  = true
        , falseDest = false
        , metadata' = [] }

    Binary { binOp, lexpr, rexpr } -> case binOp of
      Op.And -> do
        middle <- newLabel "and"
        boolean middle false lexpr
        (middle #)
        boolean true false rexpr

      Op.Or -> do
        middle <- newLabel "or"
        boolean true middle lexpr
        (middle #)
        boolean true false rexpr

      Implies -> do
        middle <- newLabel "implies"
        boolean middle true lexpr
        (middle #)
        boolean true false rexpr

      Consequent -> do
        middle <- newLabel "consequent"
        boolean true middle lexpr
        (middle #)
        boolean false true rexpr

      _ | binOp `elem` [Elem, NotElem] -> do
          lOperand <- expression lexpr
          rOperand <- expression rexpr

          item <- newLabel "item"

          lType' <- fill $ expType lexpr

          addInstruction $ item := case lType' of
            GFloat -> BitCast
              { operand0 = lOperand
              , type' = lintType
              , metadata = [] }

            GPointer _ -> PtrToInt
              { operand0 = lOperand
              , type'    = lintType
              , metadata = [] }

            lType | lType `elem` [ GInt, GChar ] -> ZExt
              { operand0 = lOperand
              , type' = lintType
              , metadata = [] }

          elemCall <- newLabel "elemCall"
          let 
            args = [rOperand, LocalReference lintType item]
            fName = case expType rexpr of
              GSet      _ -> isElemSetString
              GMultiset _ -> isElemMultisetString
              GSeq      _ -> isElemSeqString
              _           -> internal "impossible type for elem argument"
          callFunction fName args >>= addInstruction . (elemCall :=)

          let [trueDest, falseDest] = [true, false] & case binOp of
                Elem    -> id
                NotElem -> reverse

          terminate CondBr
            { condition = LocalReference boolType elemCall
            , trueDest
            , falseDest
            , metadata' = [] }
      _ | binOp `elem` [LT, LE, GT, GE] -> do
        lOperand <- expression lexpr -- The operands can only be chars, ints
        rOperand <- expression rexpr -- or floats, nothing else

        type' <- fill $ expType lexpr <> expType rexpr

        comp <- newLabel "comp"

        let
          compOp = case type' of
            GFloat -> FCmp $ case binOp of
              LT -> OLT
              LE -> OLE
              GT -> OGT
              GE -> OGE
            _ | type' `elem` [GInt, GChar, GBool] -> ICmp $ case binOp of
              LT -> SLT
              LE -> SLE
              GT -> SGT
              GE -> SGE
            _ -> internal $ "invalid comparison type " <> show type' <> " " <> show loc

        addInstruction $ comp := compOp lOperand rOperand []

        terminate CondBr
          { condition = LocalReference boolType comp
          , trueDest  = true
          , falseDest = false
          , metadata' = [] }

      _ | binOp `elem` [AEQ, ANE, BEQ, BNE] -> do

        type' <- fill $ expType lexpr <> expType rexpr

        [lOperand, rOperand] <- [lexpr, rexpr] `forM` \x ->
          case type' of
            GBool -> wrapBoolean x

            _ |  type' =:= highLevel || type' =:= GTuple GAny GAny
              || type' `elem` [GInt, GChar, GFloat] -> expression x

            _ | type' =:= GPointer GAny -> do
              cast <- newLabel "ptrEqCast"
              operand0 <- expression x
              addInstruction $ cast := PtrToInt
                { operand0
                , type'    = lintType
                , metadata = [] }
              pure $ LocalReference lintType cast

        comp <- newLabel "eqComp"

        case type' of
          _ | type' =:= GOneOf [GBool, GChar, GInt, GPointer GAny] ->
            addInstruction $ comp := ICmp
              { iPredicate = EQ
              , operand0 = lOperand
              , operand1 = rOperand
              , metadata = [] }
          GFloat ->
            addInstruction $ comp := FCmp
              { fpPredicate = OEQ
              , operand0 = lOperand
              , operand1 = rOperand
              , metadata = [] }

          _ |  type' =:= highLevel || type' =:= GTuple GAny GAny -> do
            let 
              fName = case type' of
                GSet      _ -> equalSetString
                GMultiset _ -> equalMultisetString
                GSeq      _ -> equalSeqString
                GFunc   _ _ -> equalFuncString
                GRel    _ _ -> equalRelString
                GTuple  _ _ -> equalTupleString
            callFunction fName [lOperand, rOperand] >>= addInstruction . (comp :=)
            
        let [trueDest, falseDest] = [true, false] & if
              | binOp `elem` [AEQ, BEQ] -> id
              | binOp `elem` [ANE, BNE] -> reverse
        terminate CondBr
          { condition = LocalReference boolType comp
          , trueDest
          , falseDest
          , metadata' = [] }

      _ | binOp `elem` [Subset, SSubset, Superset, SSuperset] -> do
        [lOp, rOp] <- mapM expression [lexpr, rexpr]

        comp <- newLabel "setComp"

        let 
          [lOp', rOp'] = [lOp, rOp] & if
              | binOp `elem` [Subset, SSubset] -> reverse
              | binOp `elem` [Superset, SSuperset] -> id
          fName = case expType lexpr of
            GSet      _ -> if
              | binOp `elem` [Subset, Superset]   -> supersetSetString
              | binOp `elem` [SSubset, SSuperset] -> ssupersetSetString
            GMultiset _ -> if
              | binOp `elem` [Subset, Superset]   -> supersetMultisetString
              | binOp `elem` [SSubset, SSuperset] -> ssupersetMultisetString
        
        callFunction fName [lOp', rOp'] >>= addInstruction . (comp :=)  

        terminate CondBr
          { condition = LocalReference boolType comp
          , trueDest  = true
          , falseDest = false
          , metadata' = [] }

      SeqAt -> do
          let
            SourcePos f x y = pos loc
            line = constantOperand GInt . Left . fromIntegral $ unPos x
            col  = constantOperand GInt . Left . fromIntegral $ unPos y
          filePath <- getFilePathOperand f
          lOp <- expression lexpr
          rOp <- expression rexpr
          call <- newLabel "seqAt"
          let args = [lOp, rOp, filePath, line, col]
          callFunction atSequenceString args >>= addInstruction . (call :=)

          seqAtResult <- newLabel "seqAtResult"

          addInstruction $ seqAtResult := Trunc
              { operand0 = LocalReference lintType call
              , type'    = boolType
              , metadata = [] }

          terminate CondBr
            { condition = LocalReference boolType seqAtResult
            , trueDest  = true
            , falseDest = false
            , metadata' = [] }


      BifuncAt -> internal
        "boolean funcs/rels are not implemented so @ can't produce a boolean"

      -- Plus, BMinus, Times, Div, Mod, Power, Max, Min,
      -- Difference, Intersection, Union, MultisetSum, Concat
      _ -> internal $ "operator `" <> show binOp <> "` never produces a boolean"

    Unary { unOp, inner } -> case unOp of
      Not -> boolean false true inner
      _ -> internal $ "operator `" <> show unOp <> "` cannot produce a boolean"

    I64Cast {} -> internal "lintType-cast cannot produce a boolean"

    AbstFunctionCall {fName, fArgs, fStructArgs} -> do
      fdt <- use fullDataTypes
      let
        (dtName, typeArgs) = case fStructArgs of
          Nothing -> internal $ "Calling an abstract function of unknown Abstract Data Type"
          Just x -> x
      case dtName `Map.lookup` fdt of
        Nothing -> internal $ "Could not find Data Type " <> show dtName
        Just (Struct{structProcs},_) -> case fName `Map.lookup` structProcs of
          Nothing -> internal $ "Could not find function " <>
                                show fName <> " in Data Type " <>
                                show dtName

          Just Definition{ def' = FunctionDef{ funcRecursive }} ->
            boolean true false e{exp'=FunctionCall fName fArgs False funcRecursive fStructArgs}

    FunctionCall { fName, fArgs, fRecursiveCall, fRecursiveFunc, fStructArgs } -> do
      arguments <- toList <$> mapM createArg fArgs

      fName' <- case fStructArgs of
        Just (structBaseName, typeArgs) -> do
          t' <- mapM fill (toList typeArgs)
          pure $ llvmName (fName <> "-" <> structBaseName) t'

        _ -> pure . unpack $ fName
      asserts <- use evalAssertions
      recArgs <- if fRecursiveCall && asserts
        then do
          use boundOp >>= pure . \case 
            Nothing -> []
            Just boundOperand -> [constantOperand GBool . Left $ 1, boundOperand]
        else if fRecursiveFunc && asserts
          then pure [constantOperand GBool . Left $ 0, constantOperand GInt . Left $0]
          else pure []

      label <- newLabel "funcResult"
      callFunction fName' (recArgs <> arguments) >>= addInstruction . (label :=)
      
      terminate CondBr
        { condition = LocalReference boolType label
        , trueDest  = true
        , falseDest = false
        , metadata' = [] }

      where
        createArg x@Expression { expType, exp' } = do
          type' <- fill expType

          if 
            | type' =:= GBool -> wrapBoolean x

            | type' =:= basicT || type' == I64 || 
              type' == GString || type' =:= highLevel ->
                expression x
            | type' =:= GPointer GAny -> do
                expr <- expression x
                let GPointer t = type'
                type' <- ptr <$> toLLVMType t

                label <- newLabel "argCastBAO"
                addInstruction $ label := Alloca
                  { allocatedType = type'
                  , numElements   = Nothing
                  , alignment     = 4
                  , metadata      = [] }

                addInstruction $ Do Store
                  { volatile = False
                  , address  = LocalReference type' label
                  , value    = expr
                  , maybeAtomicity = Nothing
                  , alignment = 4
                  , metadata  = [] }

                pure $ LocalReference type' label

            | otherwise -> case exp' of
                Obj o         -> do
                  label <- newLabel "argCastBOb"
                  ref <- objectRef o
                  type' <- ptr <$> toLLVMType type'

                  addInstruction $ label := BitCast
                    { operand0 = ref
                    , type'    = type'
                    , metadata = [] }

                  pure $ LocalReference type' label

                _ -> internal "bad argument"


        basicT = GOneOf [ GChar, GInt, GFloat ]

    Quantification {} -> boolQ true false e

    EConditional { eguards, trueBranch } -> do
      mapM_ guard eguards

      case trueBranch of
        Nothing -> abort Abort.If (pos loc)
        Just e  -> boolean true false e

      where
        guard (left, right) = do
          yes <- newLabel "ifExpGuardYes"
          no  <- newLabel "ifExpGuardNo"

          boolean yes no left

          (yes #)
          boolean true false right

          (no #)

wrapBoolean :: --(Expression -> LLVM Operand)
          --  -> (Object -> LLVM Operand) -- ^ object code generator (both boolean and non-boolean)
          --  -> (Object -> Bool -> LLVM Operand) -- ^ object ref code generator (both boolean and non-boolean)
             Expression
            -> LLVM Operand
wrapBoolean e@Expression { expType } = do

  t <- fill expType

  if t /= GBool
    then internal $
      "attempted to generate non-boolean expression with `wrapBoolean` \
      \instead of `expression`\n" <> drawTree (toTree e)
    else do
      [begin, true, false, end, val] <- mapM (newLabel . ("wrap" <>))
        ["Begin", "True", "False", "End", "Val"]

      terminate $ Br begin []

      (begin #)
      boolean true false e

      (true #)
      terminate $ Br end []

      (false #)
      terminate $ Br end []

      (end #)
      addInstruction $ val := Phi
        { type' = boolType
        , incomingValues =
          [ (constantOperand GBool . Left $ 1, true)
          , (constantOperand GBool . Left $ 0, false) ]
        , metadata = [] }

      pure $ LocalReference boolType val
