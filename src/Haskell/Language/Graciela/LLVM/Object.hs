{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PostfixOperators         #-}
{-# LANGUAGE TupleSections            #-}

module Language.Graciela.LLVM.Object
  ( object
  , objectRef
  ) where
--------------------------------------------------------------------------------
import {-# SOURCE #-} Language.Graciela.LLVM.Expression (expression)
--------------------------------------------------------------------------------
import qualified Language.Graciela.AST.Expression   as E (loc)
import           Language.Graciela.AST.Object       (Object (..), Object' (..))
import           Language.Graciela.AST.Struct       (Struct (..))
import           Language.Graciela.AST.Type         (ArgMode (..), Type (..),
                                                     basic, highLevel, (=:=))
import           Language.Graciela.Common
import           Language.Graciela.LLVM.Abort       (abort)
import qualified Language.Graciela.LLVM.Abort       as Abort (Abort (..))
import           Language.Graciela.LLVM.Monad
import           Language.Graciela.LLVM.State
import           Language.Graciela.LLVM.Type        
--------------------------------------------------------------------------------
import           Control.Lens               (use)
import           Data.Map                   as Map (lookup)
import           Data.Maybe                 (isJust)
import qualified Data.Set                   as Set
import qualified LLVM.AST.CallingConvention as CC (CallingConvention (C))
import qualified LLVM.AST.Constant          as C
import           LLVM.AST.Instruction       (Instruction (..), Named (..),
                                             Terminator (..))
import           LLVM.AST.IntegerPredicate  (IntegerPredicate (..))
import           LLVM.AST.Name              (Name (..))
import           LLVM.AST.Operand           (Operand (..))
import           LLVM.AST.Type              (Type (ArrayType), ptr)
import           Prelude                            hiding (Ordering (..))
--------------------------------------------------------------------------------

object :: Object -> LLVM Operand
object obj@Object { objType, obj' } = do
  objRef <- objectRef obj
  label <- newLabel "varObj"
  t <- toLLVMType objType
  addInstruction $ label := Load
    { volatile       = False
    , address        = objRef
    , maybeAtomicity = Nothing
    , alignment      = 4
    , metadata       = [] }
  pure $ LocalReference t label

-- Get the reference to the object.
-- indicate that the object is not a deref, array access or field access inside a procedure
objectRef :: Object -> LLVM Operand
objectRef (Object loc t obj') = do
  objType' <- toLLVMType t

  ref <- case obj' of

    Variable { name , mode } -> do
      name' <- getVariableName name
      pure $ LocalReference objType' name'

    Index inner isPtr indices -> if isPtr then do
        ref <- objectRef inner
        ind <- expression . head $ toList indices
        
        idxPtr <- newLabel "idxPtr"
        addInstruction $ idxPtr := Load
          { volatile       = True
          , address        = ref

          , maybeAtomicity = Nothing
          , alignment      = 4
          , metadata       = [] }

        result <- newLabel "ResultIdxPtr"
        addInstruction $ result := GetElementPtr
          { inBounds = False
          , address  = LocalReference (ptr objType') idxPtr
          , indices  = [ind]
          , metadata = [] }

        pure . LocalReference objType' $ result

      else do
        ref <- objectRef inner

        iarrPtrPtr <- newLabel "idxStruct"
        addInstruction $ iarrPtrPtr := GetElementPtr
          { inBounds = False
          , address  = ref
          , indices  = constantOperand GInt . Left <$>
            [0, fromIntegral . length $ indices]
          , metadata = [] }

        iarrPtr <- newLabel "idxArrPtr"
        addInstruction $ iarrPtr := Load
          { volatile       = True
          , address        = LocalReference (ptr . ptr $ ArrayType 1 objType') iarrPtrPtr
          -- , address        = LocalReference (ptr . ptr $ iterate (ArrayType 1) objType' !! length indices) iarrPtrPtr
          , maybeAtomicity = Nothing
          , alignment      = 4
          , metadata       = [] }

        -- ind <- zipWithM (idx ref) (toList indices) [0..] -- !!!!!!!!!!

        ind <- snd <$> 
          foldM (idx ref (fromIntegral $ length indices)) 
            (ConstantOperand (C.Int 32 1), ConstantOperand (C.Int 32 0))
            (zip (toList indices) [0..])

        result <- newLabel "idxArr"
        addInstruction $ result := GetElementPtr
          { inBounds = False
          , address  = LocalReference (ptr $ ArrayType 1 objType') iarrPtr
          -- , address  = LocalReference (ptr $ iterate (ArrayType 1) objType' !! length indices) iarrPtr
          , indices  = [ConstantOperand (C.Int 32 0), ind]
          -- , indices  = ConstantOperand (C.Int 32 0) : inds
          , metadata = [] }

        pure . LocalReference objType' $ result

      where
          idx ref ns (prod, index) (i, n) = do
            e <- expression i

            chkGEZ <- newLabel "idxChkGEZ"
            addInstruction $ chkGEZ := ICmp
              { iPredicate = SGE
              , operand0   = e
              , operand1   = ConstantOperand (C.Int 32 0)
              , metadata   = [] }

            gez <- newLabel "idxGEZ"
            notGez <- newLabel "idxNotGEZ"
            terminate CondBr
              { condition = LocalReference boolType chkGEZ
              , trueDest  = gez
              , falseDest = notGez
              , metadata' = [] }

            (notGez #)
            abort Abort.NegativeIndex (pos . E.loc $ i)

            (gez #)
            bndPtr <- newLabel "idxBoundPtr"
            addInstruction $ bndPtr := GetElementPtr
              { inBounds = False
              , address  = ref
              , indices  =
                [ ConstantOperand (C.Int 32 0)
                , ConstantOperand (C.Int 32 n)]
              , metadata = [] }

            bnd <- newLabel "idxBound"
            addInstruction $ bnd :=  Load
              { volatile       = False
              , address        = LocalReference intType bndPtr
              , maybeAtomicity = Nothing
              , alignment      = 4
              , metadata       = [] }

            chkInBound <- newLabel "idxChkInBound"
            addInstruction $ chkInBound := ICmp
              { iPredicate = SLT
              , operand0   = e
              , operand1   = LocalReference intType bnd
              , metadata   = [] }

            inBound <- newLabel "idxInBound"
            notInBound <- newLabel "idxNotInBound"
            terminate CondBr
              { condition = LocalReference boolType chkInBound
              , trueDest  = inBound
              , falseDest = notInBound
              , metadata' = [] }

            (notInBound #)
            abort Abort.OutOfBoundsIndex (pos . E.loc $ i)

            (inBound #)
            index' <- newLabel "indexCalc"
            e' <- newUnLabel
            addInstruction $ e' := Mul
              { nsw = False
              , nuw = False
              , operand0 = e
              , operand1 = prod
              , metadata = [] }
            addInstruction $ index' := Add
              { nsw = False
              , nuw = False
              , operand0 = index
              , operand1 = LocalReference intType e'
              , metadata = [] }

            prod' <- newLabel "prodCalc"
            when (n < ns - 1) $ addInstruction $ prod' := Mul
              { nsw = False
              , nuw = False
              , operand0 = prod
              , operand1 = LocalReference intType bnd
              , metadata = [] }

            pure (LocalReference intType prod', LocalReference intType index')


    Deref inner -> do
      ref        <- objectRef inner
      labelLoad  <- newLabel "derefLoad"
      labelCast  <- newLabel "derefCast"
      labelNull  <- newLabel "derefNull"
      labelCond  <- newLabel "derefCond"
      trueLabel  <- newLabel "derefNullTrue"
      falseLabel <- newLabel "derefNullFalse"

      let
        Location(p,_) = loc

      addInstruction $ labelLoad := Load
        { volatile  = False
        , address   = ref
        , maybeAtomicity = Nothing
        , alignment = 4
        , metadata  = [] }

      {- Generate assembly to verify if a pointer is NULL, when accessing to the pointed memory.
         In that case, abort the program giving the source line of the bad access instead of letting
         the OS ends the process
      -}
      addInstruction $ labelCast := PtrToInt
        { operand0 = LocalReference objType' labelLoad
        , type'    = lintType
        , metadata = [] }

      addInstruction $ labelNull := PtrToInt
        { operand0 = ConstantOperand . C.Null $ ptr objType'
        , type'    = lintType
        , metadata = [] }

      addInstruction $ labelCond := ICmp
        { iPredicate = EQ
        , operand0   = LocalReference lintType labelCast
        , operand1   = LocalReference lintType labelNull
        , metadata   = [] }

      terminate CondBr
        { condition = LocalReference boolType labelCond
        , trueDest  = trueLabel
        , falseDest = falseLabel
        , metadata' = [] }

      (trueLabel #)
      abort Abort.NullPointerAccess p

      (falseLabel #)
      castPtr <- newLabel "castPtr"
      addInstruction $ castPtr := BitCast
          { operand0 = LocalReference objType' labelLoad
          , type'    = pointerType
          , metadata = [] }
      p <- Set.member MemoryOperations <$> use mpragmas
      let
        ptr = LocalReference pointerType $ castPtr
        Location (SourcePos f l c, _) = loc
        pragma = ConstantOperand . C.Int 1 $ if p then 1 else 0
        line = constantOperand GInt . Left . fromIntegral $ unPos l
        col  = constantOperand GInt . Left . fromIntegral $ unPos c
      filePath <- getFilePathOperand f
      let args = [ptr, pragma, filePath, line, col]
      use evalAssertions >>= \b -> when b $ 
        callFunction derefPointerString args >>= addInstruction . Do

      pure . LocalReference objType' $ labelLoad


    Member { inner, fieldName, field } -> do
      ref <- objectRef inner
      member <- newLabel $ "member" <> show field

      addInstruction $ member := GetElementPtr
            { inBounds = False
            , address  = ref
            , indices  = constantOperand GInt . Left <$> [0, field]
            , metadata = []}


      st <- use structs
      isCoupling <- use coupling
      doget <- use doGet >>= \canDoGet -> do
        -- If it's self and it's doing the
        -- couple relation, must no use a getter
        let
          isSelf = case inner of
            Object{ obj' = Variable { name } } -> name == "__self"
            _                                  -> False

        pure $ canDoGet && not (isCoupling && isSelf)

      -- When its a highlevel field and its inner object is a data Type the
      -- field must be initialize with its getter (unless the LogicAnywhere pragma is active)
      when (t =:= highLevel && objType inner =:= GADataType && doget) $ do
        (sName, types, fields) <- use currentStruct >>= \case
          Just Struct{structBaseName,structTypes, structAFields} -> do
            case objType inner of
              GDataType _ Nothing _ ->
                 pure $ (structBaseName,structTypes, structAFields)

              GDataType n _ t' -> do
                if structBaseName == n
                  then pure $ (n, toList t', structAFields)
                  else case n `Map.lookup` st of
                    Just Struct{structBaseName,structTypes, structAFields} ->
                      pure $ (structBaseName,structTypes, structAFields)
                    Nothing -> internal $ "Could not find the data type " <> unpack n

          Nothing ->
            case objType inner of
              GDataType _ Nothing _ -> internal $ "Trying to use a getter of an abstract type"

              GDataType n _ t' -> case n `Map.lookup` st of
                Just Struct{structBaseName,structTypes, structAFields} ->
                  pure $ (structBaseName,structTypes, structAFields)
                Nothing -> internal $ "Could not find the data type " <> unpack n


        case fieldName `Map.lookup` fields of
          Just _ -> do
            t <- mapM fill types
            let
              name = llvmName ("get_" <> fieldName <> "-" <> sName) t
            getLabel <- newLabel $ "get" <> show field
            callFunction name [ref] >>= addInstruction . (getLabel :=)


            addInstruction $ Do Store
              { volatile = False
              , address  = LocalReference objType' member
              , value    = LocalReference (pointerType) getLabel
              , maybeAtomicity = Nothing
              , alignment = 4
              , metadata  = [] }
          Nothing -> pure ()

      pure $ LocalReference objType' member

  pure $ ref

