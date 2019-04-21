module Language.Graciela.LLVM.Type where

import LLVM.AST.Type as LLVM (Type)

llvmFunT :: LLVM.Type -> [LLVM.Type] -> LLVM.Type
