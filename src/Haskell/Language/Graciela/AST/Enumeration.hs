{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.Graciela.AST.Enumeration where
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

-- instance Serialize (Seq Type -> Either Error (Type, Text, Bool)) where
--   put = undefined
--   get = undefined

-- instance Serialize (Seq Type -> Either Error (Text, Seq ArgMode)) where
--   put = undefined
--   get = undefined

data Enumeration = 
  Enumeration 
    { type' :: Maybe Type
    , pairs :: Seq (Text, Expression) }
