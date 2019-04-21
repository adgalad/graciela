{-|
Module      : Language.Graciela.Parser.Prim
Description : Primitives for Graciela parser
Copyright   : © 2015-2016 Graciela USB
Maintainer  : moises+graciela@ackerman.space
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TypeFamilies      #-}

module Language.Graciela.Parser.Stream
  where
--------------------------------------------------------------------------------
import           Language.Graciela.Token (TokenPos (..))
import qualified Language.Graciela.Token as T (Token(TokProgram))
import           Language.Graciela.Common
import           Language.Graciela.Location
--------------------------------------------------------------------------------
import           Data.List                (foldl')
import           Data.Proxy
import qualified Text.Megaparsec.Stream    as MP
import           Text.Megaparsec.Pos
--------------------------------------------------------------------------------

-- | The Graciela parser works on a list of Tokens with position information
-- as its Stream.
defaultAdvance1 :: Pos -- ^ Tab width
  -> SourcePos         -- ^ Current position
  -> TokenPos          -- ^ Current token
  -> SourcePos         -- ^ Incremented position
defaultAdvance1 _ _ (TokenPos _ end _) = end

instance MP.Stream [TokenPos] where
  type Token [TokenPos] = TokenPos
  type Tokens [TokenPos] = [TokenPos]
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  advance1 Proxy = defaultAdvance1
  advanceN Proxy w = foldl' (defaultAdvance1 w)
  take1_ (t:ts) = Just (t, ts)
  take1_ [] = Nothing
  takeN_ n s
    | n <= 0    = let emptyTok = TokenPos { start = gracielaDef'
                                          , end   = gracielaDef'
                                          , tok   = T.TokProgram }
                  in Just ([emptyTok], s)
    | null s  = Nothing
    | otherwise = Just (splitAt n s)
  takeWhile_ = span
