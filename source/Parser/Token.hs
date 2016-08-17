{-|
Module      : Parser.Token
Description : Todos los lexemas del lenguaje
Copyright   : Graciela

Contiene los analizadores semánticos (parsers) básicos del compilador,
que funcionan como bloques para analizadores semánticos más complejos.
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}

module Parser.Token
  ( anyToken
  , satisfy
  , match
  , oneOf
  , parens
  , percents
  , brackets
  , beginEnd
  , identifier
  , identifierAndLoc
  , boolLit
  , charLit
  , stringLit
  , integerLit
  , floatLit
  ) where
--------------------------------------------------------------------------------
import           Token
import           Location
import           Error
import           Graciela
--------------------------------------------------------------------------------
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Text             (Text, pack)
import           Data.List.NonEmpty    (NonEmpty ((:|)))
import           Text.Megaparsec       (ErrorItem (Tokens), ParseError(..),
                                        token, between, getPosition)
import           Data.Int              (Int32)
import           Text.Megaparsec.Prim  (MonadParsec)
import qualified Text.Megaparsec.Prim  as Prim (Token)
--------------------------------------------------------------------------------

unex :: TokenPos -> (Set (ErrorItem TokenPos), Set a, Set b)
unex = (, Set.empty, Set.empty) . Set.singleton . Tokens . (:|[])


match :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
      => Token -> m Location
match t = token test Nothing
  where
    test tp @ TokenPos { tok, start, end } =
      if t == tok
        then Right $ Location (start, end)
        else Left . unex $ tp


satisfy :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
        => (Token -> Bool) -> m Token
satisfy f = token test Nothing
  where
    test tp @ TokenPos {tok} =
      if f tok
        then Right tok
        else Left . unex $ tp


oneOf :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
      => [Token] -> m Token
oneOf ts = satisfy (`elem` ts)


parens :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
       => m a -> m a
parens = between (match TokLeftPar) (match TokRightPar)


percents :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
         => m a -> m a
percents = between (match TokLeftPercent) (match TokRightPercent)


brackets :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
         => m a -> m a
brackets = between (match TokLeftBracket) (match TokRightBracket)


beginEnd :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
         => m a -> m a
beginEnd = between (match TokBegin) (match TokEnd)


anyToken :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
         => m Token
anyToken = token test Nothing
  where
    test TokenPos {tok} = Right tok


identifier :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
           => m Text
identifier = token test Nothing
  where
    test    TokenPos {tok = TokId i} = Right i
    test tp@TokenPos {tok}           = Left . unex $ tp


-- | Find an identifier and returns it's name and the location
identifierAndLoc :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
                 => m (Text, Location)
identifierAndLoc  = do
  from <- getPosition
  id <- identifier
  to <- getPosition
  pure (id, Location(from,to))


boolLit :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
        => m Bool
boolLit = token test Nothing
  where
    test    TokenPos {tok = TokBool b} = Right b
    test tp@TokenPos {tok}             = Left . unex $ tp


charLit :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
        => m Char
charLit = token test Nothing
  where
    test    TokenPos {tok = TokChar c} = Right c
    test tp@TokenPos {tok}             = Left . unex $ tp


stringLit :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
          => m Text
stringLit = token test Nothing
  where
    test    TokenPos {tok = TokString s} = Right s
    test tp@TokenPos {tok}               = Left . unex $ tp


integerLit :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
           => m Int32
integerLit = token test Nothing
  where
    test    TokenPos {tok = TokInteger i} = Right i
    test tp@TokenPos {tok}                = Left . unex $ tp


floatLit :: (MonadParsec e s m, Prim.Token s ~ TokenPos)
         => m Double
floatLit = token test Nothing
  where
    test    TokenPos {tok = TokFloat f} = Right f
    test tp@TokenPos {tok}              = Left . unex $ tp
