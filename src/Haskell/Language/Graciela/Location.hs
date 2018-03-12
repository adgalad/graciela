{-|
Module      : Language.Graciela.Location
Description : Location information
Copyright   : © 2015-2016 Graciela USB
Maintainer  : moises+graciela@ackerman.space
Stability   : experimental
Portability : POSIX

Provides a datatype for storing information about the location or
position of Graciela internal constructs.
-}

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}


module Language.Graciela.Location
  ( Location (..)
  , SourcePos (..)
  , Pos
  , gracielaDef
  , gracielaDef'
  , initialPos
  , pos
  , locFile
  , showPos
  , showRedefPos
  , unPos
  ) where
--------------------------------------------------------------------------------
import           Data.Semigroup         (Semigroup (..))
import           Text.Megaparsec.Pos    (SourcePos (..), Pos(..), unPos, mkPos)
--------------------------------------------------------------------------------
import           Data.Serialize         (Serialize(..))
import           GHC.Generics           (Generic)
import           Data.Text              (unpack)
import           Data.Serialize.Get     (Get)
import           Control.Monad          (liftM)
import           System.FilePath.Posix  (takeFileName)
--------------------------------------------------------------------------------

-- | This datatype stores information about the location of various
-- Graciela constructs, such as Tokens, AST nodes and Procedure/Function
-- definitions.
newtype Location = Location (SourcePos, SourcePos) -- ^ A location within a file.
  deriving (Eq, Ord, Generic)


-- | Instances for Serialize of SourcePos and Pos
-- Pos has to be instantiate manually

instance Serialize Location
instance Generic Pos

instance Serialize Pos where
    put p   = put (fromIntegral (unPos p) :: Int)
    get     = fmap (mkPos . fromIntegral) (get :: Get Int)

instance Serialize SourcePos

instance Show Location where
  show (Location (p0@(SourcePos fn l0 c0), p1@(SourcePos _ l1 c1)))
    | fn == "/GRACIELA/" = "(in the Graciela Definition)"
    | otherwise = if l0 == l1
      then " (line " <> show (unPos l0) <> ", (col " <> show (unPos c0) <>
        " -> col " <> show (unPos c1) <> "))"
      else "(" <> showPos p0 <> " -> " <> showPos p1 <> ")"

instance Semigroup Location where
  Location (from0, to0) <> Location (from1, to1)
    = Location (from0 `min'` from1, to0 `max'` to1)
    where
      SourcePos fn l0 c0 `min'` SourcePos _ l1 c1
        | l0 < l1 || l0 == l1 && c0 < c1 = SourcePos fn l0 c0
        | otherwise = SourcePos fn l1 c1
      SourcePos fn l0 c0 `max'` SourcePos _ l1 c1
        | l0 > l1 || l0 == l1 && c0 > c1 = SourcePos fn l0 c0
        | otherwise = SourcePos fn l1 c1

pos :: Location -> SourcePos
pos (Location (p, _)) = p

locFile :: Location -> String
locFile (Location (SourcePos file _ _, _)) = file

gracielaDef :: Location
gracielaDef = Location (gracielaDef', gracielaDef')

gracielaDef' :: SourcePos
gracielaDef' = SourcePos "/GRACIELA/" (mkPos 1) (mkPos 1)

initialPos :: String -> SourcePos
initialPos name = SourcePos name (mkPos 1) (mkPos 1)
-- | Shows a 'SourcePos' in a human-readable way.
showPos :: SourcePos -> String
showPos SourcePos { sourceName, sourceLine, sourceColumn }
  | sourceName == "/GRACIELA/" = "(in the Graciela Definition)"
  | otherwise =
    "(" <> takeFileName sourceName <> ":"
    <> show (unPos sourceLine) <> ":" <> show (unPos sourceColumn) <> ")"



showRedefPos :: SourcePos -> SourcePos -> String
showRedefPos prevDef actDef =
  if sourceName prevDef == sourceName actDef
    then showPos prevDef
    else sourceName prevDef <> " " <> showPos prevDef
