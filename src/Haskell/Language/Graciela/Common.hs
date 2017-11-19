module Language.Graciela.Common
  ( Semigroup(..)
  , Serialize
  , Generic
  , module Language.Graciela.Treelike
  , module Language.Graciela.Location
  , module Language.Graciela.Pragma
  , internal
  , issueMsg
  , toList
  , isNothing, isJust, fromMaybe, fromJust
  , pack, unpack
  , foldM, forM, forM_, unless, void, when, zipWithM, zipWithM_, (>=>)
  , lift
  , Int32
  , trace, traceM, traceShow, traceShowId, traceShowM
  , Seq, Set, Map, Text
  , ($>)
  , rights, eithers, Both, isRight, isLeft
  ) where
--------------------------------------------------------------------------------
import           Language.Graciela.Location
import           Language.Graciela.Pragma
import           Language.Graciela.Treelike
--------------------------------------------------------------------------------
import           Control.Monad              (foldM, forM, forM_, unless, void,
                                             when, zipWithM, zipWithM_, (>=>))
import           Control.Monad.Trans.Class  (lift)
import           Data.Either                (rights, isRight, isLeft)
import           Data.Foldable              (toList)
import           Data.Functor               (($>))
import           Data.Int                   (Int32)
import           Data.Map.Strict            (Map)
import           Data.Maybe                 (isJust, isNothing, fromMaybe, fromJust)
import           Data.Semigroup             (Semigroup (..))
import           Data.Sequence              (Seq)
import           Data.Set                   (Set)
import           Data.Text                  (pack, unpack, Text)
import           Debug.Trace                (trace, traceM, traceShow,
                                             traceShowId, traceShowM)
import           Data.Serialize             (Serialize)
import           Data.Serialize.Text
import           GHC.Generics               (Generic)
--------------------------------------------------------------------------------

internal :: String -> a
internal = error
         . ("internal error: " <>)
         . issueMsg

issueMsg :: String -> String
issueMsg = (<> "\n\tPlease open a [New issue] at\n\t\
               \https://github.com/GracielaUSB/graciela/issues\n\t\
               \with the message above and your .gcl file(s)")

type Both a = Either a a

eithers :: [Both a] -> [a]
eithers = fmap (either id id)
