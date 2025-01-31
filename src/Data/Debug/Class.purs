-- | This module provides the `Debug` type class, for converting values into
-- | their `Debug` representations.
module Data.Debug.Class
  ( class Debug
  , debug
  , diff
  , class DebugRecordRowList
  , debugRecordRowList
  , class DebugVariantRowList
  , debugVariantRowList
  ) where

import Prelude

import Data.Bifunctor (bimap)
import Data.Date (Date, day, month, year)
import Data.Debug.Type as D
import Data.Either (Either(..))
import Data.Enum (fromEnum)
import Data.List as List
import Data.List.Lazy as LazyList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Set (Set)
import Data.Set as Set
import Data.String as String
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, on)
import Effect (Effect)
import Erl.Atom (Atom)
import Erl.Atom as Atom
import Erl.Data.Binary (Binary)
import Erl.Data.Binary as Binary
import Erl.Data.List ((:))
import Erl.Data.List as ErlList
import Erl.Data.List.NonEmpty as ErlNel
import Erl.Data.Map as ErlMap
import Erl.Process.Raw (Pid)
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, RowList)
import Record (get, delete)
import Type.Proxy (Proxy(..))

-- | Ideally, all types of kind `Type` should have an instance of this class.
-- | If you are defining a type where it's difficult/impossible to do anything
-- | useful here (e.g. `Ref` or `(->)`) then you should use the `opaque`
-- | constructor.
-- |
-- | If a type has an `Eq` instance, then the `debug` function in its `Debug`
-- | instance should be *injective*, that is:
-- |
-- | ```purescript
-- | x /= y `implies` debug x /= debug y
-- | ```
class Debug a where
  debug :: a -> D.Repr

-- | Compare two values using the specified options, and record the results as
-- | a `ReprDelta` structure.
diffWith :: forall a. Debug a => D.DiffOptions -> a -> a -> D.ReprDelta
diffWith opts x y = D.diffReprWith opts (debug x) (debug y)

-- | Compare two values using the default options.
diff :: forall a. Debug a => a -> a -> D.ReprDelta
diff = diffWith D.defaultDiffOptions

-------------------------------------------------------------------------------
-- Prim

instance debugInt :: Debug Int where
  debug = D.int

instance debugNumber :: Debug Number where
  debug = D.number

instance debugBoolean :: Debug Boolean where
  debug = D.boolean

instance debugString :: Debug String where
  debug = D.string

instance debugAtom :: Debug Atom where
  debug = D.atom

instance debugChar :: Debug Char where
  debug = D.char

instance debugArray :: Debug a => Debug (Array a) where
  debug = D.array <<< map debug

instance debugFunction :: Debug (a -> b) where
  debug _ = D.opaque_ "function"

instance debugProxy :: Debug (Proxy ab) where
  debug _ = D.opaque_ "proxy"

instance debugPid :: Debug Pid where
  debug = D.string <<< show

-- | This class is part of the machinery for the `Debug (Record r)` instance;
-- | it is not intended to be used directly.
class DebugRecordRowList (list :: RowList Type) (row :: Row Type) | list -> row where
  debugRecordRowList :: Proxy list -> Record row -> ErlList.List (Tuple Atom D.Repr)

instance debugRecordRowListNil :: DebugRecordRowList Nil () where
  debugRecordRowList _ _ = ErlList.nil

instance debugRecordRowListCons ::
  ( Debug a
  , DebugRecordRowList listRest rowRest
  , Row.Cons key a rowRest rowFull
  , Row.Lacks key rowRest
  , RowToList rowFull (Cons key a listRest)
  , IsSymbol key
  ) =>
  DebugRecordRowList (Cons key a listRest) rowFull where
  debugRecordRowList _ rec =
    Tuple (Atom.atom $ reflectSymbol key) (debug val) : rest
    where
    key = Proxy :: Proxy key
    val = get key rec
    rest = debugRecordRowList (Proxy :: Proxy listRest) (delete key rec)

instance debugRecord ::
  ( RowToList row list
  , DebugRecordRowList list row
  ) =>
  Debug (Record row) where
  debug r =
    D.record $ debugRecordRowList prx r
    where
    prx = Proxy :: Proxy list

-- | This class is part of the machinery for the `Debug (Variant r)` instance;
-- | it is not intended to be used directly.
class DebugVariantRowList :: RowList Type -> Row Type -> Constraint
class DebugVariantRowList rl r | rl -> r where
  debugVariantRowList :: Proxy rl -> Variant r -> D.Repr

instance DebugVariantRowList Nil () where
  debugVariantRowList _ _ = D.atom $ Atom.atom "invalid"

instance
  ( DebugVariantRowList tailRL rowTail
  , IsSymbol sym
  , Row.Cons sym item rowTail row
  , Debug item
  ) =>
  DebugVariantRowList (Cons sym item tailRL) row where
  debugVariantRowList _ variant = do
    let
      fn item = D.prop (Atom.atom $ reflectSymbol (Proxy :: _ sym)) $ debug item
      recurse = debugVariantRowList (Proxy :: _ tailRL)
    on (Proxy :: _ sym) fn recurse variant

instance
  ( RowToList row list
  , DebugVariantRowList list row
  ) =>
  Debug (Variant row) where
  debug r =
    debugVariantRowList prx r
    where
    prx = Proxy :: Proxy list

-------------------------------------------------------------------------------
-- Prelude

instance debugOrdering :: Debug Ordering where
  debug LT = D.constructor (Atom.atom "LT") ErlList.nil
  debug EQ = D.constructor (Atom.atom "EQ") ErlList.nil
  debug GT = D.constructor (Atom.atom "GT") ErlList.nil

instance debugUnit :: Debug Unit where
  debug _ = D.constructor (Atom.atom "unit") ErlList.nil

instance debugVoid :: Debug Void where
  debug = absurd

-------------------------------------------------------------------------------
-- Core

instance debugMaybe :: Debug a => Debug (Maybe a) where
  debug (Just x) = D.constructor (Atom.atom "Just") $ ErlList.singleton $ debug x
  debug Nothing = D.constructor (Atom.atom "Nothing") ErlList.nil

instance debugEither :: (Debug a, Debug b) => Debug (Either a b) where
  debug (Right x) = D.constructor (Atom.atom "Right") $ ErlList.singleton $ debug x
  debug (Left x) = D.constructor (Atom.atom "Left") $ ErlList.singleton $ debug x

instance debugTuple :: (Debug a, Debug b) => Debug (Tuple a b) where
  debug (Tuple x y) = D.constructor (Atom.atom "Tuple") (debug x : debug y : ErlList.nil)

instance debugMap :: (Debug k, Debug v) => Debug (Map k v) where
  debug m =
    D.assoc "Map"
      (map (bimap debug debug) (Map.toUnfoldable m))

instance (Debug k, Debug v) => Debug (ErlMap.Map k v) where
  debug m =
    D.assoc "Map"
      (map (bimap debug debug) (ErlMap.toUnfoldable m))

instance debugEffect :: Debug (Effect a) where
  debug _ = D.opaque_ "Effect"

instance debugList :: Debug a => Debug (List.List a) where
  debug xs = D.collection "List" (map debug (List.toUnfoldable xs))

instance Debug a => Debug (ErlList.List a) where
  debug xs = D.collection "List" (map debug (ErlList.toUnfoldable xs))

instance Debug a => Debug (ErlList.NonEmptyList a) where
  debug xs = D.collection "List" (map debug (ErlNel.toUnfoldable xs))

instance debugLazyList :: Debug a => Debug (LazyList.List a) where
  debug xs = D.collection "List.Lazy" (map debug (LazyList.toUnfoldable xs))

instance debugSet :: Debug a => Debug (Set a) where
  debug s = D.collection "Set" (map debug (Set.toUnfoldable s))

instance debugDate :: Debug Date where
  debug d = D.opaqueLiteral "Date"
    ( ljust0 4 (show (fromEnum (year d))) <> "-"
        <> ljust0 2 (show (fromEnum (month d)))
        <> "-"
        <>
          ljust0 2 (show (fromEnum (day d)))
    )
    where
    ljust0 n str =
      power "0" (n - String.length str) <> str

instance debugRepr :: Debug D.Repr where
  debug r = D.opaque "Repr" r

instance debugReprDelta :: Debug D.ReprDelta where
  debug _ = D.opaque_ "ReprDelta"

instance Debug Binary where
  debug bin = D.constructor (Atom.atom "Binary") (ErlList.singleton (D.string $ (show $ Binary.byteSize bin) <> " bytes"))
