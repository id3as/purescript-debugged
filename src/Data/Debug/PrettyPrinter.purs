-- | A simple pretty-printer, intended for use with Debug.Repr values.
module Data.Debug.PrettyPrinter where

import Prelude

import Data.Array as Array
import Data.Foldable (fold, foldMap, intercalate)
import Data.Maybe (Maybe(..), fromJust)
import Data.Monoid.Additive (Additive(..))
import Data.String as String
import Erl.Data.List (List)
import Erl.Data.List as List
import Partial.Unsafe (unsafePartial)

data FirstMiddleLast a
  = Empty
  | Single a
  | TwoOrMore a (List a) a

firstMiddleLast :: forall a. List a -> FirstMiddleLast a
firstMiddleLast l =
  case List.uncons l of
    Nothing ->
      Empty
    Just { head, tail } ->
      if List.null tail then
        Single head
      else do
        let n = List.length tail
        unsafePartial $
          TwoOrMore
            head
            (List.slice 1 (n - 1) l)
            (fromJust $ List.last tail)

-- | An intermediate type used while folding a tree for pretty-printing.
type AnyContent r =
  -- The size of the tree so far; controls whether a tree is printed over
  -- multiple lines or not.
  { size :: Additive Int
  -- The pretty-printed content. Each element in the array corresponds to one
  -- line in the eventual output.
  , lines :: List String
  | r
  }

type Content = AnyContent
  -- If this content is eventually displayed in a context where it might need
  -- parentheses, such as in an argument to a data constructor, should those
  -- parentheses be included?
  (needsParens :: Boolean)

-- | A variant of `Content` which is guaranteed to be wrapped in parens if they
-- | are going to be necessary. The `needsParens` field has therefore been
-- | dropped. Note that this type has a Monoid instance, whereas `Content` does
-- | not (because of the extra Boolean field); this is on purpose, since we can
-- | only safely concatenate `Content` values once we know they have been
-- | parenthesised where necessary.
type ContentParens = AnyContent ()

-- | Wrap a `Content` in parens. To be used in contexts where parens might be
-- | needed.
wrap :: Content -> ContentParens
wrap c =
  { size: c.size
  , lines: if c.needsParens then surroundLines "(" ")" c.lines else c.lines
  }

-- | Mark a `Content` as being in a context where parens are not needed.
noWrap :: forall r. AnyContent r -> ContentParens
noWrap c =
  { size: c.size
  , lines: c.lines
  }

unParens :: forall r. Boolean -> AnyContent r -> Content
unParens needsParens r =
  { size: r.size
  , lines: r.lines
  , needsParens
  }

-- | Turn a `ContentParens` back into a `Content`; for use in contexts where
-- | parens might be needed.
parens :: forall r. AnyContent r -> Content
parens = unParens true

-- | Turn a `ContentParens` back into a `Content`; for use in contexts where
-- | parens will not be needed.
noParens :: forall r. AnyContent r -> Content
noParens = unParens false

emptyContent :: Content
emptyContent =
  { size: Additive 0
  , lines: List.nil
  , needsParens: false
  }

withLines
  :: forall r
   . (List String -> List String)
  -> AnyContent r
  -> AnyContent r
withLines f rec =
  rec { lines = f rec.lines }

-- | Reduce a multiple-line pretty-printed expression down to one line.
compactLines :: List String -> List String
compactLines =
  firstMiddleLast
    >>> case _ of
      Empty ->
        List.nil
      Single x ->
        List.singleton x
      TwoOrMore first middle last ->
        List.singleton $ intercalate " "
          ((List.singleton first) <> (String.trim <$> middle) <> (List.singleton $ String.trim last))

compact :: forall r. AnyContent r -> AnyContent r
compact = withLines compactLines

surroundLines :: String -> String -> List String -> List String
surroundLines start finish =
  firstMiddleLast >>>
    case _ of
      Empty ->
        List.singleton $ start <> finish
      Single item ->
        List.singleton $ start <> item <> finish
      TwoOrMore first middle last ->
        (List.singleton $ start <> first)
          <> middle
          <> (List.singleton $ last <> finish)

surround :: forall r. String -> String -> AnyContent r -> AnyContent r
surround start finish = withLines (surroundLines start finish)

printContent :: forall r. AnyContent r -> String
printContent { lines } = intercalate "\n" lines

-- | Create a `Content` from the label of a leaf node, using the label's `Show`
-- | instance.
leaf :: forall a. Show a => a -> Content
leaf x =
  { lines: List.singleton $ show x
  , size: Additive 1
  , needsParens: false
  }

verbatim :: String -> ContentParens
verbatim x =
  { lines: List.singleton x
  , size: Additive 1
  }

indent :: forall r. String -> AnyContent r -> AnyContent r
indent prefix r =
  r { lines = map (prefix <> _) r.lines }

-- | Produce a comma separated sequence over multiple lines with the given
-- | beginning and ending string sequences.
commaSeq :: forall r. String -> String -> List (AnyContent r) -> Content
commaSeq begin end contents =
  { size: foldMap _.size contents
  , lines: commaSeqLines begin end (map _.lines contents)
  , needsParens: false
  }

commaSeqLines :: String -> String -> List (List String) -> List String
commaSeqLines begin end =
  firstMiddleLast >>> go >>> List.filter (_ /= "")
  where
  go =
    case _ of
      Empty ->
        List.singleton $ begin <> end
      Single item ->
        surroundLines begin end item
      TwoOrMore first middle last ->
        surroundLines begin "," first
          <> List.concatMap (surroundLines spacer ",") middle
          <> surroundLines spacer end last

  spacer = fold (Array.replicate (String.length begin) " ")

withLast :: forall a. (a -> a) -> Array a -> Array a
withLast f xs =
  case Array.length xs of
    0 ->
      []
    n ->
      unsafePartial $
        Array.slice 0 (n - 2) xs <> [ f (Array.unsafeIndex xs (n - 1)) ]
