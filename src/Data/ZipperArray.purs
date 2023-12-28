module Data.ZipperArray
  ( ZipperArray()
  , DeleteFocusedItemFocusBumpPolicy(..)
  , DeleteItemResult(..)

  , singleton
  , fromArray
  , fromNonEmptyArray

  , cons
  , cons'
  , appendArray
  , modifyCurrent
  , modifyAt
  , deleteWith

  , toArray
  , toNonEmptyArray
  , toCurrentSingleton
  , prec
  , current
  , succ
  , prev
  , next
  , foldlCurrent
  , foldrCurrent
  , foldlWithIndexCurrent
  , foldrWithIndexCurrent

  , atStart
  , atEnd
  , curIndex
  , length

  , focusWith
  , goIndex
  , goFirst
  , goLast
  , goPrev
  , goNext
  , mapCurrent
  ) where

import Prelude

import Control.MonadPlus (guard)
import Data.Array (foldMap, foldl, foldr)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (class FoldableWithIndex, foldMapWithIndex, foldlWithIndex, foldrWithIndex)
import Data.FunctorWithIndex (class FunctorWithIndex, mapWithIndex)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Natural (Natural, intToNat, natToInt)

-- | A `ZipperArray` is represented by a `NonEmptyArray`, an index into the
-- | array, and the element at that index. The constructor is kept private so
-- | that correctness guarantees can be ensured (as long as all the functions
-- | are correct).
data ZipperArray a = Private_ (NonEmptyArray a) Int a

-- | If the focused item is the one that was deleted, this policy describes which
-- | adjacent item should be focused (if possible).
data DeleteFocusedItemFocusBumpPolicy
  = DeleteFocusedItemFocusBumpPolicyGoPrev
  | DeleteFocusedItemFocusBumpPolicyGoNext

data DeleteItemResult a
  = DeleteItemFailure
  | DeleteItemSuccessEmpty
  | DeleteItemSuccess (ZipperArray a)

--------------------------------------------------------------------------------
-- Construction ----------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Constructs a ZipperArray from a single item. The current item of the
-- | resultant ZipperArray is the given item.
singleton :: forall a. a -> ZipperArray a
singleton cur = Private_ (NEA.singleton cur) 0 cur

-- | Constructs a ZipperArray from an array of items. Returns Nothing if the
-- | array is empty. Focuses the first item.
fromArray :: forall a. Array a -> Maybe (ZipperArray a)
fromArray = NEA.fromArray >>> map fromNonEmptyArray

-- | Constructs a ZipperArray from a `NonEmptyArray`. Focuses the first item.
fromNonEmptyArray :: forall a. NonEmptyArray a -> ZipperArray a
fromNonEmptyArray arr = Private_ arr 0 (NEA.head arr)

--------------------------------------------------------------------------------
-- Mutation --------------------------------------------------------------------
--------------------------------------------------------------------------------

cons :: forall a. a -> ZipperArray a -> ZipperArray a
cons x (Private_ arr i cur) = Private_ (NEA.cons x arr) (i + 1) cur

cons' :: forall a. a -> Array a -> ZipperArray a
cons' x xs = fromNonEmptyArray $ NEA.cons' x xs

appendArray :: forall a. ZipperArray a -> Array a -> ZipperArray a
appendArray (Private_ arr i cur) tail = Private_ (NEA.appendArray arr tail) i cur

modifyCurrent :: forall a. (a -> a) -> ZipperArray a -> ZipperArray a
modifyCurrent f (Private_ arr i cur) =
  Private_ (NEA.modifyAtIndices [ i ] f arr) i (f cur)

modifyAt :: forall a. Natural -> (a -> a) -> ZipperArray a -> Maybe (ZipperArray a)
modifyAt idx_ f (Private_ arr i cur) = do
  let idx = natToInt idx_
  arr' <- NEA.modifyAt idx f arr
  let cur' = if idx == i then f cur else cur
  pure $ Private_ arr' i cur'

deleteWith
  :: forall a
   . DeleteFocusedItemFocusBumpPolicy
  -> (a -> Boolean)
  -> ZipperArray a
  -> DeleteItemResult a
deleteWith policy p (Private_ arr i cur) = case NEA.findIndex p arr of
  Nothing -> DeleteItemFailure
  Just idx -> fromMaybe DeleteItemSuccessEmpty do
    arr' <- NEA.deleteAt idx arr >>= NEA.fromArray
    let
      i' =
        -- 1. An item before the current one was deleted, which bumps
        -- the current item's position down by one.
        if idx < i then i - 1

        -- 2. An item after the current one was deleted, so the current
        -- item's position doesn't change.
        else if idx > i then i

        -- By this point, we know the item that was deleted was the
        -- currently focused one.

        -- 3. The current item was deleted, and it was also the last
        -- element in the array. No matter what, the newly focused item
        -- must be the one before it.
        else if idx == NEA.length arr - 1 then i - 1

        else case policy, idx of
          -- 4. The policy says go prev, but we're already at the
          -- start! Don't change.
          DeleteFocusedItemFocusBumpPolicyGoPrev, 0 -> 0

          -- 5. The policy says go prev, and we're not at the start
          -- (caught by 4), so there must be a previous element. Focus
          -- it!
          DeleteFocusedItemFocusBumpPolicyGoPrev, _ -> i - 1

          -- 6. The policy says go next, and we're not at the end
          -- (caught by 3), so there must be a next element.  Focus
          -- it!
          DeleteFocusedItemFocusBumpPolicyGoNext, _ -> i + 1
    cur' <- NEA.index arr' i'
    pure $ DeleteItemSuccess $ Private_ arr' i' cur'

--------------------------------------------------------------------------------
-- Deconstruction --------------------------------------------------------------
--------------------------------------------------------------------------------

toArray :: forall a. ZipperArray a -> Array a
toArray (Private_ arr _ _) = NEA.toArray arr

toNonEmptyArray :: forall a. ZipperArray a -> NonEmptyArray a
toNonEmptyArray (Private_ arr _ _) = arr

-- | Returns a ZipperArray containing just the current item.
toCurrentSingleton :: forall a. ZipperArray a -> ZipperArray a
toCurrentSingleton (Private_ _ _ cur) = singleton cur

-- | Returns all the items preceding the current item in the array.
prec :: forall a. ZipperArray a -> Array a
prec (Private_ arr i _) = NEA.take i arr

-- | Returns the current item.
current :: forall a. ZipperArray a -> a
current (Private_ _ _ cur) = cur

-- | Returns all the items succeeding the current item in the array.
succ :: forall a. ZipperArray a -> Array a
succ (Private_ arr i _) = NEA.drop (i + 1) arr

-- | Returns the item immediately preceding the current item if it exists,
-- | Nothing otherwise.
prev :: forall a. ZipperArray a -> Maybe a
prev (Private_ arr i _) = NEA.index arr (i - 1)

-- | Returns the item immediately succeeding the current item if it exists,
-- | Nothing otherwise.
next :: forall a. ZipperArray a -> Maybe a
next (Private_ arr i _) = NEA.index arr (i + 1)

-- | Performs a foldl using the provided functions to transform the items. The
-- | `cur` function is used for the current item, and the `rest` function is
-- | used for the other items.
foldlCurrent
  :: forall a b
   . { cur :: b -> a -> b, rest :: b -> a -> b }
  -> b
  -> ZipperArray a
  -> b
foldlCurrent { cur, rest } b (Private_ arr i _) =
  foldlWithIndex (\j -> if i == j then cur else rest) b arr

-- | Performs a foldr using the provided functions to transform the items. The
-- | `cur` function is used for the current item, and the `rest` function is
-- | used for the other items.
foldrCurrent
  :: forall a b
   . { cur :: a -> b -> b, rest :: a -> b -> b }
  -> b
  -> ZipperArray a
  -> b
foldrCurrent { cur, rest } b (Private_ arr i _) =
  foldrWithIndex (\j -> if i == j then cur else rest) b arr

-- | Performs a foldlWithIndex using the provided functions to transform the
-- | items. The `cur` function is used for the current item, and the `rest`
-- | function is used for the other items.
foldlWithIndexCurrent
  :: forall a b
   . { cur :: Int -> b -> a -> b, rest :: Int -> b -> a -> b }
  -> b
  -> ZipperArray a
  -> b
foldlWithIndexCurrent { cur, rest } b (Private_ arr i _) =
  foldlWithIndex (\j -> (if i == j then cur else rest) j) b arr

-- | Performs a foldrWithIndex using the provided functions to transform the
-- | items. The `cur` function is used for the current item, and the `rest`
-- | function is used for the other items.
foldrWithIndexCurrent
  :: forall a b
   . { cur :: Int -> a -> b -> b, rest :: Int -> a -> b -> b }
  -> b
  -> ZipperArray a
  -> b
foldrWithIndexCurrent { cur, rest } b (Private_ arr i _) =
  foldrWithIndex (\j -> (if i == j then cur else rest) j) b arr

--------------------------------------------------------------------------------
-- Inspection ------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Returns true if the current item is the head of the array, false otherwise.
atStart :: forall a. ZipperArray a -> Boolean
atStart (Private_ _ i _) = i == 0

-- | Returns true if the current item is the last item in the array, false
-- | otherwise.
atEnd :: forall a. ZipperArray a -> Boolean
atEnd (Private_ arr i _) = i == NEA.length arr - 1

-- | Returns the index of the current item in the array.
curIndex :: forall a. ZipperArray a -> Natural
curIndex (Private_ _ i _) = intToNat i

-- | Returns the number of items in the array.
length :: forall a. ZipperArray a -> Int
length (Private_ arr _ _) = NEA.length arr

--------------------------------------------------------------------------------
-- Traversals ------------------------------------------------------------------
--------------------------------------------------------------------------------

focusWith :: forall a. (a -> Boolean) -> ZipperArray a -> Maybe (ZipperArray a)
focusWith p (Private_ arr _ _) = do
  i <- NEA.findIndex p arr
  cur <- NEA.index arr i
  pure $ Private_ arr i cur

goIndex :: forall a. Natural -> ZipperArray a -> Maybe (ZipperArray a)
goIndex idx_ (Private_ arr i _) = do
  let idx = natToInt idx_
  guard $ idx < NEA.length arr
  cur <- NEA.index arr idx
  pure $ Private_ arr idx cur

-- | Focuses the head of the array.
goFirst :: forall a. ZipperArray a -> ZipperArray a
goFirst (Private_ arr _ _) = Private_ arr 0 (NEA.head arr)

-- | Focuses the last item in the array.
goLast :: forall a. ZipperArray a -> ZipperArray a
goLast (Private_ arr _ _) = Private_ arr (NEA.length arr - 1) (NEA.last arr)

-- | Focuses the previous item in the array if it exists. Returns Nothing if
-- | there is no item preceding the current item.
goPrev :: forall a. ZipperArray a -> Maybe (ZipperArray a)
goPrev z@(Private_ arr i cur) = do
  cur' <- prev z
  pure $ Private_ arr (i - 1) cur'

-- | Focuses the next item in the array if it exists. Returns Nothing if there
-- | is no item succeeding the current item.
goNext :: forall a. ZipperArray a -> Maybe (ZipperArray a)
goNext z@(Private_ arr i _) = Private_ arr (i + 1) <$> next z

-- | Performs a map, using the provided functions to transform the items. The
-- | `cur` function is used for the current item, and the `rest` function is
-- | used for the other items.
mapCurrent
  :: forall a b
   . { cur :: a -> b, rest :: a -> b }
  -> ZipperArray a
  -> ZipperArray b
mapCurrent { cur, rest } (Private_ arr i c) =
  Private_ (mapWithIndex (\j -> if i == j then cur else rest) arr) i (cur c)

--------------------------------------------------------------------------------
-- Instances -------------------------------------------------------------------
--------------------------------------------------------------------------------

derive instance eqZipperArray :: Eq a => Eq (ZipperArray a)

instance showZipperArray :: Show a => Show (ZipperArray a) where
  show (Private_ arr i cur) =
    "(ZipperArray " <> show arr <> " " <> show i <> " " <> show cur <> ")"

instance functorZipperArray :: Functor ZipperArray where
  map f (Private_ arr i cur) = Private_ (map f arr) i (f cur)

instance functorWithIndex :: FunctorWithIndex Int ZipperArray where
  mapWithIndex f (Private_ arr i cur) = Private_ (mapWithIndex f arr) i (f i cur)

instance foldableZipperArray :: Foldable ZipperArray where
  foldl f z zArr = foldl f z (toArray zArr)
  foldr f z zArr = foldr f z (toArray zArr)
  foldMap f zArr = foldMap f (toArray zArr)

instance foldableWithIndexZipperArray :: FoldableWithIndex Int ZipperArray where
  foldrWithIndex f z zArr = foldrWithIndex f z (toArray zArr)
  foldlWithIndex f z zArr = foldlWithIndex f z (toArray zArr)
  foldMapWithIndex f zArr = foldMapWithIndex f (toArray zArr)

derive instance eqDeleteItemResult :: Eq a => Eq (DeleteItemResult a)

instance showDeleteItemResult :: Show a => Show (DeleteItemResult a) where
  show = case _ of
    DeleteItemFailure -> "DeleteItemFailure"
    DeleteItemSuccessEmpty -> "DeleteItemSuccessEmpty"
    DeleteItemSuccess z -> "(DeleteItemSuccess " <> show z <> ")"
