module Data.Array.Unique
  ( UniqueArray
  , fromFoldable
  , toUnfoldable
  , fromArray
  , toArray
  , empty
  , singleton
  , (..)
  , range
  , null
  , length
  , cons
  , snoc
  , insert
  , insertBy
  , insertAt
  , deleteAt
  , updateAt
  , modifyAt
  , alterAt
  , head
  , last
  , tail
  , init
  , uncons
  , unsnoc
  , (!!)
  , index
  , map
  , mapMaybe
  , concatMap
  , unsafeMap
  , unsafeTraverse
  , unsafeSequence
  , unsafeFromArray
  , unsafeMapMaybe
  , unsafeConcatMap
  , unsafeAppend
  ) where

import Prelude hiding (map)
import Prelude (map) as Prelude
import Data.Array
  ( fromFoldable
  , toUnfoldable
  , range
  , null
  , length
  , cons
  , snoc
  , foldM
  , insert
  , insertBy
  , head
  , last
  , tail
  , init
  , uncons
  , unsnoc
  , index
  , deleteAt
  , insertAt
  , updateAt
  , foldl
  , mapMaybe
  , concatMap
  ) as Array
import Data.Foldable (class Foldable, elem)
import Data.Unfoldable (class Unfoldable)
import Data.Maybe (Maybe (..))
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, JsonDecodeError (TypeMismatch))
import Data.ArrayBuffer.Class (class EncodeArrayBuffer, class DecodeArrayBuffer, class DynamicByteLength, readArrayBuffer)
import Data.Traversable (traverse, sequence)
import Control.Monad.Error.Class (throwError)
import Effect.Exception (throw)
import Test.QuickCheck (class Arbitrary, arbitrary)

newtype UniqueArray a = UniqueArray (Array a)

derive instance genericUniqueArray :: Generic a a' => Generic (UniqueArray a) _
derive newtype instance showUniqueArray :: Show a => Show (UniqueArray a)
derive newtype instance eqUniqueArray :: Eq a => Eq (UniqueArray a)
derive newtype instance ordUniqueArray :: Ord a => Ord (UniqueArray a)
derive newtype instance foldableUniqueArray :: Foldable UniqueArray
derive newtype instance encodeJsonUniqueArray :: EncodeJson a => EncodeJson (UniqueArray a)
instance decodeJsonUniqueArray :: (DecodeJson a, Eq a, Show a) => DecodeJson (UniqueArray a) where
  decodeJson json = do
    xs <- decodeJson json
    case fromArray xs of
      Nothing ->
        throwError $ TypeMismatch $ "unique: " <> show xs
      Just ys -> pure ys
derive newtype instance encodeArrayBufferUniqueArray :: EncodeArrayBuffer a => EncodeArrayBuffer (UniqueArray a)
instance decodeArrayBufferUniqueArray :: (DynamicByteLength a, DecodeArrayBuffer a, Eq a, Show a) => DecodeArrayBuffer (UniqueArray a) where
  readArrayBuffer b o = do
    mXs <- readArrayBuffer b o
    case mXs of
      Nothing -> pure Nothing
      Just xs -> case fromArray xs of
        Nothing -> do
          _ <- throw $ "Array not unique: " <> show xs
          pure Nothing
        Just ys -> pure (Just ys)
derive newtype instance dynamicByteLengthUniqueArray :: DynamicByteLength a => DynamicByteLength (UniqueArray a)
instance arbitraryUniqueArray :: (Arbitrary a, Eq a) => Arbitrary (UniqueArray a) where
  arbitrary = UniqueArray <<< nub <$> arbitrary
    where
      nub :: forall b. Eq b => Array b -> Array b
      nub =
        let go acc next
              | elem next acc = acc
              | otherwise = Array.snoc acc next
        in  Array.foldl go []

fromFoldable :: forall a f. Eq a => Foldable f => f a -> Maybe (UniqueArray a)
fromFoldable = fromArray <<< Array.fromFoldable

toUnfoldable :: forall a f. Unfoldable f => UniqueArray a -> f a
toUnfoldable = Array.toUnfoldable <<< toArray

fromArray :: forall a. Eq a => Array a -> Maybe (UniqueArray a)
fromArray = Array.foldM snoc empty

unsafeFromArray :: forall a. Array a -> UniqueArray a
unsafeFromArray = UniqueArray

toArray :: forall a. UniqueArray a -> Array a
toArray (UniqueArray x) = x

empty :: forall a. UniqueArray a
empty = UniqueArray []

singleton :: forall a. a -> UniqueArray a
singleton x = UniqueArray [x]

infix 8 range as ..

range :: Int -> Int -> UniqueArray Int
range x y = UniqueArray (Array.range x y)

null :: forall a. UniqueArray a -> Boolean
null (UniqueArray x) = Array.null x

length :: forall a. UniqueArray a -> Int
length (UniqueArray x) = Array.length x

cons :: forall a. Eq a => a -> UniqueArray a -> Maybe (UniqueArray a)
cons x (UniqueArray xs)
  | elem x xs = Nothing
  | otherwise = Just (UniqueArray (Array.cons x xs))

snoc :: forall a. Eq a => UniqueArray a -> a -> Maybe (UniqueArray a)
snoc (UniqueArray xs) x
  | elem x xs = Nothing
  | otherwise = Just (UniqueArray (Array.snoc xs x))

insert :: forall a. Ord a => a -> UniqueArray a -> Maybe (UniqueArray a)
insert x (UniqueArray xs)
  | elem x xs = Nothing
  | otherwise = Just (UniqueArray (Array.insert x xs))

insertBy :: forall a. Eq a => (a -> a -> Ordering) -> a -> UniqueArray a -> Maybe (UniqueArray a)
insertBy f x (UniqueArray xs)
  | elem x xs = Nothing
  | otherwise = Just (UniqueArray (Array.insertBy f x xs))

insertAt :: forall a. Eq a => Int -> a -> UniqueArray a -> Maybe (UniqueArray a)
insertAt i x (UniqueArray xs)
  | elem x xs = Nothing
  | otherwise = UniqueArray <$> Array.insertAt i x xs

deleteAt :: forall a. Int -> UniqueArray a -> Maybe (UniqueArray a)
deleteAt i (UniqueArray xs) = UniqueArray <$> Array.deleteAt i xs

updateAt :: forall a. Eq a => Int -> a -> UniqueArray a -> Maybe (UniqueArray a)
updateAt i x (UniqueArray xs)
  | elem x xs = Nothing
  | otherwise = UniqueArray <$> Array.updateAt i x xs

modifyAt :: forall a. Eq a => Int -> (a -> a) -> UniqueArray a -> Maybe (UniqueArray a)
modifyAt i f (UniqueArray xs) = case Array.index xs i of
  Nothing -> Nothing
  Just x ->
    let y = f x
    in  if elem y xs then Nothing else UniqueArray <$> Array.updateAt i y xs

alterAt :: forall a. Eq a => Int -> (a -> Maybe a) -> UniqueArray a -> Maybe (UniqueArray a)
alterAt i f (UniqueArray xs) = case Array.index xs i of
  Nothing -> Nothing
  Just x -> case f x of
    Nothing -> UniqueArray <$> Array.deleteAt i xs
    Just y
      | elem y xs -> Nothing
      | otherwise -> UniqueArray <$> Array.updateAt i y xs

head :: forall a. UniqueArray a -> Maybe a
head (UniqueArray xs) = Array.head xs

last :: forall a. UniqueArray a -> Maybe a
last (UniqueArray xs) = Array.last xs

tail :: forall a. UniqueArray a -> Maybe (UniqueArray a)
tail (UniqueArray xs) = UniqueArray <$> Array.tail xs

init :: forall a. UniqueArray a -> Maybe (UniqueArray a)
init (UniqueArray xs) = UniqueArray <$> Array.init xs

uncons :: forall a. UniqueArray a -> Maybe {head :: a, tail :: UniqueArray a}
uncons (UniqueArray xs) = case Array.uncons xs of
  Nothing -> Nothing
  Just {head: head', tail: tail'} -> Just {head: head', tail: UniqueArray tail'}

unsnoc :: forall a. UniqueArray a -> Maybe {init :: UniqueArray a, last :: a}
unsnoc (UniqueArray xs) = case Array.unsnoc xs of
  Nothing -> Nothing
  Just {init: init', last: last'} -> Just {init: UniqueArray init', last: last'}

infixl 8 index as !!

index :: forall a. UniqueArray a -> Int -> Maybe a
index (UniqueArray xs) i = Array.index xs i

map :: forall a b. Eq b => (a -> b) -> UniqueArray a -> Maybe (UniqueArray b)
map f (UniqueArray xs) = UniqueArray <$> Array.foldM go [] xs
  where
    go :: Array b -> a -> Maybe (Array b)
    go acc x =
      let y :: b
          y = f x
      in  if elem y acc then Nothing else Just (Array.snoc acc y)

unsafeMap :: forall a b. (a -> b) -> UniqueArray a -> UniqueArray b
unsafeMap f (UniqueArray xs) = UniqueArray (Prelude.map f xs)

unsafeTraverse :: forall a b m. Applicative m => (a -> m b) -> UniqueArray a -> m (UniqueArray b)
unsafeTraverse f (UniqueArray xs) = UniqueArray <$> traverse f xs

unsafeSequence :: forall a m. Applicative m => UniqueArray (m a) -> m (UniqueArray a)
unsafeSequence (UniqueArray xs) = UniqueArray <$> sequence xs

mapMaybe :: forall a b. Eq b => (a -> Maybe b) -> UniqueArray a -> Maybe (UniqueArray b)
mapMaybe f (UniqueArray xs) =
  let ys = Array.mapMaybe f xs
  in  fromArray ys

unsafeMapMaybe :: forall a b. (a -> Maybe b) -> UniqueArray a -> UniqueArray b
unsafeMapMaybe f (UniqueArray xs) =
  let ys = Array.mapMaybe f xs
  in  UniqueArray ys

concatMap :: forall a b. Eq b => (a -> UniqueArray b) -> UniqueArray a -> Maybe (UniqueArray b)
concatMap f (UniqueArray xs) =
  let ys = Array.concatMap ((\(UniqueArray zs) -> zs) <<< f) xs
  in  fromArray ys

unsafeConcatMap :: forall a b. (a -> UniqueArray b) -> UniqueArray a -> UniqueArray b
unsafeConcatMap f (UniqueArray xs) =
  let ys = Array.concatMap ((\(UniqueArray zs) -> zs) <<< f) xs
  in  UniqueArray ys

unsafeAppend :: forall a. UniqueArray a -> UniqueArray a -> UniqueArray a
unsafeAppend (UniqueArray xs) (UniqueArray ys) = UniqueArray (xs <> ys)

unsafeSnoc :: forall a. UniqueArray a -> a -> UniqueArray a
unsafeSnoc (UniqueArray xs) x = UniqueArray (Array.snoc xs x)
