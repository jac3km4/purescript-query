module Web.Query where
import Prelude

import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap, intercalate)
import Data.Int as Int
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, over)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Unfoldable1 (singleton)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Type.Prelude (class IsSymbol, RLProxy(..), SProxy(..), reflectSymbol)

type QueryParam = Array String

newtype QueryParams = QueryParams (Object QueryParam)

derive newtype instance semigroupQueryParams :: Semigroup QueryParams
derive newtype instance monoidQueryParams :: Monoid QueryParams
derive instance newtypeQueryParams :: Newtype QueryParams _

newtype DecodeError = DecodeError String

derive instance newtypeDecodeError :: Newtype DecodeError _

class QueryDecode a where
  decodeQueryParam :: QueryParam -> Either DecodeError a

instance stringQueryDecode :: QueryDecode String where
  decodeQueryParam [str] = Right str
  decodeQueryParam _ = Left $ DecodeError "Expected a single string"

instance intQueryDecode :: QueryDecode Int where
  decodeQueryParam [str] =
    maybe (Left $ DecodeError "Expected vald integer") Right $ Int.fromString str
  decodeQueryParam _ = Left $ DecodeError "Expected a single int"

instance boolQueryDecode :: QueryDecode Boolean where
  decodeQueryParam ["true"] = Right true
  decodeQueryParam ["false"] = Right false
  decodeQueryParam _ = Left $ DecodeError "Expected a single boolean"

instance arrayQueryDecode :: (QueryDecode a) => QueryDecode (Array a) where
  decodeQueryParam = traverse (decodeQueryParam <<< Array.singleton)

instance maybeQueryDecode :: (QueryDecode a) => QueryDecode (Maybe a) where
  decodeQueryParam [] = Right Nothing
  decodeQueryParam vs = Just <$> decodeQueryParam vs

class QueryDecodeFields (xs :: RowList) (from :: # Type) (to :: # Type)
  | xs -> from to where
  decodeQueryFields
    :: RLProxy xs
    -> QueryParams
    -> Either DecodeError (Builder (Record from) (Record to))

instance decodeQueryFieldsCons ::
  ( IsSymbol name
  , QueryDecode ty
  , QueryDecodeFields tail from from'
  , Row.Lacks name from'
  , Row.Cons name ty from' to
  ) => QueryDecodeFields (Cons name ty tail) from to where
  decodeQueryFields _ (QueryParams obj) = compose <$> field <*> rest
    where
      field = do
        value <- lmap namedError $ decodeQueryParam $ fold $ Object.lookup name obj
        pure $ Builder.insert nameP value
      namedError = over DecodeError \err -> err <> " at param " <> name
      rest = decodeQueryFields tailP (QueryParams obj)
      nameP = SProxy :: SProxy name
      tailP = RLProxy :: RLProxy tail
      name = reflectSymbol nameP

instance decodeQueryFieldsNil :: QueryDecodeFields Nil () () where
  decodeQueryFields _ _ = pure identity

decodeQueryR
  :: ∀ fields fieldList
   . RowToList fields fieldList
  => QueryDecodeFields  fieldList () fields
  => QueryParams
  -> Either DecodeError (Record fields)
decodeQueryR o = flip Builder.build {} <$> decodeQueryFields fieldListP o
  where
    fieldListP = RLProxy :: RLProxy fieldList

class QueryEncode a where
  encodeQueryParam :: a -> QueryParam

instance stringQueryParam :: QueryEncode String where
  encodeQueryParam = singleton

instance intQueryParam :: QueryEncode Int where
  encodeQueryParam = singleton <<< show

instance boolQueryParam :: QueryEncode Boolean where
  encodeQueryParam = singleton <<< show

instance arrayQueryParam :: (QueryEncode a) => QueryEncode (Array a) where
  encodeQueryParam = foldMap encodeQueryParam

class QueryEncodeFields (rl :: RowList) (row :: # Type) | rl -> row where
  encodeQueryFields :: RLProxy rl -> Record row -> QueryParams

instance consRecordQueryParams ::
  ( IsSymbol name
  , QueryEncode ty
  , QueryEncodeFields tail row
  , Row.Cons name ty rowtail row
  ) => QueryEncodeFields (Cons name ty tail) row where
  encodeQueryFields _ rec = over QueryParams withParam rest
    where
      name = SProxy :: SProxy name
      nameP = reflectSymbol name
      values = encodeQueryParam $ Record.get name rec
      rest = encodeQueryFields (RLProxy :: RLProxy tail) rec
      withParam = Object.alter (Just <<< append values <<< fold) nameP

instance nilRecordQueryParams :: QueryEncodeFields Nil row where
  encodeQueryFields _ _ = mempty

encodeQueryR
  :: ∀ fields fieldList
   . RowToList fields fieldList
  => QueryEncodeFields  fieldList fields
  => Record fields
  -> QueryParams
encodeQueryR rec = encodeQueryFields (RLProxy :: RLProxy fieldList) rec

renderQuery :: QueryParams -> String
renderQuery (QueryParams obj) = "?" <> intercalate "&" params
  where
    params = foldMap param $ Object.toArrayWithKey Tuple obj
    param (Tuple k vs) = map (\v -> k <> "=" <> v) vs
