module Web.Query where
import Data.Foldable (foldMap, intercalate)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Tuple (Tuple(..))
import Data.Unfoldable1 (singleton)
import Prelude
import Prim.Row as Row
import Prim.RowList (class RowToList, Cons, Nil, kind RowList)
import Record as Record
import Type.Prelude (RLProxy(..))

type QueryParam = Array String

newtype QueryParams = QueryParams (Array (Tuple String String))

derive newtype instance semigroupQueryParams :: Semigroup QueryParams
derive newtype instance monoidQueryParams :: Monoid QueryParams

instance showQueryParams :: Show QueryParams where
  show (QueryParams map) = "?" <> intercalate "&" (param <$> map)
    where
      param (Tuple k v) = k <> "=" <> v

class QueryParamEncoder a where
  encodeParam :: a -> QueryParam

instance stringQueryParam :: QueryParamEncoder String where
  encodeParam = singleton

instance intQueryParam :: QueryParamEncoder Int where
  encodeParam = singleton <<< show

instance boolQueryParam :: QueryParamEncoder Boolean where
  encodeParam = singleton <<< show

instance arrayQueryParam :: (QueryParamEncoder a) => QueryParamEncoder (Array a) where
  encodeParam = foldMap encodeParam

class QueryEncoder a where
  encodeParams :: a -> QueryParams

instance recordQueryParams ::
  ( RowToList row rl
  , QueryParamFields rl row
  ) => QueryEncoder (Record row) where
    encodeParams rec = encodeParamsImpl (RLProxy :: RLProxy rl) rec

class QueryParamFields (rl :: RowList) (row :: # Type)
  | rl -> row where
  encodeParamsImpl :: RLProxy rl -> Record row -> QueryParams

instance consRecordQueryParams ::
  ( IsSymbol name
  , QueryParamEncoder ty
  , QueryParamFields tail row
  , Row.Cons name ty rowtail row
  ) => QueryParamFields (Cons name ty tail) row where
  encodeParamsImpl _ rec = append (QueryParams values) rest
    where
      sym :: SProxy name
      sym = SProxy
      key = reflectSymbol sym
      values = map (Tuple key) $ encodeParam $ Record.get sym rec
      rest = encodeParamsImpl (RLProxy :: RLProxy tail) rec

instance nilRecordQueryParams :: QueryParamFields Nil row where
  encodeParamsImpl _ _ = mempty
