module Data.Record.Pretty 
(
  class PPRowList
, class Pretty
, ppRowList
, ppRecord
, pp
)

where

import Prelude

import Data.Array (fromFoldable)
import Data.Foldable (class Foldable, fold, foldl)
import Data.List.NonEmpty (fromList)
import Data.List.Types (List(..), NonEmptyList(..))
import Data.Maybe (Maybe(..))
import Data.Record (get, delete)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Prettier.Printer (DOC, bracket, group, line, nest, nil, pretty, text)
import Type.Prelude (class RowToList, class RowLacks)
import Type.Row (kind RowList, Nil, Cons, RLProxy(..))

class Pretty a where
  pp :: a -> DOC

instance prettyFoldable ::
  ( Pretty a
  , Foldable f
  ) => Pretty (f a) where
  pp = pp <<< fromFoldable

instance prettyArrayRow :: 
  ( RowToList row list
  , PPRowList list row
  ) => Pretty (Record row) where
  pp = ppRecord

instance prettyArray :: (Pretty a) => Pretty (Array a) where
  pp list = text "[" <> items <> text "]"
    where
    items = foldl cc nil list
    cc b a = b <> text "," <> pp a

instance prettyShow :: (Show a) => Pretty a where
  pp = text <<< show

class PPRowList (list :: RowList) (row :: #Type) | list -> row where
  ppRowList :: RLProxy list -> Record row -> DOC

instance ppRowListNil :: PPRowList Nil () where
  ppRowList _ _ = nil

instance ppRowListConsRecord ::
  ( RowToList subrow sublist
  , PPRowList sublist subrow
  , PPRowList listRest rowRest
  , RowCons  key (Record subrow) rowRest rowFull
  , RowLacks key rowRest
  , RowToList rowFull (Cons key (Record subrow) listRest)
  , IsSymbol key
  ) => PPRowList (Cons key (Record subrow) listRest) rowFull where
  ppRowList _ rec = (text <<< reflectSymbol $ key)
                 <> text ":" 
                 <> ppRecord val
                 <> rest
    where
    key = SProxy :: SProxy key
    val = get key rec
    rest = ppRowList (RLProxy :: RLProxy listRest) (delete key rec)

instance ppRowListConsPretty ::
  ( Pretty a
  , PPRowList listRest rowRest
  , RowCons  key a rowRest rowFull
  , RowLacks key rowRest
  , RowToList rowFull (Cons key a listRest)
  , IsSymbol key
  ) => PPRowList (Cons key a listRest) rowFull where
  ppRowList _ rec = (text <<< reflectSymbol $ key)
                 <> text ":" 
                 <> (pp val)
                 <> text ","
                 <> line
                 <> rest
    where
    key = SProxy :: SProxy key
    val = get key rec
    rest = ppRowList (RLProxy :: RLProxy listRest) (delete key rec)

ppRecord
  :: forall row list
   . RowToList row list
  => PPRowList list row
  => Record row
  -> DOC
ppRecord rec = bracket "{" rowListStrs "}"
  where
  rowListStrs = ppRowList (RLProxy :: RLProxy list) rec
