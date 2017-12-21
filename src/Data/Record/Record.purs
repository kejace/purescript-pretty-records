module Data.Record.Pretty 
(
  class PPRowList
, ppRowList
, ppRecord
)

where

import Prelude

import Data.Record (get, delete)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Prelude (class RowToList, class RowLacks)
import Type.Row (kind RowList, Nil, Cons, RLProxy(..))

import Prettier.Printer (DOC, bracket, group, line, nest, nil, pretty, text)

class PPRow (row :: # Type) where
  ppRow :: Record row -> DOC

instance ppRowNil :: PPRow row where
  ppRow rec = text "hllo"

class PPRowList (list :: RowList) (row :: # Type) | list -> row where
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

instance ppRowListConsShow ::
  ( Show a
  , PPRowList listRest rowRest
  , RowCons  key a rowRest rowFull
  , RowLacks key rowRest
  , RowToList rowFull (Cons key a listRest)
  , IsSymbol key
  ) => PPRowList (Cons key a listRest) rowFull where
  ppRowList _ rec = (text <<< reflectSymbol $ key)
                 <> text ":" 
                 <> (text <<< show $ val)
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
ppRecord rec = bracket "{" rowListStrs  "}"
  where
  rowListStrs = ppRowList (RLProxy ::RLProxy list) rec
