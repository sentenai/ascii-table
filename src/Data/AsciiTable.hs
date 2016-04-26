{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.AsciiTable
  ( Table
  , TableRow
  , TableSlice
  , TableElem(..)
  , NamedObject(..)
  , makeTable
    -- * Re-exports
  , Doc
  , putDoc
  , hPutDoc
  , Pretty(..)
  , SimpleDoc(..)
  , renderPretty
  , renderCompact
  , renderSmart
  , displayS
  , displayIO
  ) where

import Sentenai.Prelude

import Data.Aeson            (Object, Value(..))
import Data.DList            (DList)
import Data.HashMap.Strict   (HashMap)
import Text.PrettyPrint.Free hiding ((<>))

import qualified Data.DList             as DList
import qualified Data.HashMap.Strict    as HashMap
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import qualified Data.Text.Lazy.Builder as LTBuilder
import qualified Data.Vector            as Vector

{-

   Example table:

   +-------------+-------------+--------
   | SliceHdr    | SliceHdr    |
   | CHdr   CHdr |             |
   +=============+=============+========
   | Cell   Cell | RowSlice    |
   | Cell   Cell | RowSlice    |
   | Cell   Cell | RowSlice    |
   | Cell   Cell | RowSlice    |
   | ...         | ...
   +------------------------------------
   | Row
   | Row
   | Row
   | Row
   | Row
   | ...
   +-------------+-------------+--------
   | TableSlice
   |
   |
   |
   |
   |
   +-------------+-------------+--------


-}

type TableRow      a = [a]
type TableSlice    a = [TableRow a]

data Table = Table
  { tableHeaders     :: [Text]
  , tableCellHeaders :: [[Text]]
  , tableSlices      :: [TableSlice [Text]]
  } deriving (Eq, Show)

instance Pretty Table where
  pretty :: Table -> Doc e
  pretty table =
    let
      widths = tableWidths table
    in
      vcat
        [ tableSliceSep '-' widths
        , ppTableHeaders widths (tableHeaders table)
        , ppTableHeaders widths (map (const "") (tableHeaders table))
        , ppTableRow widths (tableCellHeaders table)
        , tableSliceSep '=' widths
        , vsep (map (ppTableSlice widths) (tableSlices table))
        ]
   where
    ppTableSlice :: [[Int]] -> TableSlice [Text] -> Doc e
    ppTableSlice ns rs =
      vsep (map (ppTableRow ns) rs)
      `above`
      tableSliceSep '-' ns

    ppTableRow :: [[Int]] -> TableRow [Text] -> Doc e
    ppTableRow nss rs = hsep (map (uncurry ppTableElem) (zip nss rs)) <+> "|"
     where
      ppTableElem :: [Int] -> [Text] -> Doc e
      ppTableElem ns es = "|" <+> hsep (map (uncurry ppTableCell) (zip ns es))
       where
        ppTableCell :: Int -> Text -> Doc e
        ppTableCell n c = fill n (text (cs c))

    ppTableHeaders :: [[Int]] -> [Text] -> Doc e
    ppTableHeaders nss hs = hsep (map (uncurry ppTableHeader) (zip nss hs)) <+> "|"
     where
      ppTableHeader :: [Int] -> Text -> Doc e
      ppTableHeader ns h = "|" <+> fill (elemWidth ns) (text (Text.unpack h))

    tableSliceSep :: Char -> [[Int]] -> Doc e
    tableSliceSep c = (<> "+") . hcat . map elemSep
     where
      elemSep :: [Int] -> Doc e
      elemSep ns = "+" <> text (replicate (2 + elemWidth ns) c)

    -- | Possibly grow the last element in each inner lists's width, if the name
    -- of the entire element is sufficiently long.
    tableWidths :: Table -> [[Int]]
    tableWidths Table{..} =
      let
        ws0 :: [[Int]]
        ws0 = unadjustedTableWidths (tableCellHeaders : concat tableSlices)

        adjust :: (Int, [Int]) -> [Int]
        adjust (n, ns) =
          case unsnoc ns of
            Nothing -> []
            Just (ms, m) ->
              let
                len = foldl' (\x y -> x+y+1) (-1) ns
              in
                if n > len
                   then ms ++ [m + n - len]
                   else ns
      in
        map adjust (zip (map Text.length tableHeaders) ws0)
     where
      unadjustedTableWidths :: [TableRow [Text]] -> [[Int]]
      unadjustedTableWidths =
          map (map (maximum . map Text.length))
        . map transpose
        . transpose

      unsnoc :: [a] -> Maybe ([a], a)
      unsnoc [] = Nothing
      unsnoc [x] = Just ([], x)
      unsnoc (x:xs) = do
        (ys,y) <- unsnoc xs
        pure (x:ys,y)

    elemWidth :: [Int] -> Int
    elemWidth = foldr (\x y -> x+y+1) (-1)


class TableElem a where
  tableElemHeader :: a -> Text
  tableElemCells :: a -> HashMap Text Text

  tableElemCellHeaders :: a -> Set Text
  tableElemCellHeaders = Set.fromList . HashMap.keys . tableElemCells


data NamedObject = NamedObject Text Object
  deriving (Eq, Show)

instance TableElem NamedObject where
  tableElemHeader :: NamedObject -> Text
  tableElemHeader (NamedObject name _) = name

  tableElemCells :: NamedObject -> HashMap Text Text
  tableElemCells (NamedObject _ obj) = HashMap.fromList (DList.toList (objectCells obj))
   where
    objectCells :: Object -> DList (Text, Text)
    objectCells = foldl' step mempty . HashMap.toList
     where
      step :: DList (Text, Text) -> (Text, Value) -> DList (Text, Text)
      step acc (k, v) = acc <>
        case v of
          Object o ->
            map (\(k',v') ->
                  let k'' :: LTBuilder.Builder
                      k'' = LTBuilder.fromText k
                         <> LTBuilder.singleton '.'
                         <> LTBuilder.fromText k'
                  in (cs (LTBuilder.toLazyText k''), v'))
                (objectCells o)
          _ -> pure (k, cs (LTBuilder.toLazyText (showValue v)))

      -- Show a 'Value' in one line.
      showValue :: Value -> LTBuilder.Builder
      showValue (Object o) =
           LTBuilder.singleton '{'
        <> foldr (\(k,v) acc ->
                      LTBuilder.fromText k
                   <> ":"
                   <> showValue v
                   <> ", "
                   <> acc)
                 mempty
                 (HashMap.toList o)
        <> LTBuilder.singleton '}'
      showValue (Array a)  =
           LTBuilder.singleton '['
        <> Vector.foldr' (\v acc -> showValue v <> ", " <> acc) mempty a
        <> LTBuilder.singleton ']'
      showValue (String s) =
           LTBuilder.singleton '"'
        <> LTBuilder.fromText s
        <> LTBuilder.singleton '"'
      showValue (Number n) = LTBuilder.fromLazyText (show n)
      showValue (Bool b)   = LTBuilder.fromLazyText (show b)
      showValue Null       = "null"


makeTable :: forall a. TableElem a => [TableSlice a] -> Table
makeTable slices =
  let
    headers :: [Text]
    headers
      | Just slice <- head slices
      , Just row   <- head slice
        = map tableElemHeader row
      | otherwise = []

    cell_headers :: [[Text]]
    cell_headers =
      let
        step :: Set Text -> HashMap Text Text -> Set Text
        step acc x = acc <> Set.fromList (HashMap.keys x)
      in
          map (Set.toAscList . foldl' step mempty)
        . transpose
        . concat
        $ elems

    elems :: [TableSlice (HashMap Text Text)]
    elems = map (map (map tableElemCells)) slices

    text_elems :: [TableSlice [Text]]
    text_elems =
      map (map (map (uncurry go))) (map (map (flip zip cell_headers)) elems)
     where
      go :: HashMap Text Text -> [Text] -> [Text]
      go m = map (\k -> HashMap.lookupDefault "" k m)
  in
    Table headers cell_headers text_elems
