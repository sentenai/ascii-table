{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE ViewPatterns           #-}

-- |
--
-- Let\'s make a table!
--
-- @
-- > let Just ('Object' o1) = 'Data.Aeson.decode' \"{\\\"foo\\\": \\\"bar\\\"}\"
-- > let Just ('Object' o2) = 'Data.Aeson.decode' \"{\\\"baz\\\": 5}\"
-- > let Just ('Object' o3) = 'Data.Aeson.decode' \"{\\\"oink\\\": true}\"
--
-- > let slice1 = [[Just o1, Just o3], [Just o2, Nothing]]
-- > let slice2 = [[Nothing, Just o1]]
--
-- > 'pretty' ('makeTable' [\"object 1\", \"object 2\"] [slice1, slice2, slice1])
-- +-----------+------------+
-- | object 1  | object 2   |
-- |           |            |
-- | baz foo   | foo   oink |
-- +===========+============+
-- |     \"bar\" |       True |
-- | 5.0       |            |
-- +-----------+------------+
-- |           | \"bar\"      |
-- +-----------+------------+
-- |     \"bar\" |       True |
-- | 5.0       |            |
-- +-----------+------------+
-- @

module Data.AsciiTable
  ( Table
  , TableRow
  , TableSlice
  , makeTable
  , makeTableWith
    -- * Misc. helper functions
  , prettyValue
  , flattenObject
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

import Control.Applicative   (pure)
import Data.Aeson            (Object, Value(..))
import Data.Foldable         (foldl')
import Data.Hashable         (Hashable)
import Data.HashMap.Strict   (HashMap)
import Data.List             (transpose)
import Data.Maybe            (fromMaybe)
import Data.Monoid           ((<>), mempty)
import Data.Set              (Set)
import Data.Text             (Text)
import Text.PrettyPrint.Free hiding ((<>))

import qualified Data.HashMap.Strict    as HashMap
import qualified Data.Set               as Set
import qualified Data.Text              as Text
import qualified Data.Vector            as Vector

{-

   Table terminology:

   +-------------+-------------+--------
   | SliceHdr    | SliceHdr    |
   | CHdr   CHdr |             |
   +=============+=============+========
   | TableElem   | TableElem   |
   | TableElem   | TableElem   |
   | TableElem   | TableElem   |
   | TableElem   | TableElem   |
   | ...         | ...
   +------------------------------------
   | TableRow
   | TableRow
   | TableRow
   | TableRow
   | TableRow
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

-- | A single horizontal row of a 'Table'. Each row is visually separated from
-- the next by a vertical line. Each row in the table must contain the same
-- number of elements (however, any number of them can be 'Nothing').
type TableRow a = [Maybe a]

-- | A single horizontal slice of a 'Table', containing one or more 'TableRow's.
-- Each slice is visually separated from the next by a horizontal line.
type TableSlice a = [TableRow a]

-- | An opaque data type with a 'Pretty' instance, for printing to a console.
-- Build a table with 'makeTable', and show it with the pretty-printing
-- functions re-exported from this module.
data Table = Table
  { tableHeaders     :: [Text]
  , tableCellHeaders :: [[Text]]
  , tableSlices      :: [[[[Text]]]]
  } deriving (Eq, Show)

instance Pretty Table where
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
    ppTableSlice :: [[Int]] -> [[[Text]]] -> Doc e
    ppTableSlice ns rs =
      vsep (map (ppTableRow ns) rs)
      `above`
      tableSliceSep '-' ns

    ppTableRow :: [[Int]] -> [[Text]] -> Doc e
    ppTableRow nss rs = hsep (zipWith ppTableElem nss rs) <+> "|"
     where
      ppTableElem :: [Int] -> [Text] -> Doc e
      ppTableElem ns es = "|" <+> hsep (zipWith ppTableCell ns es)
       where
        ppTableCell :: Int -> Text -> Doc e
        ppTableCell n c = fill n (text (Text.unpack (escapeTabAndNewline c)))

    ppTableHeaders :: [[Int]] -> [Text] -> Doc e
    ppTableHeaders nss hs = hsep (zipWith ppTableHeader nss hs) <+> "|"
     where
      ppTableHeader :: [Int] -> Text -> Doc e
      ppTableHeader ns h = "|" <+> fill (elemWidth ns) (text (Text.unpack (escapeTabAndNewline h)))

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

        adjust :: Int -> [Int] -> [Int]
        adjust n ns =
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
        zipWith adjust (map Text.length tableHeaders) ws0
     where
      unadjustedTableWidths :: [[[Text]]] -> [[Int]]
      unadjustedTableWidths =
          map (map (maximum . map Text.length) . transpose)
        . transpose

      unsnoc :: [a] -> Maybe ([a], a)
      unsnoc [] = Nothing
      unsnoc [x] = Just ([], x)
      unsnoc (x:xs) = do
        (ys,y) <- unsnoc xs
        pure (x:ys,y)

    elemWidth :: [Int] -> Int
    elemWidth = foldr (\x y -> x+y+1) (-1)


-- | Make a 'Table' from a list of headers and a list of 'TableSlice's, each of
-- which contains a list of 'TableRow's, each of which contain a list of
-- 'Object's. It is assumed that all dimensions align properly (e.g. each row
-- contains the same number of elements, which is equal to the length of the
-- list of headers).
--
-- Each top-level object is flattened into one column per leaf. Note that this
-- means it is not possible to distinguish between e.g. @{\"foo\":{\"bar\":5}}@
-- and @{\"foo.bar\":5}@. Hopefully this is not too much of a problem in
-- practice.
--
-- Each vertically aligned element need not contain the same set of keys; for
-- example, the table corresponding to
--
-- @
-- [ [{\"foo\": \"bar\"}], [{\"baz\": \"qux\"}] ] -- One 'TableSlice'
-- @
--
-- will simply look like
--
-- @
-- +-------------+
-- | foo   baz   |
-- +=============+
-- | \"bar\"       |
-- |       \"qux\" |
-- +-------------+
-- @
--
-- That is, each missing value is simply not displayed.
--
makeTable
  :: [Text]              -- ^ Headers
  -> [TableSlice Object] -- ^ Table slices
  -> Table
makeTable headers slices =
  makeTableWith id (\_ -> id) (\_ _ -> prettyValue) headers (flat slices)
 where
  flat :: [TableSlice Object] -> [TableSlice Object]
  flat = (map . map . map . fmap) flattenObject

-- | Like 'makeTable', but takes explicit rendering functions. This is useful for
-- adding ANSI escape codes to color output, or for rendering values depending on
-- what their key is.
--
-- For example, you may wish to render 'String's with a @\"timestamp\"@ key
-- without quotation marks.
makeTableWith
  :: forall h k v.
     (Ord k, Hashable k)
  => (h -> Text)                -- ^ Header rendering function
  -> (h -> k -> Text)           -- ^ Cell header rendering function
  -> (h -> k -> v -> Text)      -- ^ Cell rendering function
  -> [h]                        -- ^ Headers
  -> [TableSlice (HashMap k v)] -- ^ Table slices
  -> Table
makeTableWith showH showK showV headers slices =
  Table headers' cell_headers' slices'
 where
  cell_headers :: [[k]]
  cell_headers =
      map (Set.toAscList . foldl' step mempty)
    . transpose
    . concat
    $ slices
   where
    step :: Set k -> Maybe (HashMap k v) -> Set k
    step acc Nothing  = acc
    step acc (Just x) = acc <> Set.fromList (HashMap.keys x)

  headers':: [Text]
  headers' = map showH headers

  cell_headers' :: [[Text]]
  cell_headers' = zipWith (map . showK) headers cell_headers

  slices' :: [[[[Text]]]]
  slices' =
    (map . map) (zipWith3 go headers cell_headers) slices
   where
    go :: h -> [k] -> Maybe (HashMap k v) -> [Text]
    go h ks (fromMaybe mempty -> m) =
      map
        (\k ->
          case HashMap.lookup k m of
            Nothing -> ""
            Just v  -> showV h k v)
        ks

-- | Pretty-print a 'Value' in one line.
prettyValue :: Value -> Text
prettyValue = \case
  Object o ->
    "{"
    <> Vector.ifoldr'
      (\i (k,v) acc ->
        "\""
        <> k
        <> "\":"
        <> prettyValue v
        <> if i == HashMap.size o - 1
            then acc
            else ", " <> acc)
      mempty
      (Vector.fromList (HashMap.toList o))
    <> "}"
  Array a ->
    "["
    <> Vector.ifoldr'
      (\i v acc ->
        if i == Vector.length a - 1
          then prettyValue v <> acc
          else prettyValue v <> ", " <> acc)
      mempty
      a
    <> "]"
  String s -> "\"" <> s <> "\""
  Number n -> Text.pack (show n)
  Bool b   -> Text.pack (show b)
  Null     -> "null"

-- | Flatten an 'Object' so that it contains no top-level 'Object' values.
flattenObject :: Object -> Object
flattenObject = foldMap go . HashMap.toList
 where
  go :: (Text, Value) -> Object
  go (k, v) =
    case v of
      Object o -> HashMap.fromList (map (prependKey k) (HashMap.toList (flattenObject o)))
      _        -> HashMap.singleton k v

  prependKey :: Text -> (Text, Value) -> (Text, Value)
  prependKey k0 (k1, v) = (k0 <> "." <> k1, v)

-- | Escape tabs and newlines in a 'Text'.
escapeTabAndNewline :: Text -> Text
escapeTabAndNewline =
    Text.replace (Text.singleton '\n') "\\n"
  . Text.replace (Text.singleton '\t') "\\t"
