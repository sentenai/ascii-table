{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

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
import Data.Char             (isPrint)
import Data.Foldable         (foldl', foldMap)
import Data.Hashable         (Hashable)
import Data.HashMap.Strict   (HashMap)
import Data.List             (transpose)
import Data.Maybe            (fromMaybe)
import Data.Monoid           ((<>), mempty)
import Data.Set              (Set)
import Data.Text             (Text, pack, unpack)
import Text.PrettyPrint.Free hiding ((<>), text)

import qualified Data.HashMap.Strict            as HashMap
import qualified Data.Set                       as Set
import qualified Data.Vector                    as Vector
import qualified Text.PrettyPrint.Free.Internal as PrettyPrint

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
  { tableHeaders     :: [String]
  , tableCellHeaders :: [[String]]
  , tableSlices      :: [[[[String]]]]
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
    ppTableSlice :: [[Int]] -> [[[String]]] -> Doc e
    ppTableSlice ns rs =
      vsep (map (ppTableRow ns) rs)
      `above`
      tableSliceSep '-' ns

    ppTableRow :: [[Int]] -> [[String]] -> Doc e
    ppTableRow nss rs = hsep (zipWith ppTableElem nss rs) <+> "|"
     where
      ppTableElem :: [Int] -> [String] -> Doc e
      ppTableElem ns es = "|" <+> hsep (zipWith ppTableCell ns es)
       where
        ppTableCell :: Int -> String -> Doc e
        ppTableCell n c = fill n (text (escapeTabAndNewline c))

    ppTableHeaders :: [[Int]] -> [String] -> Doc e
    ppTableHeaders nss hs = hsep (zipWith ppTableHeader nss hs) <+> "|"
     where
      ppTableHeader :: [Int] -> String -> Doc e
      ppTableHeader ns h = "|" <+> fill (elemWidth ns) (text (escapeTabAndNewline h))

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
        zipWith adjust (map printableLength tableHeaders) ws0
     where
      unadjustedTableWidths :: [[[String]]] -> [[Int]]
      unadjustedTableWidths =
          map (map (maximum . map printableLength) . transpose)
        . transpose

      unsnoc :: [a] -> Maybe ([a], a)
      unsnoc [] = Nothing
      unsnoc [x] = Just ([], x)
      unsnoc (x:xs) = do
        (ys,y) <- unsnoc xs
        pure (x:ys,y)

      -- Very primitive length counter that simply ignores ASNI color escape
      -- sequences, and then ignores non-printable characters.
      --
      -- For simplicity, assume that what follows "\ESC[" is an ANSI color
      -- escape sequence, and this function is probably broken if it isn't.
      printableLength :: String -> Int
      printableLength = length . filter isPrint . filterAnsiColor

    elemWidth :: [Int] -> Int
    elemWidth = foldr (\x y -> x+y+1) (-1)

    -- Escape tabs and newlines in a 'String'.
    escapeTabAndNewline :: String -> String
    escapeTabAndNewline = replace '\n' "\\n" . replace '\t' "\\t"
     where
      replace :: Char -> String -> String -> String
      replace c s = concatMap (\c' -> if c == c' then s else [c'])

    -- | Like text, but consider the length of the string after its ANSI color
    -- escape codes and unprintable characters have been filtered out.
    text :: String -> Doc e
    text s = PrettyPrint.Text (length s') s
     where
      s' = filter isPrint (filterAnsiColor s)

    filterAnsiColor :: String -> String
    filterAnsiColor "" = ""
    filterAnsiColor ('\ESC' : '[' : xs) =
      filterAnsiColor (safeTail (dropWhile (/= 'm') xs))
    filterAnsiColor (x:xs) = x : filterAnsiColor xs

    safeTail :: [a] -> [a]
    safeTail [] = []
    safeTail (_:xs) = xs


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
-- [ [{\"foo\": \"bar\"}], [{\"baz\": \"qux\"}] ] -- one 'TableSlice'
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
  :: [String]            -- ^ Headers
  -> [TableSlice Object] -- ^ Table slices
  -> Table
makeTable headers slices =
  makeTableWith
    (\_ -> id)
    (\_ _ _ -> unpack)
    (\_ _ _ _ -> prettyValue)
    headers
    (flat slices)
 where
  flat :: [TableSlice Object] -> [TableSlice Object]
  flat = (map . map . map . fmap) flattenObject

-- | Like 'makeTable', but takes explicit rendering functions. This is useful for
-- adding ANSI escape codes to color output, or for rendering values depending on
-- what their key is.
--
-- For example, you may wish to render 'Data.Aeson.String's with a
-- @\"timestamp\"@ key without quotation marks.
--
-- The @Int@ argument is the header's index. The @(Int, Int)@ argument is the
-- @(absolute, relative)@ index of the key and value. Visually,
--
-- @
-- +-------------+-------------+
-- | 0           | 1           |
-- |             |             |
-- | (0,0) (1,1) | (2,0) (3,1) |
-- +=============+=============+
-- | (0,0) (1,1) | (2,0) (3,1) |
-- | (0,0) (1,1) | (2,0) (3,1) |
-- +-------------+-------------+
-- @
--
-- This function is (unfortunately) 'String'-based as of /0.3.0.0/, because the
-- pretty printing and ANSI escape code functions are 'String'-based, too.
--
makeTableWith
  :: forall header key value.
     (Ord key, Hashable key)
  => (Int -> header -> String)                               -- ^ Header rendering function
  -> (Int -> header -> (Int, Int) -> key -> String)          -- ^ Cell header rendering function
  -> (Int -> header -> (Int, Int) -> key -> value -> String) -- ^ Cell rendering function
  -> [header]                                                -- ^ Headers
  -> [TableSlice (HashMap key value)]                        -- ^ Table slices
  -> Table
makeTableWith showH showK showV headers slices =
  Table headers' cell_headers' slices'
 where
  cell_headers :: [[key]]
  cell_headers =
      map (Set.toAscList . foldl' step mempty)
    . transpose
    . concat
    $ slices
   where
    step :: Set key -> Maybe (HashMap key value) -> Set key
    step acc Nothing  = acc
    step acc (Just x) = acc <> Set.fromList (HashMap.keys x)

  headers':: [String]
  headers' = zipWith showH [0..] headers

  cell_headers' :: [[String]]
  cell_headers' =
    zipWith3
      (\i h -> zipWith (\r (a,k) -> showK i h (a,r) k) [0..])
      [0..]
      headers
      (tag cell_headers)

  slices' :: [[[[String]]]]
  slices' =
    (map . map) (zipWith4 go [0..] headers (tag cell_headers)) slices
   where
    go :: Int -> header -> [(Int, key)] -> Maybe (HashMap key value) -> [String]
    go i h ks (fromMaybe mempty -> m) =
      zipWith
        (\r (a,k) ->
          case HashMap.lookup k m of
            Nothing -> ""
            Just v  -> showV i h (a,r) k v)
        [0..]
        ks

  -- Tag each element in a list of lists with its absolute index.
  --
  -- tag [[a,b,c],[d,e],[],[f]] = [[(0,a),(1,b),(2,c)],[(3,d),(4,e)],[],[(5,f)]]
  tag :: [[a]] -> [[(Int, a)]]
  tag = go 0 [] []
    where
    go _ acc0 acc1 [] = reverse (map reverse (acc1 : acc0))
    go !n acc0 acc1 (xs:xss) =
      case xs of
        []     -> go n (acc1 : acc0) [] xss
        (y:ys) -> go (n+1) acc0 ((n,y) : acc1) (ys:xss)


-- | Pretty-print a 'Value' in one line.
prettyValue :: Value -> String
prettyValue = unpack . prettyValue'
 where
  prettyValue' :: Value -> Text
  prettyValue' value =
    case value of
      Object o ->
        "{"
        <> Vector.ifoldr'
          (\i (k,v) acc ->
            "\""
            <> k
            <> "\":"
            <> prettyValue' v
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
              then prettyValue' v <> acc
              else prettyValue' v <> ", " <> acc)
          mempty
          a
        <> "]"
      String s -> "\"" <> s <> "\""
      Number n -> pack (show n)
      Bool b   -> pack (show b)
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


zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 f (a:as) (b:bs) (c:cs) (d:ds) = f a b c d : zipWith4 f as bs cs ds
zipWith4 _ _ _ _ _ = []
