-- IDEA
-- Singapore car licence plates consists of fout main parts:
--  - Prefix Letter  (S or E)
--  - Suffix Letters (Example: 'BA'         in SBA1234A)
--  - Numbers        (Example: '1234'       in SBA1234A)
--  - Checksum       (Example: The last 'A' in SBA1234A)

-- Prefixes can be S or E, or some other letter
-- Suffixes are letters followed by the prefix letter.
--   A license plate can have no suffix letters, or up to two suffix letters.
-- Numbers are in decimal digits from 1 to 9999, with a maximum of 4 digits.
-- The final checksum letter is a singular letter related to the rest of the
-- numbers and letters.

-- Prefix and Suffix can be thought of as one part as not all plates begin
-- with S or E.

-- Almost all licence plates should have numbers on them.

-- Checksum letters are present on almost all plates, with execptions of
-- some government owned vehicles.

-- In this challenge, a Haskell program is written to take a user specified
-- license plate with missing letters or numbers, and display every
-- possible valid combinations that fits the incomplete licence plate given

-- The combinations should exclude certain letter combinations that can be
-- associated to meaningful or vulgar words.

import Data.Attoparsec.ByteString.Char8 (isAlpha_ascii, isDigit)
import System.Environment (getArgs)
import Data.Bits ((.&.))
import Data.Char (toUpper, ord)
import Data.Array (Array)
import Data.Array.IArray (array, (!))
import Data.Generics (everything)
import Data.List (intercalate, (\\))

hasChecksum :: [Char] -> Bool
hasChecksum plateStr = do
    isAlpha_ascii $ last plateStr

checksumLettersLen :: Int
checksumLettersLen = 19

checksumLetters :: Array Int Char
checksumLetters =
    array (0, checksumLettersLen - 1)
        $ zip [0 .. (checksumLettersLen - 1)] [
            'A', 'Z', 'Y', 'X', 'U', 'T',
            'S', 'R', 'P', 'M', 'L', 'K',
            'J', 'H', 'G', 'E', 'D', 'C',
            'B'
            -- 'A', 'Y', 'U', 'S', 'P', 'L',
            -- 'J', 'G', 'D', 'B', 'Z', 'X',
            -- 'T', 'R', 'M', 'K', 'H', 'E',
            -- 'C'
        ]

possibleOneLetterPrefixes :: [String]
possibleOneLetterPrefixes = ["S"] -- There are more, this is just tentative

possibleTwoLetterPrefixes :: [String]
possibleTwoLetterPrefixes =
    concatMap (\a -> map (\b -> a: [b]) ['A' .. 'Z']) ['S', 'E', 'A', 'R']

possibleThreeLetterPrefixes :: [String]
possibleThreeLetterPrefixes = 
    [[a] ++ [b] ++ [c] |
        a <- ['S', 'F', 'G', 'P', 'T'],
        b <- ['B' .. 'Z'] \\ ['E', 'I', 'O', 'U'],
        c <- ['A' .. 'Z']
    ]
    -- (concatMap (\a -> 
    --         concatMap (\b -> map 
    --             (\c -> [a] ++ [b] ++ [c]) ['A' .. 'Z']
    --         ) (['B' .. 'Z'] \\ ['E', 'I', 'O', 'U'])
    --     ) ['S', 'F', 'G', 'P', 'T']
    -- \\ ["SHE", "SHY", "SKY", "SLY", "SPA", "SPY"]) ++ ["TIB"]

possiblePrefixes =
    possibleOneLetterPrefixes ++ possibleTwoLetterPrefixes
        ++ possibleThreeLetterPrefixes

getChecksumLetterFromSum :: Int -> Char
getChecksumLetterFromSum n = (!) checksumLetters $ mod n checksumLettersLen

getSumFromPlateNoCS :: [Char] -> Int
getSumFromPlateNoCS plateNoCS = do
    let prefix  = extractLetters plateNoCS
    let nums    = map (\x -> 15 .&. ord x) $ extractDigits plateNoCS
    let factors = [5, 4, 3, 2]

    let prefixNums =
            map (\x -> 31 .&. ord x) $ drop (max (length prefix - 2) 0) prefix
    let prefixFactors = [9, 4]

    -- foldl1 (+) $ map (\x -> (fst x) * (snd x)) (zip prefixNums $ drop (max (length prefixFactors - length prefixNums) 0) prefixFactors) ++ (map (\x -> (fst x) * (snd x)) $ zip nums $ drop (max (length factors - length nums) 0) factors)
    sum  $ zipWith (*) prefixNums 
            (drop (max (length prefixFactors - length prefixNums) 0) prefixFactors)
            ++ zipWith (*) nums (drop (max (length factors - length nums) 0) factors)

getChecksumLetter :: [Char] -> Char
getChecksumLetter plateNoCS = getChecksumLetterFromSum $ getSumFromPlateNoCS plateNoCS

extractChecksum :: [Char] -> Maybe Char
extractChecksum plate = do
    if hasChecksum plate then
        Just $ last plate
    else
        Nothing

extractDigits :: [Char] -> [Char]
extractDigits = filter isDigit

extractLetters :: [Char] -> [Char]
extractLetters = filter isAlpha_ascii

possiblePlates :: [String] -> [String]
possiblePlates matchPrefixes = do
    let plates = [i ++ j | i <- matchPrefixes, j <- map show [1..9999]]
    map (\plateNoCS -> plateNoCS ++ [getChecksumLetter plateNoCS]) plates

matchStr :: String -> String -> Bool
matchStr [] [] = True
matchStr (c1:s1) (c2:s2)
    | length s1 /= length s2 = False
    | c1 == '?' || c2 == '?' = matchStr s1 s2
    | c1 == c2 = matchStr s1 s2
    | otherwise = False

extractPossibleLettersTrim :: String -> Int -> [String]
extractPossibleLettersTrim trimmed trimmedLen
    | trimmedLen == 1 = [take 1 trimmed]
    | isDigit(trimmed !! 1) = [take 1 trimmed]
    | trimmedLen == 2 = [take 1 trimmed, trimmed]
    | isDigit(trimmed !! 2) = [take n trimmed | n <- [1..2]]
    | otherwise = [take n trimmed | n <- [1..3]]

extractPossibleLetters :: String -> [String]
extractPossibleLetters incompleteStr = do
    let possibleTrim = init $ init incompleteStr
    extractPossibleLettersTrim possibleTrim $ length possibleTrim

possibleCombinationsThatMatches :: [Char] -> [[Char]]
possibleCombinationsThatMatches pattern = do
    let possibleLetters = extractPossibleLetters $ init pattern
    let prefixPattern   =
            filter (\x -> any (`matchStr` x) possibleLetters) possiblePrefixes

    filter (matchStr pattern) $ possiblePlates prefixPattern

wrapLoadingText :: [Char] -> [Char]
wrapLoadingText line = "\x1b[2K\x1b[G" ++ line ++ "\nThis is gonna take a while..."

wrapLoadingTextLines :: [[Char]] -> [[Char]]
wrapLoadingTextLines = map wrapLoadingText

main :: IO ()
main = do
    -- getArgs >>= print . map hasChecksum
    -- getArgs >>= (\pureArgs -> putStrLn $ unlines (map (show . hasChecksum) pureArgs))
    -- getArgs >>= putStrLn . unlines . map (show . extractChecksum . map toUpper)

    getArgs >>= putStrLn . intercalate "\n" . map (intercalate "\n" . possibleCombinationsThatMatches . map toUpper)
