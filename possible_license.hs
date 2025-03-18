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
import System.Environment (getArgs, getProgName)
import System.IO (hIsTerminalDevice, hPutStrLn, hSetBuffering, stderr, BufferMode (BlockBuffering), stdout)
import Data.Bits ((.&.))
import Data.Char (toUpper, ord)
import Data.Array (Array)
import Data.Array.IArray (array, (!))
import Data.Generics (everything)
import Data.List (intercalate, (\\), insert)

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
        c <- ['A' .. 'Z'] \\ ['I', 'O']
    ] ++ ["TIB"]
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
            \\ ["S" ++ show i | i <- [1..10]]
    map (\plateNoCS -> plateNoCS ++ [getChecksumLetter plateNoCS]) plates

matchStr :: String -> String -> Bool
matchStr [] [] = True
matchStr (c1:s1) (c2:s2)
    | length s1 /= length s2 = False
    | c1 == '?' || c2 == '?' = matchStr s1 s2
    | c1 == c2 = matchStr s1 s2
    | otherwise = False

extractPossibleLettersAfterTrim :: String -> Int -> [String]
extractPossibleLettersAfterTrim trimmed trimmedLen
    | trimmedLen == 1 = [take 1 trimmed]
    | isDigit(trimmed !! 1) = [take 1 trimmed]
    | trimmedLen == 2 = [take 1 trimmed, trimmed]
    | isDigit(trimmed !! 2) = [take n trimmed | n <- [1..2]]
    | otherwise = [take n trimmed | n <- [1..3]]

extractPossibleLetters :: String -> [String]
extractPossibleLetters incompleteStr = do
    let possibleTrim = init incompleteStr
    extractPossibleLettersAfterTrim possibleTrim $ length possibleTrim

possibleCombinationsThatMatches :: [Char] -> [[Char]]
possibleCombinationsThatMatches pattern = do
    let possibleLetters = extractPossibleLetters $ init pattern
    let prefixPattern   =
            filter (\x -> any (`matchStr` x) possibleLetters) possiblePrefixes

    filter (matchStr pattern) $ possiblePlates prefixPattern

-- wrapLoadingText :: [Char] -> [Char]
-- wrapLoadingText line =
--     "\x1b[2K\x1b[G" ++ line ++ "\nThis is gonna take a while..."
-- 
-- wrapLoadingTextLines :: [[Char]] -> [[Char]]
-- wrapLoadingTextLines = map wrapLoadingText

insertHeaderAndIndent :: [Char] -> [[Char]] -> [[Char]]
insertHeaderAndIndent pattern lines =
    ["Possible matches for \"\x1b[1;32m" ++ pattern ++ "\x1b[0m\":"]
        ++ map ("    " ++) lines
        ++ [""]

printUsage :: IO ()
printUsage = do
    arg0 <- getProgName
    hPutStrLn stderr $ "usage: " ++ arg0 ++ " sg_plate_patterns ..."
    hPutStrLn stderr $ "       sg_plate_patterns  A car plate number with "
                    ++ "missing alphabets or numbers you want to search."
    hPutStrLn stderr $ "                          To denote missing characters,"
                    ++ " use \"?\"."
    hPutStrLn stderr   "                          For example, \"SBA1234?\"."
    hPutStrLn stderr $ "                          NOTE: Only accurate for "
                    ++ "licence plates with checksums,"
    hPutStrLn stderr $ "                                (Example: S1 - S10 does"
                    ++ " not have checksums.)"
    hPutStrLn stderr   ""
    hPutStrLn stderr $ "This command will list out all possible matches for "
                    ++ "each pattern specified."
    hPutStrLn stderr  "DISCLAIMER: Not all valid license plates are added here."

printUsageFormatted :: IO ()
printUsageFormatted = do
    arg0 <- getProgName
    hPutStrLn stderr $ "\x1b[1;32musage\x1b[0m: " ++ arg0 ++ " sg_plate_patterns ..."
    hPutStrLn stderr   "       sg_plate_patterns  A car plate number with missing alphabets or numbers you want to search."
    hPutStrLn stderr   "                          To denote missing characters, use \"?\"."
    hPutStrLn stderr   "                          For example, \"SBA1234?\"."
    hPutStrLn stderr   "                          \x1b[1;34mNOTE\x1b[0m: Only accurate for licence plates with checksums,"
    hPutStrLn stderr   "                                (Example: S1 - S10 does not have checksums.)"
    hPutStrLn stderr   ""
    hPutStrLn stderr   "This command will list out all possible matches for each pattern specified."
    hPutStrLn stderr   "\x1b[1;33mDISCLAIMER\x1b[0m: Not all valid license plates are added here."

main :: IO ()
main = do
    -- getArgs >>= putStrLn . intercalate "\n"
    --     . map (intercalate "\n" . possibleCombinationsThatMatches . map toUpper)
    args <- getArgs
    isErrTTY <- hIsTerminalDevice stderr

    hSetBuffering stdout $ BlockBuffering Nothing
    if null args
    then
        if isErrTTY then printUsageFormatted else printUsage
    else
        getArgs >>= putStrLn . intercalate "\n"
            . map (\x -> 
                intercalate "\n" 
                    $ insertHeaderAndIndent x 
                    $ possibleCombinationsThatMatches 
                    $ map toUpper x
            )
