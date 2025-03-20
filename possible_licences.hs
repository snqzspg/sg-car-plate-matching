----------------------------------------------------------------------------------------
--                                                                                    --
-- This source file is licensed under a custom Non-Libre Permissive License           --
--                                                                                    --
--     Copyright (C) 2025 Snqzs' PG                                                   --
--                                                                                    --
-- Redistributions and uses of the source code is permitted provided the following    --
-- terms are met:                                                                     --
--                                                                                    --
--  (1) Redistributions must retain this copyright notice.                            --
--                                                                                    --
--  (2) Source code files should include this boilerplate at the top.                 --
--                                                                                    --
--  (3) The names of the copyright holder, collaborators or the Land Transport        --
--      Authority (LTA) of Singapore may NOT be used to endorse or promote products   --
--      derived from this software without specific prior written permission from     --
--      any of the aforementioned parties.                                            --
--                                                                                    --
--  (4) All modified source files must include a notice, describing all the           --
--      modifications performed on the files.                                         --
--                                                                                    --
--  (5) LTA reserves the right to order the removal of this work. Should this         --
--      happen, all authors of redistributions of this work are required to remove    --
--      remove their redistributed works at the earliest opportunity for removal upon --
--      gaining knowledge of this order.                                              --
--                                                                                    --
----------------------------------------------------------------------------------------

-- IDEA
-- Singapore car licence plates consists of four main parts:
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

allAlphabetsExceptIO :: [Char]
allAlphabetsExceptIO = alphNoIO where
    alphNoIO = ['A' .. 'Z'] \\ ['I', 'O']

allAlphabetsExceptVowels :: [Char]
allAlphabetsExceptVowels = alphNoVowels where
    alphNoVowels = ['B' .. 'Z'] \\ ['E', 'I', 'O', 'U']

possibleOneLetterPrefixes :: [String]
possibleOneLetterPrefixes = ["S"] -- There are more, this is just tentative

possibleTwoLetterPrefixes :: [String]
possibleTwoLetterPrefixes =
    ([a : [b] | a <- ['S', 'F', 'E', 'A', 'R', 'Q', 'W', 'X', 'Y'],
                b <- allAlphabetsExceptIO]
    \\ [
        "SJ" -- Supreme Court Judges (Plates have no checksum)
    ])
        ++ [
            "CB", "PA", "PC", "PD", "PH", "PZ", -- Private Buses
            "PU"                                -- Pulau Ubin
        ]

possibleThreeLetterPrefixes :: [String]
possibleThreeLetterPrefixes = 
    ([[a] ++ [b] ++ [c] |
        a <- ['S', 'F', 'G', 'P', 'T'],
        b <- allAlphabetsExceptVowels,
        c <- allAlphabetsExceptIO
    ] \\ [                                        -- --- Exclusions --- --
        "SCB",                                    -- I think I know why
        "SCC", "SCD", "SCS", "SCT", "STE",        -- Dk why they're excluded..
        "SHE", "SHY", "SKY", "SLY", "SPA", "SPY", -- Excluded because they can
                                                  -- be words
        "SEP", -- Singapore Elected President (SEP 1) (Does not have checksum at the back)
        "SPF", -- Commissioner of Police (SPF 1) (Does not have checksum at the back)
        "FCK"  -- You know why..
    ]) ++ [    -- --- Inclusions --- --
        "TIB", -- Trans-Island Buses (No longer exist)
        "CSS", -- City Shuttle Service (No longer issued, but still exists)
        "EVS" -- Special for first 100 locally assembled Electronic Vehicls
    ]
    ++ ["TR" ++ [a]  | a <- ['A' .. 'Z'] \\ ['I', 'O']] -- TR_: Trailers
    ++ ['Q' : [a, b] | a <- ['E', 'B', 'C'],
                       b <- ['A' .. 'Z'] \\ ['I', 'O']] -- QE_, QB_, QC_:
                                                        -- Once issued to 
                                                        -- Company-registered
                                                        -- vehicles.

possiblePrefixes =
    concat [
        possibleOneLetterPrefixes,
        possibleTwoLetterPrefixes,
        possibleThreeLetterPrefixes
    ]

getChecksumLetterFromSum :: Int -> Char
getChecksumLetterFromSum n = (!) checksumLetters $ mod n checksumLettersLen
                            -- Who write array accesses like this???

vectorMul :: [Int] -> [Int] -> Int
vectorMul a b = sum $ zipWith (*) a b

keepLastN :: [a] -> Int -> [a]
keepLastN xs n = foldl (const . drop 1) xs $ drop n xs
-- Note: foldl  = Java's Stream.reduce(init, binaryop)
-- Note: foldl1 = Java's Stream.reduce(binaryop)
-- Thanks https://stackoverflow.com/questions/17252851/how-do-i-take-the-last-n
--        -elements-of-a-list

getSumFromPlateNoCS :: [Char] -> Int
getSumFromPlateNoCS plateNoCS = do
    let prefix  = extractLetters plateNoCS `keepLastN` 2 >>= (\x -> [31 .&. ord x])
    let nums    = extractDigits plateNoCS >>= (\x -> [15 .&. ord x])
    let factors = [5, 4, 3, 2]
    let prefixFactors = [9, 4]
    
    let a = vectorMul prefix $ keepLastN prefixFactors $ length prefix
    let b = vectorMul nums $ keepLastN factors $ length nums

    a + b

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

fmtStrMatches :: String -> String -> String
fmtStrMatches [] [] = ""
fmtStrMatches (p:pattern) (r:result)
    | p == '?'  = "\x1b[1;34m" ++ [r]
                    ++ "\x1b[0m" ++ fmtStrMatches pattern result
    | otherwise = r : fmtStrMatches pattern result

extractPossibleLetters :: String -> [String]
extractPossibleLetters incompleteStr = do
    let possibleTrim = init incompleteStr
    extractAfterTrimming possibleTrim $ length possibleTrim
    where
        extractAfterTrimming :: String -> Int -> [String]
        extractAfterTrimming trimmed trimmedLen
            | trimmedLen == 1 = [take 1 trimmed]
            | isDigit(trimmed !! 1) = [take 1 trimmed]
            | trimmedLen == 2 = [take 1 trimmed, trimmed]
            | isDigit(trimmed !! 2) = [take n trimmed | n <- [1..2]]
            | otherwise = [take n trimmed | n <- [1..3]]

possibleCombinationsThatMatches :: [Char] -> [[Char]]
possibleCombinationsThatMatches pattern = do
    let possibleLetters = extractPossibleLetters $ init pattern
    let prefixPattern   =
            possiblePrefixes >>= (\x -> [x | any (`matchStr` x) possibleLetters])

    possiblePlates prefixPattern >>= (\x -> [x | matchStr pattern x])

wrapLoadingText :: String -> [String]
wrapLoadingText line =
    [concat [
            "\x1b[2K\x1b[G",
            line,
            "\n[\x1b[1;34mINFO\x1b[0m] ",
            "This is gonna take a while..."
        ]]

wrapLoadingTextLines :: [String] -> String
wrapLoadingTextLines lines = 
    concat $ (init lines >>= wrapLoadingText) ++ ["\x1b[2K\x1b[G"]

highlightQMs :: String -> String
highlightQMs s =
    s >>= (\x -> if x == '?' then "\x1b[1;34m" ++ [x] ++ "\x1b[0m" else [x])

insertHeaderAndIndent :: Bool -> [Char] -> [[Char]] -> [[Char]]
insertHeaderAndIndent isTTY pattern lines = do
    let fmtPattern = if isTTY then highlightQMs pattern else pattern
    concat [
            ["Possible matches for \"" ++ fmtPattern ++ "\":"],
            lines >>= (\x -> ["    " ++ x]),
            [""]
        ]

printUsage :: IO ()
printUsage = do
    arg0 <- getProgName
    hPutStrLn stderr $ tail $ concat $ [
            "usage: " ++ arg0 ++ " sg_plate_patterns ...",
            "       sg_plate_patterns  A car plate number with missing alphabets or numbers you want to search.",
            "                          To denote missing characters, use \"?\".",
            "                          For example, \"SBA1234?\".",
            "                          NOTE: Only accurate for licence plates with checksums,",
            "                                (Example: S1 - S10 does not have checksums.)",
            "",
            "This command will list out all possible matches for each pattern specified.",
            "",
            "DISCLAIMER: Not all valid license plates are added here.",
            "",
            "NOTE: Currently there's no way to preserve colors when piping to something like 'less'.",
            "      You can still do so using",
            "",
            "          sh -c 'script -efq /dev/null -c \"./" ++ arg0 ++ " plate_patterns ...\"' | /usr/bin/less -R",
            "",
            "      but it's a bit messy."
        ] >>= (\x -> ["\n" ++ x])

printUsageFormatted :: IO ()
printUsageFormatted = do
    arg0 <- getProgName
    hPutStrLn stderr $ tail $ concat $ [
            "\x1b[1;32musage\x1b[0m: " ++ arg0 ++ " sg_plate_patterns ...",
            "       sg_plate_patterns  A car plate number with missing alphabets or numbers you want to search.",
            "                          To denote missing characters, use \"?\".",
            "                          For example, \"SBA1234?\".",
            "                          \x1b[1;34mNOTE\x1b[0m: Only accurate for licence plates with checksums,",
            "                                (Example: S1 - S10 does not have checksums.)",
            "",
            "This command will list out all possible matches for each pattern specified.",
            "",
            "\x1b[1;33mDISCLAIMER\x1b[0m: Not all valid license plates are added here.",
            "\x1b[1;34mNOTE\x1b[0m: Currently there's no way to preserve \x1b[1;34mcolors\x1b[0m when piping to something like '\x1b[1;32mless\x1b[0m'.",
            "      You can still do so using",
            "",
            "          \x1b[1msh -c 'script -efq /dev/null -c \"./" ++ arg0 ++ " plate_patterns ...\"' | /usr/bin/less -R\x1b[0m",
            "",
            "      but it's a bit messy. (In less you can scroll to clear up)"
        ] >>= (\x -> ["\n" ++ x])

main :: IO ()
main = do
    args <- getArgs
    isErrTTY <- hIsTerminalDevice stderr
    isOutTTY <- hIsTerminalDevice stdout

    hSetBuffering stdout $ BlockBuffering Nothing
    if null args
    then
        if isErrTTY then printUsageFormatted else printUsage
    else
        putStrLn $ args >>= intercalate "\n" . combineAndFormatMatches isOutTTY
        -- putStrLn $ args >>= wrapLoadingTextLines . combineAndFormatMatches isOutTTY
    where
        coloriseMatchIfTTY :: Bool -> String -> String -> [String]
        coloriseMatchIfTTY isTTY x y = [if isTTY then fmtStrMatches x y else y]

        combineAndFormatMatches :: Bool -> String -> [String]
        combineAndFormatMatches isTTY x =
            insertHeaderAndIndent isTTY x $ 
                possibleCombinationsThatMatches (map toUpper x) 
                    >>= coloriseMatchIfTTY isTTY x
