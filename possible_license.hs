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
--
-- Prefix and Suffix can be thought of as one part as not all plates begin
-- with S or E.
--
-- Almost all licence plates should have numbers on them.
--
-- Checksum letters are present on almost all plates, with execptions of
-- some government owned vehicles.
--
-- In this challenge, a Haskell program is written to take a user specified
-- license plate with missing letters or numbers, and display every
-- possible valid combinations that fits the incomplete licence plate given
--
-- The combinations should exclude certain letter combinations that can be
-- associated to meaningful or vulgar words.


import Data.Attoparsec.ByteString.Char8 (isAlpha_ascii, isDigit)
import System.Environment (getArgs)
import Data.Char (toUpper)
import Data.Array (Array)
import Data.Array.IArray (array, (!))
import Data.Generics (everything)
import Data.List ((\\))

hasChecksum :: [Char] -> Bool
hasChecksum plateStr = do
    isAlpha_ascii $ last plateStr

checksumLettersLen :: Int
checksumLettersLen = 19

checksumLetters :: Array Int Char
checksumLetters =
    array (0, checksumLettersLen - 1)
        $ zip [0 .. (checksumLettersLen - 1)] [
            'A', 'Y', 'U', 'S', 'P', 'L',
            'J', 'G', 'D', 'B', 'Z', 'X',
            'T', 'R', 'M', 'K', 'H', 'E',
            'C'
        ]

possibleOneLetterPrefixes :: [String]
possibleOneLetterPrefixes = ["S"] -- There are more, this is just tentative

possibleTwoLetterPrefixes :: [String]
possibleTwoLetterPrefixes = concatMap (\a -> map (\b -> a: [b]) ['A' .. 'Z']) ['S', 'E', 'A', 'R']

possibleThreeLetterPrefixes :: [String]
possibleThreeLetterPrefixes = 
    (concatMap (\a -> 
            concatMap (\b -> map (\c -> [a] ++ [b] ++ [c]) ['A' .. 'Z']) (['B' .. 'Z'] \\ ['E', 'I', 'O', 'U'])
        ) ['S', 'F', 'G', 'P']
    \\ ["SHE", "SHY", "SKY", "SLY", "SPA", "SPY"]) ++ ["TIB"]

possiblePrefixes = possibleOneLetterPrefixes ++ possibleTwoLetterPrefixes ++ possibleThreeLetterPrefixes

getChecksumLetterFromSum :: Int -> Char
getChecksumLetterFromSum n = (!) checksumLetters $ mod n (checksumLettersLen - 1)

-- getChecksumLetter :: [Char] -> Char
-- getChecksumLetter plateNoCS = 

extractChecksum :: [Char] -> Maybe Char
extractChecksum plate = do
    if hasChecksum plate then
        Just $ last plate
    else
        Nothing

extractDigits :: [Char] -> [Char]
extractDigits = filter isDigit

extractLetters :: [Char] -> [Char]
extractLetters plate = filter isDigit $ init plate

possibleCombinations :: [Char] -> Maybe [[Char]]
possibleCombinations incompleteStr = do
    Just [incompleteStr] -- Tentative

main :: IO ()
main = do
    -- getArgs >>= print . map hasChecksum
    -- getArgs >>= (\pureArgs -> putStrLn $ unlines (map (show . hasChecksum) pureArgs))
    getArgs >>= putStrLn . unlines . map (show . extractChecksum . map toUpper)

    print $ possibleTwoLetterPrefixes ++ possibleThreeLetterPrefixes

