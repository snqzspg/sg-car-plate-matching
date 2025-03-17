import Control.Lens (Plated(plate))
import System.Environment (getArgs)
import Data.Char (toUpper)

isQuestionMark i plate = plate !! i == '?'

canBePrefixLetter :: Int -> [Char] -> Bool
canBePrefixLetter i plate
    | i > 2 || length plate < 3 = False
    | length plate > 3 = True
    | otherwise = i == 0

canBeChecksumLetter :: Int -> [Char] -> Bool
canBeChecksumLetter i plate
    | i /= (length plate - 1) = False
    | otherwise = length plate > 2

canBeLetter :: Int -> [Char] -> Bool
canBeLetter i plate
    | isQuestionMark i plate = canBePrefixLetter i plate || canBeChecksumLetter i plate
    | otherwise = False

canBeNumber :: Int -> [Char] -> Bool
canBeNumber i plate
    | not $ isQuestionMark i plate = False
    | length plate > 7 = i >= length plate - 5 && i < length plate - 1
    -- | length plate > 

-- genCandidatesForEachQM :: [Char] -> [[Char]]
-- genCandidatesForEachQM plate = do
--     let qmLetters

main :: IO ()
main = do
    getArgs >>= putStrLn . unlines . map (\msg -> show $ map (`canBeLetter` msg) [0 .. length msg - 1])
