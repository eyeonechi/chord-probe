------------------------------------------------------------------------------
-- | File     : ChordProbe.hs
-- | Author   : Ivan Ken Weng Chee
-- | Origin   : Monday 04 September 2017
------------------------------------------------------------------------------

module ChordProbe (initialGuess, nextGuess, GameState) where

-- Libraries used
import Data.List
import qualified Data.Map as Map

-- Represents a guess returned to the conductor
type Guess = [String]

-- Game state used to pass information between guesses
type GameState = (Int, [[String]])

-- Represents a feedback structure returned by the conductor
type Feedback = (Int, Int, Int)

-- Strings of pitches to generate possibilities
pitches = ["A1","A2","A3",
           "B1","B2","B3",
           "C1","C2","C3",
           "D1","D2","D3",
           "E1","E2","E3",
           "F1","F2","F3",
           "G1","G2","G3"]

-- Random first guess to start with
firstGuess = ["A1","B1","C2"]

------------------------------------------------------------------------------
-- | Takes no input arguments, and returns a pair of an initial guess and a
-- | game state.
------------------------------------------------------------------------------
initialGuess :: (Guess, GameState)
initialGuess = (firstGuess, (0, guesses (validGuesses)))

------------------------------------------------------------------------------
-- | Takes as input a pair of the previous guess and game state, and the
-- | feedback to this guess as a triple of correct pitches, notes, and
-- | octaves, and returns a pair of the next guess and game state.
------------------------------------------------------------------------------
nextGuess :: (Guess, GameState) -> Feedback -> (Guess, GameState)
nextGuess (g, (n, ts)) f = (guess, state)
  where
    -- Expected likelihood calculation
    guess = expLikelihood (inconsistent g f ts)
            (expCount (inconsistent g f ts) (inconsistent g f ts)) 100.0 g
    state = (pitchValue f, (inconsistent g f ts) \\ [guess])
    -- Maximum likelihood calculation
    -- guess = extract (maxLikelihood (inconsistent g f ts))
    -- state = (pitchValue f, (inconsistent g f ts) \\ [guess])

------------------------------------------------------------------------------
-- | Generates all possible values for 3-pitch chords
------------------------------------------------------------------------------
possibles :: [a] -> [a] -> [a] -> [a] -> [[a]]
possibles [] (y:ys) (z:zs) o     = []
possibles (x:xs) [] (z:zs) o     = possibles xs o (z:zs) o
possibles (x:xs) (y:ys) [] o     = possibles (x:xs) ys o o
possibles (x:xs) (y:ys) (z:zs) o = [x, y, z] : possibles (x:xs) (y:ys) zs o

------------------------------------------------------------------------------
-- | Removes chords containing duplicate pitches
------------------------------------------------------------------------------
validGuesses :: [[String]]
validGuesses
  = nub (map quickSort (
      (possibles pitches pitches pitches pitches) \\ [firstGuess])
    )

------------------------------------------------------------------------------
-- | Removes chords which contain duplicated pitches
------------------------------------------------------------------------------
guesses :: (Ord a, Eq a) => [[a]] -> [[a]]
guesses [] = []
guesses (x:xs)
  | duplicate x = guesses xs
  | otherwise   = x : guesses xs

------------------------------------------------------------------------------
-- | Checks if a chord contains duplicated pitches
------------------------------------------------------------------------------
duplicate :: Eq a => [a] -> Bool
duplicate []                      = False
duplicate x
  | (length (nub x)) < (length x) = True
  | otherwise                     = False

------------------------------------------------------------------------------
-- | Sorts a list using the QuickSort algorithm
------------------------------------------------------------------------------
quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort (pivot:others) = (quickSort lowers) ++ [pivot] ++ (quickSort highers)
  where
    lowers = filter (<=pivot) others
    highers = filter (>pivot) others

------------------------------------------------------------------------------
-- | Guesses the feedback returned by the conductor for a given guess
------------------------------------------------------------------------------
feedback :: Guess -> Guess -> Feedback
feedback g t = (pitch, note, octave)
  where
    gt = intersect g t
    pitch = length gt
    note = (similarNotes (g \\ gt) (t \\ gt))
    octave = (similarOctaves (g \\ gt) (t \\ gt))

------------------------------------------------------------------------------
-- | Checks if a chord contains duplicated pitches
------------------------------------------------------------------------------
similarPitches :: Guess -> Guess -> Int
similarPitches ([a, b, c]) g
  = (containsPitch a g) + (containsPitch b g) + (containsPitch c g)

------------------------------------------------------------------------------
-- | Checks if a chord contains duplicated notes
------------------------------------------------------------------------------
similarNotes :: Guess -> Guess -> Int
similarNotes [] _ = 0
similarNotes (x:xs) g
  | null (containsNote x g) = similarNotes xs g
  | otherwise = 1 + similarNotes xs (g \\ [containsNote x g])

------------------------------------------------------------------------------
-- | Checks if a chord contains duplicated octaves
------------------------------------------------------------------------------
similarOctaves :: Guess -> Guess -> Int
similarOctaves [] _ = 0
similarOctaves (x:xs) g
  | null (containsOctave x g) = similarOctaves xs g
  | otherwise = 1 + similarOctaves xs (g \\ [(containsOctave x g)])

------------------------------------------------------------------------------
-- | Checks if a chord contains a particular pitch
------------------------------------------------------------------------------
containsPitch :: String -> Guess -> Int
containsPitch p ([x, y, z])
  | ((equalPitch p x) + (equalPitch p y) + (equalPitch p z)) > 0 = 1
  | otherwise = 0

------------------------------------------------------------------------------
-- | Checks if a chord contains a particular note
------------------------------------------------------------------------------
containsNote :: String -> Guess -> String
containsNote p [] = ""
containsNote p (x:xs)
  | (equalNote p x) == 1 = x
  | otherwise = containsNote p xs

------------------------------------------------------------------------------
-- | Checks if a chord contains a particular octave
------------------------------------------------------------------------------
containsOctave :: String -> Guess -> String
containsOctave p [] = ""
containsOctave p (x:xs)
  | equalOctave p x == 1 = x
  | otherwise = containsOctave p xs

------------------------------------------------------------------------------
-- | Checks if two pitches are equal
------------------------------------------------------------------------------
equalPitch :: String -> String -> Int
equalPitch x y
  | x == y = 1
  | otherwise = 0

------------------------------------------------------------------------------
-- | Checks if two octaves are equal
------------------------------------------------------------------------------
equalOctave :: String -> String -> Int
equalOctave x y
  | (last x) == (last y) = 1
  | otherwise = 0

------------------------------------------------------------------------------
-- | Checks if two notes are equal
------------------------------------------------------------------------------
equalNote :: String -> String -> Int
equalNote x y
  | (head x) == (head y) = 1
  | otherwise = 0

------------------------------------------------------------------------------
-- | Removes inconsistent guesses from the list of possible guesses
------------------------------------------------------------------------------
inconsistent :: Guess -> Feedback -> [[String]] -> [[String]]
inconsistent _ _ [] = []
inconsistent g f (t:ts)
  | (feedback g t) == f = t : inconsistent g f ts
  | otherwise         = inconsistent g f ts

------------------------------------------------------------------------------
-- | Returns a guess with the minimum expected number of remaining candidates
------------------------------------------------------------------------------
expLikelihood :: [[String]] -> [Double] -> Double -> Guess -> Guess
expLikelihood [] [] m g = g
expLikelihood (t:ts) (e:es) m g
  | e < m = expLikelihood ts es e t
  | otherwise = expLikelihood ts es m g

------------------------------------------------------------------------------
-- | Returns a list of doubles corresponding to guesses' expectations
------------------------------------------------------------------------------
expCount :: [[String]] -> [[String]] -> [Double]
expCount _ [] = []
expCount o (t:ts)
  = (expCalculate fbs len) : expCount o ts
  where
    fbs = Map.toList $ Map.fromListWith
        (+) [(f, 1) | f <- (expCount' t (o \\ [t]))]
    len = fromIntegral (length fbs)

------------------------------------------------------------------------------
-- | Helper function used by expCount
------------------------------------------------------------------------------
expCount' :: [String] -> [[String]] -> [Feedback]
expCount' _ [] = []
expCount' p (t:ts) = (feedback p t) : expCount' p ts

------------------------------------------------------------------------------
-- | Calculates the expectation of a guess
------------------------------------------------------------------------------
expCalculate :: [(Feedback, Int)] -> Int -> Double
expCalculate [] _ = 0.0
expCalculate ((f, n):fs) len
  = (((fromIntegral (n * n)) / (fromIntegral len)) / 100) + expCalculate fs len

------------------------------------------------------------------------------
-- | Returns a guess which will most likely leave less possible targets
------------------------------------------------------------------------------
maxLikelihood :: [[String]] -> ([String], Int)
maxLikelihood (t:ts) = maxCount (t:ts) (t:ts) (t, 0)

------------------------------------------------------------------------------
-- | Returns a list containing the guesses and their feedback counts
------------------------------------------------------------------------------
maxCount :: [[String]] -> [[String]] -> ([String], Int) -> ([String], Int)
maxCount _ [] (u, m) = (u, m)
maxCount o (t:ts) (u, m)
  | n > m = maxCount o ts (t, n)
  | otherwise = maxCount o ts (u, m)
  where n = maxCount' t (o \\ [t]) (o \\ [t]) 0

------------------------------------------------------------------------------
-- | Helper function used by maxCount
------------------------------------------------------------------------------
maxCount' :: [String] -> [[String]] -> [[String]] -> Int -> Int
maxCount' _ _ [] m = m
maxCount' p o (t:ts) m
  | n > m = maxCount' p o ts n
  | otherwise = maxCount' p o ts m
  where n = maxCount'' p (o \\ [p]) (feedback p t)

------------------------------------------------------------------------------
-- | Helper function used by maxCount'
------------------------------------------------------------------------------
maxCount'' :: [String] -> [[String]] -> Feedback -> Int
maxCount'' _ [] _ = 0
maxCount'' q (t:ts) f
  | (feedback q t) == f = 1 + maxCount'' q ts f
  | otherwise = maxCount'' q ts f

------------------------------------------------------------------------------
-- | Helper function to take a string out of a tuple
------------------------------------------------------------------------------
extract :: ([String], Int) -> [String]
extract (s, n) = s

------------------------------------------------------------------------------
-- | Helper function to take an int out of a feedback
------------------------------------------------------------------------------
pitchValue :: Feedback -> Int
pitchValue (a, _, _) = a
