module Proj1 (Pitch, toPitch, feedback, GameState, initialGuess, nextGuess) where
    import Data.List
    import Data.Function (on)

    data Pitch = Pitch String
        deriving (Eq)
        
    data GameState = GameState [([Pitch], Int)]
        deriving (Show)
    
    instance Show Pitch where
        show (Pitch a) = a
    
    pitchSpectrum :: [String]
    pitchSpectrum = [[a,b]| a <- ['A'..'G'], b <- ['1'..'3']]
    
    
    -- takes no input arguments, and returns a pair of an initial guess and a game state.
    initialGuess :: ([Pitch], GameState)
    initialGuess = ([Pitch "A1", Pitch "B2", Pitch "C3"], GameState possibleGameState )
    

    -- all potential game states
    possibleGameState :: [([Pitch], Int)]
    possibleGameState = [ (map Pitch [a,b,c], 0) | a <- pitchSpectrum, b <- pitchSpectrum, c <- pitchSpectrum, a < b, b < c ]
    
    
    -- choose the first element from the possible candidates as the next guess, 
    -- the first element has the least similarity in current GameState
    nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
    nextGuess (prevGuess, GameState currGameState) prevFeedBack = (newGuess, GameState newGameState) where
        newGuess = fst (head newGameState)
        newGameState = sortGameState possibleCandidates
        possibleCandidates = filterPitch prevGuess currGameState prevFeedBack
    
    

    -- filter pitches that do not have the same feedback as previous guess
    filterPitch :: [Pitch] -> [([Pitch], Int)] -> (Int,Int,Int) -> [([Pitch], Int)]
    filterPitch a [] _ = []
    filterPitch prevGuess (x:xs) prevFeedBack
            | compare' (feedback prevGuess (fst x)) prevFeedBack = x : filterPitch prevGuess xs prevFeedBack
            | otherwise = filterPitch prevGuess xs prevFeedBack

    
    -- sort the current gamestate by the similarity score in ascending order
    sortGameState :: [([Pitch], Int)] -> [([Pitch], Int)]
    sortGameState [] = []
    sortGameState (x:xs) = result where
        score  = similarity (fst x) (x:xs)
        result = sortBy (compare `on` snd) ((fst x, score) : sortGameState xs)



    -- naive approach, choose the candidate pitch with the same feedback as previous guess

    -- updateGameState :: [Pitch] -> [[Pitch]] -> (Int,Int,Int) ->[[Pitch]]
    -- updateGameState prevGuess currGameState prevFeedback 
    --         | compare' (feedback prevGuess (head currGameState)) prevFeedback = head currGameState : updateGameState prevGuess (tail currGameState) prevFeedback
    --         | otherwise = updateGameState prevGuess (tail currGameState) prevFeedback


    -- compare whether two feedbacks are same
    compare' :: (Int,Int,Int) -> (Int,Int,Int) -> Bool
    compare' (a,b,c) (x,y,z) = a==x && b==y && c==z


    -- calculate the similarity score for a particular pitch. the score is defined as a summation of all elements in the GameState
    similarity :: [Pitch] -> [([Pitch], Int)] -> Int
    similarity guess [] = 0
    similarity guess (x:xs) = score + similarity guess xs where
        -- score = pitch + note + octave
        score = 5*pitch + 2*note + octave
        -- score = 27*pitch + 7*note + 3*octave
        (pitch, note, octave) = feedback guess (fst x)
        
    

    -- takes a target and a guess, respectively, and returns the appropriate feedback, as speciﬁed above.
    feedback :: [Pitch] -> [Pitch] -> (Int,Int,Int)
    feedback _ [] = error "Guess cannot be blank"
    feedback [] _ = error "Target cannot be blank"
    feedback target guess = (pitch, note, octave) where
        t = map getPitch target
        g = map getPitch guess
        pitch   = length (t `intersect` g)
        note    = length ( map head (t \\ g)  `intersect'`  map head (g \\ t) )
        octave  = length ( map tail (t \\ g)  `intersect'`  map tail (g \\ t) )
    
    
    -- count the intersection part of two lists, without repeated elem
    intersect' :: Eq t => [t] -> [t] -> [t]
    intersect' xs ys = xs \\ (xs \\ ys)
    
    
    
    -- gives Just the Pitch named by the string, or Nothing if the string is not a valid pitch name.
    toPitch :: String -> Maybe Pitch
    toPitch x = if x `elem` pitchSpectrum
                then Just (Pitch x)
                else Nothing
    
    
    -- convert pitch ot string
    getPitch :: Pitch -> String
    getPitch (Pitch a) = a
