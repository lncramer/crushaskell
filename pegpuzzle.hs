-- PegPuzzle.hs
-- Written by Kurt Eislet

-- NOTE THAT THE COMMENTING DONE BELOW IS NOT HOW WE WANT YOU TO 
-- COMMENT YOUR PROGRAMS.  THINK OF THESE OVERLY WORDY COMMENTS
-- AS A MINI-LECTURE ABOUT HOW THIS PROGRAM WORKS, NOT AS AN
-- EXAMPLE OF CONCISE DOCUMENTATION.
--
-- A version of something known as the peg puzzle consists of four pegs
-- (two blue and two red) and six holes.  Each of the holes may hold one
-- peg.  Initially the two red pegs are in the left-most holes, and the
-- two blue pegs are in the right-most holes.  (Note that the example in 
-- class used only one hole, not two.  That just allows the diagrams to
-- fit on a single PowerPoint slide better.  Makes no real difference.)
--
-- The object of this puzzle (and remember that it's a puzzle with one person
-- playing, not a game with two opponents), is to put the red pegs in the
-- right-most holes and the blue pegs in the left-most holes by moving one
-- peg at a time according to the following rules:
--
--    A peg may be moved into an adjacent empty hole.
--    A peg may jump over a single peg of another color into an empty hole.
--    The blue pegs may only move or jump to the left.
--    The red pegs may only move or jump to the right.
--
-- For this problem, we'll represent the current state of the puzzle as
-- a String, and the pegs by letters representing the color of the peg
-- B = blue, R = red, and _ = and empty hole
--
-- So the initial state described above is "RR__BB"
--
-- Below is the typical state space search algorithm implemented in
-- Haskell.

pegpuzzle start goal = reverse (statesearch [start] goal [])

statesearch :: [String] -> String -> [String] -> [String]
statesearch unexplored goal path
   | null unexplored              = []
   | goal == head unexplored      = goal:path
   | (not (null result))          = result
   | otherwise                    = 
        statesearch (tail unexplored) goal path
     where result = statesearch 
                       (generateNewStates (head unexplored)) 
                       goal 
                       ((head unexplored):path)


-- Now we have to generate all the next states that can be reached
-- from the current state in one move.  This is called move-generation.
--
-- So what moves do we want to generate?  How will that work?
-- First, let's take a look at the starting state.  It looks like
-- this: "RR__BB"
-- From any given state, there are at most four different types of
-- moves that could be made:
--  a red piece could slide one spot to the right into an adjacent empty spot
--  a red piece could jump to the right over a blue piece into an empty spot
--    just past the blue piece
--  a blue piece could slide one spot to the left into an adjacent empty spot
--  a blue piece could jump to the left over a red piece into an empty spot
--    just past the red piece
--
-- So in order to generate all the moves possible from a given state,
-- we want to apply four different move-generators to that state, one
-- for each type of move.  And then we'd like to combine the results of
-- those four move-generators into a single list to be used by the
-- top-level peg-puzzle function.  Going back to our start state,
-- if we apply the first move-generator described, we should see
-- this state generated: "R_R_BB"
-- And if we apply the second move-generator, we shouldn't see any
-- states generated, because there are no jumps that a red piece
-- can make.
-- Applying the third move-generator would give this state: "RR_B_B",
-- and applying the fourth move-generator would generate no new states,
-- because there are no jumps for a blue piece either.
--
-- It will be useful to keep in mind as we go that, at least for this
-- particular puzzle, the movement of blue pieces is exactly the same
-- as the movement of red pieces, except the blue pieces travel in the
-- opposite direction.  This suggests that we only need to worry about
-- solving the move-generation problem for the red pieces.  Then when
-- it comes time to deal with the blue pieces, we can just reverse
-- the list that represents the current state, apply the move-generators
-- for the red pieces, and then reverse the results.
--
-- So let's take a look at move-generation for red pieces.  We'll start
-- with sliding red pieces one space to the right to an empty space.
-- To do this, we'll want to look at the current state starting at the
-- left end and proceeding to the right, and every time we see a 
-- portion of that String that looks like "R_", we'll want to replace
-- that pair of elements with "_R", and return the resulting String as
-- a new possible state.  At the heart of this, we'll want a function
-- that takes a String as an argument, a position number (starting with 0
-- for the first element in the list), and a segment of pieces and 
-- spaces to replace what's already in the list.  For example, 
-- we might call this function "replaceSegment", and it would work
-- like this:
-- > replaceSegment "RR__BB" 1 "_R" => "R_R_BB"  
--
-- Here's what that function looks like:

replaceSegment oldList pos segment
   | pos == 0  = segment ++ drop (length segment) oldList
   | otherwise = 
        (head oldList):
        (replaceSegment (tail oldList) (pos - 1) segment)

-- Now we'd like to be able to use what we just wrote as we scoot along
-- a String representing a state, testing to see if we find "R_" at
-- each spot and replacing that segment with "_R" if we do.  And
-- we want to make sure that we don't inadvertently try to go past
-- the end of the String, right?  So this new function might be called
-- like this and return a list of new states:
-- > generateNewRedSlides "R_R_BB" => ["_RR_BB","R__RBB"]
--
-- Here's what that function looks like:

generateNewRedSlides currState =
   generateNew currState 0 "R_" "_R"

generateNew currState pos oldSegment newSegment
   | pos + (length oldSegment) > length currState    = []
   | segmentEqual currState pos oldSegment           =
        (replaceSegment currState pos newSegment):
        (generateNew currState (pos + 1) oldSegment newSegment)
   | otherwise                                       =
        (generateNew currState (pos + 1) oldSegment newSegment)

-- To make the functions above work, we need to be able to compare
-- a specified segment of one String to another String, character by
-- character, to see if they're the same, before we do the replacement.
-- This is similar to a subseq or subsequence function, but not
-- quite the same.

segmentEqual currState pos oldSegment = 
   (oldSegment == take (length oldSegment) (drop pos currState))

-- We've done pretty much all the hard work now.  To generate
-- the red jumps that are possible, we just re-use most of
-- what we've already created but pass different arguments:

generateNewRedJumps currState = 
   generateNew currState 0 "RB_" "_BR"

-- Now to make the blue slides and the blue jumps, we just have
-- to reverse the String that is the currState as we pass it
-- into our move-generators, and then we also have to reverse 
-- the individual Strings that are generated:

generateNewBlueSlides currState =
   reverseEach (generateNew (reverse currState) 0 "B_" "_B")

generateNewBlueJumps currState = 
   reverseEach (generateNew (reverse currState) 0 "BR_" "_RB")

--reverseEach listOfLists
--   | null listOfLists      = []
--   | otherwise             = (reverse (head listOfLists)):
--                             (reverseEach (tail listOfLists))

reverseEach listOfLists = map reverse listOfLists

-- And finally, we need to be able to tie all these move generators together
-- to create one big list of next possible moves:

generateNewStates :: String -> [String]
generateNewStates currState =
    concat  [generateNewRedSlides currState, generateNewRedJumps currState,
             generateNewBlueSlides currState, generateNewBlueJumps currState]

-- That wasn't so hard, was it?  Here's the whole thing in action with
-- one hole.  Add a second hole and it should work just fine.
--
-- Main> pegpuzzle "RR_BB" "BB_RR"
-- ["RR_BB","R_RBB","RBR_B","RBRB_","RB_BR","_BRBR","B_RBR","BBR_R","BB_RR"]






