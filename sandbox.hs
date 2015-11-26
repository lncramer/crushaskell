-- Piece is a data representation of possible pieces on a board
-- where D is an empty spot on the board
--		 W is a piece of the White player
--		 B is a piece of the Black player
--

data Piece = D | W | B deriving (Eq, Show)

--
-- Direction is a data representation of a possible move direction on a board
-- where R is right
--       L is left
--       UL is up left
--       DL is down left
--       and so on...
--

data Direction = R | L | UL | UR | DL | DR deriving (Eq, Show)

--
-- Point is a tuple of 2 elements
-- representing a point on a grid system
-- where the first element represents the x coordinate
--       the second element represents the y coordinate
--

type Point = (Int, Int)

--
-- Tile is a tuple of 2 elements 
-- representing what a point is occupied by
-- where the first element represents a piece 
--       the second element represents a point
--

type Tile  = (Piece, Point)

--
-- Board is a list of Pieces, thus it is an internal representation
-- of the provided string representation of the board, it maintains
-- the same order as the string representation of the board
--

type Board = [Piece]

--
-- Grid is a list of Points, thus it is an internal representation
-- of the hexagonal grid system translated into a coordinate 
-- system to easily maintain and make moves on the board
--

type Grid = [Point]

--
-- State is a list of Tile, thus it is an internal representation precisely
-- for the purposes of zipping the board and the grid together in order
-- to keep easier track of the effects on the pieces of making moves on grid
--

type State = [Tile]

--
-- Next is a data representation for storing and passing around information within
-- the tree generating function, allowing it to correctly generate new children
-- 
-- Next consists of 4 elements
-- where usedDepth is an integer reprsenting the current depth level
--		 newBoard is the next board to add to the tree
-- 		 seenBoards is the updated history to avoid possible future trouble boards
-- 		 cplayer is the current player for whom the board was generated for
--

data Next a = Next {usedDepth :: Int, newBoard :: a, seenBoards :: [a], cplayer :: Piece}

--
-- Tree is a data representation for the search tree, it is an extention of 
-- the rose tree widely used for implementing such unequally branched search trees
--
-- Tree consists of 3 elements
-- where depth is an integer representing the depth level of the node
-- 		 board is the game state at that node
-- 		 nextBoards are the child nodes of the current node
--

data Tree a = Node {depth :: Int, board :: a, nextBoards :: [Tree a]} deriving (Show)

--
-- BoardTree is the internal representation of the search tree of the given board
-- that is to be generatated for correctly implementing the minimax algorithm.
--

type BoardTree = Tree Board

--
-- Slide is a tuple of 2 elements
-- an internal representation of a slide
-- where the first element represents the point to move from
-- 		 the second element represents the adjacent point to move to
--

type Slide = (Point,Point)

--
-- Jump is a tuple of 2 elements
-- an internal representation of a leap
-- where the first element represents the point to move from
-- 		 the second element represents the adjacent point to move over
--		 the third element represents the point to move to
--

type Jump = (Point,Point,Point)

--
-- Move is a tuple of 2 elements
-- an internal representation of a move
-- where the first element represents the point to move from
-- 		 the second element represents the point to move to
--
-- Note: in essence it is the same as a slide however the idea
--		 is that a jump can be reduced to a move as in effect 
--		 nothing happens the point moved over in a jump
--

type Move = (Point,Point)

--
-- generateSlides
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible slides from any point on the grid to
-- any adjacent point on the grid
--
-- Arguments:
-- -- b: the Grid to generate slides for 
-- -- n: an Integer representing the dimensions of the grid
-- 
-- Note: This function is only called at the initial setup of the game, 
-- 		 it is a part of the internal representation of the game, this 
--		 list of all possible slides is only generated once; and when 
-- 		 generating next moves, the program decides which slides out of 
--		 all these possible slides could a player actually make
--
-- Returns: the list of all Slides possible on the given grid
--

-- Sample grid for testing (n = 3)
-- [(1,1),(2,1),(3,1),(1,2),(2,2),(3,2),(4,2),(1,3),(2,3),(3,3),(4,3),(5,3),(1,4),(2,4),(3,4),(4,4),(1,5),(2,5),(3,5)]

generateSlides :: Grid -> Int -> [Slide]
generateSlides b n
    | null b        = []
    | otherwise     = generatedSlides ++ (generateSlides (tail b) n)
    where
        point = head b
        generatedSlides = generateSlidesHelper point n

generateSlidesHelper :: Point -> Int -> [Slide]
generateSlidesHelper point n = map (\toPoint -> (point,toPoint)) legalToPoints
    where
        directions = [R,L,UL,UR,DL,DR]
        toPoints = map (\d -> move point d n) directions
        legalToPoints = filter (\p -> withinBoard p n) toPoints  -- Filter for points that fall within the board

withinBoard :: Point -> Int -> Bool
withinBoard point n = x > 0 && 
                      y > 0 && 
                      y <= max && 
                      x <= relativeMax
    where
        x = fst point
        y = snd point 
        max = (2 * n) - 1                 -- Based on board dimensions
        relativeMax = max - (abs (n - y)) -- Max possible x relative to its y position on the grid

generateLeaps :: Grid -> Int -> [Jump]
generateLeaps b n
    | null b       = []
    | otherwise    = possibleLeaps ++ (generateLeaps (tail b) n)
    where
        point = head b
        possibleLeaps = generateLeapsHelper point n

generateLeapsHelper :: Point -> Int -> [Jump]
generateLeapsHelper point n = map (\l -> (point, fst l, snd l)) legalLeaps
    where
        directions = [R,L,UL,UR,DL,DR]
        allLeaps = map (\d -> moveTwice point d n) directions
        legalLeaps = filter (\l -> withinBoard (snd l) n) allLeaps -- Filter on leaps where final position (snd l) is within board

-- Outputted slide represents the point that's being "jumped over" and the final jump position
moveTwice :: Point -> Direction -> Int -> Slide
moveTwice point direction n = (first,second)
    where
        first = move point direction n
        second = move first direction n

-- Move in a direction on the grid based on starting position
-- Where you are relative to the horizontal midpoint line (n) affects the delta, hence the UL/UR/DL/DR functions 
move :: Point -> Direction -> Int -> Point
move point direction n
    | direction == L = (x-1,y)
    | direction == R = (x+1,y)
    | direction == UL = moveUpLeft point n
    | direction == UR = moveUpRight point n
    | direction == DL = moveDownLeft point n
    | direction == DR = moveDownRight point n
    where
        x = fst point
        y = snd point

moveUpLeft :: Point -> Int -> Point
moveUpLeft point n
    | y <= n    = (x-1,y-1)
    | otherwise = (x,y-1)
    where
        x = fst point
        y = snd point

moveUpRight :: Point -> Int -> Point
moveUpRight point n
    | y <= n    = (x,y-1)
    | otherwise = (x+1,y-1)
    where
        x = fst point
        y = snd point

moveDownLeft :: Point -> Int -> Point
moveDownLeft point n
    | y >= n    = (x-1,y+1)
    | otherwise = (x,y+1)
    where
        x = fst point
        y = snd point

moveDownRight :: Point -> Int -> Point
moveDownRight point n
    | y >= n    = (x,y+1)
    | otherwise = (x+1,y+1)
    where
        x = fst point
        y = snd point