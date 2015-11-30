{-**************************************************************--
--*****************PLAY FUNCTION (FOR TESTING)******************--
--**************************************************************-}

-- "Plays" a game of crusher by alternating players with a specified depth
-- E.g. play ["WWW-WW-------BB-BBB"] 'W' 2 3
play :: [String] -> Char -> Int -> Int -> IO ()
play history@(current:old) player depth n
  | gameOver (sTrToBoard current) (map sTrToBoard old) n = putStrLn "Game over."
  | otherwise = do 
       let history'@(new:_) = crusher history player depth n
       putStrLn $ player:" played: " ++ new
       play history' (if player == 'W' then 'B' else 'W') depth n

{-**************************************************************
--*****************DATA TYPES***********************************
--*************************************************************-}

-- Piece is a data representation of possible pieces on a board
-- where D is an empty spot on the board
--       W is a piece of the White player
--       B is a piece of the Black player
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
--       newBoard is the next board to add to the tree
--       seenBoards is the updated history to avoid possible future trouble boards
--       cplayer is the current player for whom the board was generated for
--

data Next a = Next {usedDepth :: Int, newBoard :: a, seenBoards :: [a], cplayer :: Piece}

--
-- Tree is a data representation for the search tree, it is an extention of 
-- the rose tree widely used for implementing such unequally branched search trees
--
-- Tree consists of 3 elements
-- where depth is an integer representing the depth level of the node
--       value is the current score of the node, either its heuristic or its minimax applied value
--       board is the game state at that node
--       nextBoards are the child nodes of the current node
--

data Tree a = Node {depth :: Int, value :: Int, board :: a, nextBoards :: [Tree a]} deriving (Eq, Show)

--
-- BoardTree is the internal representation of the search tree of the given board
-- that is to be generatated for correctly implementing the minimax algorithm.
--

type BoardTree = Tree Board

--
-- Slide is a tuple of 2 elements
-- an internal representation of a slide
-- where the first element represents the point to move from
--       the second element represents the adjacent point to move to
--

type Slide = (Point,Point)

--
-- Jump is a tuple of 2 elements
-- an internal representation of a leap
-- where the first element represents the point to move from
--       the second element represents the adjacent point to move over
--       the third element represents the point to move to
--

type Jump = (Point,Point,Point)

--
-- Move is a tuple of 2 elements
-- an internal representation of a move
-- where the first element represents the point to move from
--       the second element represents the point to move to
--
-- Note: in essence it is the same as a slide however the idea
--       is that a jump can be reduced to a move as in effect 
--       nothing happens the point moved over in a jump
--

type Move = (Point,Point)

{-**************************************************************
--*****************COMMON HELPER FUNCTIONS**********************
--*************************************************************-}

--
-- sTrToBoard
--
-- This function consumes a list of characters which can be either 'W' or 'B'
-- or '-' and converts them to a list of pieces, i.e W or B or D respectively
--
sTrToBoard :: String  -> Board
sTrToBoard s = map (\ x -> check x) s
    where 
        check 'W' = W
        check 'B' = B
        check '-' = D

--
-- boardToStr
--
-- This function consumes a board which is a list of either W or B  or D and 
-- converts them to a list of characters, i.e 'W' or 'B' or 'D' respectively
--
boardToStr :: Board -> String
boardToStr b = map (\ x -> check x) b
    where 
        check W = 'W'
        check B = 'B'
        check D = '-'

-- Convert a char to its piece representation
charToPiece :: Char -> Piece
charToPiece c
    | c == 'B'  = B
    | c == 'W'  = W
    | otherwise = D

-- Converts a state to its board representation (only the pieces without the points)
stateToBoard :: State -> Board
stateToBoard state = map (\tile -> fst tile) state

-- Converts a board to its state representation (pieces with the corresponding points)
boardToState :: Board -> Int -> State
boardToState board n
    | otherwise      = boardToStateHelper board grid
    where 
        grid = generateGrid n (n-1) (2 * (n-1)) []

-- Helper function to map pieces to points
boardToStateHelper :: Board -> Grid -> State
boardToStateHelper board grid
    | null board = []
    | otherwise  = [(piece, point)] ++ boardToStateHelper remainingBoard remainingGrid
    where
        piece = head board
        point = head grid
        remainingBoard = tail board
        remainingGrid = tail grid

-- Returns the starting point of a leap
leapStart :: Jump -> Point
leapStart(point,_,_) = point

-- Returns the middle point of a leap
leapMiddle :: Jump -> Point
leapMiddle (_,point,_) = point

-- Returns the end point of a leap
leapDestination :: Jump -> Point
leapDestination (_,_,point) = point

-- Returns true if the point in the provided state matches the piece
-- e.g. to check if the current state at (1,1) is a W
containsPiece :: Point -> State -> Piece -> Bool
containsPiece point state piece = fst matchingTile == piece
    where
        matchingTile = head (filter (\tile -> (snd tile) == point) state)

-- Returns true if the point is within the board based on the dimensions
-- of the regular hexagon
withinBoard :: Point -> Int -> Bool
withinBoard point n = x >= 0 && 
                      y >= 0 && 
                      y <= max - 1 && 
                      x <= relativeMax - 1
    where
        x = fst point
        y = snd point 
        max = (2 * n) - 1                 -- Based on board dimensions
        relativeMax = max - (abs (n - (y + 1))) -- Max possible x relative to its y position on the grid

--
-- generateGrid
--
-- This function consumes three integers (described below) specifying how to
-- properly generate the grid and also a list as an accumulator; to generate a
-- regular hexagon of side length n, pass n (n- 1) (2 * (n - 1)) and []
--
generateGrid :: Int -> Int -> Int -> Grid -> Grid
generateGrid n1 n2 n3 acc 
    | n3 == -1      = acc
    | otherwise     = generateGrid nn1 (n2 - 1) (n3 - 1) (row ++ acc)
        where
            row = map (\ x -> (x,n3)) [0 .. (n1 - 1)]
            nn1 = if n2 > 0 then n1 + 1 else n1 - 1

-- Count the number of a particular piece on the board
countPieces :: Piece -> Board -> Int
countPieces piece board = length pieces
    where
        pieces = filter (\p -> p == piece) board

{-**************************************************************
--*****************BOARD EVAULATOR******************************
--*************************************************************-}

-- This function consumes a board and performs a static board evaluation, by 
-- taking into account whose perspective the program is playing from, the list 
-- of boards already seen, the size of the board, and whether or not it is the
-- program's turn or not; to generate quantitative measures of the board, and 
-- accordingly produce a goodness value of the given board 
--
-- Arguments:
-- -- player: W or B representing the player the program is
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
-- -- board: a Board representing the most recent board
-- -- myTurn: a Boolean indicating whether it is the program's turn or the opponents.
--
-- Returns: the goodness value of the provided board
--
boardEvaluator :: Piece -> [Board] -> Int -> Board -> Bool -> Int
boardEvaluator player history n board myTurn
    | (not myTurn) && isLosingBoard = 20 * dimensionMultiplier   -- Game over on the opponents turn (program wins)
    | myTurn && isLosingBoard       = -20 * dimensionMultiplier  -- Game over on program's turn (program loses)
    | player == W                   = ((numWhites - numBlacks) + turnBonus) * dimensionMultiplier
    | otherwise                     = ((numBlacks - numWhites) + turnBonus) * dimensionMultiplier
    where
        numWhites = countPieces W board
        numBlacks = countPieces B board
        isLosingBoard = gameOver board history n
        turnBonus = if myTurn then 5 else 0      -- If it's your turn you have a chance to further improve your position. May not actually make sense to have this?
        dimensionMultiplier = ceiling (10 * (1 / (fromIntegral n))) -- Difference is more significant for smaller dimensions

{-**************************************************************
--*****************MINIMAX FUNCTION*****************************
--*************************************************************-}

-- Apply minimax to a tree. Note that this only updates the node values in the tree
-- based on whether it's selecting the maximum or minimum, it has nothing to do with
-- choosing the next best board to move to (this is in the main crusher function)
applyMinimax :: BoardTree -> Bool -> BoardTree
applyMinimax node selectMax
    | null children                   = node   -- No children so node is unaffected by minimax
    | selectMax == True               = selectMaximumValue node minimaxAppliedChildren
    | otherwise                       = selectMinimumValue node minimaxAppliedChildren
    where
        children = nextBoards node
        minimaxAppliedChildren = map (\subtree -> applyMinimax subtree (not selectMax)) children -- Recursively apply minimax to children

-- Selects the maximum value of a root node's children and updates the root node with that value
selectMaximumValue :: BoardTree -> [BoardTree] -> BoardTree
selectMaximumValue root children = updatedRoot
    where
        childValues = map (\child -> value child) children
        maximumValue = maximum childValues
        updatedRoot  = Node (depth root) maximumValue (board root) children

-- Selects the minimum value of a root node's children and updates the root node with that value
selectMinimumValue :: BoardTree -> [BoardTree] -> BoardTree
selectMinimumValue root children = updatedRoot
    where
        childValues = map (\child -> value child) children
        minimumValue = minimum childValues
        updatedRoot  = Node (depth root) minimumValue (board root) children

{-**************************************************************
--*****************MAIN FUNCTIONS*******************************
--*************************************************************-}

--
-- crusher
--
-- This function consumes a list of boards, a player, the depth of 
-- search tree, the size of the provide boards, and produces the 
-- next best board possible for the provided player, and accordingly
-- makes the move and returns new board consed onto the list of boards
--
-- Arguments:
-- -- (current:old): current represents the most recent board, old is
--                   the history of all boards already seen in game
-- -- p: 'W' or 'B' representing the player the program is
-- -- d: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: a list of String with the new current board consed onto the front
--

crusher :: [String] -> Char -> Int -> Int -> [String]
crusher (current:old) p d n
    | gameOver currentBoard history n = error "Game is over."
    | otherwise                       = [boardToStr nextBoard] ++ (current:old) -- Find the next best board and add to list
    where
        currentBoard = sTrToBoard current
        history = map (\str -> sTrToBoard str) old
        tree = generateTree currentBoard 0 d (charToPiece p) True n history -- Assume the player goes first w/ depth 0
        minimaxAppliedTree = applyMinimax tree True -- Apply minimax to the tree, pass in True because root node should select max value for next board
        valueToMatch = value minimaxAppliedTree     -- Determine the value of the child node to select as the next best board
        nextBoard    = board (head (filter (\subtree -> (value subtree) == valueToMatch) (nextBoards minimaxAppliedTree))) -- Select the board from the root's children with the value to match

--
-- gameOver
--
-- This function consumes a board, a list of boards, and the dimension
-- of board and determines whether the given board is in a state where
-- the game has ended by checking if the board is present in the provided
-- list of boards or either the W or B pieces are less than dimension of board
--
gameOver :: Board -> [Board] -> Int -> Bool
gameOver board history n = elem board history ||
                           countPieces W board < n ||
                           countPieces B board < n

-- Generates a tree for the board for some depth with all the resulting boards and their
-- appropriate heuristic values. Note that this tree has not had minimax applied.
generateTree :: Board -> Int -> Int -> Piece -> Bool -> Int -> [Board] -> BoardTree
generateTree currBoard depth maxDepth piece isPlayersTurn n history
    | gameOver currBoard history n || depth == maxDepth = Node depth score currBoard []  -- Leaf node
    | otherwise                                         = Node depth score currBoard filteredChildren -- Root node with children
    where
        score       = boardEvaluator piece history n currBoard isPlayersTurn -- Evaluate current node (board)
        childBoards = generateMoves currBoard turn n -- Generate all possible moves from current board
        children    = map (\child -> generateTree child         -- Recursively generate child subtrees
                                                  (depth + 1)   -- Increase depth
                                                  maxDepth      -- Max depth stays the same
                                                  piece           
                                                  (not isPlayersTurn) -- Flip whether it's the player's turn
                                                  n                    
                                                  (currBoard:history)) -- Add current board onto history
                           childBoards
        filteredChildren = filter (\child -> not (elem (board child) history)) children -- Filter out boards that have already occurred
        turn          = if isPlayersTurn then piece else opponent -- Determine turn for evaluating heuristic
        opponent      = if piece == W then B else W

-- Generate all the possible (valid) moves for the current piece on the board
-- Piece is either B or W
generateMoves :: Board -> Piece -> Int -> [Board]
generateMoves board piece n = generateMovesHelper boardState piece legalSlides legalLeaps
    where
        boardState = boardToState board n
        playerTiles = filter (\tile -> (fst tile) == piece) boardState -- Select tiles the player occupies
        playerSlides = concat (map (\tile -> generateSlides (snd tile) n) playerTiles) -- Generate all possible slides for player, concat flattens [[Slide]] to [Slide]
        playerLeaps = concat (map (\tile -> generateLeaps (snd tile) n) playerTiles) -- Generate all possible leaps for player, concat flattens
        legalSlides = findLegalSlides playerSlides boardState      -- Select legal slides (moving to an empty space)
        legalLeaps = findLegalLeaps playerLeaps boardState piece   -- Select legal leaps (jumping over player piece to an empty or opponent occupied space)

-- Helper for generating the next state based on a move and turning it back into a board
generateMovesHelper :: State -> Piece -> [Slide] -> [Jump] -> [Board]
generateMovesHelper state piece slides jumps = allResultingBoards
    where
        statesFromSlides = map (\slide -> buildNextStateFromSlide state piece slide) slides  -- Resulting states from each slide
        statesFromJumps = map (\jump -> buildNextStateFromJump state piece jump) jumps       -- Resulting states from each jump
        allResultingStates = statesFromSlides ++ statesFromJumps -- Combine the two together
        allResultingBoards = map (\state -> stateToBoard state) allResultingStates -- Convert all possible resulting states into boards

-- Generates the next state based on a piece sliding from one point to another
buildNextStateFromSlide :: State -> Piece -> Slide -> State
buildNextStateFromSlide state piece slide = map (\tile -> buildTile tile start end ) state
    where
        start = fst slide
        end   = snd slide
        buildTile tile start end
            | snd tile == start   = (D, snd tile)     -- Sliding from this tile, so change it to empty
            | snd tile == end     = (piece, snd tile) -- Sliding to this tile, so change it to player piece
            | otherwise           = tile -- Tile is unaffected by the slide, so remains unchanged

-- Generates the next state based on a piece jumping from one point to another
buildNextStateFromJump :: State -> Piece -> Jump -> State
buildNextStateFromJump state piece jump = map (\tile -> buildTile tile start end) state
    where
        start = leapStart jump
        end = leapDestination jump
        buildTile tile start end
            | snd tile == start  = (D, snd tile)     -- Jumping from this tile, so change to empty space
            | snd tile == end    = (piece, snd tile) -- Jumping to this tile, so change it to the player piece
            | otherwise          = tile  -- Tile unaffected by the jump, so leave it unchanged

-- Slide is only legal if the point being moved to is empty
findLegalSlides :: [Slide] -> State -> [Slide]
findLegalSlides slides boardState = filter (\slide -> containsPiece (snd slide) boardState D) slides

-- Jump is legal if the point being jumped over is the same as the piece and if the
-- point being jumped to is either empty or the opposing piece
findLegalLeaps :: [Jump] -> State -> Piece -> [Jump]
findLegalLeaps leaps boardState playerPiece =  filter 
                                               (\leap -> not (containsPiece (leapDestination leap) boardState playerPiece)
                                                          && (containsPiece (leapMiddle leap) boardState playerPiece) )
                                               leaps

-- Generates all possible slides from some point based on the dimensions of the grid
generateSlides :: Point -> Int -> [Slide]
generateSlides point n = map (\toPoint -> (point,toPoint)) slidesWithinBoard
    where
        directions = [R,L,UL,UR,DL,DR] -- All possible directions
        toPoints = map (\d -> move point d n) directions -- All possible points that can be moved to
        slidesWithinBoard = filter (\p -> withinBoard p n) toPoints  -- Only select points that fall within the grid

-- Generates all possible leaps from some point based on the dimensions of the grid
generateLeaps :: Point -> Int -> [Jump]
generateLeaps point n = map (\l -> (point, fst l, snd l)) leapsWithinBoard
    where
        directions = [R,L,UL,UR,DL,DR] -- All possible directions
        allLeaps = map (\d -> moveTwice point d n) directions -- All possible leaps, includes both the point being jumped over and the point being jumped to
        leapsWithinBoard = filter (\l -> withinBoard (snd l) n) allLeaps -- Only select points that fall within the grid, where (snd l) is the final leap position

-- Outputted slide represents the point that's being "jumped over" and the final jump position
-- Used to represent a jump
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
    | y <= n-1  = (x-1,y-1) -- Falls in the middle / upper half of grid
    | otherwise = (x,y-1)   -- Falls in lower half
    where
        x = fst point
        y = snd point

moveUpRight :: Point -> Int -> Point
moveUpRight point n
    | y <= n-1  = (x,y-1)   -- Falls in middle / upper half of grid
    | otherwise = (x+1,y-1) -- Falls in lower half
    where
        x = fst point
        y = snd point

moveDownLeft :: Point -> Int -> Point
moveDownLeft point n
    | y >= n-1  = (x-1,y+1) -- Falls in middle / lower half of grid
    | otherwise = (x,y+1)   -- Falls in upper half
    where
        x = fst point
        y = snd point

moveDownRight :: Point -> Int -> Point
moveDownRight point n
    | y >= n-1  = (x,y+1)   -- Falls in middle / lower half of grid
    | otherwise = (x+1,y+1) -- Falls in upper half
    where
        x = fst point
        y = snd point