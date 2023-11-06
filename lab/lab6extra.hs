import Text.Printf

data Orientation = Orientation { xMovement :: Int, yMovement :: Int }
    deriving Show

orientations :: [Orientation]
orientations = o 0 1:o 1 1:o 1 0:o 1 (-1):o 0 (-1):o (-1) (-1):o (-1) 0:o (-1) 1:orientations
    where o = Orientation

data Turtle = Turtle { xPosition :: Int, yPosition :: Int, orientationIterator :: [Orientation] }

instance Show Turtle where
    show :: Turtle -> String
    show (Turtle xPos yPos (orientation:_)) =
        printf "Turtle {xPosition = %d, yPosition = %d, orientationIterator = [%s, ...]}" xPos yPos (show orientation)

startingTurtle :: Turtle
startingTurtle = Turtle 0 0 orientations

data Action = Step |
              Turn |
              Repeat { action :: Action, times :: Int } |
              Wait |
              Seq { first :: Action, second :: Action }
    deriving Show

executeAction :: Turtle -> Action -> Turtle

executeAction (Turtle xPos yPos oIter) Step =
    Turtle (xMove + xPos) (yMove + yPos) oIter
    where Orientation xMove yMove = head oIter

executeAction t@(Turtle _ _ oIter) Turn = t { orientationIterator = tail oIter }

executeAction turtle (Repeat _ 0) = turtle
executeAction turtle (Repeat action n) = executeAction nextTurtle $ Repeat action $ n - 1
    where nextTurtle = executeAction turtle action

executeAction turtle Wait = turtle

executeAction turtle (Seq first second) = let
    resultOfFirst = executeAction turtle first
    in
    executeAction resultOfFirst second

combineActions :: [Action] -> Action
combineActions = foldr Seq Wait
