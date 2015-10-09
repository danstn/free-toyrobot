{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}

import           Text.Read
import           Prelude
import           Data.Int
import           Data.String
import           Data.Maybe
import           Control.Monad.Free

type Vec2 = (Int, Int)

-- Data
data Grid = Grid {
  robot :: Robot,
  size  :: Vec2
} deriving Show

data Position = Position {
  coord  :: Vec2,
  facing :: Int
} deriving Show

data Robot = Robot {
  pos :: Position,
  dir :: Int
} deriving Show

type InstructionSet a r = Free (ToyRobotDSL a) r

-- Language
data ToyRobotDSL p next =
    Place p next
  | Report next
  | Move next
  | TurnLeft next
  | TurnRight next
  | Fire next
  | Exit
  deriving Functor

report :: InstructionSet a ()
report = liftF $ Report ()

place :: a -> InstructionSet a ()
place p = liftF $ Place p ()

move :: InstructionSet a ()
move = liftF $ Move ()

turnLeft :: InstructionSet a ()
turnLeft = liftF $ TurnLeft ()

turnRight :: InstructionSet a ()
turnRight = liftF $ TurnRight ()

halt :: InstructionSet a ()
halt = liftF Exit

-- Example instruction sets
p1 :: InstructionSet Position ()
p1 = place (Position (2, 3) 4) >> report >> move >> move >> move >> halt

p2 :: InstructionSet Position ()
p2 = place (Position (1, 4) 1) >> turnLeft >> move >> turnRight >> halt

-- Interpreters
showInstructionSet :: (Show a, Show r) => InstructionSet a r -> String
showInstructionSet (Free (Place p x) )  = "Placing robot at: " ++ show p ++ "\n" ++ showInstructionSet x
showInstructionSet (Free (Report x))    = "Reporting robot's position...\n" ++ showInstructionSet x
showInstructionSet (Free (Move x))      = "Moving forward....\n" ++ showInstructionSet x
showInstructionSet (Free (TurnLeft x))  = "Turning left...\n" ++ showInstructionSet x
showInstructionSet (Free (TurnRight x)) = "Turning right....\n" ++ showInstructionSet x
showInstructionSet (Free (Fire x))      = "Firing!\n" ++ showInstructionSet x
showInstructionSet (Free Exit)          = "Instruction terminated.\n"
showInstructionSet (Pure r)             = "return " ++ show r ++ "\n"

prettyPrint :: (Show a, Show r) => InstructionSet a r -> IO ()
prettyPrint = putStr . showInstructionSet

main :: IO ()
main = do
  putStrLn "Welcome to toy-robot simulator!"
  prettyPrint p1
  prettyPrint p2

