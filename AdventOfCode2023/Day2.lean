import Lean.Data.Parsec
import AdventOfCode2023.DaySolution

open Lean.Parsec
open DaySolution

namespace Day2

-- Part 1

def sepBy1 (sep : Lean.Parsec a) (p: Lean.Parsec b) : Lean.Parsec (Array b) := do
  let first <- p
  let rest <- Lean.Parsec.many (sep *> p)
  pure $ Array.insertAt rest 0 first

def nat : Lean.Parsec Nat := do
  let digits <- Lean.Parsec.many1 Lean.Parsec.digit <* Lean.Parsec.many (Lean.Parsec.pchar ' ')
  let toNat (c : Char) := Char.toNat c - 48
  pure $ Array.foldl (fun acc cur => 10*acc + toNat cur) 0 digits

-- How many of each color was revealed at one time
structure Cubes where
  red : Nat
  green : Nat
  blue : Nat
deriving Inhabited, Repr

inductive Color where
  | Red : Color
  | Green : Color
  | Blue : Color
deriving Repr

open Color

def color : Lean.Parsec Color :=
  (Lean.Parsec.pstring "red" *> pure Red)
  <|> (Lean.Parsec.pstring "green" *> pure Green)
  <|> (Lean.Parsec.pstring "blue" *> pure Blue)

def cubes : Lean.Parsec Cubes := do
  let cubes <- sepBy1 (Lean.Parsec.pstring ", ") ((· , ·) <$> nat <*> color)
  pure $ Array.foldr pushColor (Cubes.mk 0 0 0) cubes
  where
    pushColor : (Nat × Color) -> Cubes -> Cubes
      | (quantity, color), acc =>
        match color with
          | Red => { acc with red := acc.red + quantity }
          | Green => { acc with green := acc.green + quantity }
          | Blue => { acc with blue := acc.blue + quantity }

structure Game where
  gameId : Nat
  rounds : Array Cubes
deriving Repr

-- Each game is listed with its ID number (like the 11 in Game 11: ...)
-- followed by a semicolon-separated list of subsets of cubes that were revealed
-- from the bag (like 3 red, 5 green, 4 blue).
def game : Lean.Parsec Game := do
  let gameId <- Lean.Parsec.pstring "Game " *> nat <* Lean.Parsec.pstring ": "
  let rounds <- sepBy1 (Lean.Parsec.pstring "; ") cubes
  pure { gameId, rounds }

def maxCubesInGame (g : Game) : Cubes :=
  Array.foldr maxByColor (Cubes.mk 0 0 0) g.rounds
  where
    maxByColor : Cubes -> Cubes -> Cubes
      | (Cubes.mk r1 g1 b1), (Cubes.mk r2 g2 b2) =>
        { red := max r1 r2
        , green := max g1 g2
        , blue := max b1 b2
        }

-- The Elf would first like to know which games would have been possible if the
-- bag contained only 12 red cubes, 13 green cubes, and 14 blue cubes?
def sumOfPossibleGameIds (games : Array Game) : Nat :=
  Array.foldr (fun game acc => acc + game.gameId) 0
  $ Array.filter isPossible games
  where
    isPossible game :=
      match maxCubesInGame game with
        | (Cubes.mk r g b) => r <= 12 && g <= 13 && b <= 14

def solve (compute : Array Game -> Nat) : IO String := do
  let lines <- IO.FS.lines "inputs/day2" -- FIXME: read the file lazily
  let parsedGames :=
        Array.sequenceMap lines
        $ Lean.Parsec.run game
  match parsedGames with
    | Except.error err => panic $ s!"Error parsing input: {err}"
    | Except.ok games => pure $ toString $ compute games

def part1 : IO String := solve sumOfPossibleGameIds

--
-- Part 2
--

-- the Elf poses a second question: in each game you played, what is the fewest
-- number of cubes of each color that could have been in the bag to make the game
-- possible?
--
-- For each game, find the minimum set of cubes that must have been present.
-- What is the sum of the power of these sets?
def sumOfPowerOfMinimumSets (games : Array Game) : Nat :=
  Array.foldl (· + ·) 0
  $ Array.map (power ∘ maxCubesInGame)
  $ games
  where
    -- The power of a set of cubes is equal to the numbers of red, green, and
    -- blue cubes multiplied together.
    power : Cubes -> Nat
      | (Cubes.mk r g b) => r * g * b

def part2 : IO String := solve sumOfPowerOfMinimumSets

def solutions : DaySolution := DaySolution.completeSolution part1 part2

end Day2
