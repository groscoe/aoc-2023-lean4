import Lean.Data.HashMap
import AdventOfCode2023.DaySolution

open DaySolution
open Lean.HashMap

namespace Day3

--
-- Part 1
--

structure Pos where
  line : Nat
  char : Nat
deriving BEq, Repr, Inhabited, Hashable

structure PartNumber where
  startPos : Pos
  endPos : Pos
  digits : String
deriving Repr

abbrev Symbols := Lean.HashMap Pos Char

-- A simple stack machine for parsing each line into its symbols and numbers
def lineInfo
  (symbolPositions : Symbols)
  (numbers : Array PartNumber)
  (lineno : Nat)
  (line : String)
  : (Symbols × Array PartNumber) :=
    Prod.fst $ String.foldl parseChar ((symbolPositions, numbers), (0, none)) line
    where
      parseChar
        -- FIXME: factor out common cases to reduce duplication
        | ((syms, nums), (i, curNumber)), c =>
          let nextChar := i + 1
          -- cases: c is dot, c is a symbol, c is a digit, end of the line.
          let nextSyms := if c != '.' && not c.isDigit -- i.e. c is a symbol
            then syms.insert { line := lineno, char := i} c
            else syms

          let nextNums := match curNumber with
            | some (startChar, revDigits) =>
              if not c.isDigit then -- number boundary: push current digits
                let newNumber : PartNumber :=
                  { startPos := { line := lineno, char := startChar }
                  , endPos := { line := lineno, char := i-1 }
                  , digits := List.asString $ List.reverse revDigits
                  }
                nums.push newNumber
              else if nextChar >= line.length then -- end of line: push c and current digits
                let newNumber : PartNumber :=
                  { startPos := { line := lineno, char := startChar }
                  , endPos := { line := lineno, char := i }
                  , digits := List.asString $ List.reverse (c :: revDigits)
                  }
                nums.push newNumber
              else nums -- digit in the middle of a number: keep parsing
            | none => nums -- not in the middle of a number, ignore

          let nextCurNumber := match curNumber with
            | some (startChar, revDigits) =>
              if c.isDigit then some (startChar, c :: revDigits) else none
            | none => if c.isDigit then some (i, [c]) else none

          ((nextSyms, nextNums), (nextChar, nextCurNumber))

def lineInfos (lines : Array String) : (Symbols × Array PartNumber) :=
  Array.foldr parseLine (Lean.HashMap.empty, #[]) $ Array.mapIdx lines (fun i line => (i, line))
  where
    parseLine
      | (lineno, line), (symbolPositions, numbers) => lineInfo symbolPositions numbers lineno line

def isPartNumber : Symbols -> PartNumber -> Bool
  | symbolPositions, {startPos, endPos, digits} =>
      let (line, startChar) := (startPos.line, startPos.char)
      let endChar := endPos.char
      let boundaries : List Pos :=
              -- clockwise from the bottom-left corner
              [ {line := line + 1, char := startChar - 1}
              , {line            , char := startChar - 1}
              , {line := line - 1, char := startChar - 1}
              , {line := line - 1, char := endChar + 1}
              , {line            , char := endChar + 1}
              , {line := line + 1, char := endChar + 1}
              ]
      let upperAndLower :=
            List.foldr (· ++ ·) []
            $ (λ (i, _) => [{line := line - 1, char := i}, {line := line + 1, char := i}])
            <$> List.enumFrom startChar (String.toList digits)
      let adjacentSymbols :=
        List.filter symbolPositions.contains (boundaries ++ upperAndLower)
      not $ List.isEmpty adjacentSymbols

def solve (compute : Symbols -> Array PartNumber -> Array Int) : IO String := do
  let lines <- IO.FS.lines "inputs/day3"
  let (symbols, numbers) := lineInfos lines
  pure
    ∘ toString
    ∘  Array.foldr (fun number acc => acc + number) 0
    ∘  compute symbols
    $ numbers

def part1 : IO String :=
  solve $ λ symbols =>
    Array.map (λ part => String.toInt! part.digits)
    ∘ Array.filter (isPartNumber symbols)

--
-- Part 2
--

def getAdjacentNumbers (pos : Pos) (numbers : Array PartNumber) : Array PartNumber :=
  numbers.filter isAdjacent
  where
    isAdjacent
      | { startPos, endPos, digits := _} =>
          ( pos.line == startPos.line -- same line
            || startPos.line == pos.line + 1 -- or above a number
            || pos.line == startPos.line + 1 -- or below a number
          )
          && pos.char + 1 >= startPos.char -- at most one char before the number
          && pos.char <= endPos.char + 1 -- and at most one char after

def isGearRatio (pos : Pos) (symbol : Char) (numbers : Array PartNumber) : Option Int :=
  if symbol == '*' then
    match getAdjacentNumbers pos numbers with
      | #[first, second] => some (first.digits.toInt! * second.digits.toInt!)
      | _ => none
  else none

def part2 : IO String :=
  solve $ λ symbols numbers =>
    Lean.HashMap.fold (pushIfGearRatio numbers) #[] symbols
  where
    pushIfGearRatio numbers acc pos symbol :=
      match isGearRatio pos symbol numbers with
        | some gearRatio => acc.push gearRatio
        | none => acc

def solutions : DaySolution := DaySolution.completeSolution part1 part2

end Day3
