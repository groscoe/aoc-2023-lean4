import Mathlib.Control.Traversable.Basic

import «AdventOfCode2023»

open DaySolution

def printDay : Nat -> DaySolution -> IO Unit
  -- FIXME: remove duplicated code for part 1
  | day, DaySolution.partialSolution part1 => do
      let part1Solution <- part1
      IO.println s!"Day {day}, part 1: {part1Solution}"
      IO.println s!"Day {day}, part 2: <not completed yet>"
  | day, DaySolution.completeSolution part1 part2 => do
      let part1Solution <- part1
      IO.println s!"Day {day}, part 1: {part1Solution}"
      let part2Solution <- part2
      IO.println s!"Day {day}, part 2: {part2Solution}"

def void (action : IO a) : IO Unit := action *> pure ()

def main : IO Unit := do
  let indexedSolutions := List.enumFrom 1
    [ Day1.solutions
    , Day2.solutions
    ]
  List.forM indexedSolutions (Function.uncurry printDay)
