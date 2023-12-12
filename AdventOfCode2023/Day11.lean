import Std.Data.BitVec
import AdventOfCode2023.DaySolution
import AdventOfCode2023.Utils

open Utils
open DaySolution

namespace Day11

--
-- Part 1
--

abbrev Pos := (Nat × Nat)

def readPositions (lines : Array String) : (Array Nat × Array Nat × Array Pos) :=
  let (_, emptyRows, emptyColumns, stars) := lines.foldl readLine (0, .mkArray lines.size true, .mkArray lines[0]!.length true, #[])
  let bitmapToArray (v : Array Bool) := v.zipWithIndex.filter (·.fst) |>.map (·.snd)
  (bitmapToArray emptyRows, bitmapToArray emptyColumns, stars)
  where
  readLine
  | (y, emptyRows, emptyColumns, stars), line =>
    (y+1, Prod.snd $ line.foldl (readChar y) (0, emptyRows, emptyColumns, stars))

  readChar
  | y, (x, emptyRows, emptyColumns, stars), char =>
    match char with
      | '#' => (x+1, emptyRows.set! y false, emptyColumns.set! x false, stars.push (x, y))
      | _ => (x+1, emptyRows, emptyColumns, stars)

def distance : Nat → Array Nat → Array Nat → Pos → Pos → Nat
| expansionFactor, emptyRows, emptyCols, (x0, y0), (x1, y1) =>
    let (minX, maxX) := if x0 <= x1 then (x0, x1) else (x1, x0)
    let (minY, maxY) := if y0 <= y1 then (y0, y1) else (y1, y0)
    maxX - minX
      -- we subtract 1 from the expansion factor because we already have the
      -- original empty cols/rows on the map.
      + ((emptyCols.filter (λx => minX < x && x < maxX)).size * (expansionFactor - 1))
      + maxY - minY
      + ((emptyRows.filter (λy => minY < y && y < maxY)).size * (expansionFactor - 1))

def part1 : IO String := do
  let (emptyRows, emptyCols, stars) ← readPositions <$> IO.FS.lines "inputs/day11"
  pure
    $ toString
    $ Prod.fst
    $ stars.foldl
      (λ(acc, i) star => let k := (stars[i+1:].foldl (λs p => s + (distance 2 emptyRows emptyCols star p)) 0); (acc + k, i+1))
      (0, 0)


def part2 : IO String := do
  let (emptyRows, emptyCols, stars) ← readPositions <$> IO.FS.lines "inputs/day11"
  pure
    $ toString
    $ Prod.fst
    $ stars.foldl
      (λ(acc, i) star => let k := (stars[i+1:].foldl (λs p => s + (distance 1000000 emptyRows emptyCols star p)) 0); (acc + k, i+1))
      (0, 0)


def solutions : DaySolution := DaySolution.completeSolution part1 part2

end Day11
