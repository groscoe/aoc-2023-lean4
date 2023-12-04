import Lean.Data.HashSet
import Std.Lean.HashSet
import Lean.Data.Parsec
import AdventOfCode2023.DaySolution
import AdventOfCode2023.Utils
import Std.Data.Array.Init.Basic

open DaySolution
open Utils
open Lean.Parsec
open Lean.HashSet

namespace Day4

--
-- Part 1
--

structure Card where
  winningNumbers : Lean.HashSet Nat
  drawNumbers : Array Nat
deriving Inhabited

def points (c : Card) : Nat :=
  match Array.size (c.drawNumbers.filter c.winningNumbers.contains) with
    | 0 => 0
    | n => 2^(n - 1)

def card : Lean.Parsec Card := do
  void $ keyword "Card" *> token nat <* keyword ":"
  let winningNumbers <- many1 (token nat)
  keyword "|"
  let drawNumbers <- many1 (token nat)
  pure { winningNumbers := Lean.HashSet.ofArray winningNumbers, drawNumbers }

def solve (compute : Array Card -> Nat) : IO String := do
  let lines <- IO.FS.lines "inputs/day4"
  match lines.sequenceMap $ Lean.Parsec.run card with
    | Except.error err => panic s!"Error parsing input: {err}"
    | Except.ok cards => pure $ toString $ compute cards

def part1 : IO String := solve (sum ∘ Array.map points)

-- def part2 

def countCardCopies (cards : Array Card) : Nat := -- FIXME: make this less imperative?
  let copies := (cards.map (λ _ => 1))
  Prod.fst $ cards.zipWithIndex.foldl (λ (total, copies) (c, i) => countCopies total i c copies) (0, copies)
  where
    countCopies (total : Nat) (i : Nat) (c : Card) (copies : Array Nat) :=
      let numWinning := Array.size (c.drawNumbers.filter c.winningNumbers.contains)
      let numCopies := copies.get! i
      let newCopies := Array.foldl
        (λ updatedCopies offset => 
          let j := i + offset + 1
          if j < updatedCopies.size
            then updatedCopies.modify j (· + numCopies)
            else updatedCopies
        )
        copies
        (Array.range numWinning)
      (total + numCopies, newCopies)

def part2 := solve countCardCopies

def solutions : DaySolution := DaySolution.completeSolution part1 part2

end Day4
