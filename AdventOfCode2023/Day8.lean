import Lean.Data.Parsec
import Init.Data.Range
import Std.Data.Nat.Gcd

import AdventOfCode2023.DaySolution
import AdventOfCode2023.Utils

open Lean.Parsec

open DaySolution
open Utils

namespace Day8

inductive Direction where
  | L
  | R
deriving Inhabited, Repr

def direction : Lean.Parsec Direction :=
  (pchar 'L' *> pure .L) <|> (pchar 'R' *> pure .R)

structure NetworkMap where
  directions : Array Direction
  network : Lean.HashMap String (String × String)

def node : Lean.Parsec (String × String × String) := do
  let alphaNum := many1Chars (satisfy Char.isAlphanum)
  let origin <- token alphaNum <* pstring "= ("
  let left <- alphaNum <* pstring ", "
  let right <- alphaNum <* pstring ")" <* spaces
  return (origin, left, right)

def network : Lean.Parsec (Lean.HashMap String (String × String)) := do
  let nodes <- many1 (node <* (skipChar '\n' <|> eof))
  return nodes.foldl (λacc (o, (l ,r)) => acc.insert o (l, r)) .empty

def networkMap : Lean.Parsec NetworkMap := do
  let directions <- many1 direction <* skipString "\n\n"
  let network <- network
  return {directions, network}

partial def countSteps (initial : String) (finished : String → Bool) (nm : NetworkMap) : Nat :=
  go initial 0 0
  where
  go
  | node, i, steps =>
    if finished node then steps
    else
      let (nextLeft, nextRight) := nm.network.find! node
      let nextNode := match nm.directions[i % nm.directions.size]! with
        | .L => nextLeft
        | .R => nextRight
      go nextNode (i + 1) (steps + 1)

def solve (count : NetworkMap -> Nat) : IO String := do
  match Lean.Parsec.run networkMap (← IO.FS.readFile "inputs/day8") with
  | .error err => panic s!"Error parsing input: {err}"
  | .ok nm => pure $ toString $ count nm

def part1 : IO String := solve $ countSteps "AAA" (· == "ZZZ")

--
-- Part 2: LCM is the key
--

def countSimultaneousPaths (nm : NetworkMap) : Nat :=
  nm.network.fold
    (λ acc k _ => if k.endsWith "A" then acc.lcm (countSteps k (·.endsWith "Z") nm) else acc)
    1

def part2 : IO String := solve countSimultaneousPaths

def solutions : DaySolution := DaySolution.completeSolution part1 part2

end Day8
