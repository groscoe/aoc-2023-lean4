import Lean.Data.Parsec
import Std.Data.Nat.Basic

import AdventOfCode2023.DaySolution
import AdventOfCode2023.Utils

open Lean.Parsec

open DaySolution
open Utils

namespace Day6

-- Part 1

-- This part is easy: let T be the time limit for the race, v the velocity and
-- t_press the time spent pressing the button. We know the distance d moved
-- will be `d = v * (T - t_press)`. We also know `v = t_press`, so `d = T*t_press - t_press^2`.
-- Let D be the distance to beat; then, we want to get
-- all the integer solutions of `T*t_press - t_press^2 > D`. Solving the
-- inequality for t_press, we have:
--  
--   (T - sqrt(T^2 - 4*D))/2 < t_press < (T + sqrt(T^2 - 4*D)/2), for integer T_press
-- 
-- Now we can just subtract the minimum t_press from the maximum t_press
-- (adding 1, because the minimum winning time still wins) to get the number of
-- winning presses.

structure Race where
  time : Nat
  recordDistance : Nat
deriving Repr

def races : Lean.Parsec (Array Race) := do
  let times <- keyword "Time:" *> many1 (token nat) <* pchar '\n'
  let records <- keyword "Distance:" *> many1 (token nat)
  pure $ times.zipWith records Race.mk

def numberOfWinningPresses : Race -> Float
  | {time, recordDistance} =>
    let delta := Float.sqrt $ Float.ofNat $ time^2 - 4*recordDistance
    let minWinningTime := Float.floor $ (Float.ofNat time - delta) / 2 + 1
    let maxWinningTime := Float.ceil $ (Float.ofNat time + delta) / 2 - 1
    maxWinningTime - minWinningTime + 1

def solve (parse: Lean.Parsec (Array Race)) (fold : Array Float -> Float) : IO String := do
  let input <- IO.FS.readFile "inputs/day6"
  match Lean.Parsec.run parse input with
    | Except.error err => panic s!"Error parsing input: {err}"
    | Except.ok curRecords =>
      pure
      -- FIXME: filthy hack to show only the integral part of floats
      $ (fold $ curRecords.map numberOfWinningPresses).toString.takeWhile (· != '.')

def part1 : IO String := solve races prod

-- 
-- Part 2
--

-- Part 2 is just a matter of parsing the input differently

def race : Lean.Parsec (Array Race) := do
  let time <- digitsToNat <$> (keyword "Time:" *> many1 (token digit) <* pchar '\n')
  let recordDistance <- digitsToNat <$> (keyword "Distance:" *> many1 (token digit) <* pchar '\n')
  pure #[{time, recordDistance}]

def part2 : IO String := solve race sum

def solutions : DaySolution := DaySolution.completeSolution part1 part2

end Day6
