import Lean.Data.Parsec
import Init.Data.Range
import Std.Data.Nat.Gcd
import Mathlib.Data.Nat.Factorial.Basic

import AdventOfCode2023.DaySolution
import AdventOfCode2023.Utils

open Lean.Parsec

open DaySolution
open Utils

namespace Day9

-- Part 1 is easy once we expand the recurrences. Let L_0 be the "first"
-- sequence (i.e, our input) and
--
--     L_i(n) = L_(i-1)(n+1) - L(i-1)(n)
--
-- for each subsequence level i. If we expand the triangle above the first row
-- of all zeros, we'll eventualy reach a row with only a single 0 on level n.
-- Then:
--
-- L_1(n) = L_0(n+1) - L_0(n)
--
-- L_2(n) = L_1(n+1)            - L_1(n)
--      = [L_0(n+2) - L_0(n+1)] - [L_0(n+1) - L_0(n)]
--      = L_0(n+2) - 2*L_0(n+1) + L_0(n)
--
-- L_3(n) = L_2(n+1) - L_2(n)
--      = [L_0(n+3) - 2*L_0(n+2) + L_0(n+1)] - [L_0(n+2) - 2*L_0(n+1) + L_0(n)]
--      = L_0(n+3) - 3*L_0(n+2) + 3*L_0(n+1) - L_0(n)
--
-- ...
--
-- L_n(m) = C(n,0)*L_0(m+(n-1)) - C(n,1)*L_0(m+(n-2)) + C(n,2)*L_0(m+(n-3)) ... +- C(n,n)*L_0(m) <- the sign depends on n
--
-- But we know that L_n(0) = 0 (it's the only value in the nth row). So
--
-- L_n(0) = 0
--        = C(n,0)*L_0(n-1) - C(n,1)*L_0(n-2) + C(n,2)*L_0(n-3) ... +- C(n,n)*L_0(0)
--
-- Where the missing number is L_0(n-1). Since C(n,0) = 1, we just solve for L_0(n-1):
--
-- MissingNumber = C(n,1)*L_0(n-2) - C(n,2)*L_0(n-3) ... -+ C(n,n)*L_0(0)
--
-- Note that the signs are inverted.

def binomial (n : ℕ) (r : ℕ) : ℕ := n.factorial / ((n-r).factorial * r.factorial)

def getMissingRightValue (seq : Array ℤ) : ℤ :=
  seq.foldr
    (λ n (acc, mul, i) => (acc + (mul * n * binomial seq.size i), -mul, i-1))
    (0, 1, seq.size - 1)
  |> Prod.fst


def solve (getMissingValue : Array ℤ -> ℤ) : IO String := do
  match Lean.Parsec.run (many1 (many1 (token int) <* (skipChar '\n' <|> eof))) (← IO.FS.readFile "inputs/day9") with
    | .error err => panic s!"Error reading line: {err}"
    | .ok seqs => seqs.map getMissingValue |> sum |> toString |> pure

def part1 : IO String := solve getMissingRightValue

--
-- Part 2
--

-- Same thing, but now we displace L_0 by 1 and want to find L_0(0):
--
-- L_0(0) = -L_0(1) + C(n,2)*L_0(2) - C(n,3)*L_0(3) - ... -+ C(n,n-1)*L_0(n)

def getMissingLeftValue (seq : Array ℤ) : ℤ :=
  seq.foldl
    (λ (acc, mul, i) n => (acc + (mul * n * binomial seq.size (i + 1)) , -mul , i+1))
    (0, 1, 0)
  |> Prod.fst

def part2 : IO String := solve getMissingLeftValue

def solutions : DaySolution := DaySolution.completeSolution part1 part2

end Day9
