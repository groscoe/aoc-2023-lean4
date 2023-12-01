import Lean.Data.Parsec
import AdventOfCode2023.DaySolution

open Lean.Parsec
open DaySolution

namespace Day1

--
-- Part 1
--

-- "[...] each line originally contained a specific calibration value that the
-- Elves now need to recover. On each line, the calibration value can be found
-- by combining the first digit and the last digit (in that order) to form a
-- single two-digit number."
def getCalibrationValue (line: String) : Option Int :=
  go none none (String.toList line) >>= String.toInt?
  where
    go : Option Char -> Option Char -> List Char -> Option String
      | fstDigit, lastDigit, c :: cs =>
        if Char.isDigit c
          -- FIXME: this is incorrect in the general case; pathological strings
          -- having only 1 digit will have this digit repeated as the output.
          then go (fstDigit <|> some c) (some c <|> lastDigit) cs
          else go fstDigit lastDigit cs
      | fstDigit, lastDigit, [] =>
        let toString x y := List.asString [x, y]
        toString <$> fstDigit <*> lastDigit

-- Sum the calibration values of each line of the input file.
def solve (compute : String -> Option Int) : IO String := do
  let lines <- IO.FS.lines "inputs/day1" -- FIXME: read the file lazily
  let calibrationValues := Array.sequenceMap lines compute
  match calibrationValues with
    | none => panic "Error parsing input"
    | some values => pure $ toString $ Array.foldl (· + ·) 0 values

-- "Consider your entire calibration document. What is the sum of all of the
-- calibration values?"
def part1 : IO String := solve getCalibrationValue

--
-- Part 2
--

-- Parse a sequence of matchers (not necessarily sequentially) until the end of
-- the input
partial def manyNonSequentially (p : Lean.Parsec a) : Lean.Parsec (List a) :=
  (Lean.Parsec.eof *> pure [])
  <|> ((· :: ·) <$> p <*> manyNonSequentially p)
  <|> (Lean.Parsec.skip *> manyNonSequentially p)

-- Parse a digit either as a numeral or as a spelled out number
def digit : Lean.Parsec Char := Lean.Parsec.digit <|> spelled
  where
  spelled : Lean.Parsec Char :=
    List.foldl (· <|> ·) (fail "Expected a spelled digit")
    $ (fun (numeral, digit) => pstring numeral *> pure digit)
    <$> [ ("zero", '0')
        , ("one", '1')
        , ("two", '2')
        , ("three", '3')
        , ("four", '4')
        , ("five", '5')
        , ("six", '6')
        , ("seven", '7')
        , ("eight", '8')
        , ("nine", '9')
        ]

-- "It looks like some of the digits are actually spelled out with letters: one,
-- two, three, four, five, six, seven, eight, and nine also count as valid
-- 'digits'."
def parseCalibrationValue (line: String) : Option Int :=
  match Except.toOption $ Lean.Parsec.run (manyNonSequentially digit) line with
    | some digits => do
      let fstDigit := List.head? digits
      let lastDigit := List.getLast? digits
      let calibrationValue <- (fun x y => List.asString [x, y]) <$> fstDigit <*> lastDigit
      String.toInt? calibrationValue
    | _ => none

-- "Equipped with this new information, you now need to find the real first and
-- last digit on each line."
def part2 : IO String := solve parseCalibrationValue

def solutions : DaySolution := DaySolution.completeSolution part1 part2

end Day1
