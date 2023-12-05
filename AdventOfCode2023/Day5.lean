import Lean.Data.Parsec
import Std.Data.Array.Basic

import AdventOfCode2023.DaySolution
import AdventOfCode2023.Utils

open Lean.Parsec

open DaySolution
open Utils

namespace Day5

structure Mapping where -- i.e. if n >= start && n < end then some (n + offset) else none
  sourceStart : Nat -- inclusive
  sourceEnd : Nat -- exclusive
  offset : Int
deriving Inhabited, Repr, Ord

def mkMapping (destStart : Nat) (sourceStart : Nat) (rangeLength : Nat) : Mapping :=
  { sourceStart := sourceStart
  , sourceEnd := sourceStart + rangeLength
  , offset := destStart - sourceStart
  }

structure Almanac where
  seeds : Array Nat
  seedToSoil : Array Mapping
  soilToFertilizer : Array Mapping
  fertilizerToWater : Array Mapping
  waterToLight : Array Mapping
  lightToTemperature : Array Mapping
  temperatureToHumidity : Array Mapping
  humidityToLocation : Array Mapping
deriving Repr

def almanac : Lean.Parsec Almanac := do
  let sortBySourceStart (arr : Array Mapping) := arr.qsort (fun m1 m2 => m1.sourceStart < m2.sourceStart)
  let seeds <- keyword "seeds:" *> many1 (token nat) <* pstring "\n\n" 
  let mappings := many1 (mkMapping <$> token nat <*> token nat <*> token nat <* pchar '\n')
  let seedToSoil <- keyword "seed-to-soil map:\n" *> mappings <* pchar '\n'
  let soilToFertilizer <- keyword "soil-to-fertilizer map:\n" *> mappings <* pchar '\n'
  let fertilizerToWater <- keyword "fertilizer-to-water map:\n" *> mappings <* pchar '\n'
  let waterToLight <- keyword "water-to-light map:\n" *> mappings <* pchar '\n'
  let lightToTemperature <- keyword "light-to-temperature map:\n" *> mappings <* pchar '\n'
  let temperatureToHumidity <- keyword "temperature-to-humidity map:\n" *> mappings <* pchar '\n'
  let humidityToLocation <- keyword "humidity-to-location map:\n" *> mappings <* eof
  pure
    { seeds
    , seedToSoil := sortBySourceStart seedToSoil
    , soilToFertilizer := sortBySourceStart soilToFertilizer
    , fertilizerToWater := sortBySourceStart fertilizerToWater
    , waterToLight := sortBySourceStart waterToLight
    , lightToTemperature := sortBySourceStart lightToTemperature
    , temperatureToHumidity := sortBySourceStart temperatureToHumidity
    , humidityToLocation := sortBySourceStart humidityToLocation
    }

--
-- Part 1
--

-- Brute force is fine for this input size
def apply (mappings : Array Mapping) (x : Nat) : Nat :=
  match mappings.find? (fun m => x >= m.sourceStart && x < m.sourceEnd) with
    | some interval => Int.toNat $ x + interval.offset
    | none => x

def seedToLocation (almanac : Almanac) (seed : Nat) : Nat :=
  seed
  |> apply almanac.seedToSoil
  |> apply almanac.soilToFertilizer
  |> apply almanac.fertilizerToWater
  |> apply almanac.waterToLight
  |> apply almanac.lightToTemperature
  |> apply almanac.temperatureToHumidity
  |> apply almanac.humidityToLocation

def minLocation (seeds: Array Nat) (almanac : Almanac) : Nat :=
  arrayMin! $ seeds.map (seedToLocation almanac)

def solve (compute : Almanac -> Nat) : IO String := do
  let input <- IO.FS.readFile "inputs/day5"
  match Lean.Parsec.run almanac input with
    | Except.error err => panic s!"Error parsing input: {err}"
    | Except.ok almanac => pure $ toString $ compute almanac

def part1 : IO String :=
  solve $ fun almanac => minLocation almanac.seeds almanac

--
-- Part 2
--

-- NOTE: Brute force works, but takes way too long here (1h15 min on my
--       machine). We need a smarter solution: "compose" the mappings by
--       spliting a range (which can be seen as a mapping with an offset of 0)
--       in its intersections with each interval in the range.

-- Split a mapping into its intersections with other mappings.
-- The idea here is to "traverse" the transposed interval from left to right,
-- keeping track of all intersections.
-- NOTE: assumes that `maps` is disjoint and sorted by `sourceStart`.
def split (maps : Array Mapping) (interval : Mapping) : Array Mapping :=
  match maps.foldl splitInterval (#[], some interval) with
    | (acc, some finalInterval) => acc.push finalInterval
    | (acc, _) => acc
  where
    splitInterval : Array Mapping × Option Mapping -> Mapping -> Array Mapping × Option Mapping
    | (acc, some current), splitter =>
      let offsetStart := current.sourceStart + current.offset
      let offsetEnd := current.sourceEnd + current.offset
      if offsetEnd < splitter.sourceStart then
        -- current interval ends to the left of all remaining intervals (since
        -- they're sorted by their `sourceStart`); no more comparisons are
        -- needed.

        -- dbg_trace "Interval {repr current} (mapped to ({offsetStart}, {offsetEnd})) is before {repr splitter}"
        (acc.push current, none)
      else if offsetStart >= splitter.sourceEnd then
        -- current interval starts to the right of the splitter; move to the
        -- next splitter.

        -- dbg_trace "Interval {repr current} (mapped to ({offsetStart}, {offsetEnd})) is after {repr splitter}"
        (acc, some current)
      else
        -- There's an intersection between the current interval and the
        -- splitter; accumulate both the region before the intersection (if
        -- any) and the intersection and move along with the part after the
        -- intersection (if any).
        --
        -- NOTE: for the intersection, the new offset will be the sum of the
        -- current interval's offset and the splitter's. For the parts outside
        -- the intersection, the original offset should be kept.
        let beforeIntersection : Option Mapping :=
          if offsetStart < splitter.sourceStart then
            some
              { sourceStart := current.sourceStart
              , sourceEnd := Int.toNat $ splitter.sourceStart - current.offset
              , offset := current.offset
              }
          else none

        let afterIntersection : Option Mapping :=
          if offsetEnd > splitter.sourceEnd then
            some
              { sourceStart := Int.toNat $ splitter.sourceEnd - current.offset
              , sourceEnd := current.sourceEnd
              , offset := current.offset
              }
          else none

        let intersection : Mapping :=
          { sourceStart := Int.toNat $ (max offsetStart splitter.sourceStart) - current.offset
          , sourceEnd := Int.toNat $ (min offsetEnd splitter.sourceEnd) - current.offset
          , offset := current.offset + splitter.offset
          }


        -- dbg_trace "Interval {repr current} (mapped to ({offsetStart}, {offsetEnd})) intersects {repr splitter}: {repr intersection}"
        let newAcc :=
          match beforeIntersection with
            | some i => (acc.push i).push intersection
            | none => acc.push intersection
        (newAcc, afterIntersection)

    | (acc, none), _ => (acc, none) -- there's nothing left of the interval to split.

-- Compute the location ranges corresponding to each initial seed range.
def seedRangeToLocations : Almanac -> Nat × Nat -> Array Nat
  | almanac, (start, rangeLength) =>
      let initialInterval := { sourceStart := start, sourceEnd := start + rangeLength, offset := 0 }
      let finalMappings := split almanac.seedToSoil initialInterval
        |> Array.concatMap (split almanac.soilToFertilizer)
        |> Array.concatMap (split almanac.fertilizerToWater)
        |> Array.concatMap (split almanac.waterToLight)
        |> Array.concatMap (split almanac.lightToTemperature)
        |> Array.concatMap (split almanac.temperatureToHumidity)
        |> Array.concatMap (split almanac.humidityToLocation)
      finalMappings.map (fun m => Int.toNat $ m.sourceStart + m.offset)

def pairs (arr : Array a) : Array (a × a) := List.toArray $ go arr.toList
  where
    go
      | (x :: y :: xs) => (x, y) :: go xs
      | [] => []
      | _ => panic "cannot pair off an odd number of elements"
  
def part2 : IO String := 
  solve $ fun almanac => 
    arrayMin!
      $ Array.map (arrayMin! ∘ seedRangeToLocations almanac)
      $ pairs almanac.seeds

def solutions : DaySolution := DaySolution.completeSolution part1 part2

end Day5
