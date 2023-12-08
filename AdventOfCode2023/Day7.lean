import Lean.Data.Parsec
import Init.Data.Range

import AdventOfCode2023.DaySolution
import AdventOfCode2023.Utils

open Lean.Parsec

open DaySolution
open Utils

namespace Day7

abbrev Card := Char

def card : Lean.Parsec Card :=
  pchar 'A' <|>
  pchar 'K' <|>
  pchar 'Q' <|>
  pchar 'J' <|>
  pchar 'T' <|>
  pchar '9' <|>
  pchar '8' <|>
  pchar '7' <|>
  pchar '6' <|>
  pchar '5' <|>
  pchar '4' <|>
  pchar '3' <|>
  pchar '2'

inductive HandType where -- we want `rest` to always be sorted from largest to smallest
  | HighCard (c : Card) (rest : List Card)
  | OnePair (pair : Card) (rest : List Card)
  | TwoPair (highPair : Card) (lowPair : Card) (rest : List Card) -- highPair > lowPair
  | ThreeOfAKind (c : Card) (rest : List Card)
  | FullHouse (triple : Card) (pair : Card) -- triple > pair
  | FourOfAKind (quadruple : Card) (rest : List Card)
  | FiveOfAKind (quintuple : Card)
  deriving Inhabited, BEq, Repr

open HandType

structure Hand where
  type : HandType
  repr : List Card
  bid : Nat
deriving Repr

def smallerHandThan (toCardRank : Card -> Nat) (h1 h2 : Hand) : Bool :=
    match compare (toHandRank h1.type) (toHandRank h2.type) with
      | Ordering.eq => h1.repr.map toCardRank < h2.repr.map toCardRank
      | Ordering.lt => true
      | Ordering.gt => false
  where
    toHandRank
    | HighCard _ _ => 0
    | OnePair _ _ => 1
    | TwoPair _ _ _ => 2
    | ThreeOfAKind _ _ => 3
    | FullHouse _ _ => 4
    | FourOfAKind _ _ => 5
    | FiveOfAKind _ => 6

def handType (toCardRank : Card -> Nat) (first : Card) (rest : List Card) : HandType :=
  go (HighCard first []) rest
  where
    -- promote the hand and/or stash the lower card with the others
    go : HandType -> List Card -> HandType
    | cur, [] => cur
    | cur, c :: cs => match cur with -- note: this assumes `others`is always sorted and does not contain pairs, triples, etc.
      | HighCard k others =>
        if c == k then go (OnePair c others) cs
        else if others.contains c then go (OnePair c $ insertCopiesIntoSorted 1 k $ others.filter (· != c)) cs
        else
          let (high, low) := if toCardRank k >= toCardRank c then (k, c) else (c, k)
          go (HighCard high $ insertCopiesIntoSorted 1 low others) cs
      | OnePair k others =>
        if c == k then go (ThreeOfAKind c others) cs
        else if others.contains c then
          let (highPair, lowPair) := if toCardRank k >= toCardRank c then (k, c) else (c, k)
          go (TwoPair highPair lowPair $ others.filter (· != c)) cs
        else go (OnePair k $ insertCopiesIntoSorted 1 c others) cs
      | TwoPair highPair lowPair _ =>
        if c == highPair then go (FullHouse highPair lowPair) cs
        else if c == lowPair then go (FullHouse lowPair highPair) cs
        else go (TwoPair highPair lowPair [c]) cs -- NOTE: assumes the game will only ever be played with 5 cards (i.e. c is the last card)
      | ThreeOfAKind k others =>
        if c == k then go (FourOfAKind k others) cs
        else if others.contains c then go (FullHouse k c) cs
        else go (ThreeOfAKind k $ insertCopiesIntoSorted 1 c others) cs
      | FourOfAKind k others =>
        if c == k then go (FiveOfAKind k) cs
        else go (FourOfAKind k $ insertCopiesIntoSorted 1 c others) cs
      | otherHand => panic s!"Impossible hand <{repr otherHand}> with cards left to process: {repr cs}"

    insertCopiesIntoSorted : Nat -> Card -> List Card -> List Card
      | n, i, [] => List.replicate n i
      | n, i, rest@(x::xs) => if toCardRank x < toCardRank i then List.replicate n i ++ rest else x :: insertCopiesIntoSorted n i xs

def hand (toCardRank : Card -> Nat) : Lean.Parsec Hand := do
  let first <- card
  let rest <- Array.toList <$> many1 card <* spaces
  let type := handType toCardRank first rest
  let repr := first :: rest
  let bid <- nat
  pure {type, bid, repr}

def solve (toCardRank : Card -> Nat) (sortHands : Array Hand -> Array Hand) : IO String := do
  let input <- IO.FS.readFile "inputs/day7"
  match Lean.Parsec.run (many1 $ hand toCardRank <* (void (pchar '\n') <|> eof)) input with
    | .error err => panic s!"Error parsing input: {err}"
    | .ok hands =>
      hands
      |> sortHands -- |> reprStr
      |>.zipWithIndex
      |>.foldl (λ acc (hand, rank) => acc + hand.bid * (rank + 1)) 0
      |> toString
      |> pure

def part1 : IO String := solve pt1Rank (flip .qsort $ smallerHandThan pt1Rank)
  where
  pt1Rank (c : Card) : Nat := ("23456789TJQKA".find (· == c)).byteIdx -- filthy hack

--
-- Part 2
--

def part2 : IO String := solve pt2Rank sortHands
  where
  sortHands (hands : Array Hand) : Array Hand :=
    hands.map promoteHandsWithJokers |>.qsort (smallerHandThan pt2Rank)

  pt2Rank (c : Card) : Nat := ("J23456789TQKA".find (· == c)).byteIdx

  promoteHandsWithJokers (h : Hand) : Hand :=
    match h.repr.find? (· == 'J') with
      | none => h
      | _ => { h with type := match h.type with
        | FourOfAKind 'J' [c] => FiveOfAKind c
        | FourOfAKind c ['J'] => FiveOfAKind c
        | FullHouse 'J' p => FiveOfAKind p
        | FullHouse t 'J' => FiveOfAKind t
        | ThreeOfAKind 'J' [k1, k2] => FourOfAKind k1 [k2]
        -- NOTE: J will always be last card, since `rest` is ordered
        | ThreeOfAKind t [k1, 'J'] => FourOfAKind t [k1]
        | TwoPair 'J' l [k] => FourOfAKind l [k]
        | TwoPair h 'J' [k] => FourOfAKind h [k]
        | TwoPair h l ['J'] => FullHouse h l
        | OnePair 'J' (k :: ks) => ThreeOfAKind k ks
        | OnePair p [k1, k2, 'J'] => ThreeOfAKind p [k1, k2]
        | HighCard 'J' (k :: ks) => OnePair k ks
        | HighCard h [k1, k2, k3, 'J'] => OnePair h [k1, k2, k3]
        | _ => h.type
      }

def solutions : DaySolution := DaySolution.completeSolution part1 part2

end Day7
