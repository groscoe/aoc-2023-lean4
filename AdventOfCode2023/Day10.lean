import Lean.Data.HashMap
import Std.Lean.HashSet
import Init.Data.Queue
import AdventOfCode2023.DaySolution

open DaySolution

namespace Day10

--
-- Part 1
--

abbrev Pos := (Int × Int)

def north : Pos → Pos | (x, y) => (x  , y-1)
def south : Pos → Pos | (x, y) => (x  , y+1)
def west  : Pos → Pos | (x, y) => (x-1, y  )
def east  : Pos → Pos | (x, y) => (x+1, y  )

def neighbors p := #[north p, south p, east p, west p]

abbrev Graph := Lean.HashMap Pos (Char × Array Pos)

def buildGraph (input : String) : (Pos × Graph) :=
  withStartingConnections
  $ input.foldl readChar (none, ((0, 0) : Pos), Lean.HashMap.empty)
  where
    nextLine | (_, y) => (0, y+1)

    readChar
    | (_, p, acc), 'S' => (some p, east p, acc)
    | (startPos, p, acc), '\n' => (startPos, nextLine p, acc)
    | (startPos, p, acc), '.' => (startPos, east p, acc)
    | (startPos, p, acc), c =>
      let acc' := acc.insert p $ match c with
        | '|' => (c, #[north p, south p])
        | '-' => (c, #[east p, west p])
        | 'L' => (c, #[north p, east p])
        | 'J' => (c, #[north p, west p])
        | '7' => (c, #[south p, west p])
        | 'F' => (c, #[south p, east p])
        | other => panic s!"Unexpected char at position {p}: {other}"
      (startPos, east p, acc')

    withStartingConnections
      | (none, _, _) => panic "Start position (S) not found in input"
      | (some startPos, _, g) =>
        let connectsToStartPos p := match g.find? p with
          | none => false
          | some (_, connections) => connections.contains startPos

        let adjacentToStartNode := neighbors startPos |>.filter connectsToStartPos

        (startPos, g.insert startPos ('S', adjacentToStartNode))

partial def findLoop (startNode : Pos) (g : Graph) : Array Pos :=
  let firstNeighbor := g.find! startNode |>.snd |>.get! 0
  go #[startNode] startNode firstNeighbor
  where
    go
    | path, curNode, nextNode =>
      -- dbg_trace "{steps}: curNode := {curNode}"
      if nextNode == startNode then path
      else
        let nextNeighbor := g.find! nextNode |>.snd |>.filter (· ≠ curNode) |>.get! 0
        go (path.push nextNode) nextNode nextNeighbor

def solve (compute : Graph → Array Pos → Nat) : IO String := do
  let (startNode, g) := buildGraph (←IO.FS.readFile "inputs/day10")
  let cycle := findLoop startNode g
  pure $ toString $ compute g cycle

def part1 : IO String :=
  solve $ λ_ cycle => cycle.size / 2 -- note: the length must always be even

--
-- Part 2
--

-- A nice visualization for debugging
def showGraph (g : Graph) (cycle: Array Pos) (isInside : Graph → Array Pos → Pos → Bool) (maxRow maxCol : Nat) : String :=
  Id.run $ do
    let mut s := ""
    for i in [0:maxRow] do
      for j in [0:maxCol] do
        s := s ++ s!"{g.findD (j, i) ('*', #[]) |>.fst |> toBoxChar (j, i)}"
      s := s ++ "\n"
    pure s
  where
  toBoxChar : Pos → Char → Char
    | p, 'F' => if cycle.contains p then '┏' else '┌'
    | p, '7' => if cycle.contains p then '┓' else '┐'
    | p, 'L' => if cycle.contains p then '┗' else '└'
    | p, 'J' => if cycle.contains p then '┛' else '┘'
    | p, '-' => if cycle.contains p then '━' else '─'
    | p, '|' => if cycle.contains p then '┃' else '│'
    | _, 'S' => 'X'
    | p, '*' => if isInside g cycle p then '▒' else '░'
    | _, c => c

def enclosedArea (vertices : Array Pos) : Nat :=
  -- NOTE: A naive calculation of the area will also include the border tiles.
  -- We need to subtract them to get only the fully enclosed tyles.
  (Int.natAbs (shoelaceArea vertices.toList) - vertices.size) / 2 + 1
  where
  shoelaceArea -- actually 2*area.
  | (x1, y1) :: p2@(x2, y2) :: rest => (x1 * y2) - (y1 * x2) + shoelaceArea (p2 :: rest)
  | [(xn, yn)] =>
    let (x0, y0) := vertices[0]!
    (xn * y0) - (yn * x0)
  | _ => panic "Need at least two vertices"

def part2 : IO String := solve $ λ_ cycle =>
  -- dbg_trace showGraph g cycle (λ_ _ _ => false) 11 20
  enclosedArea cycle

def solutions : DaySolution := DaySolution.completeSolution part1 part2

end Day10
