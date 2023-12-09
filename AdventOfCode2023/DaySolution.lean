namespace DaySolution

inductive DaySolution where
  | partialSolution (part1 : IO String)
  | completeSolution (part1 : IO String) (part2 : IO String)

end DaySolution
