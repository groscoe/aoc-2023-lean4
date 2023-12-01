namespace DaySolution

inductive DaySolution where
  | partialSolution : IO String -> DaySolution
  | completeSolution : IO String -> IO String -> DaySolution

end DaySolution
