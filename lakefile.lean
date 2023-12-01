import Lake
open Lake DSL

package «advent-of-code-2023» where
  -- add package configuration options here

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"

lean_lib «AdventOfCode2023» where
  -- add library configuration options here

@[default_target]
lean_exe «advent-of-code-2023» where
  root := `Main
  -- Enables the use of the Lean interpreter by the executable (e.g.,
  -- `runFrontend`) at the expense of increased binary size on Linux.
  -- Remove this line if you do not need such functionality.
  supportInterpreter := true
