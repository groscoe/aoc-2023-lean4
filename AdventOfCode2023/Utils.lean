import Lean.Data.Parsec
import Std.Data.Array.Basic

open Lean.Parsec

namespace Utils

def void [Monad m] (action : m a) : m Unit := action *> pure ()

def sepBy1 (sep : Lean.Parsec a) (p: Lean.Parsec b) : Lean.Parsec (Array b) := do
  let first <- p
  let rest <- Lean.Parsec.many (sep *> p)
  pure $ Array.insertAt rest 0 first

def nat : Lean.Parsec Nat := do
  let digits <- Lean.Parsec.many1 Lean.Parsec.digit
  let toNat (c : Char) := Char.toNat c - 48
  pure $ Array.foldl (fun acc cur => 10*acc + toNat cur) 0 digits

def spaces : Lean.Parsec Unit := void $ many (pchar ' ')

def token (p : Lean.Parsec a) : Lean.Parsec a := p <* spaces

def keyword (w : String) : Lean.Parsec Unit := void $ token $ pstring w

def sum [Add α] [OfNat α 0] (xs: Array α) : α := Array.foldl (· + ·) 0 xs

def arrayMin! [Ord α] [Inhabited α] (arr : Array α) : α := arr.min?.get!

end Utils
