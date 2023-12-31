import Lean.Data.Parsec
import Std.Data.Array.Basic

open Lean.Parsec

namespace Utils

def void [Monad m] (action : m a) : m Unit := action *> pure ()

def sepBy1 (sep : Lean.Parsec a) (p: Lean.Parsec b) : Lean.Parsec (Array b) := do
  let first <- p
  let rest <- Lean.Parsec.many (sep *> p)
  pure $ Array.insertAt rest 0 first

def digitsToNat (digits : Array Char) : Nat :=
  let toNat (c : Char) := Char.toNat c - 48
  Array.foldl (fun acc cur => 10*acc + toNat cur) 0 digits

def nat : Lean.Parsec Nat := do
  let digits <- Lean.Parsec.many1 Lean.Parsec.digit
  pure $ digitsToNat digits

def int : Lean.Parsec Int :=
  (pchar '-' *> Int.neg <$> nat)
  <|> ↑nat

def lineEnd : Lean.Parsec Unit := skipChar '\n' <|> eof

def spaces : Lean.Parsec Unit := void $ many (pchar ' ')

def token (p : Lean.Parsec a) : Lean.Parsec a := p <* spaces

def keyword (w : String) : Lean.Parsec Unit := void $ token $ pstring w

def sum [Add α] [OfNat α 0] (xs: Array α) : α := Array.foldl (· + ·) 0 xs

def prod [Mul α] [OfNat α 1] (xs: Array α) : α := Array.foldl (· * ·) 1 xs

def arrayMin! [Ord α] [Inhabited α] (arr : Array α) : α := arr.min?.get!

def compareLists [Ord α] : List α -> List α -> Ordering
| x::xs, y::ys => match compare x y with
  | Ordering.eq => compareLists xs ys
  | ord => ord
| [], [] => Ordering.eq
| [], _ => Ordering.lt
| _, [] => Ordering.gt

instance ordList [Ord α] : Ord (List α) where compare := compareLists

end Utils
