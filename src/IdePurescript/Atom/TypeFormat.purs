module IdePurescript.Atom.TypeFormat where

import Data.Maybe (Maybe(..), fromJust)
import Prelude (map, const, (==), (<>), (-), (+), (<), (&&), (>=), (>>>), (>), ($), (#))
import Data.Array as A
import Data.Array (uncons, snoc, cons, zipWith)
import Data.Char (toUpper, toCharCode)
import Data.String (drop, replace, toCharArray, fromCharArray, split, joinWith, length, take)
import Partial.Unsafe (unsafePartial)
-- DIRTY type formatting and parsing

-- wrapFirst s stop = case
temporaryFormat :: String -> String
temporaryFormat s
  = "<div style='text-align: left; word-wrap: break-word;'>"
  <> joinWith "<br>" (map beautify res)
  <> "</div>"

  where
    res :: Array String
    res = if (foralls <> constraints) == []
      then allTypes
      else cons (fromCharArray (foralls <> ['.',' '] <> constraints)) allTypes

    c1 = cutAfter (toCharArray ".") (toCharArray s)
    foralls = c1.pre
    wholeTpe = c1.fin
    c2 = cutAfter (toCharArray "=>") wholeTpe
    constraints = c2.pre
    allTypes :: Array String
    allTypes =  customSplit c2.fin
      # map fromCharArray
      # map limitLength
      # zipWith (\x y -> x <> y) (cons " =>" $ map (const " ->") c2.fin)

    limitLength s =
      if len > 150
      then (take 70 s) <> "  ...  " <> (drop (len - 70) s)
      else s
      where len = length s

-- isSubsequenceOf [1,2,3]  [1,2,3,4]
beautify :: String -> String
beautify str = joinWith " " words
  where
    words = map colorize (split " " str)
    escape = replace ">" "&gt;" >>> replace "<" "&lt;"
    writeIn :: String -> String -> String
    writeIn col str = "<span style='color:"<>col<>"'>"<>escape str<>"</span>"
    writeInBold col str = "<span style='color:"<>col<>"'><b>"<>escape str<>"</b></span>"
    colorize word = case word of
      "forall" -> writeIn "green" "fa"
      "=>" -> writeIn "green" "=>"
      "->" -> writeIn "green" "->"
      _ -> case uncons (toCharArray word) of
        Nothing -> word
        Just a ->
          if toCharCode (a.head) >= toCharCode 'A' && toCharCode (a.head) < toCharCode 'Z'
          then
            if fromCharArray (map toUpper (toCharArray word)) == word
            then writeInBold "orange" word
            else writeIn "blue" word
          else word

customSplit :: Array Char -> Array (Array Char)
customSplit l = if res.pre == [] then [res.fin] else cons res.pre (customSplit res.fin)
  where res = cutAfter (toCharArray "->") l

cutAfter :: Array Char -> Array Char -> {pre:: Array Char, fin:: Array Char}
cutAfter l1 l2 = go 0 [] l1 l2
  where
    go i pre [] fin =
      if i == 0
      then {
        pre: A.take (A.length pre - A.length l1) pre,
        fin: fin
      }
      else go i pre l1 fin
    go _ pre _  []  = {pre: [] , fin: l2}
    go i pre pat arr =
      if pat'.head == arr'.head
        then go nexti keep pat'.tail arr'.tail
        else go nexti keep pat       arr'.tail
      where
        nexti = case arr'.head of
         '{' -> i + 1
         '(' -> i + 1
         ')' -> i - 1
         '}' -> i - 1
         _   -> i
        keep = case arr'.head of
         '{' -> pre <> [' ', '{', ' ']
         '(' -> pre <> [' ', '(', ' ']
         ')' -> pre <> [' ', ')', ' ']
         '}' -> pre <> [' ', '}', ' ']
         _   -> snoc pre arr'.head

        pat' = unsafePartial (fromJust (uncons pat))
        arr' = unsafePartial (fromJust (uncons arr))
