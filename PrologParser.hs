module PrologParser where

parse [] = []
parse (x:xs) = x: parse xs