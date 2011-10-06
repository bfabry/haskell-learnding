module Prettify where

data Doc = undefined

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []      = []
punctuate p [d]     = [d]
punctuate p (d:ds)  = (d <> p) : punctuate p ds

import Prettify (Doc, (<>), char, double, fsep, hcat, punctuate, text,
                 compact, pretty)

(<>) :: Doc -> Doc -> Doc
(<>) = undefined

char :: Char -> Doc
char = undefined

double :: Double -> Doc
double = undefined

fsep :: [Doc] -> Doc
fsep = undefined

hcat :: [Char] -> Doc
hcat = undefined

punctuate :: Char -> [Doc] -> Doc
punctuate = undefined

text :: String -> Doc
text = undefined

compact :: Doc
compact = undefined

pretty :: Doc -> String
pretty = undefined
