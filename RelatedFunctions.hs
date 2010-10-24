{------------------------------------------------------------------------------
Copyright (c) 2010, Jiří Daněk
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Jiří Daněk nor the
      names of his contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL JIŘÍ DANĚK BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
----------------------------------------------------------------------------------}

module RelatedFunctions where

--rozdělí seznam na pole n-prvkových seznamů
chunk :: Integer -> [a] -> [[a]]
chunk n [] = []
chunk n xs = ys : chunk n zs
  where (ys,zs) = splitAt (fromInteger n :: Int) xs

-- vybrat radek matice a sloupec. Použije se to ((sloupec 2) . (radek 3)) matice
radek :: (Num t) => t -> [a] -> a
radek 1 mat = head mat
radek m mat = radek (m-1) (tail mat)

sloupec = radek

-- pro všechny i,j od jedničky do n to zavolá funkci a ta funkce musi po kazdy vratit True.
prokazdy :: Integer -> ([[Bool]] -> (Integer, Integer) -> Bool) -> [[Bool]] -> Bool
prokazdy n funkce matice = all (==True) $ cyklusPresMatici n funkce matice

{--
n :: Int - dimension of the square matrix
funkce :: [[Bool]] -> (Int, Int) -> a - funcion called
matice :: [[Bool]] - matrix given to the function
result - list of values returned by function
--}
cyklusPresMatici :: Integer -> ([[Bool]] -> (Integer, Integer) -> a) -> [[Bool]] -> [a]
cyklusPresMatici n funkce matice = map (funkce matice)  [(i,j) | i <- [1..n], j <- [1..n]]

debugMatice :: [[Bool]] -> String
debugMatice matice = foldl radky "" matice
  where
--    radky :: [Bool] -> [String] -> [String]
    radky acc radek = acc ++ (foldl prvky "" radek) ++ "\n"

      where
        prvky :: String -> Bool -> String
        prvky acc prvek
          | prvek == True = acc ++ "1 " 
          | prvek == False = acc ++ "0 "
{--
--- Append a list to the beginning of all lists in a given list
--}
appendToAll :: a -> [[a]] -> [[a]]
appendToAll what to_what = map (++[what]) to_what


prependToAll :: a -> [[a]] -> [[a]]
prependToAll what to_what = map ([what]++) to_what

{--
--- Splits a list into a list of lists of increasing lengths (from len to as far as it goes)

groupTakingMore 1 [1,2,3,4,5,6,7,8,9,10]
> [[1],[2,3],[4,5,6],[7,8,9,10]]
--}
groupTakingMore :: Integer -> [a] -> [[a]]
groupTakingMore _ [] = []
groupTakingMore len list = [take len_as_int list] ++ (groupTakingMore (len+1) (drop len_as_int list))
  where
    len_as_int = fromInteger len :: Int

groupTakingLess :: Integer -> [a] -> [[a]]
groupTakingLess _ [] = []
groupTakingLess len list = [take len_as_int list] ++ (groupTakingLess (len-1) (drop len_as_int list))
  where
    len_as_int = fromInteger len :: Int
