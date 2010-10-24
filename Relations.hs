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

module Relations where

import Data.List(transpose)
import RelatedFunctions

{--
--- Creates all possible reflexive and symetric square matrices of a given size
---
--- that means putting together the top, ones and the bottom,
--- which is reformated top, because of symetry
 _______
|1|_ top|
| |1|_  |
|   |1|_|
|botto|1| m

 _______
|1|a|b|c|
|a|1|d|e|
|b|d|1|f|
|c|e|f|1| 

--}

all_reflexive_symetric_matrices :: Integer -> [[[Bool]]]
all_reflexive_symetric_matrices n = map (new_reflexive_symetric_matrix_from_vector n) vectors
  where
    vectors = sequence $ ple vector_length
    -- the result of the division is supposed to be integer
    vector_length = ((n)*(n) - (n)) `div` 2

   
{--
--- The heart of the stuff.
--- new_reflexive_symetric_matrix_from_vector 3 [True,False,True]
> [[True,True,False],
>  [True,True,True],
>  [False,True,True]]
--}
new_reflexive_symetric_matrix_from_vector :: Integer -> [Bool] -> [[Bool]]
new_reflexive_symetric_matrix_from_vector n vector = combine bottom_with_padding top_with_padding
  where
    bottom = groupTakingLess (n-1) vector
    -- discovered by throughtful experimentation :)
    --Example:
    -- reverse $ transpose $ map reverse ["abc","de","f"]
    -- > ["a","bd","cef"]
    bottom_converted_into_rows = reverse $ transpose $ map reverse bottom
    bottom_with_last_row_ended_with_one = (init bottom_converted_into_rows) ++ (appendToAll True [last bottom_converted_into_rows])
    bottom_with_padding = [[]] ++ bottom_with_last_row_ended_with_one
    
    top = groupTakingLess (n-1) vector
    top_with_leading_ones = prependToAll True top
    top_with_padding = top_with_leading_ones ++ [[]]
    
    combine bottom top = zipWith (++) bottom top
    

-- vytvori n-prvkový vektor a udělá z něj sqrt(n)-prvkovou čtvercovou matici
matice :: Integer -> [[[Bool]]]
matice n = zmaticuj n $ sequence $ ple (n*n)

--
zmaticuj :: Integer -> [[Bool]] -> [[[Bool]]]
zmaticuj n m = map (chunk n) m

-- vygeneruje vsechny moznosti jak udělat n-prvkový vektor 
vektor :: Integer -> [[Bool]]
vektor 1 = [[True], [False]]
vektor n = map fce (vektor (n-1)) ++ map (++[False]) (vektor (n-1))
  where
    fce = (++[True])

-- to stejné, jen kratšeji
ple :: Integer -> [[Bool]]
ple 1 = [[True,False]]
ple n = [True,False] : (ple (n-1)) 


--    
reflexivni [[True]] = True
reflexivni [[False]] = False
reflexivni (b:xb) = head b && reflexivni (map tail xb)

tranzitivni :: Integer -> [[Bool]] -> (Integer, Integer) -> Bool
tranzitivni n matice (i, j)
  | i == j = True
  | a == True = all (==True) b
  | otherwise = True
    where
      a = ((sloupec j) . (radek i)) matice  
      g = radek j matice
      b = map fce [1..n]
        where fce r
                | sloupec r g == True = ((sloupec r) . (radek i)) matice
                | otherwise = True

antisymetricka :: [[Bool]] -> (Integer, Integer) -> Bool
antisymetricka matice (i, j)
  | i == j = True
  | a == True && b == True = False
  | otherwise = True
    where
        a = ((sloupec j) . (radek i)) matice  
        b = ((sloupec i) . (radek j)) matice
