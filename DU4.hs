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

module DU4
where

import Relations
import RelatedFunctions

-- ukolem je matice 4x4
rozmer :: Integer
rozmer = 4 

-- ze vsech matic vyfiltruji ty, co popisuji relaci a z nich ty, kde 3 a 2 nejsou srovnatelné
uplny_vysledek = vysledek $ relace $ matice rozmer

-- matice, kde 3 a 2 nejsou v relaci
vysledek mat' = filter fce mat'
  where
    fce m = ((sloupec 3) . (radek 2)) m == False && ((sloupec 2) . (radek 3)) m == False

-- relace usporadani je taková matice, která je reflexivni, tranzitivni, ...
relace mat' = filter fce mat'
  where
    -- to prokazdy je muj pokus o cyklus foreach na všechny prvky v matici ;-)
    fce m = reflexivni m && prokazdy rozmer (tranzitivni rozmer) m && prokazdy rozmer antisymetricka m
