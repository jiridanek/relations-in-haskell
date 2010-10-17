import Data.List

-- ukolem je matice 4x4
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
    fce m = reflexivni m && prokazdy rozmer tranzitivni m && prokazdy rozmer antisymetricka m

main = do
  let v = uplny_vysledek
  putStrLn ("Celkem matic: " ++ show ( length  v))
  let retezce = map debugMatice v
  putStr $ vypis v
  return ()

-- vytvori n-prvkový vektor a udělá z něj sqrt(n)-prvkovou čtvercovou matici
matice n = zmaticuj n $ sequence $ ple (n*n)

--
zmaticuj :: Int -> [[Bool]] -> [[[Bool]]]
zmaticuj n m = map (chunk n) m

--rozdělí seznam na pole n-prvkových seznamů
chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = ys : chunk n zs
  where (ys,zs) = splitAt n xs

-- vygeneruje vsechny moznosti jak udělat n-prvkový vektor 
vektor :: Integer -> [[Bool]]
vektor 1 = [[True], [False]]
vektor n = map fce (vektor (n-1)) ++ map (++[False]) (vektor (n-1))
  where
    fce = (++[True])

-- to stejné, jen kratšeji
ple :: Int -> [[Bool]]
ple 1 = [[True,False]]
ple n = [True,False] : (ple (n-1)) 


--    
reflexivni [[True]] = True
reflexivni [[False]] = False
reflexivni (b:xb) = head b && reflexivni (map tail xb)

tranzitivni :: [[Bool]] -> (Int, Int) -> Bool
tranzitivni matice (i, j)
  | i == j = True
  | a == True = all (==True) b
  | otherwise = True
    where
      a = ((sloupec j) . (radek i)) matice  
      g = radek j matice
      b = map fce [1..rozmer]
        where fce r
                | sloupec r g == True = ((sloupec r) . (radek i)) matice
                | otherwise = True

antisymetricka :: [[Bool]] -> (Int, Int) -> Bool
antisymetricka matice (i, j)
  | i == j = True
  | a == True && b == True = False
  | otherwise = True
    where
        a = ((sloupec j) . (radek i)) matice  
        b = ((sloupec i) . (radek j)) matice

-- vybrat radek matice a sloupec. Použije se to ((sloupec 2) . (radek 3)) matice
radek 1 mat = head mat
radek m mat = radek (m-1) (tail mat)

sloupec = radek

-- pro všechny i,j od jedničky do n to zavolá funkci a ta funkce musi po kazdy vratit True.
prokazdy :: Int -> ([[Bool]] -> (Int, Int) -> Bool) -> [[Bool]] -> Bool
prokazdy n funkce matice = all (==True) $ map (funkce matice)  [(i,j) | i <- [1..n], j <- [1..n]]

ocisluj retezec cislo = show cislo ++ ")\n" ++ retezec

vypis s = unlines (zipWith ocisluj vysle ([1..(length vysle)]))
  where
    vysle = map debugMatice s

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