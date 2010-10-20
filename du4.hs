import Data.List
import IO
import List    
import Graphics.HGL

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
radek :: (Num t) => t -> [a] -> a
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

main2 = do
  let v = uplny_vysledek
  putStrLn ("Celkem matic: " ++ show ( length  v))
  let retezce = map debugMatice v
  putStr $ vypis v
  return ()

{--
Code by Martin Žák
--}

main = runGraphics $ do
		let v = uplny_vysledek 			
		w <- openWindowEx "Relace" Nothing (windowX,windowY) DoubleBuffered (Just 1000) --open window
		menu w v


 -- help function (you can see all again and again)
menu :: Window -> [[[Bool]]] -> IO b
menu w v = do
	drawMbyM w v 1	
	menu w v

 -- draw matrix by matrix
drawMbyM :: (Num a) => Window -> [[[Bool]]] -> a -> IO ()
drawMbyM w [] _ = return ()
drawMbyM w (x:xs) n = do
		clearWindow w
		drawInWindow w $ withTextAlignment (Left',Top) $ withTextColor (RGB 180 255 180) $ text (0, 0) ((show n) ++ " -> " ++ debugMatice x)

		let len = (length x)
		drawRow w x (zip (map (getCirclePosX len) [0..(len-1)]) (map (getCirclePosY len) [0..(len-1)]))

		p <- wGetChar w --just wait for key press

		drawMbyM w xs (n+1)

 -- draw row
drawRow :: Window -> [[Bool]] -> [(Int, Int)] -> IO ()
drawRow _ [] _ = return ()
drawRow w (x:xs) (y:ys) = do
		let len = (length ys) - (length xs)
		let act = (y:ys) !! len

		drawInWindow w $ withTextAlignment (Left',Top) $ withTextColor (RGB 180 255 180) (text ((fst act),(snd act)) (show (len + 1)))
		drawRelations w x act (y:ys)

		drawRow w xs (y:ys)

 -- draw arrows when is in R
drawRelations :: Window -> [Bool] -> (Int, Int) -> [(Int, Int)] -> IO ()
drawRelations _ [] _ _ = return ()
drawRelations w (x:xs) (pX,pY) (y:ys) = do
		
		let elX = fst y + ((pX - (fst y)) `div` 5)
		let elY = snd y + ((pY - (snd y)) `div` 5)

		case x of
		   True -> do 
			drawInWindow w $ line (pX,pY) y
			drawInWindow w $ ellipse (elX - 3,elY - 3) (elX + 3,elY + 3)
			drawRelations w xs (pX,pY) ys								
		   False -> drawRelations w xs (pX,pY) (ys)
						
 -- just get Location from unit circle
getCirclePosX :: Int -> Int -> Int
getCirclePosX t p = fromInteger (truncate $ (cos $ 2 * pi * (fromIntegral p) / (fromIntegral t)) * 100) + 120

getCirclePosY :: Int -> Int -> Int
getCirclePosY t p = fromInteger (truncate $ (sin $ 2 * pi * (fromIntegral p) / (fromIntegral t)) * 100) + 140


windowX :: Int
windowX = 400

windowY :: Int
windowY = 400

           
windowCenterH :: Int             
windowCenterH = (windowX) `div` 2

windowCenterV :: Int             
windowCenterV = (windowY) `div` 2

