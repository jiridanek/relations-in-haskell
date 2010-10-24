
import IO
import List    
import Graphics.HGL

import Maybe
import Data.Function(on)
import Debug.Trace(trace)

import DU4

{--




Based on code by Martin Žák -- hope you don't mind
--}

data Tree a = Null
            | Node {value :: a,
                    next :: [Tree a]
                   } deriving (Eq, Ord, Show, Read)

-- main = runGraphics $ do
--		let v = uplny_vysledek 			
--		w <- openWindowEx "Relace" Nothing (windowX,windowY) DoubleBuffered (Just 1000) --open window
--		menu w v

{--
It ignores edges leading from an element to the same element (reflexive relation)

This function is suitable for use with cyklusPresMatici
--}
isItEdge :: [[Bool]] -> (Int, Int) -> Maybe (Int, Int)
isItEdge matrix (i,j) = if ((sloupec j) . (radek i)) matrix && j /= i
                           then Just (i,j)
                           else Nothing

{--
n - dimension
Returns Just edges and filters Nothings
--}
getEdges n matrix = catMaybes $ cyklusPresMatici n isItEdge matrix

{--
Returns roots of the hasse diagram and list of non roots
--}
getRootsInHasseDiagram :: Int -> [(Int, Int)] -> ([Int],[Int])
getRootsInHasseDiagram n edges = ( [1..n] \\ not_roots , nub not_roots)
  where not_roots = map snd edges

{--

--}
baseHasseDiagram roots = map (\n -> Node {value = n, next = []}) roots

{--
List of nodes from 'hrany' that are in relation with 'from'
--}
pathsFrom from hrany = map snd ( filter ((==from) . fst) hrany)

{--
We know there is no going back (no loops)
--}
pathsFromToEnd :: Int -> [(Int, Int)] -> [[Int]] -> [[Int]]
pathsFromToEnd from hrany acc
  | paths == [] = addToAll from acc
  | otherwise = foldr1 (++) otaznik  --error $ show otaznik
  where
    otaznik = map (\n -> pathsFromToEnd n hrany (addToAll from acc)) paths
    paths = pathsFrom from hrany
    addToAll :: a -> [[a]] -> [[a]]
    addToAll what to = map (++[what]) to

{--
Groups path by destination, then takes the longest paths possible for each destination

The short path means we are taking shortcut, We do not want any shortcuts in Hasse diagram
--}
dropAllShorter :: (Ord a) => [[a]] -> [[[a]]]
dropAllShorter paths = map ((!!0) . groupByLength) (groupByLast paths)
  where
    groupByLast = groupBy ((==) `on` last)
    groupByLength = groupBy ((==) `on` length)

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
