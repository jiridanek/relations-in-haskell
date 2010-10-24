module Main (main)
where

import IO
import List    
import Graphics.HGL

import DU4

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
