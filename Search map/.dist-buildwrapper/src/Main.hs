module Main where
import Debug.Trace
import Data.Char (isSpace)


skip :: String -> String
skip = f . f
   where f = reverse . dropWhile isSpace

main = do
    content <- readFile("map4.txt")
    let temp = lines content
    let numberoflines = map skip temp     
    let rowlength = length (numberoflines !! 0)
    
    if (all (\x->(length x)==rowlength) numberoflines) 
        then do
        putStrLn "here is my challenge"
        putStrLn content
        let _lines = lines content
        let (pointX,pointY) = (0,0)
        putStrLn "\nThis is my challenge: \n"
        putStrLn content
        putStrLn ""
        let _lines = lines content
        let (pointX,pointY) = (0,0)
        let _answer=travel((pointX,pointY),_lines)
        if fst(_answer) then putStrLn "Woo hoo, I found the treasure :-) \n" else putStrLn "Uh oh, I could not find the treasure :-( \n"
        putStrLn $ unlines (snd(_answer))
        else
        putStrLn "Maze is not in correct format. Exiting..."


travelup :: (Int,Int) -> (Int,Int)
travelup (x,y) = (x-1,y)

travelleft :: (Int,Int) -> (Int,Int)
travelleft (x,y) = (x,y+1)

traveldown :: (Int,Int) -> (Int,Int)
traveldown (x,y) = (x+1,y)

travelright :: (Int,Int) -> (Int,Int)
travelright (x,y) = (x,y-1)


track :: (Int,Int,Char,[String]) -> [String]
track (pointX,pointY,char,_lines) = (take pointX _lines) ++((change(pointY,char,(_lines!!pointX))) : (drop (pointX + 1) _lines))


change :: (Int,Char,String) -> String
change (pointY,char,mapLine) = take (pointY) mapLine ++ [char] ++ drop (pointY + 1) mapLine

check :: (Int,Int,[String]) -> Bool
check (pointX,pointY,_lines)
        | pointX < length (_lines) && pointY < length (_lines !! 0) && pointX >= 0 && pointY >= 0 = True
        | otherwise = False


travel :: ((Int,Int),[String]) -> (Bool,[String])
travel ((pointX,pointY),_lines)
        | not(check(pointX,pointY,_lines)) = (False,_lines)
        | _lines !! pointX !! pointY == '#' = (False,_lines)
        | _lines !! pointX !! pointY == '!' = (False,_lines)
        | _lines !! pointX !! pointY == '+' = (False,_lines)
        | _lines !! pointX !! pointY == '@' = (True,_lines)
        | fst(up) = up
        | fst(left) = left
        | fst(down) = down
        | fst(right) = right
        | not(fst(right)) = (False,snd(travel(travelup(pointX,pointY),snd(travel(travelleft(pointX,pointY),snd(travel(traveldown(pointX,pointY),track(pointX,pointY,'!',snd(right)))))))))
            where
                up = travel(travelup(pointX,pointY),track(pointX,pointY,'+',_lines))
                left = travel(travelleft(pointX,pointY),track(pointX,pointY,'+',snd(up)))
                down = travel(traveldown(pointX,pointY),track(pointX,pointY,'+',snd(left)))
                right = travel(travelright(pointX,pointY),track(pointX,pointY,'+',snd(down)))