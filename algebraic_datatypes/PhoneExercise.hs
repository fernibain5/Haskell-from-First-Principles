-- 11-exercise-phone.hs

module ExercisePhone where

import Data.Char
import Data.Maybe
import Data.List

type Key = Char
type Presses = Int

daPhone :: [String]
daPhone = ["abc2","def3","ghi4","jkl5",
           "mno6","pqrs7","tuv8","wxyz9",
           "+ 0",".,#"]

test :: String
test = "Hello world"

convo :: [String]
convo =
    ["Wanna play 20 questions",
     "Ya",
     "U 1st haha",
     "Lol ok. Have u ever tasted alcohol lol",
     "Lol ya",
     "Wow ur cool haha. Ur turn",
     "Ok. Do u think I am pretty Lol",
     "Lol ya",
     "Haha thanks just making sure rofl ur turn"]

reverseTaps :: String -> [Maybe (Key, Presses)]
reverseTaps [] = []
reverseTaps (x:xs)
  | isUpper x = Just ('*', 1) : reverseTap (toLower x) : reverseTaps xs
  | otherwise = reverseTap x : reverseTaps xs

fingerTaps :: [(Key, Presses)] -> Presses
fingerTaps =  sum . (map snd)

tapsInConvo :: [[(Key, Presses)]]
tapsInConvo = map (catMaybes . reverseTaps) convo

fingerTapsInConvo :: [Presses]
fingerTapsInConvo = map fingerTaps tapsInConvo


-- Helper Functions  

reverseTap :: Char -> Maybe (Key, Presses)
reverseTap x = foldr (\h g -> getJustVal (isCharInString x h) g ) Nothing daPhone

getJustVal :: Maybe a -> Maybe a -> Maybe a
getJustVal (Just x) _ = Just x
getJustVal _ x = x

isCharInString :: Char -> String -> Maybe (Key, Presses)
isCharInString x s = case elemIndex x s of  Just i -> Just (last s, i + 1)
                                            Nothing -> Nothing
-- fingerTap :: [Maybe (Key, Presses)] -> Presses
-- fingerTap [] = 0
-- fingerTap (Nothing : xs) = fingerTap xs
-- fingerTap (Just (_, num):xs) = num + fingerTap xs


                  


  
