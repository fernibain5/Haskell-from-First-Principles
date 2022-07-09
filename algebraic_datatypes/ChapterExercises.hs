-- ChapterExercises.hs

import Data.Char

data Weekday =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

isSubseqOf :: (Eq a)
           => [a]
           -> [a]
           -> Bool       
isSubseqOf [] _ = True
isSubseqOf _ [] = False      
isSubseqOf (x:xs) (y:ys)
  | x == y = isSubseqOf xs ys
  | otherwise = isSubseqOf (x:xs) ys

capitalizeWords :: String
                -> [(String, String)]
capitalizeWords [] = []                
capitalizeWords s = let word = takeWhile (/= ' ') $ dropWhile (== ' ') s
                        (_, rest) = splitAt (length word) s
                    in ((toUpper (head word) : tail word), (toLower (head word) : tail word)) : capitalizeWords (dropWhile (== ' ') rest)

capitalizeWord :: String -> String
capitalizeWord w = toUpper (head w) : map toLower (tail w)

capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph p = let sentence = takeWhile (/= '.') p
                            (senWithPoint, rest) = splitAt (length sentence + 1) p
                        in (takeWhile (== ' ') senWithPoint ++ capitalizeWord (dropWhile (== ' ') senWithPoint)) ++ capitalizeParagraph rest

-- Phone exercise

data DaPhone = DaPhone [String]

convo :: [String]
convo =
  ["Wanna play 20 questions",
   "Ya",
   "U 1st haha",
   "Lol ok. Have u ever tasted alcohol",
   "Lol ya",
   "Wow, ur cool haha. Ur turn",
   "Ok. Do u think I am pretty Lol",
   "Lol ya",
   "Just making sure rofl ur turn"]

-- validButtons = "12345678*#"
type Digit = Char

-- valid presses: 1 and up
type Presses = Int





