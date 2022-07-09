--SumTypes.hs

module SumTypes where

data BigSmall = Big Bool | Small Bool deriving (Eq, Show)

data Person = Person { name :: String
                     , age :: Bool }
                     deriving (Eq, Show)

-- data Fiction = Fiction deriving Show

-- data NonFiction = NonFiction deriving Show

-- data BookType = FictionBook Fiction | NonFictionBook NonFiction deriving Show

-- type AuthorName = String

-- data Author = Author (AuthorName, BookType)

-- data Author' = Fiction AuthorName | NonFiction AuthorName

-- other exercises

newtype NumCow = NumCow Int deriving (Eq, Show)

newtype NumPig = NumPig Int deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

newtype NumSheep = Int deriving (Eq, Show)

data BigFarmhouse = BigFarmhouse NumCow NumPig NumSheep

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep)


type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo = CowInfo Name Age deriving (Eq, Show)

data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)

data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)

data Animal = Cow CowInfo | Pig PigInfo | Sheep SheepInfo deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)