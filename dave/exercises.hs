--
--
-- all about monads exercises

-- everything you need to know about sheep
import Control.Monad
import Data.Maybe

data Sheep = Sheep {
    name::String
    , mother::Maybe Sheep
    , father::Maybe Sheep
    --  , parents::(Sheep -> (Maybe Sheep,Maybe Sheep))
    }

--this builds our sheep family tree
breedSheep :: Sheep
breedSheep = let adam   = Sheep "Adam" Nothing Nothing
                 eve    = Sheep "Eve" Nothing Nothing
                 uranus = Sheep "Uranus" Nothing Nothing
                 gaea   = Sheep "Gaea" Nothing Nothing
                 kronos = Sheep "Kronos" (Just gaea) (Just uranus)
                 holly  = Sheep "Holly" (Just eve) (Just adam)
                 roger  = Sheep "Roger" (Just eve) (Just kronos)
                 molly  = Sheep "Molly" (Just holly) (Just roger)
          in Sheep "Dolly" (Just molly) Nothing
--  parent :: Sheep -> (Maybe Sheep, Maybe Sheep)
--  parent s = parents' s >>= (\(mum, pop) -> mum `mplus` pop) 

parents' :: Sheep -> (Maybe Sheep, Maybe Sheep)
parents' s = (mother s, father s)

-- we show sheep by name
instance Show Sheep where
  show s = show (name s)

-- we can also use do-notation to build complicated sequences
mothersPaternalGrandfather :: Sheep -> Maybe Sheep
mothersPaternalGrandfather s = do m  <- mother s
                                  gf <- father m
                                  father gf

--  +|...........................................// Exe 01 //---|{{{
--   |DONE|  Exe 01
-- rewrite the maternalGrandfather shiate without do
--  dumb


--  +|.............// 62aff31e-0f81-476e-a18d-e1151ffafe68 //---|}}}

--  +|...........................................// Exe 02 //---|{{{
--   |DONE|  Exe 02

parent :: Sheep -> Maybe Sheep
parent s = mother s `mplus` father s
grandparent :: Sheep -> Maybe Sheep
grandparent x = return x >>= parent >>= parent

--  +|.............// f05985a5-e36a-43ca-aa81-f9e96030b4d6 //---|}}}

--  +|...........................................// Exe 03 //---|{{{
 --  |DONE|  Exe 03
parents :: Sheep ->  [Sheep]
parents s = (maybeToList $ mother s) ++ (maybeToList $ father s)


--  +|.............// d4ecc861-f2f7-4865-a3af-b40d63088237 //---|}}}

--  +|...........................................// Exe 04 //---|{{{
 --  |TODO|  Exe 04

parentM :: (MonadPlus m) => Sheep -> m Sheep
parentM s = (maybeToMonad $ mother s) `mplus` (maybeToMonad $ father s)

maybeToMonad :: (MonadPlus m) => Maybe a -> m a
maybeToMonad Nothing = mzero
maybeToMonad (Just x) = return x

--  +|.............// 033d9f71-0298-4560-a700-a5a3e35141e6 //---|}}}
