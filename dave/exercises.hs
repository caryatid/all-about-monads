--
--
-- all about monads exercises

-- everything you need to know about sheep
import Control.Monad

data Sheep = Sheep {
    name::String
    , mother::Maybe Sheep
    , father::Maybe Sheep
    --  , parents::(Sheep -> (Maybe Sheep,Maybe Sheep))
    }

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
--   |TODO|  Exe 01
-- rewrite the maternalGrandfather shiate without do
--  dumb


--  +|.............// 62aff31e-0f81-476e-a18d-e1151ffafe68 //---|}}}

--  +|...........................................// Exe 02 //---|{{{
--   |TODO|  Exe 02

parent :: Sheep -> Maybe Sheep
parent s = mother s `mplus` father s
grandparent :: Sheep -> Maybe Sheep
grandparent = undefined

--  +|.............// f05985a5-e36a-43ca-aa81-f9e96030b4d6 //---|}}}
