module Prices where

data PriceTag = Item String Double

--items--
apple::PriceTag
apple = Item "Apple" 2.50

--functions--
showPriceTag :: PriceTag -> String
showPriceTag (Item n price) =
    n ++ " -- " ++ show price


addVAT :: PriceTag -> PriceTag
addVAT (Item nm price) = Item nm (1.2*price)