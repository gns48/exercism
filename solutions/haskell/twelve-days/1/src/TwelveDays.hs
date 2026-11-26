module TwelveDays (recite) where

import Data.List (intercalate)

gift0 :: String
gift0 = " a Partridge in a Pear Tree."

gifts ::  [String]
gifts = [" two Turtle Doves",
         " three French Hens",
         " four Calling Birds",
         " five Gold Rings",
         " six Geese-a-Laying",
         " seven Swans-a-Swimming",
         " eight Maids-a-Milking",
         " nine Ladies Dancing",
         " ten Lords-a-Leaping",
         " eleven Pipers Piping",
         " twelve Drummers Drumming"]

days :: [String]
days = ["first", "second", "third", "fourth", "fifth", "sixth",
        "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"]

ontheday :: Int -> String
ontheday num = "On the " ++ days !! (num-1) ++ " day of Christmas my true love gave to me:"

giftlist :: Int -> String
giftlist 0 = gift0
giftlist num = (intercalate "," $ reverse $ take num gifts) ++ ", and" ++ gift0

num2text :: Int-> String
num2text num = (ontheday num) ++ (giftlist (num-1))

recite :: Int -> Int -> [String]
recite start stop =  map (\n -> num2text n) [start..stop]

