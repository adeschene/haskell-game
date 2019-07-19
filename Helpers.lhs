> module Helpers where

> import Rooms     -- Rooms.lhs
> import Screens   -- Screens.lhs
> import GTypes    -- GTypes.lhs
> import Data.Maybe
> import Data.Char
> import Data.List
> import System.Console.ANSI -- Useful terminal manipulation functionality

In order to avoid runtime errors, I decided to implement a safer version of the !! operator
as the function getElem. Returns Just a if n is in bounds or Nothing if n is OOB.

> getElem                               :: Int -> [a] -> Maybe a
> getElem n xs | n >= 0 && n < length xs = Just (xs !! n)
>              | otherwise               = Nothing

We need to take the text entered by the user after the initial command, split it up into
individual words, search through for the FIRST relevant keyword (an item name, object name, etc.)
and then return that keyword, or Nothing if none of the arguments were meaningful.

> parseArgs  :: String -> String
> parseArgs s = let x = find (`elem` keywords) tailS in
>                   case x of
>                        Just x  -> x
>                        Nothing -> unwords tailS
>   where tailS = (tail . words) s

To maintain consistency when working with item names, we need to convert every item
name referenced to an all lowercase version of that name before doing anything with it.

> lowerStr  :: Item -> Item
> lowerStr i = [toLower x | x <- i]

Defining a function to check whether an item is present in the current room
allows us to more strictly define individual rooms and their boundaries.

> isInRoom :: Item -> Room -> Bool
> isInRoom i r | i `elem` currRoom = True
>              | otherwise         = False
>                  where currRoom = case r of
>                                        "Porch"    -> pItems++pObjs
>                                        "Living"   -> lrItems++lrObjs
>                                        "Bathroom" -> brItems++brObjs
>                                        "Hallway"  -> hwItems++hwObjs
>                                        "Closet"   -> clItems++clObjs
>                                        "Backyard" -> byItems++byObjs
>                                        "Basement" -> bmItems++bmObjs
>                                        _          -> []

We need to be able to determine which rooms are accessible from a given room,
so that we can control the player's movement in a realistic way.

> isAdjTo     :: Room -> Room -> Maybe Int
> isAdjTo r r' = case find (==r') validRooms of
>                     Just e  -> findIndex (==e) roomKeywords
>                     Nothing -> Nothing
>   where validRooms = case r of
>                           "Porch"    -> pAdjRooms
>                           "Living"   -> lrAdjRooms
>                           "Bathroom" -> brAdjRooms
>                           "Hallway"  -> hwAdjRooms
>                           "Closet"   -> clAdjRooms
>                           "Backyard" -> byAdjRooms
>                           "Basement" -> bmAdjRooms
>                           _          -> []

Helpers for finding correct room when player attempts to move based on direction
instead of by room name.

> findFwd    :: Room -> Argument -> Room
> findFwd r d = case r of
>                 "Porch"   -> "living"
>                 "Living"  -> "hallway"
>                 "Hallway" -> "backyard"
>                 _         -> "BAD_ROOM"

> findBwd    :: Room -> Argument -> Room
> findBwd r d = case r of
>                 "Living"   -> "porch"
>                 "Bathroom" -> "hallway"
>                 "Hallway"  -> "living"
>                 "Closet"   -> "living"
>                 "Backyard" -> "hallway"
>                 "Basement" -> "hallway"
>                 _          -> "BAD_ROOM"

> findRgt    :: Room -> Argument -> Room
> findRgt r d = case r of
>                 "Living"  -> "closet"
>                 "Hallway" -> "basement"
>                 _         -> "BAD_ROOM"

> findLft    :: Room -> Argument -> Room
> findLft r d = case r of
>                 "Hallway" -> "bathroom"
>                 _         -> "BAD_ROOM"

Find the list of items in a given room and return it for when the player wants to
"take all" in a room.

> findAllItems  :: Room -> [Item]
> findAllItems r = case r of
>               "Porch"    -> pItems
>               "Living"   -> lrItems
>               "Bathroom" -> brItems
>               "Hallway"  -> hwItems
>               "Closet"   -> clItems
>               "Backyard" -> byItems
>               "Basement" -> bmItems

Clears the screen and draws a new screen for whatever room or scene version the player is in.
Some rooms have different screen versions to correspond to what the player has done in the room.

> loadScreen     :: Room -> [Item] -> IO ()
> loadScreen r is = do clearScreen
>                      case r of
>                        "Menu"     -> putStr mainMenuS
>                        "Porch"    -> if "mat" `elem` is
>                                        then if "key" `elem` is
>                                          then putStr porchS3
>                                        else putStr porchS2
>                                      else putStr porchS1
>                        "Living"   -> if "painting" `elem` is
>                                        then if "vase" `elem` is
>                                               then putStr livingRmS4
>                                             else putStr livingRmS2
>                                        else if "vase" `elem` is
>                                            then putStr livingRmS3
>                                      else putStr livingRmS1
>                        "Hallway"  -> putStr hallwayS
>                        "Bathroom" -> if "paper" `elem` is
>                                        then putStr bathroomS2
>                                      else putStr bathroomS1
>                        "Closet"   -> if "shoebox" `elem` is
>                                        then putStr closetS2
>                                      else putStr closetS1
>                        "Basement" -> if "cup" `elem` is
>                                        then if "bigbox" `elem` is
>                                               then if "shovel" `elem` is
>                                                      then if "boots" `elem` is
>                                                             then putStr basementS10
>                                                           else putStr basementS4
>                                                    else if "boots" `elem` is
>                                                           then putStr basementS9
>                                                         else putStr basementS3
>                                             else if "boots" `elem` is
>                                                    then putStr basementS8
>                                                  else putStr basementS2
>                                      else if "boots" `elem` is
>                                             then if "bigbox" `elem` is
>                                                    then if "shovel" `elem` is
>                                                           then putStr basementS7
>                                                         else putStr basementS6
>                                                  else putStr basementS5
>                                           else if "bigbox" `elem` is
>                                                  then if "shovel" `elem` is
>                                                         then putStr basementS12
>                                                       else putStr basementS11
>                                                else putStr basementS1
>                        "Backyard" -> putStr backyardS
>                        "Quit"     -> putStr quitS
>                        _          -> putStr drawLine

