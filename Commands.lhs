> module Commands where

> import Screens   -- Screens.lhs
> import Rooms     -- Rooms.lhs
> import Save      -- Save.lhs
> import Helpers   -- Helpers.lhs
> import Screens   -- Screens.lhs
> import GTypes    -- GTypes.lhs
> import Output    -- Output.lhs
> import Data.Char (toLower,isSpace)
> import Data.Maybe
> import Data.List (nub)
> import System.IO
> import System.Console.Readline -- Rich terminal IO (binding of GNU library)
> import System.Console.ANSI -- Useful terminal manipulation functionality

The main game loop, prompts the user for input and passes it to the exeCmd function.
Also maintains history of user input that the player can recall with the arrow keys.

> play         :: GState -> Prompt -> IO ()
> play (is,r) p = do
>      loadScreen r is
>      l <- readline (p++"\n> ")
>      case l of
>        Nothing    -> play (is,r) noCommand   
>        Just ""    -> play (is,r) noCommand
>        Just input -> do
>          addHistory input -- Enables player to use up/down arrows to look at past input lines
>          exeCmd (is,r) input

The readCmd function isn't named very intuitively, because all it really does is take in a
command that was entered by the player, see if it's valid, and return it in all lowercase if
it is. If it isn't a valid command, we just return "BAD_CMD" to inform the calling function.

> readCmd  :: Maybe String -> String
> readCmd s | (isJust s) && lowCmd `elem` cmds = lowCmd
>           | otherwise                        = "BAD_CMD"
>               where lowCmd = map toLower (fromJust s)

Takes user input, grabs and passes the command to readCmd, then calls the function that
corresponds to the determined command, if appropriate.

> exeCmd :: GState -> Argument -> IO ()
> exeCmd (is,r) s =  case readCmd (getElem 0 (words s)) of
>                       c|c `elem` takeSyns     -> addItem (is,r) (parseArgs s)
>                       c|c `elem` dropSyns     -> remItem (is,r) (parseArgs s)
>                       c|c `elem` moveSyns     -> moveTo  (is,r) (parseArgs s)
>                       c|c `elem` examSyns     -> exam (is,r) (parseArgs s)
>                       c|c `elem` openSyns     -> openSsm (is,r) (parseArgs s)
>                       c|c `elem` saveSyns     -> do saveGame (is,r)
>                                                     play (is,r) gameSaved
>                       c|c `elem` loadSyns     ->
>                         case loadGame save of
>                              Just saveState -> play saveState "Game loaded! :)\n"
>                              Nothing        -> play (is,r) loadFail
>                       c|c `elem` whoaSyns     -> play (is,r) noViolence
>                       c|c `elem` helpSyns     -> helpDisp (is,r)
>                       c|c `elem` quitSyns     -> do setSGR []; loadScreen "Quit" []
>                       _ -> play (is,r) (fromMaybe "Nothing" (getElem 0 (words s))
>                                          ++" is not a valid command!\n")

In order to separate out the command validation process from the argument parsing, and
to declutter those functions a little, we can just create a big function that takes a
command and its validated arguments and figures out what to do with them:

> doCmd :: Command -> Argument -> GState -> IO ()
> doCmd c a (is,r) = case c of
>                      "add"   -> if a `elem` staticObjs
>                                   then play (is,r) addFail3
>                                 else play (a:is,r) (addSucc a)
>                      "drop"  -> case a of
>                                   "mat"            -> play (is,r) dropMat
>                                   "key"            -> play (is,r) dropKey
>                                   z|z `elem` boxes -> play (is,r) dropBox
>                                   _                -> play (filter (\x -> x /= a) is,r) (dropSucc a)
>                      "examObj"  -> case a of
>                                   "mat"      -> play (is,r) eMat
>                                   "key"      -> if isForbidden (is,r) "exam" "key"
>                                                   then play (is,r) examFail
>                                                 else play (is,r) eKey
>                                   "window"   -> if "painting" `elem` is
>                                                   then if "vase" `elem` is
>                                                          then displayWait (is,r) windowS4 eWindow
>                                                        else displayWait (is,r) windowS2 eWindow
>                                                 else if "vase" `elem` is
>                                                        then displayWait (is,r) windowS3 eWindow
>                                                      else displayWait (is,r) windowS1 eWindow
>                                   "lighter"  -> play (is,r) eLighter
>                                   "painting" -> displayWait (is,r) paintingS ePainting1
>                                   "vase"     -> play (is,r) eVase1
>                                   "door"     -> play (is,r) eDoor
>                                   "doors"    -> play (is,r) eDoors
>                                   "toilet"   -> play (is,r) eToilet
>                                   "sink"     -> play (is,r) eSink
>                                   "mirror"   -> play (is,r) eMirror
>                                   "shower"   -> play (is,r) eShower
>                                   "paper"    -> play (is,r) ePaper
>                                   "box"      -> if r == "Closet"
>                                                   then play (is,r) eShoeBox
>                                                 else play (is,r) eQBox
>                                   "clothes"  -> play (is,r) eClothes
>                                   "fence"    -> play (is,r) eFence
>                                   z|z `elem` dirtSyns -> if "shovel" `elem` is
>                                                            then digUp (is,r)
>                                                          else displayWait (is,r) dirtS1 eDirt
>                                   "cup"      -> play (is,r) eCup
>                                   "boots"    -> play (is,r) eBoots
>                                   "shovel"      -> if isForbidden (is,r) "exam" "shovel"
>                                                   then play (is,r) examFail
>                                                 else play (is,r) eShovel
>                                   _          -> play (is,r) (notExamable a)
>                      "examItem"  -> case a of
>                                    "mat"      -> play (is,r) eMat
>                                    "key"      -> play (is,r) eKey
>                                    "lighter"  -> play (is,r) eLighter
>                                    "painting" -> play (is,r) ePainting2
>                                    "vase"     -> play (is,r) eVase2
>                                    "paper"    -> play (is,r) ePaper
>                                    "shoebox"  -> play (is,r) eShoeBox
>                                    "bigbox"   -> play (is,r) eQBox
>                                    "cup"      -> play (is,r) eCup
>                                    "boots"    -> play (is,r) eBoots
>                                    "shovel"   -> play (is,r) eShovel
>                                    _          -> play (is,r) (notExamable a)
>                      "open"  -> case a of
>                                   "door" -> case r of
>                                               "Porch"       -> if isForbidden (is,r) "open" "door"
>                                                                  then play (is,r) doorLocked
>                                                                else moveTo (is,r) ("living")
>                                               "Living" -> moveTo (is,r) ("closet")
>                                               "Bathroom"    -> moveTo (is,r) ("hallway")
>                                               "Hallway"     -> play (is,r) hwAmbiguity
>                                               "Closet"      -> moveTo (is,r) ("hallway")
>                                               "Basement"    -> moveTo (is,r) ("hallway")
>                                               "Backyard"    -> moveTo (is,r) ("hallway")
>                                   _      -> play (is,r) (notOpenable a)
>   where staticObjs = ["door","doors","window","toilet","sink","mirror","shower","clothes","fence","dirt"]
>         boxes = ["box","bigbox","shoebox"]
>         dirtSyns = ["dirt","soil","mound"]
>         digUp (is,r) = do displayWait (is,r) dirtS2 digText

In order to add an item to the list of player's items in the game state, we must check if
the player doesn't have it already, if the item is even in the room with the player, and if
the player is trying to "take all", and then react accordingly.

> addItem         :: GState -> Item -> IO ()
> addItem (is,r) i = if lowi `elem` is
>                      then play (is,r) (addFail1 lowi) -- Already in inventory: Fail
>                    else if lowi `isInRoom` r == False
>                           then if (dropWhile isSpace lowi) == ""
>                                  then play (is,r) addFail3 -- No text after cmd: Fail
>                                else if lowi == "all"
>                                       then takeAll (is,r)
>                                     else if lowi == "box"
>                                            then case r of
>                                                   "Closet"   -> doCmd "add" "shoebox" (is,r)
>                                                   "Basement" -> doCmd "add" "bigbox" (is,r)
>                                                   _          -> play (is,r) (addFail2 lowi)
>                                          else play (is,r) (addFail2 lowi) -- Item not in room: Fail
>                         else case isForbidden (is,r) "add" lowi of
>                           True  -> play (is,r) addFail3 -- Contextually forbidden item: Fail
>                           False -> doCmd "add" lowi (is,r) -- Item is valid: Success
>   where lowi = lowerStr i
>         takeAll (is,r) = play ((nub $ is++(findAllItems r)),r) "Took all\n"

Checks to make sure the player isn't trying take items or do things that are not allowed
in the current context. Applies to multiple commands.

> isForbidden           :: GState -> Command -> Argument -> Bool
> isForbidden (is,r) c a = case c of
>                           x|x `elem` ["add","exam"] -> case a of
>                             "key"    -> "mat" `notElem` is
>                             "shovel" -> "bigbox" `notElem` is
>                             _     -> False
>                           "open" -> case a of
>                                       "door" -> "key" `notElem` is
>                                       _      -> False
>                           "move" -> case a of
>                              y|y `elem` ["living","forward"] -> "key" `notElem` is
>                              _                               -> False
>                           _      -> False

If we can add an item, we need a way to remove an item, but some items are too crucial
to allow the player to drop because it would slightly break the game, so we check for
that in doCmd. Only fails directly if the item isn't in the player inventory.

> remItem         :: GState -> Item -> IO ()
> remItem (is,r) i = if lowi `elem` is || lowi == "box"
>                       then doCmd "drop" lowi (is,r)
>                    else play (is,r) dropFail -- Item not in inventory: Fail
>   where lowi = lowerStr i

Command to move from one room to another. Allows the player to move to adjacent rooms
by mentioning them by name or to move around via directional keywords.

> moveTo          :: GState -> Room -> IO ()
> moveTo (is,r) r' = case r `isAdjTo` r' of
>                        Just x  -> if isForbidden (is,r) "move" r'
>                                     then play (is,r) doorLocked -- Door is locked; No key
>                                   else play (is,(rooms !! x)) (roomPrompt !! x)
>                        Nothing -> if r' `elem` dirKeywords
>                                     then moveDir (is,r) r'
>                                   else play (is,r) moveFail -- In wrong room: Fail
>   where moveDir (is,r) d = case d of
>                              "forward" -> moveTo (is,r) (findFwd r d)
>                              "back"    -> moveTo (is,r) (findBwd r d)
>                              "right"   -> moveTo (is,r) (findRgt r d)
>                              "left"    -> moveTo (is,r) (findLft r d)

Examining something is either looking at an item, looking at an object, or look at the
player's general surroundings. Respond differently for items in vs out of inventory.

> exam         :: GState -> Object -> IO ()
> exam (is,r) i = if lowi `elem` is
>                   then doCmd "examItem" lowi (is,r)
>                 else if lowi `isInRoom` r || lowi == "box"
>                        then doCmd "examObj" lowi (is,r) -- Success
>                      else moveTo (is,r) (lowerStr r) -- Examine current room by default
>   where lowi = lowerStr i

Opening is supposed to be applied to items and objects in general, but is currently only
applicable to doors, which essentially just results in moving to the room on the other side.

> openSsm         :: GState -> Object -> IO ()
> openSsm (is,r) i = doCmd "open" lowi (is,r)
>   where lowi = lowerStr i

This is a workaround to not have to deal with IO from readFile directly. Writes to and loads
from a module SavedGame.lhs, meaning the game must be recompiled before a new save will be
recognized and usable. Not exactly ideal, but it works for now...

> saveGame       :: GState -> IO ()
> saveGame (is,r) = writeFile "./Save.lhs"
>                            ("> module Save where\n\n> save = ("++(show is)++","++(show r)++")")

Before loading a game, we determine whether or not there even is a saved game in SavedGame.lhs.
The default game state is compared to in order to see if loading would result in a change from default.

> loadGame :: GState -> Maybe GState
> loadGame (is,r) = if is /= [] || r /= "Porch"
>                     then Just save
>                   else Nothing

Display a temporary informational screen and then wait for user input to continue.

> displayWait               :: GState -> String -> String -> IO ()
> displayWait gs screen text =
>             do clearScreen; putStr screen; putStr text; hideCursor;
>                readline genericPrompt; showCursor
>                if screen == dirtS2 
>                  then exeCmd gs "quit" -- GAME OVER
>                else play gs "Well, that was exciting, eh?\n"

Displays the help menu. Displays command info and current inventory and location for player.

> helpDisp       :: GState -> IO ()
> helpDisp (is,r) =
>          do clearScreen; putStr helpText; displayCmds; displayState (is,r);
>             hideCursor; readline helpPrompt; showCursor; play (is,r) "Hope that helped ;)\n\n"

Dislaying the information contained within the game state may be called with a user command
at some point. Currently shows the list of items and then prints the room below that.

> displayState       :: GState -> IO ()
> displayState ([],r) = putStrLn ("\nItems: None\n\n"++"Current Location: "++r)
> displayState (is,r) = putStrLn ("\nItems:\n"++(unlines is)++"Current Location: "++r)

Using displayCmds with the play function, we can display all the possible commands in a nice format
to the player when they invoke the "help" (or "?") command. We do this using the cmds and dCmds lists.

> displayCmds :: IO ()
> displayCmds  = putStrLn (helpCmdTitle++(unlines hCmds))

Basic commands and their synonyms, if any:

> takeSyns = ["take","t","grab","pick","steal"]
> dropSyns = ["drop","d"]
> moveSyns = ["move","m","walk","go","enter","cd"]
> examSyns = ["examine","e","look","view","check","read","inspect"]
> openSyns = ["open","o"]
> saveSyns = ["save","s"]
> loadSyns = ["load","l"]
> whoaSyns = ["punch","kick","bite","kill","stab","beat","die","murder"]
> helpSyns = ["help","h","?","how","what"]
> quitSyns = ["quit","q","exit","shutdown"]

cmds is a list of strings that represents the possible valid commands that a player can enter.
hCmds is a helper list for zipping with cmds when displaying command usage to player.

> cmds  = takeSyns++dropSyns++moveSyns++examSyns++openSyns++saveSyns
>         ++loadSyns++whoaSyns++helpSyns++quitSyns

> hCmds = ["take [item] ------------ Take item, put in your inventory"
>         ,"drop [item] ------------ Remove item from inventory forever"
>         ,"go [direction/room] ---- Move from current room to an adjacent room"
>         ,"look at [object/item] -- Take a closer look at something"
>         ,"open [door/item] ------- Open and enter door / Open an item or object"
>         ,"save (s) --------------- Save your game (overwrites last save)"
>         ,"load (l) --------------- Load your previously saved game [Must restart game to load a new save!]"
>         ,"help (?) --------------- Display this list of commands..."
>         ,"quit (q) --------------- Quit the game (Save first!)"]

