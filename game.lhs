A text-based adventure game made in Haskell by Alec Deschene

> module Main where

> import Commands  -- Commands.lhs
> import Save      -- Save.lhs
> import Screens   -- Screens.lhs
> import Output    -- Output.lhs
> import Helpers   -- Helpers.lhs
> import Data.Char
> import System.IO
> import System.Console.Readline -- Rich terminal IO (binding of GNU library)
> import System.Console.ANSI -- Useful terminal manipulation functionality

In order to make the game easier and more intuitive to start, we have
the player start the game by typing 'start', which calls start, sets up game
colors, loads start screen, and calls the main function to deal with saves.

> start :: IO ()
> start = do setSGR [ SetColor Foreground Vivid Green, SetColor Background Dull Black ]
>            loadScreen "Menu" []
>            main

Introductory function, serves as a kind of menu, looks for a save file, and asks the player
if they want to load the game if it finds one.

> main :: IO ()
> main  = case loadGame save of
>           Just loadState ->
>             do answer <- readline "\n> Continue from last save? [y/n]: "
>                case answer of
>                  Nothing -> freshStart -- Start game at porch with empty inventory
>                  Just "" -> freshStart
>                  Just s  ->
>                    case (toLower . head) s of
>                      'y' -> play save "Game loaded! :)\n"
>                      _   -> freshStart
>           Nothing        -> do hideCursor; readline startText; showCursor; freshStart
>   where freshStart = do moveTo ([],"Porch") ("porch")

