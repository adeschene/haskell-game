> module GTypes where

GState contains the game state, which consists of a list of the items that the player
currently has in their inventory and the room that the player is currently in. 

> type GState   = ([Item], Room)
> type Item     = String -- Physical item that can be owned by the player
> type Room     = String -- Physical location that the player can be in
> type Object   = String -- Same as an item, but can't be held or taken by player
> type Prompt   = String -- The text displayed to the user to prompt for input
> type Command  = String -- The player's desired command
> type Argument = String -- What the player wants the command to act upon
