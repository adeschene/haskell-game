> module Rooms where

> import GTypes -- GTypes.lhs
> import Data.Char (toLower)
> import Data.List (find,findIndex)

Essentially, there are two versions of the rooms used throughout the program. These lists
represent those two versions. I'm not sure how or why this is the case, but it should be changed.

> rooms = ["Porch","Living","Bathroom","Hallway","Closet","Backyard","Basement"]
> roomKeywords = ["porch","living","bathroom","hallway","closet","backyard","basement"]

Directional keywords for moving intuitively based on contextual position.

> dirKeywords = ["forward","back","left","right"]

For each room, we want to keep a list of all the obtainable items:

> univItems = ["all","box"]
> pItems    = ["key","lighter","mat"]
> lrItems   = ["painting","vase"]
> brItems   = ["paper"]
> hwItems   = []
> clItems   = ["shoebox"]
> byItems   = []
> bmItems   = ["cup","boots","bigbox","shovel"]

It's useful to keep a list of the interactable objects in each room:

> pObjs  = ["door","key","lighter","mat","window"]
> lrObjs = ["door","painting","vase"]
> brObjs = ["toilet","sink","mirror","shower","paper"]
> hwObjs = ["doors"]
> clObjs = ["clothes","shoebox"]
> byObjs = ["fence","dirt","soil","mound"] -- Includes dirt object synonyms
> bmObjs = ["cup","boots","bigbox","shovel"]

Another useful set of lists to have available is that of the lists of
rooms that can be accessed from each room:

> pAdjRooms  = ["porch","living"]
> lrAdjRooms = ["living","porch","hallway","closet"]
> brAdjRooms = ["bathroom","hallway"]
> hwAdjRooms = ["hallway","living","bathroom","basement","backyard"]
> clAdjRooms = ["closet","living"]
> byAdjRooms = ["backyard","hallway"]
> bmAdjRooms = ["basement","hallway"]

Although having the individual lists is great, it's also useful to have
a list of every (possibly) valid item, object, and room regardless of context:

> keywords = pItems++lrItems++brItems++hwItems++clItems++byItems++bmItems
>            ++pObjs++lrObjs++brObjs++hwObjs++clObjs++byObjs++bmObjs
>            ++roomKeywords++dirKeywords++univItems

Text that is displayed when player first enters a room or uses the look command w/o arguments.

> roomPrompt = [
>   "You're on the porch of an old family friend's house.\n" -- Front Porch text
>   ++"You can see a bit of the living room through the window, but it's hard\n"
>   ++"to make anything out very clearly from this distance.\n",
>   "Everything seems very clean and well taken care of.\nThe house is completely "
>   ++"silent...no signs of life.\n", -- Living room text
>   "The bathroom, like everything else, is immaculate, and smells vaguely of lemony " -- Bathroom text
>   ++"disinfectant.\n",
>   "There are three doors. You can feel a draft coming from the end of the hallway,\n" -- Hallway text
>   ++"but other than that, everything seems almost disturbingly ordinary.\n",
>   "The closet is filled with clothes that have been placed in identical dress-holders, which " -- Closet text
>   ++"seems odd.\n",
>   "Standing on the back porch, you look out and see a fence surrounding a very barren " -- Backyard text
>   ++"backyard.\nYou can see a conspicuous mound of recently disturbed soil...\n",
>   "The basement is dark but feels oddly sterile and clean compared to most.\n" -- Basement text
>   ++"In front of you is a storage rack with several things on it.\n"
>              ]
