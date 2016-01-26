module Sudoku.Samples where

import Sudoku.Sudoku

-- www.puzzle-sudoku.net, Intermediate, ID 37,054,878
sample1 :: Board
sample1 = parseBoard
  [ "   8 9   "
  , " 1 537 8 "
  , " 87   95 "
  , "   3 4   "
  , "7 36 15 8"
  , " 62   43 "
  , "2 1 7 3 6"
  , "  8   7  "
  , "9  1 3  5"
  ]

-- sudoku.zeit.de, Leicht, 03.10.2006
sample2 :: Board
sample2 = parseBoard
  [ "  28    7"
  , "   1  2 5"
  , " 7 2 93  "
  , "5 7    13"
  , "    91  4"
  , "94    56 "
  , " 6 7    2"
  , " 586 2 39"
  , " 1  4    "
  ]

-- sudoku.zeit.de, Mittel, 03.10.2006
sample3 :: Board
sample3 = parseBoard
  [ " 2  5    "
  , "7  4  851"
  , "   96 2  "
  , "849  6   "
  , "63  8  97"
  , "  2      "
  , "  56   7 "
  , "3   925 4"
  , "     1 3 "
  ]

-- sudoku.zeit.de, Schwer, 03.10.2006
sample4 :: Board
sample4 = parseBoard
  [ "1   3  9 "
  , " 6 7  1 3"
  , "8 95  27 "
  , " 9  485  "
  , "       6 "
  , " 2    8 7"
  , " 76      "
  , " 143  95 "
  , "  841    "
  ]


basicSample :: Board
basicSample = parseBoard
  [ "96 1  37 "
  , "1   3   9"
  , "     7  8"
  , "2   6 1  "
  , " 5 8 2 3 "
  , "  6 7   5"
  , "6  7     "
  , "5   8   7"
  , " 84  5 63"
  ]



easySample :: Board
easySample = parseBoard
  [ "  4 2 5  "
  , "5 2 1 3 9"
  , "  35 47  "
  , "3  196  5"
  , "  1   9  "
  , "2  857  6"
  , "  89 16  "
  , "1 5 7 8 4"
  , "  6 8 2  "
  ]

-- |Sample with level "intermediate" (ID 46,913,653).
intermediateSample :: Board
intermediateSample = parseBoard
  [ "2        "
  , "    5961 "
  , "      793"
  , "    36 4 "
  , " 9 8     "
  , " 2 5   87"
  , " 32      "
  , " 513 7  9"
  , "  7  8 6 "
  ]

advancedSample :: Board
advancedSample = parseBoard
  [ "6 52 91 7"
  , "   1 5   "
  , "    7    "
  , "  3   5  "
  , "95 7 6 83"
  , "86  2  71"
  , "3       9"
  , " 48   76 "
  , "   6 1   "
  ]

extremeSample :: Board
extremeSample = parseBoard
  [ "87  5  4 "
  , "4 1 7   2"
  , " 5 6  7 1"
  , "  7   8  "
  , "52   34  "
  , "    6   9"
  , "  591    "
  , "9      7 "
  , " 13  5   "
  ]

evilSample :: Board
evilSample = parseBoard
  [ "25 47   9"
  , "39       "
  , "  7  3 8 "
  , " 76  1   "
  , "    3  7 "
  , "   62 1  "
  , "    6    "
  , " 6 39 5 4"
  , "5  2     "
  ]

samples :: [(String, Board)]
samples =
  [ ("basic"       , basicSample       )
  , ("easy"        , easySample        )
  , ("intermediate", intermediateSample)
  , ("advanced"    , advancedSample    )
  , ("extreme"     , extremeSample     )
  , ("evil"        , evilSample        )
  ]

