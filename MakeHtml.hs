#!/usr/bin/runghc
import Floorball
import Html
import Prelude hiding ((-))


-- Game Statistics
stats =
  [ game 1 Dingoes
      --           S  G  A PIM GF GA
    [ collopy - P  8  0  1  0  4  4
    , basterA - P  1  0  0  0  0  0
    , emms    - P  3  2  0  2  4  3
    , obrien  - P  2  1  0  0  4  4
    , yeoh    - P  5  1  2  0  4  4
    , diffen  - P  2  0  0  0  2  3
    , tng     - P  2  0  0  0  2  2
    ] --           SA GA A PIM
    [ stanley - G  27 4  0  0
    ]

  , game 2 Redbacks
      --            S  G  A PIM GF GA
    [ collopy  - P  2  1  0  0  3  2
    , basterA  - P  2  1  1  0  3  2
    , emms     - P  0  0  0  0  0  2
    , obrien   - P  2  0  0  0  1  2
    , mitchell - P  2  0  0  0  1  2
    , yeoh     - P  2  0  0  0  2  0
    , diffen   - P  1  0  0  0  3  0
    , tng      - P  1  1  0  5  2  0
    ] --            SA GA A PIM
    [ stanley  - G  29 2  0  0
    ]

  ]


-- Players
collopy  = Player 4  "Kris Collopy"
basterA  = Player 6  "Adrian Baster"
emms     = Player 7  "Morgan Emms"
obrien   = Player 10 "Don O'Brien"
ebeling  = Player 12 "Kade Ebeling"
basterP  = Player 13 "Patrick Baster"
mitchell = Player 17 "Ben Mitchell"
broome   = Player 19 "Steve Broome"
keogh    = Player 21 "Tara Keogh"
blades   = Player 23 "Justin Blades"
yeoh     = Player 24 "Nicholas Yeoh"
diffen   = Player 25 "Oli Diffen"
salt     = Player 32 "Matthew Salt"
tng      = Player 38 "Melvin Tng"
king     = Player 77 "Steve King"
stanley  = Player 91 "Jacob Stanley"


-- Render statistics as HTML
main = putStrLn (renderHtmlStats stats)
