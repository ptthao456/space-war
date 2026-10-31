-- | Cac ham thuan ve toa do: khoang cach, bien map, huong, di chuyen.
module Game.World
    ( distance
    , inBounds
    , allCells
    , movePos
    , compassFrom
    , withinRadius
    , ringCells
    ) where

import Game.Config
import Game.Types

-- | Khoang cach Manhattan giua hai o (bang so buoc phai di).
--
-- >>> distance (Pos 0 0) (Pos 3 4)
-- 7
distance :: Pos -> Pos -> Int
distance (Pos x1 y1) (Pos x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- | O co nam trong map khong (toa do tu 0 den kich thuoc tru 1).
inBounds :: Config -> Pos -> Bool
inBounds cfg (Pos x y) =
    x >= 0 && y >= 0 && x < cfgMapWidth cfg && y < cfgMapHeight cfg

-- | Moi o cua map.
allCells :: Config -> [Pos]
allCells cfg =
    [ Pos x y | y <- [0 .. cfgMapHeight cfg - 1], x <- [0 .. cfgMapWidth cfg - 1] ]

-- | Di mot buoc theo huong. Ham nay khong kiem tra bien map.
movePos :: Direction -> Pos -> Pos
movePos DirUp    (Pos x y) = Pos x (y - 1)
movePos DirDown  (Pos x y) = Pos x (y + 1)
movePos DirLeft  (Pos x y) = Pos (x - 1) y
movePos DirRight (Pos x y) = Pos (x + 1) y

-- | Huong (8 phia) tu o thu nhat toi o thu hai.
--
-- Neu doc lech >= 2 lan ngang lech thi chi tinh Bac/Nam (va nguoc lai cho
-- Dong/Tay), con lai la huong cheo. Cung o thi tra ve 'Here'.
compassFrom :: Pos -> Pos -> Compass
compassFrom (Pos fx fy) (Pos tx ty)
    | dx == 0 && dy == 0     = Here
    | abs dx >= 2 * abs dy   = if dx > 0 then East else West
    | abs dy >= 2 * abs dx   = if dy > 0 then South else North
    | dx > 0 && dy > 0       = SouthEast
    | dx > 0                 = NorthEast
    | dy > 0                 = SouthWest
    | otherwise              = NorthWest
  where
    dx = tx - fx
    dy = ty - fy

-- | O thu hai co nam trong ban kinh (Manhattan) quanh o thu nhat khong.
withinRadius :: Int -> Pos -> Pos -> Bool
withinRadius r center p = distance center p <= r

-- | Cac o trong map cach o trung tam tu @lo@ den @hi@ buoc (Manhattan).
ringCells :: Config -> Pos -> Int -> Int -> [Pos]
ringCells cfg center@(Pos cx cy) lo hi =
    [ p
    | y <- [cy - hi .. cy + hi]
    , x <- [cx - hi .. cx + hi]
    , let p = Pos x y
    , inBounds cfg p
    , let d = distance center p
    , d >= lo && d <= hi
    ]