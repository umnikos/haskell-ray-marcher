module Color
    ( Color (..)
    , (^*^)
    , (^+^)
    ) where

data Color f = RGB f f f deriving (Show)

(^*^) :: (Num f) => Color f -> Color f -> Color f -- Combine colors by multiplication
(^*^) (RGB r0 g0 b0) (RGB r1 g1 b1) = RGB (r0 * r1) (g0 * g1) (b0 * b1)

(^+^) :: (Num f) => Color f -> Color f -> Color f -- Add colors
(^+^) (RGB r0 g0 b0) (RGB r1 g1 b1) = RGB (r0 + r1) (g0 + g1) (b0 + b1)
