import Data.List
import Data.Map.Strict
main = print $ hanoi 3 "A" "B" "C"
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c =
    let move 0 src dest tmp moves = 
            moves
        move 1 src dest tmp moves = 
            moves ++ [(src, dest)]
        move cnt src dest tmp moves = 
            do let moves' = move (cnt-1) src tmp dest moves
               let moves'' = move 1 src dest tmp moves'
               move (cnt-1) tmp dest src moves''
    in
        move n a b c []