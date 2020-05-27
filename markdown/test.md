# Compute the Area of a Polygon

You are given the cartesian coordinates of a set of points in a `2D` plane. When traversed sequentially, these points form a Polygon,`P` , which is not self-intersecting in nature. Can you compute the area of polygon `P`?

## Input Format

The first line contains an integer,`N` , denoting the number of points.
The `N` subsequent lines each contain `2` space-separated integers denoting the respective `x` and `y` coordinates of a point.

## Constraints

- No `2` points are coincident, and polygon `P` is obtained by traversing the points in a counter-clockwise direction.
- ![Equation](https://render.githubusercontent.com/render/math?math=4%20\leq%20N\leq%201000)
- ![Equation](https://render.githubusercontent.com/render/math?math=0%20\leq%20x,y\leq%201000)
  


## Output Format

For each test case, print the area of `P` (correct to a scale of one decimal place).

**Note**: Do not add any leading/trailing spaces or units; it is assumed that your result is in square units.

## Sample Input

    4
    0 0
    0 1  
    1 1  
    1 0
## Sample Output

    1
## Explanation

The given polygon is a square, and each of its sides are `1` unit in length.
`area = length * width = 1 * 1 = 1`, so we print `1` on a new line.

## Code

**Approach**

Area = ![Equation](https://www.geeksforgeeks.org/wp-content/ql-cache/quicklatex.com-8dd9477c5c0dc039ce5b6e0fd34a6a14_l3.svg)

Let the input vertices be
 
    (0, 1), (2, 3), and (4, 7). 

Evaluation procedure matches with process of tying
shoelaces.

We write vertices as below

    0    1
    2    3
    4    7
    0    1  [written twice]

we evaluate positive terms as below

    0  \  1
    2  \  3
    4  \  7
    0     1  
i.e., `0*3 + 2*7 + 4*1 = 18` 

we evaluate negative terms as below

    0     1
    2  /  3
    4  /  7
    0  /  1  
i.e., `0*7 + 4*3 + 2*1 = 14`

    Area = 1/2 (18 - 14) = 2 

```haskell
import Control.Monad
import Debug.Trace
import System.IO
import System.IO.Unsafe

type Point = (Double, Double)                   -- input coordinates
type Polygon = [Point]                    -- list of all input coordinates

polyArea :: Polygon -> Double
polyArea inputPoint = polygonArea (head $ cordinates inputPoint) (head $ tail $ cordinates inputPoint)

cordinates :: [(a, a)] -> [[a]]
cordinates input = [xCordinates input, yCordinates input]

xCordinates :: [(a, b)] -> [a]
xCordinates input = map fst input

yCordinates :: [(a, b)] -> [b]
yCordinates input = map snd input

polygonArea :: Fractional a => [a] -> [a] -> a
polygonArea xCordinates yCordinates =  abs $ ((sum $ (zipWith (*)) xCordinates (slide yCordinates)) -  (sum $ (zipWith (*)) (slide xCordinates) yCordinates)) / 2

-- slide 0 1 2 3 -> 1 2 3 0
slide :: [a] -> [a]
slide [] = []
slide (x:xs) = xs ++ [x]

main = do
    sides <- fmap (read::String->Double) getLine
    input <- forM [1..sides] (\_->do fmap ((\[a, b]->(a,b)).map (read::String->Double).words) getLine :: IO (Double, Double))
    putStrLn $ show $ polyArea input 
```