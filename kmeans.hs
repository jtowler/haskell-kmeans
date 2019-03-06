import Data.Function (on)
import Data.List (groupBy, maximum, minimumBy, genericLength)
import System.Random (getStdGen, randomRs) 
import System.Environment

data Point = Point Double Double deriving (Show, Eq)

assignToCluster :: [Point] -> [Point] -> [Int]
assignToCluster [] _ = []
assignToCluster (p:ps) means = (getBestCluster p means):(assignToCluster ps means)

getBestCluster :: Point -> [Point] -> Int 
getBestCluster p means = fst . minimumBy (compare `on` euclidean p . snd) $ zip [0..] means

euclidean :: Point -> Point -> Double
euclidean (Point x1 y1) (Point x2 y2) = sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2 

pointsForLabel :: [(Point, Int)] -> Int -> [Point]
pointsForLabel points label = map fst filtered
    where filtered = filter (\x -> snd x == label) points

mean :: [Double] -> Double
mean xs = realToFrac (sum xs) / genericLength xs

updateMean :: [Point] -> Point
updateMean points = Point x y
    where x = mean $ map (\(Point a b) -> a) points
          y = mean $ map (\(Point a b) -> b) points

updateMeans :: [Point] -> [Int] -> [Point]
updateMeans points labels = map (\l -> updateMean $ pointsForLabel z l) [0..maximum labels]   
    where z = zip points labels

minMax :: [Point] -> (Point, Point)
minMax points = (Point lx ly, Point ux uy)
    where xs = map (\(Point a b) -> a) points
          ys = map (\(Point a b) -> b) points
          lx = minimum xs
          ly = minimum ys
          ux = maximum xs
          uy = maximum ys

runKMeans :: [Point] -> [Point] -> Int -> [Int]
runKMeans points means iter
    | iter <= 0 = labels
    | means == newMeans = labels 
    | otherwise = runKMeans points newMeans $ iter - 1 
    where labels = assignToCluster points means
          newMeans = updateMeans points labels

lineToPoint :: String -> Point
lineToPoint line =
    let (x:y:[]) = groupBy (\a b -> b /= ',') line
        xd = read x :: Double
        yd = read $ filter (\q -> q /= ',') y :: Double
    in Point xd yd

main = do
    (k:fname:iters:_) <- getArgs 
    gen <- getStdGen
    d <- readFile fname
    let l = lines d
        ki = read k :: Int
        points = map lineToPoint l
        (Point minX minY, Point maxX maxY) = minMax points
        xMeans = take ki (randomRs (minX,maxX) gen)
        yMeans = take ki (randomRs (minY,maxY) gen)
        means = zipWith (\a b -> Point a b) xMeans yMeans
        labels = runKMeans points means (read iters :: Int)
    mapM  (putStrLn . show) labels
