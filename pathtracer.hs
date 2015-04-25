import Data.List
import Control.Monad (replicateM, liftM)
import System.Random
import qualified Data.Vector.Mutable as V
import Text.Printf
import Data.IORef

data Vec = Vec !Float !Float !Float
         deriving (Show, Eq)
data Ray = Ray Vec Vec
data Refl = Diff
          | Spec
          | Refr
data Sphere = Sphere Float Vec Vec Vec Refl

epsilon :: Float
epsilon = 1e-4

maxD :: Integer
maxD = 5

clamp :: (Num a, Ord a) => a -> a
clamp x = if x > 1 then 1 else if x < 0 then 0 else x

color :: Float -> Float -> Float -> (Int, Int, Int)
color r g b = 
    (floor $ clamp r ** (1 / 2.2) * 255 + 0.5,
     floor $ clamp g ** (1 / 2.2) * 255 + 0.5,
     floor $ clamp b ** (1 / 2.2) * 255 + 0.5)

rand :: IO Float
rand = liftM (\l -> l!!0) (replicateM 1 (randomIO :: IO Float))

(***) :: Float -> Vec -> Vec
(***) c (Vec x y z) = Vec (c * x) (c * y) (c * z)

norm :: Vec -> Vec
norm (Vec x y z) = (1 / sqrt (x * x + y * y + z * z)) *** (Vec x y z)

(.>) :: Vec -> Vec -> Float
(.>) (Vec x y z) (Vec x' y' z') = x * x' + y * y' + z * z'

(><) :: Vec -> Vec -> Vec
(><) (Vec a b c) (Vec x y z) = Vec (b * z - c * y)
                               (c * x - a * z)
                               (a * y - b * x)
infixl 7 .>
infixl 7 ><
instance Num Vec where (+) (Vec a b c) (Vec x y z)=Vec (a+x) (b+y) (c+z)
                       (-) (Vec a b c) (Vec x y z)=Vec (a-x) (b-y) (c-z)
                       (*) (Vec a b c) (Vec x y z)=Vec (a*x) (b*y) (c*z)
                       abs (Vec x y z) = Vec (abs x) (abs y) (abs z)
                       fromInteger = const (Vec 0 0 0)

(-@-) :: Ray -> Sphere -> Maybe Float
(-@-) (Ray o d) (Sphere r p e c rf) =
    if (dt < 0) then Nothing
    else if b - (sqrt dt) > epsilon then Just (b - sqrt dt)
         else if b + (sqrt dt) > epsilon then Just (b + sqrt dt) else Nothing
       where op = p - o
             b = op .> d
             dt = b * b - op .> op + r * r
infixl 6 -@-

shoot :: Ray -> (Maybe Float, Sphere)
shoot ray = foldl' closest (Nothing, undefined) spheres where
    closest (i, sph) sph' = case (i, ray -@- sph') of
                              (Nothing, Just t) -> (Just t, sph')
                              (Just t', Just t) ->
                                  if t < t' then (Just t, sph') else (i, sph)
                              _ -> (i, sph)
                              
spheres :: [Sphere]
spheres = [Sphere 1e5 (Vec (1e5 + 1) 40.8 81.6) (Vec 0 0 0)
                      (Vec 0.75 0.25 0.25) Diff,
          Sphere 1e5 (Vec (-1e5 + 99) 40.8 81.6) (Vec 0 0 0)
                     (Vec 0.25 0.25 0.75) Diff,
          Sphere 1e5 (Vec 50 40.8 1e5) (Vec 0 0 0)
                     (Vec 0.75 0.75 0.75) Diff,
          Sphere 1e5 (Vec 50 40.8 (-1e5 + 170)) (Vec 0 0 0)
                     (Vec 0 0 0) Diff,
          Sphere 1e5 (Vec 50 1e5 81.6) (Vec 0 0 0)
                     (Vec 0.75 0.75 0.75) Diff,
          Sphere 1e5 (Vec 50 (-1e5 + 81.6) 81.6) (Vec 0 0 0)
                     (Vec 0.75 0.75 0.75) Diff,
          Sphere 16.5 (Vec 27 16.5 47) (Vec 0 0 0)
                     (Vec 0.6 0.6 0.6) Spec,
          Sphere 16.5 (Vec 73 16.5 78) (Vec 0 0 0)
                     (Vec 0.65 0.5 0.999) Refr,
          Sphere 16.5 (Vec 55 60 30) (Vec 0 0 0) (Vec 1 1 0.3) Diff,
          Sphere 600 (Vec 50 681.33 81.6) (Vec 12 12 12) (Vec 0 0 0) Diff]

getDiff :: Vec -> Vec -> Vec -> Integer -> Vec -> IO Vec
getDiff nl e x dep' f = do
  r1 <- fmap ((2 * pi) *) rand
  r2 <- rand
  let r2s = sqrt r2
      Vec wx _ _ = nl
      u = norm $ (if abs wx > 0.1 then Vec 0 1 0 else Vec 1 0 0) >< nl
      v = nl >< u
      d' = norm $ ((cos r1 * r2s) *** u) + ((sin r1 * r2s) *** v) +
           ((sqrt (1 - r2)) *** nl)
  fmap ((+) e . (*) f) (rad (Ray x d') dep')

getSpec :: Vec -> Vec -> Integer -> Vec -> Vec -> Vec -> IO Vec
getSpec n d dep' f e x = let d' = d - ((2 * n .> d) *** n) in
                         fmap ((+) e . (*) f) (rad (Ray x d') dep')

getRefr :: Vec -> Vec -> Vec -> Integer -> Vec -> Vec -> Vec -> IO Vec
getRefr d n f dep' x e nl = do
  let rfRay = Ray x (d - (2 * n .> d) *** n)
      isInside = n .> nl > 0
      nt = 1
      nc = 1.3
      ior = nc / nt
      nnt = if isInside then 1 / ior else ior
      ddn = d .> nl
      cos2t = 1 - nnt * nnt * (1 - ddn * ddn)
      tmp = (if isInside then 1 else -1) * (ddn * nnt + sqrt cos2t)
      tdir = norm $ (nnt *** d) - (tmp *** n)
      a = nt - nc
      b = nt + nc
      r0 = a * a / (b * b)
      c = 1 - (if isInside then -ddn else tdir .> n)
      re = r0 + (1 - r0) * (c ** 5)
      tr = 1 - re
      q = 0.25 + re / 2
      rp = re / q
      tp = tr / (1 - q)
      dep = dep' - 1
      reflOrRefr = if dep > 2 then
                       (do
                         er <- rand
                         if er < q then fmap (\r -> rp *** r) (rad rfRay dep')
                         else fmap (\r ->  tp *** r) (rad (Ray x tdir) dep')
                       )
                   else
                       (do
                         rad0 <- fmap (\r -> re *** r) (rad rfRay dep')
                         rad1 <- fmap (\r -> tr *** r) (rad (Ray x tdir) dep')
                         return $ rad0 + rad1
                       )
  if cos2t < 0 then fmap ((+) e . (*) f) (rad rfRay dep')
  else fmap ((+) e . (*) f) reflOrRefr

rad :: Ray -> Integer -> IO Vec
rad ray dep = 
    case shoot ray of
      (Nothing, _) -> return $ Vec 0 0 0
      (Just t, Sphere r p e c rf) -> do
                     if dep' < maxD then bounceWith c else
                         rand >>= \er -> if (er < pr)
                                         then bounceWith $ (1 / pr) *** c
                                         else return e
                             where
                               Ray o d = ray
                               x = o + (t *** d)
                               n = norm $ x - p
                               maxV (Vec r g b) = maximum [r,g,b]
                               nl = if (n .> d < 0) then n else -n
                               dep' = dep + 1
                               pr = maxV c
                               bounceWith f =
                                   case rf of
                                     Diff -> getDiff nl e x dep' f
                                     Spec -> getSpec n d dep' f e x
                                     Refr -> getRefr d n f dep' x e nl

mHelp1 sx sy cx cy x y w h c i samps dir pos = do
  r <- newIORef (Vec 0 0 0)
  flip mapM_ [0..samps - 1] $ \s -> do
           r1 <- fmap (2*) rand
           r2 <- fmap (2*) rand
           let dx = if r1 < 1 then sqrt r1 - 1 else 1 - sqrt (2 - r1)
               dy = if r2 < 1 then sqrt r2 - 1 else 1 - sqrt (2 - r2)
               d = dir +
                   ((((sx + 0.5 + dx) / 2 + fromIntegral x) /
                     fromIntegral w - 0.5) *** cx) +
                   ((((sy + 0.5 + dy) / 2 + fromIntegral y) /
                     fromIntegral h - 0.5) *** cy)
           rad' <- rad (Ray (pos + (140 *** d)) (norm d)) 0
           modifyIORef r (+ ((1 / fromIntegral samps) *** rad'))
  ci <- V.unsafeRead c i
  Vec rr rg rb <- readIORef r
  V.unsafeWrite c i $ ci + (0.25 *** (Vec (clamp rr) (clamp rg) (clamp rb)))

mHelp2 cx cy x y w h c samps dir pos = do
  let i = (h - y - 1) * w + x
  flip mapM_ [0..1] $ \sy -> do
           flip mapM_ [0..1] $ \sx ->
               mHelp1 sx sy cx cy x y w h c i samps dir pos

main = do
  let w = 100 :: Int
      h = 100 :: Int
      samps = 60
      pos = Vec 50 52 295.6
      dir = norm $ Vec 0 (-0.042612) (-1)
      cx = Vec (fromIntegral w * 0.5135 / fromIntegral h) 0 0
      cy = 0.5135 *** (norm (cx >< dir))
  c <- V.replicate (w * h) (Vec 0 0 0)
  -- apply colors to the image
  flip mapM_ [0..h-1] $ \y -> do
         flip mapM_ [0..w-1] $ \x ->
             mHelp2 cx cy x y w h c samps dir pos
  -- print out path traced image
  printf "P3\n%d %d\n%d\n" w h (255 :: Int)
  flip mapM_ [0..w * h - 1] $ \i -> do
         Vec r g b <- V.unsafeRead c i
         let (r', g', b') = color r g b
         printf "%d %d %d " r' g' b'
