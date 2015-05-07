import Data.List
import Data.Maybe
import Text.Printf

data Type
  = TBase
  | TArr Type Type
  deriving (Show, Eq)

data Exp
  = EVar String
  | EApp Exp Exp
  | EAbs String Type Exp
  | EConst Int
  deriving (Show, Eq)

type Ctx = [(String, Type)]

numOfConsts :: Int
numOfConsts = 2

normal :: Exp -> Bool
normal (EVar x)               = True
normal (EApp (EAbs _ _ _) e2) = False
normal (EApp _            e2) = normal e2
normal (EAbs _ _ e)           = normal e
normal (EConst _)             = True

freshName :: String -> Ctx -> String
freshName prefix g = fresh 0
  where
    names   = map fst g
    fresh n
      | var `elem` names = fresh (n+1)
      | otherwise        = var
        where
          var = prefix ++ show n

partitionPairs :: Int -> [(Int, Int)]
partitionPairs n = count 1
  where
    count k
      | k < n     = (k, n-k) : count (k+1)
      | otherwise = []

ugenTyp :: Int -> [Type]
ugenTyp 0 = []
ugenTyp 1 = [TBase]
ugenTyp n = concatMap genArr $ partitionPairs n
  where
    genArr (n1, n2) = [TArr t1 t2 | t1 <- ugenTyp n1, t2 <- ugenTyp n2]

ugen :: Ctx -> Int -> [Exp]
ugen g 0 = []
ugen g 1 = map EConst [1..numOfConsts] ++ map (EVar . fst) g
ugen g n = concatMap genAbs sizes ++ concatMap genApp sizes
  where
    sizes           = partitionPairs (n-1)
    genAbs (n1, n2) = [EAbs x t e | t <- ugenTyp n1, e <- ugen ((x,t):g) n2]
      where
        x = freshName "x" g
    genApp (n1, n2) = [EApp e1 e2 | e1 <- ugen g n1, e2 <- ugen g n2]

tc :: Ctx -> Exp -> Maybe Type
tc g (EVar x)      = lookup x g
tc g (EApp e1 e2)  = do
  t <- tc g e1
  case t of
    TArr t1 t2 -> do
      t1' <- tc g e2
      if t1 == t1' then return t2 else Nothing
    _ -> Nothing
tc g (EAbs x t1 e) = do
  t2 <- tc ((x,t1):g) e
  return $ TArr t1 t2
tc g (EConst _)    = return TBase

welltyped :: Ctx -> Exp -> Bool
welltyped g e = isJust $ tc g e

welltypedAt :: Ctx -> Type -> Exp -> Bool
welltypedAt g t e =
  case tc g e of
    Just t' -> t == t'
    Nothing -> False

calculateCounts :: Ctx -> Type -> Int -> [(Int, Int, Int, Int, Int)]
calculateCounts g t n =
  zip5 indices rawCounts typedCounts typedAtCounts normalCounts
  where
    aggregate :: [Int] -> [Int]
    aggregate xs = reverse res
      where
        (_, res) = foldl (\(px, res) x -> (px+x, px+x : res)) (0, []) xs

    indices       = [1..n]
    terms         = map (ugen g) indices
    allLengths    = aggregate . map length
    rawCounts     = allLengths terms
    typedCounts   = allLengths $ map (filter (welltyped g)) terms
    typedAtCounts = allLengths $ map (filter (welltypedAt g t)) terms
    normalCounts  = allLengths $ map (filter normal . filter (welltypedAt g t)) terms

printData :: Ctx -> Type -> Int -> IO ()
printData g t n =
  mapM_ putStrLn lines
  where
    counts = calculateCounts g t n
    addDeltaIfZero :: Int -> Double
    addDeltaIfZero n = if n == 0 then 1e-6 else fromIntegral n
    lines  = [printf "%f %f %f %f %f"
      (addDeltaIfZero n)
      (addDeltaIfZero w)
      (addDeltaIfZero x)
      (addDeltaIfZero y)
      (addDeltaIfZero z) | (n, w, x, y, z) <- counts]
