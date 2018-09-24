import Data.List as List (find)
-- Parameterize boolean connectives,
-- as we shall apply them both to boolean propositions and monadic schemeta
data TwoPlace = And | Or | Imply | Iff
    deriving Show
-- instance Show TwoPlace where
--     show And = "&&"
--     show Or  = "||"
--     show Imply = "->"
--     show Iff   = "<->"

data OnePlace = Id | Not
    deriving Show
-- instance Show OnePlace where
--     show Id = ""
--     show Not = "-"

data Connective t = One OnePlace t | Two TwoPlace t t
    deriving Show
-- instance Show t => Show (Connective t) where
--     show (One op t)     = show op ++ "(" ++ show t ++ ")"
--     show (Two op t1 t2) = "(" ++ show t1 ++ " " ++ show op ++ " " ++ show t2 ++ ")"

data Prop = Var String | Comp (Connective Prop)
    deriving Show
-- instance Show Prop where
--     show (Var v) = show v
--     show (Comp  op) = show op

data Monadic = Forall Prop | Exists Prop | COMP (Connective Monadic)
    deriving Show
-- instance Show Monadic where
--     show (Forall p) = "(∀ " ++ show p ++ ")"
--     show (Exists p) = "(∃ " ++ show p ++ ")"
--     show (COMP  op) = "(" ++ show op ++ ")"

p = (Comp (Two Or (Comp (One Not (Var "P")))
                  (Var "Q")))
m = (COMP (Two And (Forall p)
                   (Exists (Comp (One Not (Var "R"))))))
-- show m == (&& (∀x "P"x) (∃x (- "Q"x)))

vars :: Monadic -> [String]
vars (Exists p) = propVars p
vars (Forall p) = propVars p
vars (COMP   m) = case m of
                    One op m -> vars m
                    Two op m n -> vars m ++ vars n

-- Interpretations of propositions
propVars :: Prop -> [String]
propVars (Var v)  = [v]
propVars (Comp p) = case p of
                      One op p -> propVars p
                      Two op p q -> propVars p ++ propVars q

type Assoc k v = [(k,v)]
type Interpretation = Assoc String Bool

interpretationsp :: Prop -> [Interpretation]
interpretationsp p = map (zip vs) (assignments (length vs))
                          where
                              vs = propVars p
                              assignments :: Int -> [[Bool]]
                              assignments 0 = [[]]
                              assignments n = map (True:) prev ++ map (False:) prev
                                              where prev = assignments (n-1)

interpretations :: Monadic -> [Interpretation]
interpretations m = map (zip vs) (assignments (length vs))
                          where
                              vs = vars m
                              assignments :: Int -> [[Bool]]
                              assignments 0 = [[]]
                              assignments n = map (True:) prev ++ map (False:) prev
                                              where prev = assignments (n-1)

interpret ::  Prop -> Interpretation -> Bool
interpret (Var v) i  = case find (\(k',b) -> k' == v) i of
                         Just (_,b)  -> b
                         Nothing -> error("unknown variable")
interpret (Comp p) i = case p of
                         One f p -> case f of
                                      Id  -> interpret p i
                                      Not -> not $ interpret p i
                         Two f p1 p2 ->
                           case f of
                             And   -> (&&) (interpret p1 i)
                                           (interpret p2 i)
                             Or    -> (||) (interpret p1 i)
                                           (interpret p2 i)
                             Imply -> (||) (not $ interpret p1 i)
                                           (interpret p2 i)
                             Iff   -> (||) (interpret p1 i && interpret p2 i)
                                           ((&&) (not $ interpret p1 i)
                                                 (not $ interpret p2 i))

-- extract the proposition from the pure monadic schemeta in a monadic schema
-- just for pratice.
purify :: Monadic -> [Prop]
purify (Forall p) = [p]
purify (Exists p) = [p]
purify (COMP   m) = case m of
                      One _ m -> purify m
                      Two _ m n -> purify m ++ purify n

proposify :: Monadic -> Prop
proposify m = let
                -- all possible interpretations, paired with their respective indices
                i_s = zip (interpretations m) [show i | i <- [0..]]
                -- "and" together a list of strings representing propositional vars
                andns :: [String] -> Prop
                andns [v] = Var v
                andns (v:vs) = Comp (Two And (Comp (One Not (Var v)))
                                             (andns vs))
                -- "or" together a list of strings representing propositional vars
                ors   :: [String] -> Prop
                ors   [v] = Var v
                ors   (v:vs) = Comp (Two Or (Var v) (ors vs))
                -- recursion
                helper :: Monadic -> Prop
                helper (Forall p) = (andns ls)
                                    where ls = [s | (i,s) <- i_s, not $ interpret p i]
                helper (Exists p) = (ors ls)
                                    where ls = [s | (i,s) <- i_s, interpret p i]
                helper (COMP m) = case m of
                                    One f m   -> Comp (One f (helper m))
                                    Two f m n -> Comp (Two f (helper m) (helper n))
              in
                helper m

isSatP :: Prop -> Bool
isSatP p = or $ map (interpret p) (interpretationsp p)

isSat :: Monadic -> Bool
isSat = isSatP . proposify

isValidP :: Prop -> Bool
isValidP p = and $ map (interpret p) (interpretationsp p)

isValid :: Monadic -> Bool
isValid = isValidP . proposify
