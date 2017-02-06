-----------------------------------------------------------------------------
-- |
-- Module      :  Types
-- author      : cdannapp & tmielcza
--
-- Types returned by parsing and used for Resolution
--
-----------------------------------------------------------------------------
module Types
(
  Expr(Xor, Or, And, Fact, Not),
  Relation(Eq, Imply),
  Init(Init),
  Query(Query),
  State(..),
  Trilean(..),
  FactState,
  rhs, lhs,
  mapSnd,
    ) where



-- | the type of expressions all constructors are recursives except Fact
data Expr = Xor Expr Expr |
            Or Expr Expr |
            And Expr Expr |
            Fact String |
            Not Expr
            deriving (Eq)
infixl 4 `Xor`
infixl 5 `Or`
infixl 6 `And`
infixl 7 `Imply`
infixl 8 `Eq`

-- | The type of the relations between the Exprs. They form rules.
data Relation = Eq Expr Expr | Imply Expr Expr

-- | this type contains an Expr array. They are init Fact obtained by parsing.
newtype Init = Init [Expr]

-- | this type contains an Expr array. They are queries obtained by parsing.
newtype Query = Query [Expr]

data State = Unknown Bool | Known Bool | Ambiguous | Invalid deriving (Show, Eq)

type FactState = (Expr, State)

instance Show Expr where
    show (Xor e1 e2) = "(" ++ show e1 ++ "^" ++ show e2 ++ ")"
    show (Or e1 e2) = "(" ++ show e1 ++ "|" ++ show e2 ++ ")"
    show (And e1 e2) = "(" ++ show e1 ++ "+" ++ show e2 ++ ")"
    show (Fact c) = c
    show (Not e) = "!" ++ show e

instance Show Relation where
    show (Imply e1 e2) = "{" ++ show e1 ++ "=>" ++ show e2 ++ "}"
    show (Eq e1 e2) = "{" ++ show e1 ++ "<=>" ++ show e2 ++ "}"

instance Show Init where
    show (Init facts) = "Init: "++ show facts

instance Show Query where
    show (Query facts) = "Query: "++ show facts

rhs (Eq _ r) = r
rhs (Imply _ r) = r

lhs (Eq l _) = l
lhs (Imply l _) = l

class (Eq t) => Trilean t where
  true, false, ambiguous :: t --invalid est retourné mais jamais reçu puisqu'il est immediatement transformé en Left
  unknown, notunknown :: t
  t_not :: t -> t
  (@+), (@|), (@^) :: t -> t -> t -- return true false unknown notunknown
  a @+ b
    | a == false = false
    | b == false = false
    | a == true && b == true = true
    | a == ambiguous || b == ambiguous = ambiguous
    | (a == unknown && b == notunknown) || (b == unknown && a == notunknown) = unknown
    | otherwise = unknown
  a @| b
    | a == true = true
    | b == true = true
    | a == false && b == false = false
    | a == ambiguous || b == ambiguous = ambiguous
    | (a == unknown && b == notunknown) || (b == unknown && a == notunknown) = unknown
    | otherwise = unknown
  t_not a
    | a == true = false
    | a == false = true
    | a == unknown = notunknown
    | a == notunknown = unknown
    | otherwise = a
  a @^ b = (a @| b) @+ t_not (a @+ b)

instance Trilean State where
    true = Known True
    false = Known False
    unknown = Unknown True
    notunknown = Unknown False
    ambiguous = Ambiguous

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)


