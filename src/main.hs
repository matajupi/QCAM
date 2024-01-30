import Data.Set as Set
import Data.Maybe

data Term = Var String | Negate Term | Tensor Term Term | Const String
    deriving (Eq, Ord, Read, Show)
type Context = [Term]
type Soup = [(Term, Term)]
type DaggerLambda = (Context, Soup, Term)

data Combinator = Id | Alpha | Cur Combinator | Tensor Combinator Combinator
    | Cut Combinator Combinator deriving (Eq, Ord, Read, Show)

type VCPair = (Set.Set DaggerLambda, Set.Set Combinator)

tryId :: DaggerLambda -> VCPair -> Maybe VCPair
tryId sequent vc

tryNegation :: DaggerLambda -> VCPair -> Maybe VCPair
tryTensorR :: DaggerLambda -> VCPair -> Maybe VCPair
tryTensorL :: DaggerLambda -> VCPair -> Maybe VCPair
tryUntensorL :: DaggerLambda -> VCPair -> Maybe VCPair
tryCut :: DaggerLambda -> VCPair -> Maybe VCPair
tryCurry :: DaggerLambda -> VCPair -> Maybe VCPair
tryUncurry :: DaggerLambda -> VCPair -> Maybe VCPair

compile' :: DaggerLambda -> VCPair -> VCPair
compile' sequent (visited, combinators)
    | Set.member sequent visited = (visited, combinators)
    | otherwise = (visited', combinators')
    where
        (visited', combinators') = foldl updateVC (Set.insert sequent visited, combinators) [
            tryId, tryNegaion, tryTensorR, tryTensorL, tryUntensorL,
            tryCut, tryCurry, tryUnCurry
        ]
        updateVC :: (DaggerLambda -> VCPair -> Maybe VCPair) -> VCPair -> VCPair
        updateVC func vc = fromMaybe vc $ func sequent vc

compile :: DaggerLambda -> Maybe Combinator
compile sequent = listToMaybe combinators
    where (visited, combinators) = compile' sequent Set.empty, Set.empty

main :: IO ()
main = do

