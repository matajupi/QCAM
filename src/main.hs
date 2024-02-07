import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe

data DaggerType = DTAtomic String
                | DTTensor DaggerType DaggerType
                | DTExp DaggerType DaggerType
                deriving (Eq, Ord, Read, Show)

type DaggerIdentifier = String

-- TODO: Lambdaのバインドにパターンマッチングを導入
-- TODO: Constantを導入
data DaggerLambda = DLVar DaggerIdentifier
                  | DLTensor DaggerLambda DaggerLambda
                  | DLLambda DaggerIdentifier DaggerType DaggerLambda
                  | DLApp DaggerLambda DaggerLambda
                  deriving (Eq, Ord, Read, Show)

data DaggerCombinator = DCId DaggerType
                      | DCTensor DaggerCombinator DaggerCombinator
                      | DCComp DaggerCombinator DaggerCombinator
                      | DCCur DaggerCombinator
                      | DCEval
                      deriving (Eq, Ord, Read, Show)

type DaggerContext = M.Map String DaggerType

enumerateVars :: DaggerLambda -> S.Set String
enumerateVars (DLVar ident) =
    S.singleton ident
enumerateVars (DLTensor term1 term2) =
    S.union (enumerateVars term1) (enumerateVars term2)
-- 本来Binding variableがContextにあることはないはずだが、Lambdaは
-- NegationとTensorのAbbreviationということなのでとりあえず追加している
enumerateVars (DLLambda ident tp term) =
    S.insert ident (enumerateVars term)
enumerateVars (DLApp term1 term2) =
    S.union (enumerateVars term1) (enumerateVars term2)

filterContext :: DaggerLambda -> DaggerContext -> DaggerContext
filterContext term context =
    let vars = enumerateVars term in
        M.filterWithKey (\v _ -> S.member v vars) context

compile' :: DaggerLambda -> DaggerContext -> Maybe DaggerCombinator
compile' (DLVar ident) context
    | M.size context == 1 =
        do  tp <- M.lookup ident context
            Just (DCId tp)
    | otherwise =
        Nothing
compile' (DLTensor term1 term2) context
    | M.intersection context1 context2 == M.empty =
        do  comb1 <- compile' term1 context1
            comb2 <- compile' term2 context2
            Just (DCTensor comb1 comb2)
    | otherwise =
        Nothing
    where
        context1 = filterContext term1 context
        context2 = filterContext term2 context
-- Pattern matchが導入されたら、identをTensor Leftする必要がある
compile' (DLLambda ident tp term) context =
    do  comb <- compile' term (M.insert ident tp context)
        Just (DCCur comb)
compile' (DLApp term1 term2) context
    | M.intersection context context2 == M.empty =
        do  comb1 <- compile' term1 context1
            comb2 <- compile' term2 context2
            Just (DCComp DCEval (DCTensor comb1 comb2))
    | otherwise =
        Nothing
    where
        context1 = filterContext term1 context
        context2 = filterContext term2 context
-- ContextをMapで管理しているため、Exchangeは不要
-- TODO: Cutは今の所いらない？（Idでは無限にCutできてしまう気がする）

compile :: DaggerLambda -> Maybe DaggerCombinator
compile dl = compile' dl M.empty

-- TODO: 書きにくいからAbbreviationを作りたい
main :: IO ()
main = do
    program <- readLn :: IO DaggerLambda
    print $ compile program

