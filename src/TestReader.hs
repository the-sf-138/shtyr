module TestReader
    ( readTest
    ) where

import Text.Read

data Operator = Plus | Minus deriving Show

data Token = OperatorToken Operator | ValueToken Int deriving Show

data TestTree = Leaf Int
        | Node Operator TestTree TestTree deriving Show


readTest :: IO ()
readTest = putStrLn "readingJson"


convertStringToToken :: String -> Maybe Token  
convertStringToToken "+" = Just $ OperatorToken Plus
convertStringToToken "-" = Just $ OperatorToken Minus
convertStringToToken s   = (readMaybe s::Maybe Int) >>= (\ x -> Just $ ValueToken x)

-- left associative
buildTree :: [Maybe Token] -> TestTree -> Maybe TestTree
buildTree (Nothing:xs) ph = Nothing
buildTree (Just (OperatorToken o):Just (ValueToken v):xs) t = buildTree xs $ Node o t (Leaf v)
buildTree [] t = Just t
buildTree ph ph2 = Nothing

convertTokensToTree :: [Maybe Token] -> Maybe TestTree
convertTokensToTree (Just (ValueToken v):ts) = buildTree ts (Leaf v)
convertTokensToTree ph                       = Nothing

convertStringToTree :: String -> Maybe TestTree
convertStringToTree s = convertTokensToTree (map convertStringToToken $ words s)
