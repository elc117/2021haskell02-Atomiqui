-- Prática 02 de Haskell
-- Nome: Alisson Costa Schmidt

import Funcoes

comFebre :: [Float] -> [Float]
comFebre fbr = filter febre fbr

comFebre' :: [Float] -> [Float]
comFebre' fbr = filter (\x -> x >= 37.8) fbr

itemize :: [String] -> [String]
itemize str = map (\ x -> "<li>"++ x ++ "</ li>") str

bigCircles :: Float -> [Float] -> [Float]
bigCircles tam lst = filter (\x -> x^2 * pi > tam) lst

quarentena :: [(String,Float)] -> [(String,Float)]
quarentena temp = filter (\ (_, temp) -> febre temp) temp

idadesEm :: [Int] -> Int -> [Int]
idadesEm lst ref = map (\ x -> ref - x) lst

changeNames :: [String] -> [String]
changeNames nome = map (\ nome -> if(head(nome) == 'A') then "Super " ++ nome else nome) nome

onlyShorts :: [String] -> [String]
onlyShorts lst = filter (\ x -> length(x) < 5) lst