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