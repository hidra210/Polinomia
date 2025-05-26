{-# LANGUAGE InstanceSigs #-}

module Polinomios (
    Polinomio,
    polCero,
    esPolCero,
    grado,
    coefPral,
    restoPol,
    valor,
    sumaPol,
    pegarPol,
    derivar,
    multiplicarPol,
    mostrarPolinomio,
    -- Extras con Orden Superior
    PolinomioL(..),
    valorL,
    sumaPolL,
    gradoMax,
    crearPolinomioL,
    aPolinomioL,
    aPolinomio,
    -- Nuevas funcionalidades
    mostrarDivisionRuffini,
    dividirRuffini,
    buscarRaicesEnteras,
    factorizarPolinomio,
    mostrarFactorizacion,
    integrar
) where

import Data.List
import Data.Ratio (approxRational, numerator, denominator)

-- Representación algebraica

data Polinomio a = PolCero | ConsPol Int a (Polinomio a) deriving Eq

polCero :: Polinomio a
polCero = PolCero

esPolCero :: Num a => Polinomio a -> Bool
esPolCero PolCero = True
esPolCero _       = False

pegarPol :: (Num a, Eq a) => Int -> a -> Polinomio a -> Polinomio a
pegarPol _ 0 p = p
pegarPol n b PolCero = ConsPol n b PolCero
pegarPol n b (ConsPol m c p)
    | n > m     = ConsPol n b (ConsPol m c p)
    | n < m     = ConsPol m c (pegarPol n b p)
    | b+c == 0  = p
    | otherwise = ConsPol n (b+c) p

grado :: Polinomio a -> Int
grado PolCero = 0
grado (ConsPol n _ _) = n

coefPral :: Num a => Polinomio a -> a
coefPral PolCero = 0
coefPral (ConsPol _ b _) = b

restoPol :: Polinomio a -> Polinomio a
restoPol PolCero = PolCero
restoPol (ConsPol _ _ p) = p

valor :: Num a => Polinomio a -> a -> a
valor p c
    | esPolCero p = 0
    | otherwise   = b * c^n + valor r c
  where
    n = grado p
    b = coefPral p
    r = restoPol p

sumaPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
sumaPol p q
    | esPolCero p = q
    | esPolCero q = p
    | n1 > n2     = pegarPol n1 a1 (sumaPol r1 q)
    | n1 < n2     = pegarPol n2 a2 (sumaPol p r2)
    | a1+a2 /= 0  = pegarPol n1 (a1+a2) (sumaPol r1 r2)
    | otherwise   = sumaPol r1 r2
  where
    n1 = grado p
    a1 = coefPral p
    r1 = restoPol p
    n2 = grado q
    a2 = coefPral q
    r2 = restoPol q

derivar :: (Num a, Eq a) => Polinomio a -> Polinomio a
derivar PolCero = PolCero
derivar (ConsPol 0 _ _) = PolCero
derivar (ConsPol n b p) = pegarPol (n-1) (fromIntegral n * b) (derivar p)

multiplicarPol :: (Num a, Eq a) => Polinomio a -> Polinomio a -> Polinomio a
multiplicarPol PolCero _ = PolCero
multiplicarPol _ PolCero = PolCero
multiplicarPol p q = sumaPol (multiplicarTermino p q) (multiplicarPol (restoPol p) q)
  where
    multiplicarTermino PolCero _ = PolCero
    multiplicarTermino (ConsPol n b p') q' =
        sumaPol (foldr (\(m, c) acc -> pegarPol (n + m) (b * c) acc) PolCero (toLista q'))
                (multiplicarTermino p' q')

toLista :: Polinomio a -> [(Int, a)]
toLista PolCero = []
toLista (ConsPol n c p) = (n, c) : toLista p

mostrarNumero :: Double -> String
mostrarNumero d =
    let r = approxRational d 1e-6
        n = numerator r
        m = denominator r
    in if m == 1 then show n else "(" ++ show n ++ "/" ++ show m ++ ")"

instance Show (Polinomio Double) where
    show PolCero = "0"
    show (ConsPol 0 b PolCero) = mostrarNumero b
    show (ConsPol 0 b p) = mostrarNumero b ++ " + " ++ show p
    show (ConsPol 1 b PolCero) = mostrarNumero b ++ "*x"
    show (ConsPol 1 b p) = mostrarNumero b ++ "*x + " ++ show p
    show (ConsPol n 1.0 PolCero) = "x^" ++ show n
    show (ConsPol n 1.0 p) = "x^" ++ show n ++ " + " ++ show p
    show (ConsPol n (-1.0) PolCero) = "-x^" ++ show n
    show (ConsPol n (-1.0) p) = "-x^" ++ show n ++ " + " ++ show p
    show (ConsPol n b PolCero) = mostrarNumero b ++ "*x^" ++ show n
    show (ConsPol n b p) = mostrarNumero b ++ "*x^" ++ show n ++ " + " ++ show p

mostrarPolinomio :: Polinomio Double -> String
mostrarPolinomio p = replaceExponents (limpiarSignos (show p))
  where
    replaceExponents :: String -> String
    replaceExponents [] = []
    replaceExponents ('^':'0':xs) = "⁰" ++ replaceExponents xs
    replaceExponents ('^':'1':xs) = "¹" ++ replaceExponents xs
    replaceExponents ('^':'2':xs) = "²" ++ replaceExponents xs
    replaceExponents ('^':'3':xs) = "³" ++ replaceExponents xs
    replaceExponents ('^':'4':xs) = "⁴" ++ replaceExponents xs
    replaceExponents ('^':'5':xs) = "⁵" ++ replaceExponents xs
    replaceExponents ('^':'6':xs) = "⁶" ++ replaceExponents xs
    replaceExponents ('^':'7':xs) = "⁷" ++ replaceExponents xs
    replaceExponents ('^':'8':xs) = "⁸" ++ replaceExponents xs
    replaceExponents ('^':'9':xs) = "⁹" ++ replaceExponents xs
    replaceExponents ('*':xs) = "·" ++ replaceExponents xs  -- Cambia * por ·
    replaceExponents (x:xs) = x : replaceExponents xs

    -- Función limpiarSignos original sin cambios
    limpiarSignos [] = []
    -- Caso: "+ -" → " - "
    limpiarSignos ('+':' ':'-':xs) = " - " ++ limpiarSignos xs
    -- Caso: "- -" → " + "
    limpiarSignos ('-':' ':'-':xs) = " + " ++ limpiarSignos xs
    -- Caso: "- 5" (número negativo) → mantener igual
    limpiarSignos ('-':' ':x:xs) | isDigit x = " - " ++ x : limpiarSignos xs
    -- Caso: "+ 5" (número positivo) → mantener igual
    limpiarSignos ('+':' ':x:xs) | isDigit x = " + " ++ x : limpiarSignos xs
    -- Caso general
    limpiarSignos (x:xs) = x : limpiarSignos xs
    
    isDigit c = c >= '0' && c <= '9' || c == '.'  -- Incluye punto para decimales

-- =============================================
-- Representación por listas y orden superior
-- =============================================

data PolinomioL a = Pol [a] deriving (Eq, Show)

valorL :: Num a => PolinomioL a -> a -> a
valorL (Pol cs) x = foldr (\c acc -> c + x * acc) 0 cs

sumaPolL :: Num a => PolinomioL a -> PolinomioL a -> PolinomioL a
sumaPolL (Pol l1) (Pol l2) = Pol (zipWith (+) (ext l1) (ext l2))
  where
    maxLen = max (length l1) (length l2)
    ext l = take maxLen (l ++ repeat 0)

gradoMax :: (Eq a, Num a) => PolinomioL a -> Int
gradoMax (Pol cs) = length (dropWhile (== 0) (reverse cs)) - 1

crearPolinomioL :: (Num a, Eq a) => [a] -> Polinomio a
crearPolinomioL cs = foldr (\(i, c) acc -> pegarPol i c acc) PolCero (zip [n,n-1..0] cs)
  where n = length cs - 1

aPolinomioL :: (Num a, Eq a) => Polinomio a -> PolinomioL a
aPolinomioL p = Pol (coefList p)
  where
    coefList PolCero = []
    coefList (ConsPol _ c p') = c : coefList p'

aPolinomio :: (Num a, Eq a) => PolinomioL a -> Polinomio a
aPolinomio (Pol []) = PolCero
aPolinomio (Pol xs) = crearPolinomioL xs

dividirRuffini :: (Fractional a, Eq a) => Polinomio a -> a -> (Polinomio a, a)
dividirRuffini p r = (crearPolinomioL (init qs), last qs)
  where
    Pol coefs = aPolinomioL p
    qs = scanl (\q a -> a + q * r) 0 coefs

mostrarDivisionRuffini :: Polinomio Double -> Double -> String
mostrarDivisionRuffini p r =
    let (cociente, resto) = dividirRuffini p r
    in "División de " ++ mostrarPolinomio p ++ " por (x - " ++ mostrarNumero r ++ "):\n" ++
       "Cociente: " ++ mostrarPolinomio cociente ++ "\n" ++
       "Resto: " ++ mostrarNumero resto

buscarRaicesEnteras :: Polinomio Double -> [Double]
buscarRaicesEnteras p = 
    let posibles = [-10.0, -9.0 .. 10.0]
    in filter (\x -> abs (valor p x) < 1e-6) posibles

factorizarPolinomio :: Polinomio Double -> ([Double], Polinomio Double)
factorizarPolinomio p = factorizarAux p []
  where
    posiblesRaices = posiblesRaicesDe p
    factorizarAux q rs
      | esPolCero q = (rs, PolCero)
      | grado q == 0 = (rs, q)
      | otherwise =
          case find (\r -> abs (valor q r) < 1e-6) posiblesRaices of
            Just r -> let (q', _) = dividirRuffini q r
                      in factorizarAux q' (rs ++ [r])
            Nothing -> (rs, q)

posiblesRaicesDe :: Polinomio Double -> [Double]
posiblesRaicesDe p = 
    let Pol coefs = aPolinomioL p
        a0 = last coefs
        an = head coefs
        factoresA0 = if a0 == 0 then [] else factores (numerator (approxRational a0 1e-6))
        factoresAn = if an == 0 then [] else factores (numerator (approxRational an 1e-6))
        candidatos = nub [fromIntegral p / fromIntegral q | p <- factoresA0, q <- factoresAn]
    in (candidatos ++ map negate candidatos) \\ [0]
  where
    factores n = filter (\x -> n `mod` x == 0) [1..abs n]

mostrarFactorizacion :: Polinomio Double -> String
mostrarFactorizacion p =
    let (raices, resto) = factorizarPolinomio p
        -- Formatear cada raíz correctamente
        factores = concatMap (\r -> 
                    let signo = if r < 0 then "+" else "-"
                        abs_r = abs r
                    in "(x " ++ signo ++ " " ++ mostrarNumero abs_r ++ ")"
                  ) raices
        restoLegible = mostrarPolinomio resto
        restoStr = if esPolCero resto || restoLegible `elem` ["1", "1.0"]
                  then ""
                  else " * (" ++ restoLegible ++ ")"
    in if null raices 
       then mostrarPolinomio p 
       else factores ++ restoStr

integrar :: (Fractional a, Eq a) => Polinomio a -> Polinomio a
integrar PolCero = PolCero
integrar (ConsPol n b p) = pegarPol (n+1) (b / fromIntegral (n+1)) (integrar p)