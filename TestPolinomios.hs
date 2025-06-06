{-# LANGUAGE OverloadedStrings #-}

module Main where

import Polinomios
import System.IO (hSetEncoding, stdout, utf8)

main :: IO ()
main = do
    hSetEncoding stdout utf8

    putStrLn "===================================="
    putStrLn "   PRUEBAS DE OPERACIONES BÁSICAS   "
    putStrLn "===================================="

    -- Polinomios de prueba
    let p0     = crearPolinomioL [0]              -- 0
        p1     = crearPolinomioL [1, 2, 1]         -- x² + 2x + 1
        p2     = crearPolinomioL [1, 0, 1, 0]      -- x³ + x
        p3     = crearPolinomioL [1, 0, -1]        -- x² - 1
        p4     = crearPolinomioL [1, -5, 6]        -- x² - 5x + 6
        p5     = crearPolinomioL [1, 0, 0, 0]      -- x³
        p6     = crearPolinomioL [3, 0, 0]         -- 3x²
        pConst = crearPolinomioL [5]               -- 5

    putStrLn "\nPolinomios de prueba:"
    mapM_ putStrLn
      [ "p0     = " ++ mostrarPolinomio p0
      , "p1     = " ++ mostrarPolinomio p1
      , "p2     = " ++ mostrarPolinomio p2
      , "p3     = " ++ mostrarPolinomio p3
      , "p4     = " ++ mostrarPolinomio p4
      , "p5     = " ++ mostrarPolinomio p5
      , "p6     = " ++ mostrarPolinomio p6
      , "pConst = " ++ mostrarPolinomio pConst
      ]

    putStrLn "\n1. Suma de polinomios:"
    putStrLn $ "p1 + p3 = " ++ mostrarPolinomio (sumaPol p1 p3) ++ " (esperado: 2x² + 2x)"
    putStrLn $ "p3 + p4 = " ++ mostrarPolinomio (sumaPol p3 p4) ++ " (esperado: 2x² - 5x + 5)"
    putStrLn $ "p1 + p0 = " ++ mostrarPolinomio (sumaPol p1 p0) ++ " (esperado: x² + 2x + 1)"
    putStrLn $ "p5 + p6 = " ++ mostrarPolinomio (sumaPol p5 p6) ++ " (esperado: x³ + 3x²)"

    putStrLn "\n2. Multiplicación de polinomios:"
    putStrLn $ "p1 * p3 = " ++ mostrarPolinomio (multiplicarPol p1 p3) ++ " (esperado: x⁴ + 2x³ - 2x - 1)"
    putStrLn $ "p1 * p4 = " ++ mostrarPolinomio (multiplicarPol p1 p4) ++ " (esperado: x⁴ - 3x³ - 3x² + 7x + 6)"
    putStrLn $ "p1 * p0 = " ++ mostrarPolinomio (multiplicarPol p1 p0) ++ " (esperado: 0)"
    putStrLn $ "p1 * pConst = " ++ mostrarPolinomio (multiplicarPol p1 pConst) ++ " (esperado: 5x² + 10x + 5)"

    putStrLn "\n3. Derivadas:"
    putStrLn $ "derivada(p4)     = " ++ mostrarPolinomio (derivar p4) ++ " (esperado: 2x - 5)"
    putStrLn $ "derivada(pConst) = " ++ mostrarPolinomio (derivar pConst) ++ " (esperado: 0)"
    putStrLn $ "derivada(p2)     = " ++ mostrarPolinomio (derivar p2) ++ " (esperado: 3x² + 1)"
    putStrLn $ "derivada(p5)     = " ++ mostrarPolinomio (derivar p5) ++ " (esperado: 3x²)"
    putStrLn $ "derivada(p6)     = " ++ mostrarPolinomio (derivar p6) ++ " (esperado: 6x)"

    putStrLn "\n4. Evaluación:"
    putStrLn $ "p4(0) = " ++ show (valor p4 0) ++ " (esperado: 6.0)"
    putStrLn $ "p4(2) = " ++ show (valor p4 2) ++ " (esperado: 0.0)"
    putStrLn $ "p3(1) = " ++ show (valor p3 1) ++ " (esperado: 0.0)"
    putStrLn $ "p2(1) = " ++ show (valor p2 1) ++ " (esperado: 2.0)"
    putStrLn $ "p5(2) = " ++ show (valor p5 2) ++ " (esperado: 8.0)"
    putStrLn $ "p6(3) = " ++ show (valor p6 3) ++ " (esperado: 27.0)"

    putStrLn "\n5. División de Ruffini:"
    putStrLn $ mostrarDivisionRuffini p4 2 -- x - 2
    putStrLn $ mostrarDivisionRuffini p4 3 -- x - 3

    putStrLn "\n6. Factorización:"
    putStrLn $ "factorizar(p4) = " ++ mostrarFactorizacion p4 ++ " (esperado: (x - 2)(x - 3))"
    putStrLn $ "factorizar(p3) = " ++ mostrarFactorizacion p3 ++ " (esperado: (x + 1)(x - 1))"
    putStrLn $ "factorizar(p1) = " ++ mostrarFactorizacion p1 ++ " (esperado: (x + 1)²)"

    putStrLn "\n7. Integración:"
    putStrLn $ "∫p1 dx = " ++ mostrarPolinomio (integrar p1) ++ " (esperado: (1/3)x³ + x² + x)"
    putStrLn $ "∫p2 dx = " ++ mostrarPolinomio (integrar p2) ++ " (esperado: (1/4)x⁴ + (1/2)x²)"

    putStrLn "\n===================================="
    putStrLn "     TODAS LAS PRUEBAS COMPLETADAS  "
    putStrLn "===================================="
