{-# LANGUAGE OverloadedStrings #-}

module Main where

import Polinomios
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

readDoubles :: [String] -> [Double]
readDoubles = map (fromMaybe 0 . readMaybe)

main :: IO ()
main = do
    lineCoeffs <- getLine
    lineOperacion <- getLine
    
    let coefStrs = words lineCoeffs
        coefs = readDoubles coefStrs
        p = crearPolinomioL (reverse coefs)
        operacion = head (words lineOperacion)
    
    putStrLn $ "Polinomio p: " ++ mostrarPolinomio p
    putStrLn $ "Grado: " ++ show (grado p)

    case operacion of
        "raices" -> do
            let raices = buscarRaicesEnteras p
            putStrLn $ "Raices enteras: " ++ show (map (round :: Double -> Int) raices)
            putStrLn $ "Factorizado: " ++ mostrarFactorizacion p
        
        "dividir" -> do
            lineX <- getLine
            let divisorD = fromMaybe 0 (readMaybe lineX :: Maybe Double)
                (cociente, resto) = dividirRuffini p divisorD
            putStrLn $ "Dividir por Ruffini con (x - " ++ show divisorD ++ "):"
            putStrLn $ "Divisor usado: " ++ show divisorD                      
            putStrLn $ "Cociente: " ++ mostrarPolinomio cociente
            putStrLn $ "Resto: " ++ show resto

        "derivar" -> do
            let derivada = derivar p
            putStrLn $ "Derivada: " ++ mostrarPolinomio derivada
        
        "integrar" -> do
            let integral = integrar p
            putStrLn $ "Integral: " ++ mostrarPolinomio integral
        
        "factorizar" -> do
            putStrLn $ "Factorizar: " ++ mostrarFactorizacion p
        
        "evaluar" -> do
            lineX <- getLine
            let x = fromMaybe 0 (readMaybe lineX :: Maybe Double)
                resultado = valor p x
            putStrLn $ "Evaluar en x=" ++ show x ++ ": " ++ show resultado
        
        _ -> do
            putStrLn $ "Polinomio p: " ++ mostrarPolinomio p
            putStrLn "Operacion no reconocida"