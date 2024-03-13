module FizzBuzz where

import Data.List

fizzbuzz :: Int -> String
fizzbuzz n = defNumero n

menorQue20 :: Int -> String
menorQue20 n
    | n >= 0 && n < 20 =
        let answers = words ("cero uno dos tres cuatro cinco seis siete ocho nueve diez " ++
                             "once doce trece catorce quince dieciseis " ++
                             "diecisiete dieciocho diecinueve")
        in answers !! (n)

decenas :: Int -> String
decenas n
    | n == 20 = "veinte"
    | n < 30  =
        answers !! 0 ++ "" ++ menorQue20 (mod n 10)
    | n >= 30 && n < 100 && mod n 10 == 0 =
        answers !! (div n 10 - 2)
    | otherwise =
        answers !! (div n 10 - 2) ++ " y " ++ menorQue20 (mod n 10)
    where
        answers = words ("veinti treinta cuarenta cincuenta sesenta setenta ochenta noventa")

centenas :: Int -> String
centenas n
    | n == 100 = "cien"
    | n > 100 && n <= 1000 && mod n 100 == 0 =
        answers !! (div n 100 - 1)
    | otherwise =
        answers !! (div n 100 - 1) ++ " " ++ defNumero (mod n 100)
    where
        answers = words ("ciento doscientos trescientos cuatrocientos quinientos seiscientos setecientos ochocientos novecientos mil")

defNumero :: Int -> String
defNumero n
    | n < 20 = menorQue20 n
    | n < 100 = decenas n
    | n <= 1000 = centenas n
    | otherwise = "Número fuera de rango"
