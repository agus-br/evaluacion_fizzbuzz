module FizzBuzz where

import Data.List

-- Función que llama a las demás funciones
fizzbuzz :: Int -> String
fizzbuzz n  
    | esPrimo n  == True = "FizzBuzz!" -- Si el número es primo devuelve "FizzBuzz!"
    | otherwise = nombrar n -- si no es primo llama a la función nombrar que devuelve el nombre del número

--Identificar si un numero es primo true/false
esPrimo :: Int -> Bool
esPrimo n
    | n <= 1 = False  -- Los números menores o iguales a 1 no son primos
    | otherwise = not $ any (\x -> n `mod` x == 0) [2..intSqrt n] -- Cualquier otro caso identifica si en la lista de los números entre 2 y la raiz cuadrada entera del núemro ingresado existe alguno que sea divisor del número. En caso de que exista devuelve True
    where
        -- Raíz cuadrada entera
        intSqrt :: Int -> Int 
        intSqrt = floor . sqrt . fromIntegral -- Devuelve la raiz cuadrada entera de un número

-- Nombra los números menores que 20 
menorQue20 :: Int -> String
menorQue20 n
    | n >= 0 && n < 20 =
        -- Lista que contiene todos los posibes nombres
        let answers = words ("cero uno dos tres cuatro cinco seis siete ocho nueve diez " ++
                             "once doce trece catorce quince dieciseis " ++
                             "diecisiete dieciocho diecinueve")
        in answers !! (n) -- Devuelve el elemento de la lista de nombres en la posición del número ingresado

-- Nombra los números entre 20 y 99
decenas :: Int -> String
decenas n
    | n == 20 = "veinte" -- Caso de que sea 20
    | n < 30  =
        answers !! 0 ++ "" ++ menorQue20 (mod n 10) -- Casos menores que 30 pero mayores a 20
    | n >= 30 && n < 100 && mod n 10 == 0 =
        answers !! (div n 10 - 2) -- Números divisibles por 10
    | otherwise =
        answers !! (div n 10 - 2) ++ " y " ++ menorQue20 (mod n 10) -- Si no es divisible por 10 significa que susu unidades no son 0 
        -- Se concatena menorQue20 para nombrar con número entre 1 y 9
    where
        answers = words ("veinti treinta cuarenta cincuenta sesenta setenta ochenta noventa") -- Lista de posibles respuestas de decenas

-- Nombra todos los núemros entre 100 y 999
centenas :: Int -> String
centenas n
    | n == 100 = "cien" -- Caso específico del 100
    | n > 100 && n < 1000 && mod n 100 == 0 =
        answers !! (div n 100 - 1) -- centenas divisibles por 100 sin residuo
    | otherwise =
        answers !! (div n 100 - 1) ++ " " ++ nombrar (mod n 100) -- se toma el primer dígito para obtener su centena y se llama de nuevo 
        -- a nombrar para que evalúe el residuo de dividir por 100.
    where
        -- Lista de probables casos para las centenas
        answers = words ("ciento doscientos trescientos cuatrocientos quinientos seiscientos setecientos ochocientos novecientos")

-- Nombra todos los núemros entre 1000 y 999999
miles :: Int -> String
miles n
    | n == 1000 = "mil" -- caso particular del mil
    | n > 1000 && n < 2000 = 
        "mil " ++ nombrar (mod n 1000) -- No hay "uno mil" simplemente "mil". este caso evalua el caso de numeros entre 1001 y 1999
    | n > 1000 && n < 1000000 =
        nombrar (div n 1000) ++ " mil " ++ nombrar (mod n 1000) -- Todos los demas casos simplemente crea una pila de llamadas que nombras cada parte por separado, la primera parte nombra todos los millares ya que son núemros ya soportados pero con el sufijo mil. 
        -- La segunda parte toma todo lo que no es mil y lo nombra

-- Evalúa el rango de números para determinar que sistema de nombramiento es el adecuado
nombrar :: Int -> String
nombrar n
    | n < 20 = menorQue20 n -- Para números menores que 20
    | n < 100 = decenas n -- De 20 a 99
    | n < 1000 = centenas n -- De 100 a 999
    | n < 1000000 = miles n -- De 1000 a 999999
    | otherwise = "un millon" --  Caso partcular de un millón
