module FizzBuzz where 

    fizzbuzz :: Int -> String
    fizzbuzz n  = defNumero n
    --  | n `mod` 3 == 0 && n `mod` 5 == 0     = "fizzbuzz"
    --  | otherwise                            = defNumero n
 

    menorQue20 :: Int -> String
    menorQue20 n 
        | n > 0 && n < 20 =
        let answers = words("uno dos tres cuatro cinco seis siete ocho nueve diez " ++   
                            "once doce trece catorce quince diesciseis " ++ 
                            "diescisiente diesciocho diescinueve")
        in answers !! (n-1) 

    decenas :: Int -> String 
    decenas n
        | n == 20 = "veninte"
        | n >= 2 && n < 10 =
            answers !! (n - 2) ++ " y " ++ menorQue20 (n `mod` 10)
            where 
                answers = words ("veinti treinta cuarenta cincuenta sesenta setenta ochenta noventa")

    centenas :: Int -> String
    centenas n
        | n == 100 = "cien"
        | n >= 1 && n <=10 =
            answers !! (n - 2) 
            where 
                answers = words ("ciento doscentenas trescentenas cuatrocentenas quinientos seiscentenas setecentenas ochocentenas novecentenas mil")

    defNumero :: Int -> String
    defNumero n
        | 1 <= n && n < 20                 = menorQue20 n
        | n < 100                           = decenas n
        -- | n > 100             = centenas (n `div` 10) ++ " " ++ menorQue20 (n `mod` 10)
        -- | n == 100                          = "one hundred"