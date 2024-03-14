module Main where

import Test.Tasty
import Test.Tasty.HUnit
import FizzBuzz

fizzBuzzSuite :: TestTree
fizzBuzzSuite = testGroup "FizzBuzz tests"
                [ testGroup "Pruba de numeros entre 0 y 30" $
                    [ testCase "0" $ menorQue20 0 @?= "cero"
                    , testCase "1" $ menorQue20 1 @?= "uno"
                    , testCase "2" $ menorQue20 2 @?= "dos"
                    , testCase "3" $ menorQue20 3 @?= "tres"
                    , testCase "4" $ menorQue20 4 @?= "cuatro"
                    , testCase "5" $ menorQue20 5 @?= "cinco"
                    , testCase "6" $ menorQue20 6 @?= "seis"
                    , testCase "7" $ menorQue20 7 @?= "siete"
                    , testCase "8" $ menorQue20 8 @?= "ocho"
                    , testCase "9" $ menorQue20 9 @?= "nueve"
                    , testCase "10" $ menorQue20 10 @?= "diez"
                    , testCase "11" $ menorQue20 11 @?= "once"
                    , testCase "12" $ menorQue20 12 @?= "doce"
                    , testCase "13" $ menorQue20 13 @?= "trece"
                    , testCase "14" $ menorQue20 14 @?= "catorce"
                    , testCase "15" $ menorQue20 15 @?= "quince"
                    , testCase "20" $ decenas 20 @?= "veinte"
                    , testCase "30" $ decenas 30 @?= "treinta"
                    ]
                , testGroup "Prueba de numeros entre 16 y 29" $
                    [ testCase "16" $ menorQue20 16 @?= "dieciseis"
                    , testCase "17" $ menorQue20 17 @?= "diecisiete"
                    , testCase "18" $ menorQue20 18 @?= "dieciocho"
                    , testCase "20" $ decenas 20 @?= "veinte"
                    , testCase "21" $ decenas 21 @?= "veintiuno"
                    , testCase "24" $ decenas 24 @?= "veinticuatro"
                    , testCase "26" $ decenas 26 @?= "veintiseis"
                    , testCase "27" $ decenas 27 @?= "veintisiete"
                    , testCase "28" $ decenas 28 @?= "veintiocho"
                    , testCase "29" $ decenas 29 @?= "veintinueve"
                    ]
                , testGroup "Prueba de numeros entre 30 y 100" $
                    [ testCase "30" $ decenas 30 @?= "treinta"
                    , testCase "31" $ decenas 31 @?= "treinta y uno"
                    , testCase "37" $ decenas 37 @?= "treinta y siete"
                    , testCase "40" $ decenas 40 @?= "cuarenta"
                    , testCase "50" $ decenas 50 @?= "cincuenta"
                    , testCase "60" $ decenas 60 @?= "sesenta"
                    , testCase "70" $ decenas 70 @?= "setenta"
                    , testCase "80" $ decenas 80 @?= "ochenta"
                    , testCase "90" $ decenas 90 @?= "noventa"
                    , testCase "100" $ centenas 100 @?= "cien"
                    ]
                , testGroup "Prueba de numeros entre 101 y 999" $
                    [ testCase "200" $ centenas 200 @?= "doscientos"
                    , testCase "300" $ centenas 300 @?= "trescientos"
                    , testCase "400" $ centenas 400 @?= "cuatrocientos"
                    , testCase "500" $ centenas 500 @?= "quinientos"
                    , testCase "600" $ centenas 600 @?= "seiscientos"
                    , testCase "700" $ centenas 700 @?= "setecientos"
                    , testCase "800" $ centenas 800 @?= "ochocientos"
                    , testCase "900" $ centenas 900 @?= "novecientos"
                    , testCase "426" $ centenas 426 @?= "cuatrocientos veintiseis"
                    , testCase "109" $ centenas 109 @?= "ciento nueve"
                    ]
                , testGroup "Pruebas con 1,000 y 999,999"$
                    [ testCase "1000" $ fizzbuzz 1000 @?= "mil"
                    , testCase "1952" $ fizzbuzz 1952 @?= "mil novecientos cincuenta y dos"
                    , testCase "1150" $ fizzbuzz 1150 @?= "mil ciento cincuenta"
                    , testCase "7864" $ fizzbuzz 7864 @?= "siete mil ochocientos sesenta y cuatro"
                    , testCase "435721" $ fizzbuzz 435721 @?= "cuatrocientos treinta y cinco mil setecientos veintiuno"
                    , testCase "999999" $ fizzbuzz 999999 @?= "novecientos noventa y nueve mil novecientos noventa y nueve"
                    , testCase "27777" $ fizzbuzz 27777 @?= "veintisiete mil setecientos setenta y siete"
                    , testCase "32101" $ fizzbuzz 32101 @?= "treinta y dos mil ciento uno"
                    , testCase "56982" $ fizzbuzz 56982 @?= "cincuenta y seis mil novecientos ochenta y dos"
                    , testCase "765432" $ fizzbuzz 765432 @?= "setecientos sesenta y cinco mil cuatrocientos treinta y dos"
                    , testCase "888888" $ fizzbuzz 888888 @?= "ochocientos ochenta y ocho mil ochocientos ochenta y ocho"
                    ]
                , testGroup "Pruebas con cifras de 1,000,000" 
                    [
                      testCase "1000000" $ fizzbuzz 1000000 @?= "un millon"
                    ]
                ]

main = defaultMain fizzBuzzSuite
