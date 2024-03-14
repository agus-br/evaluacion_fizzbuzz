module Main where

import Test.Tasty
import Test.Tasty.HUnit
import FizzBuzz

fizzBuzzSuite :: TestTree
fizzBuzzSuite = testGroup "FizzBuzz tests"
                [ testGroup "Pruba de números entre 0 y 30" $
                    [ testCase "0" $ menorQue20 0 @?= "cero"
                    , testCase "1" $ menorQue20 2 @?= "uno"
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
                , testGroup "Prueba de números entre 16 y 29" $
                    [ testCase "16" $ menorQue20 16 @?= "diesciseis"
                    , testCase "17" $ menorQue20 17 @?= "diescisiete"
                    , testCase "18" $ menorQue20 18 @?= "diesciocho"
                    , testCase "20" $ decenas 20 @?= "veinte"
                    , testCase "21" $ decenas 21 @?= "ventiuno"
                    , testCase "24" $ decenas 24 @?= "veinticuatro"
                    , testCase "26" $ decenas 26 @?= "veintiseis"
                    , testCase "27" $ decenas 27 @?= "veintisiete"
                    , testCase "28" $ decenas 28 @?= "veintiocho"
                    , testCase "29" $ decenas 29 @?= "veintinueve"
                    ]
                , testGroup "Prueba de números entre 30 y 100" $
                    [ testCase "30" $ decenas 30 @?= "treinta"
                    , testCase "31" $ decenas 31 @?= "treinta y uno"
                    , testCase "37" $ decenas 37 @?= "treinta y siete"
                    , testCase "40" $ decenas 40 @?= "cuarenta"
                    , testCase "50" $ decenas 21 @?= "cincuenta"
                    , testCase "60" $ decenas 24 @?= "sesenta"
                    , testCase "70" $ decenas 26 @?= "setenta"
                    , testCase "80" $ decenas 27 @?= "ochenta"
                    , testCase "90" $ decenas 28 @?= "noventa"
                    , testCase "100" $ centenas 100 @?= "cien"
                    ]
                , testGroup "Prueba de números entre 101 y 999" $
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
                ]

main = defaultMain fizzBuzzSuite