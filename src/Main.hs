module Main (main) where

import Documento
import PPON
import Test.HUnit
import qualified Data.Type.Bool as parametro

main :: IO ()
main = runTestTTAndExit allTests

allTests :: Test
allTests =
  test
    [ "Ejercicio 2" ~: testsEj2,
      "Ejercicio 3" ~: testsEj3,
      "Ejercicio 4" ~: testsEj4,
      "Ejercicio 6" ~: testsEj6,
      "Ejercicio 7" ~: testsEj7,
      "Ejercicio 8" ~: testsEj8,
      "Ejercicio 9" ~: testsEj9
    ]

testsEj2 :: Test
testsEj2 =
  test
    [ vacio <+> vacio ~?= vacio,
      texto "a" <+> texto "b" ~?= texto "ab",
      (texto "a" <+> linea) <+> texto "b" ~?= texto "a" <+> (linea <+> texto "b")
      -- test propios
      -- por como funciona el (<+>) solo importa el ultimo elemento antes de Vacio con respecto al primer parametro y el primer elemento del segundo parametro.
      -- lo que venga antes  y despues de esto no imorta ya que no se tocan.
      -- entonces, hay 6 casos por ver
      -- Texto <+> Vacio
      texto "a" <+> vacio ~?= texto "a",
      -- Vacio <+> Texto
      vacio <+> texto "a" ~?= texto "a",
      -- Texto <+> Linea 
      --Linea  <+> Texto
      --Linea  <+> Linea
      --Texto  <+> Texto
    ]

testsEj3 :: Test
testsEj3 =
  test
    [ indentar 2 vacio ~?= vacio,
      indentar 2 (texto "a") ~?= texto "a",
      indentar 2 (texto "a" <+> linea <+> texto "b") ~?= texto "a" <+> indentar 2 (linea <+> texto "b"),
      indentar 2 (linea <+> texto "a") ~?= indentar 1 (indentar 1 (linea <+> texto "a")),
      --tests propios siguen
      indentar 0 (texto "a" <+> linea <+> texto "b") ~?= texto "a" <+> linea <+> texto "b",
      indentar 3 (linea <+> linea <+> texto "b") ~?= indentar 1 (indentar 2 (linea <+> linea <+> texto "b")),
      indentar 3 (linea <+> linea <+> texto "b") ~?= indentar 2 (indentar 1 (linea <+> linea <+> texto "b")),
      indentar 1 (indentar 1 (indentar 1 (texto "a" <+> linea <+> linea <+> texto "b"))) ~?= indentar 3 (texto "a" <+> linea <+> linea <+> texto "b")
    ]

testsEj4 :: Test
testsEj4 =
  test
    [ mostrar vacio ~?= "",
      mostrar linea ~?= "\n",
      mostrar (indentar 2 (texto "a" <+> linea <+> texto "b")) ~?= "a\n  b"
    ]

pericles, merlina, addams, familias :: PPON
pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
merlina = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]
addams = ObjetoPP [("0", pericles), ("1", merlina)]
familias = ObjetoPP [("Addams", addams)]
--ppons de test propios
nivel1, nivel2, nivel3, nivel4 :: PPON
nivel1 = ObjetoPP [("info", TextoPP "Nivel 1"), ("siguiente", nivel2)]
nivel2 = ObjetoPP [("info", TextoPP "Nivel 2"), ("siguiente", nivel3)]
nivel3 = ObjetoPP [("info", TextoPP "Nivel 3"), ("siguiente", nivel4)]
nivel4 = ObjetoPP [("info", TextoPP "Nivel 4")]

pponAnidado :: Int -> PPON
pponAnidado 0 = TextoPP "Nivel 0 Fin"
pponAnidado n = ObjetoPP [("Nivel " ++ show n, pponAnidado (n-1))]

testsEj6 :: Test
testsEj6 =
  test
    [ pponObjetoSimple pericles ~?= True,
      pponObjetoSimple addams ~?= False
    ]

a, b, c, l:: Doc
a = texto "a"
b = texto "b"
c = texto "c"
l = linea

testsEj7 :: Test
testsEj7 =
  test
    [ mostrar (intercalar (texto ", ") []) ~?= "",
      mostrar (intercalar (texto ", ") [a, b, c]) ~?= "a, b, c",
      mostrar (entreLlaves []) ~?= "{ }",
      mostrar (entreLlaves [a, b, c]) ~?= "{\n  a,\n  b,\n  c\n}",
      --tests propios siguen
      intercalar (vacio) [a, b, c] ~?= texto "abc",
      intercalar a [b, l, c] ~?= texto "ba" <+> l <+> a <+> c,
      intercalar l [a,b,c] ~?= a <+> l <+> b <+> l <+> c,
      intercalar l [a, b, l, c] ~?= a <+> l <+> intercalar l [b, l, c],
      intercalar (c <+> l) [a,vacio,b,vacio] ~?= a <+> c <+> l <+> vacio <+> c <+> l <+> b <+> c <+> l <+> vacio
    ]

testsEj8 :: Test
testsEj8 =
  test
    [ mostrar (aplanar (a <+> linea <+> b <+> linea <+> c)) ~?= "a b c"
    ]

testsEj9 :: Test
testsEj9 =
  test
    [ mostrar (pponADoc pericles) ~?= "{ \"nombre\": \"Pericles\", \"edad\": 30 }",
      mostrar (pponADoc addams) ~?= "{\n  \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n  \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n}",
      mostrar (pponADoc familias) ~?= "{\n  \"Addams\": {\n    \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n    \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n  }\n}",
      --tests propios siguen:
      mostrar (pponADoc nivel3) ~?= "{\n  \"info\": \"Nivel 3\",\n  \"siguiente\": { \"info\": \"Nivel 4\" }\n}",
      mostrar (pponADoc nivel1) ~?= "{\n  \"info\": \"Nivel 1\",\n  \"siguiente\": {\n    \"info\": \"Nivel 2\",\n    \"siguiente\": {\n      \"info\": \"Nivel 3\",\n      \"siguiente\": { \"info\": \"Nivel 4\" }\n    }\n  }\n}"
    ]
--nivel1, nivel2, nivel3, nivel4 :: PPON
--nivel1 = ObjetoPP [("info", TextoPP "Nivel 1"), ("siguiente", nivel2)]
--nivel2 = ObjetoPP [("info", TextoPP "Nivel 2"), ("siguiente", nivel3)]
--nivel3 = ObjetoPP [("info", TextoPP "Nivel 3"), ("siguiente", nivel4)]
--nivel4 = ObjetoPP [("info", TextoPP "Nivel 4")]

--pponAnidado :: Int -> PPON
--pponAnidado 0 = TextoPP "Nivel 0 Fin"
--pponAnidado n = ObjetoPP [("Nivel " ++ show n, pponAnidado (n-1))]

--testsllavesSimples = 
  --test 
    --[ 
    --]
