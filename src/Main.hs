module Main (main) where

import Documento
import PPON
import Test.HUnit
--import qualified Data.Type.Bool as parametro

main :: IO ()
main = runTestTTAndExit allTests

allTests :: Test
allTests =
  test
    [ "Ejercicio 2" ~: testsEj2,
      "Ejercicio 3" ~: testsEj3,
      "Ejercicio 4" ~: testsEj4,
      "Ejercicio 5" ~: testsEj5,
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
      (texto "a" <+> linea) <+> texto "b" ~?= texto "a" <+> (linea <+> texto "b"),
      
      --  Test propios:     
      texto "a" <+> vacio ~?= texto "a",
      vacio <+> texto "a" ~?= texto "a"
    ]

testsEj3 :: Test
testsEj3 =
  test
    [ indentar 2 vacio ~?= vacio,
      indentar 2 (texto "a") ~?= texto "a",
      indentar 2 (texto "a" <+> linea <+> texto "b") ~?= texto "a" <+> indentar 2 (linea <+> texto "b"),
      indentar 2 (linea <+> texto "a") ~?= indentar 1 (indentar 1 (linea <+> texto "a")),
      -- Tests propios:
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
      mostrar (indentar 2 (texto "a" <+> linea <+> texto "b")) ~?= "a\n  b",
      mostrar (texto "a" <+> linea <+> linea <+> texto "b") ~?= "a\n\nb",
      mostrar (texto "a" <+> linea <+> indentar 3 (linea <+> texto "b")) ~?= "a\n\n   b",
      mostrar (texto "Hola" <+> linea <+> texto "Mundo" <+> linea <+> (texto "Hello" <+> linea <+> indentar 4 (linea <+> texto "World" ))  ) ~?= "Hola\nMundo\nHello\n\n    World"
    ]

pericles, merlina, addams, familias :: PPON
pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
merlina = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]
addams = ObjetoPP [("0", pericles), ("1", merlina)]
familias = ObjetoPP [("Addams", addams)]

-- PPONs de test propios:
nivel1, nivel2, nivel3, nivel4 :: PPON
nivel1 = ObjetoPP [("info", TextoPP "Nivel 1"), ("siguiente", nivel2)]
nivel2 = ObjetoPP [("info", TextoPP "Nivel 2"), ("siguiente", nivel3)]
nivel3 = ObjetoPP [("info", TextoPP "Nivel 3"), ("siguiente", nivel4)]
nivel4 = ObjetoPP [("info", TextoPP "Nivel 4")]

objetolistavacia :: PPON
objetolistavacia = ObjetoPP []

pponAnidado :: Int -> PPON
pponAnidado 0 = TextoPP "Nivel 0 Fin"
pponAnidado n = ObjetoPP [("Nivel " ++ show n, pponAnidado (n-1))]

testsEj5 :: Test
testsEj5 =
  test
    [ pponAtomico pericles ~?= False,
      pponAtomico  merlina~?= False,
      pponAtomico  addams~?= False,
      pponAtomico  familias~?= False,
      pponAtomico  nivel1~?= False,
      pponAtomico  nivel2~?= False,
      pponAtomico  nivel3~?= False,
      pponAtomico  nivel4~?= False,
      pponAtomico  objetolistavacia~?= False,
      pponAtomico  (TextoPP "a") ~?= True,
      pponAtomico  (IntPP 2) ~?= True
    ]

testsEj6 :: Test
testsEj6 =
  test
    [ pponObjetoSimple pericles ~?= True,
      pponObjetoSimple addams ~?= False,
      pponObjetoSimple familias ~?= False,
      pponObjetoSimple nivel1 ~?= False,
      pponObjetoSimple nivel2 ~?= False,
      pponObjetoSimple nivel3 ~?= False,
      pponObjetoSimple nivel4 ~?= True,
      pponObjetoSimple objetolistavacia ~?= True,
      pponObjetoSimple (TextoPP "a") ~?= False,
      pponObjetoSimple (IntPP 2) ~?= False
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
      -- Tests propios:
      intercalar (vacio) [a, b, c] ~?= texto "abc",
      intercalar a [b, l, c] ~?= texto "ba" <+> l <+> a <+> c,
      intercalar l [a,b,c] ~?= a <+> l <+> b <+> l <+> c,
      intercalar l [a, b, l, c] ~?= a <+> l <+> intercalar l [b, l, c],
      intercalar (c <+> l) [a,vacio,b,vacio] ~?= a <+> c <+> l <+> vacio <+> c <+> l <+> b <+> c <+> l <+> vacio
    ]

testsEj8 :: Test
testsEj8 =
  test
    [ mostrar (aplanar (a <+> linea <+> b <+> linea <+> c)) ~?= "a b c",
      mostrar (aplanar (a  <+> b  <+> c)) ~?= "abc",
      mostrar(aplanar (a <+> linea <+> b <+> linea <+> linea <+> linea <+> c)) ~?= "a b   c",
      aplanar (vacio) ~?= vacio,
      aplanar (a) ~?= a,
      mostrar (aplanar (linea)) ~?= " "
    ]

testsEj9 :: Test
testsEj9 =
  test
    [ mostrar (pponADoc pericles) ~?= "{ \"nombre\": \"Pericles\", \"edad\": 30 }",
      mostrar (pponADoc addams) ~?= "{\n  \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n  \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n}",
      mostrar (pponADoc familias) ~?= "{\n  \"Addams\": {\n    \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n    \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n  }\n}",
      -- Tests propios:
      mostrar (pponADoc nivel3) ~?= "{\n  \"info\": \"Nivel 3\",\n  \"siguiente\": { \"info\": \"Nivel 4\" }\n}",
      mostrar (pponADoc nivel1) ~?= "{\n  \"info\": \"Nivel 1\",\n  \"siguiente\": {\n    \"info\": \"Nivel 2\",\n    \"siguiente\": {\n      \"info\": \"Nivel 3\",\n      \"siguiente\": { \"info\": \"Nivel 4\" }\n    }\n  }\n}"
    ]
