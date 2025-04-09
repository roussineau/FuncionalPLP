module Documento
  ( Doc,
    vacio,
    linea,
    texto,
    foldDoc,
    (<+>),
    indentar,
    mostrar,
    imprimir,
  )
where
import Data.Foldable (Foldable(fold))

data Doc
  = Vacio
  | Texto String Doc
  | Linea Int Doc
  deriving (Eq, Show)

vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio



-- Ejercicio 1

foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc  vacio fTexto fLinea doc = case doc of
          Vacio -> vacio
          Texto x documento -> fTexto x (rec documento)
          Linea y documento -> fLinea y (rec documento)
          where rec = foldDoc vacio fTexto fLinea 

{-
Es un fold de un tipo algebraico siguiendo la estructura tal cual como vimos en las clases practicas
recive una funcion para el constructor de Texto, una funcion para el constructor de Linea y 1 para el caso Vacio
-}


--Ejercicio 2

esTexto:: Doc -> Bool
esTexto (Texto _ _) = True
esTexto _ = False

-- Predicado que si es un texto da True


añadirTextoAlPrimero :: Doc ->String ->  Doc 
añadirTextoAlPrimero doc s1 = case doc of 
                            Vacio -> Vacio
                            Linea n d -> Linea n d
                            Texto s2 d -> Texto (s1++s2) d

{-
Recive un doc y un string, si el primer elemento del doc es linea o vacio no hace nada, en cambio si es texto
concatena el string a el string del texto
-} 

infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
(<+>) = foldDoc id (\ x rec -> \doc -> if esTexto (rec doc) 
                                      then añadirTextoAlPrimero doc x 
                                      else Texto x (rec doc))         
                                    (\ n rec -> \doc -> Linea n (rec doc) ) 

{-
Doc -> (Doc -> Doc)
Una funcion que toma un Doc, devuelve una funcion que toma otro Doc la cual devuelve un Doc. 
A esta funcion le pasamos un Doc y nos devuelve el resultado.

El Vacio del primer Doc debe ser remplazado por el segundo Doc.
Sin enbargo esto puede llegar a incumplir el invariante porque el primer Doc antes del Vacio podria terminar con un Texto
y el segundo Doc podria empezar con un Texto. Por ende, 
utilizamos un if en el fTexto. En el predicado del if preguntamos si el siguiente elemento es texto.
Este se va a evaluar y devolver True en el caso de ..Texto "s1" (Texto "s2" ..). Como suponemos que el 
primer Doc y el segundo cumplen el invariante, esto nada mas va a a pasar cuando ya se concateno el Doc 2 al final del Doc 1.
Si el predicado devuelve True y al estar en el ultimo texto del Doc 1, devuelvo el Doc 2 con el string de ese texto añadido.
En el caso de devolver falso, como el Doc 2 ya se concateno, devuelve la concatenacion realizada.
-}


-- Ejercicio 3

indentar :: Int -> Doc -> Doc
indentar n doc = indentarPrima doc n

indentarPrima :: Doc -> Int -> Doc
indentarPrima = foldDoc (const Vacio) (\s rec -> \n -> Texto s (rec n)) (\n1 rec -> \n2 -> Linea (n1+n2) (rec n2))

{-
Indentar le pasa el parametro de tipo documento a la funcion indentarPrima para que pueda ser
procesado con recursion estructural usando foldDoc

En el caso del documento Vacio queda igual
En el caso de tener Texto llamamos recursivamente la funcion sobre su 'argumento' documento
(indentamos el documento que le sigue)
y en el caso de encontrar una Linea le sumamos n espacios y recursivamente indentamos el documento que sigue

Como el n >= 0 (precondicion de la funcion) al sumarlo con los espacios de
los documentos del tipo 'Linea m d' se mantiene que n+m >= 0
A su vez, la funcion no modifica los string de los documentos construidos por 'Texto s d' si se
cumple que s no contiene '\n' y s != "" al entrar a la funcion, se cumplira lo mismo al salir
Por ultimo, asumamos que doc = Texto s1 d1 cumple el invariante si el resultado de (indentar n doc)
fuera de la forma 'Texto s1 (Texto s2 d2)'
necesariamente Texto s2 d2 es el resultado de (rec n) donde rec es el llamado recursivo evaluado en d1
Pero si rec n = Texto s2 d2 entonces es porque foldDoc () () () d1 entro
por la rama del constructor Texto, osea que d1 era de la forma Texto s' d' y el invariante ya estaba roto (Contradiccion)
-}


-- Ejercicio 4

nEspacios:: Int -> String
nEspacios n = [const ' ' x | x <- [1..n] ]

-- Una funcion que toma un Int y devuelve una lista de chars que tiene solo " " y de largo n 

mostrar :: Doc -> String
mostrar = foldDoc [] (\s rec -> s ++ rec) (\n rec ->"\n" ++ nEspacios n ++ rec)

{-
En el caso de Vacio hay que devolver una lista vacia ya que vamos a concatenar todo a partir de eso
en el caso de Texto simplemente se concatena al llamado recursivo 
en el caso de Linea tenes que concatenar el caracter de nueva linea ademas de que llamamos a nEspacion para concatenar la cantidad de espacio necesario
-}

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
