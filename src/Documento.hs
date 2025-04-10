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


-- | Ejercicio 1 |

foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc vacio fTexto fLinea doc =
  case doc of
    Vacio -> vacio
    Texto x documento -> fTexto x (rec documento)
    Linea y documento -> fLinea y (rec documento)
  where rec = foldDoc vacio fTexto fLinea 
{-
  Es un fold de un tipo algebraico siguiendo la estructura tal cual
  vimos en las clases prácticas. Recibe una función para el constructor
  Texto, una función para el constructor Linea, y otro para Vacio.
-}


-- | Ejercicio 2 |

esTexto :: Doc -> Bool
esTexto (Texto _ _) = True
esTexto _ = False
-- Predicado auxiliar que devuelve True si y sólo si el parámetro es un Texto.

añadirTextoAlPrimero :: Doc -> String ->  Doc 
añadirTextoAlPrimero (Texto s1 d) s2 = Texto (s2++s1) d
añadirTextoAlPrimero d _ = d
{-
  Función auxiliar que recibe un Doc y un String.
  Si el primer parámetro es Texto, concatena el String pasado al String del Texto.
  Sino, no hace nada sobre ese primer parámetro.
-} 

infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
(<+>) = foldDoc id fTexto fLinea
  where fTexto x rec doc = if esTexto (rec doc)
                           then añadirTextoAlPrimero doc x
                           else Texto x (rec doc)
        fLinea n rec doc = Linea n (rec doc)
{-
  Toma un Doc y devuelve una función que toma otro Doc y devuelve un Doc, que sería el resultado esperado.

  Intuitivamente, el Vacio del primer Doc podría ser remplazado por el segundo Doc. Sin embargo, esto incumpliría
  el invariante si el Doc resultante tiene en alguna parte algo de la forma Texto String (Texto String _).
  Por eso utilizamos un condicional en fTexto que:
    Si el predicado devuelve True:
      Como asumimos que ambos Doc cumplen el invariante, esto solo puede llegar a pasar cuando ya se concatenó el Doc 2
      al final del Doc 1. Al estar en el último Texto del Doc 1, devuelvo el Doc 2 con el string de ese texto añadido.
    Si el predicado devuelve False:
      Como el Doc 2 ya se concatenó, devuelvo la concatenación realizada.
-}


-- | Ejercicio 3 |

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
