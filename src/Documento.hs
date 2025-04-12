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

añadirTextoAlPrincipio :: Doc -> String ->  Doc 
añadirTextoAlPrincipio (Texto s1 d) s2 = Texto (s2++s1) d
añadirTextoAlPrincipio d _ = d


infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
(<+>) = foldDoc id fTexto fLinea
  where fTexto x rec doc = if esTexto (rec doc)
                           then añadirTextoAlPrincipio doc x
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

indentarPrima :: Doc -> Int -> Doc
indentarPrima = foldDoc (const Vacio) (\s rec -> \n -> Texto s (rec n)) (\n1 rec -> \n2 -> Linea (n1+n2) (rec n2))
{-
  Creamos la función auxiliar indentarPrima por sencillez a la hora de leer y justificar en el punto 10,
  pero tranquilamente podríamos haber hecho un flip a toda esta deficición en la definición de indentar.
-}

indentar :: Int -> Doc -> Doc
indentar n doc = indentarPrima doc n
{-
  Indentar le pasa el parametro de tipo documento a la funcion indentarPrima
  para que pueda ser procesado con recursion estructural usando foldDoc.

  En el caso de Vacio, queda igual.
  En el caso de Texto, llamamos recursivamente la funcion sobre su constructor recursivo de tipo
    Doc, e indentamos el documento que le sigue.
  En el caso de Linea, le sumamos n espacios y recursivamente indentamos el documento que sigue.

  Como el n >= 0 (precondicion de la función) al sumarlo con los espacios
    de los documentos del constructor "Linea m d" se mantiene que n+m >= 0.

  A su vez, la función no modifica los String de los documentos de la forma "Texto s d", por lo que si se
  cumple que s no contiene "\n" y s != "" al entrar a la funcion, se cumplirá lo mismo al salir.

  Por último, asumamos que doc = Texto s1 d1 cumple el invariante. Si el resultado de (indentar n doc)
  fuera de la forma "Texto s1 (Texto s2 d2)" (la primera línea no se modifica), necesariamente
  "Texto s2 d2" es el resultado de "rec n", donde rec es el llamado recursivo evaluado en d1.
  Pero si rec n = Texto s2 d2 entonces es porque foldDoc _ _ _ d1 entró por la rama del constructor Texto,
  osea que d1 era de la forma Texto s' d' y no se estaba cumpliendo el invariante, lo cual es una contradicción.
  O sea que si no se cumple el invariante a la salida es porque no se estaba cumpliendo a la entrada.
-}


-- | Ejercicio 4 |

nEspacios:: Int -> String
nEspacios n = [const ' ' x | x <- [1..n] ]

mostrar :: Doc -> String
mostrar = foldDoc [] (++) (\ n rec -> "\n" ++ nEspacios n ++ rec)
{-
  En el caso de Vacio hay que devolver una lista vacia ya que vamos a
    concatenar todo a partir de eso, porque es el final del Doc.
  En el caso de Texto simplemente se concatena el String al llamado
    recursivo. Como se hace de derecha a izquierda, se mantiene el orden correcto.
  En el caso de Linea hay que concatenar el caracter de nueva linea,
    y llamar a nEspacion para concatenar la cantidad de espacio necesarios.
-}

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
