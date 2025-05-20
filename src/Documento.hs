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

--por hacer, reguntar si hacer justificaciones, preguntar justificacion del 2 y 3
--cambiar justificacion 6 (facil), cambiar justificacion 7, hacer justificacion 8, ver correcion justificacion 9

-- | Ejercicio 1 |

foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc fvacio fTexto fLinea doc =
  case doc of
    Vacio -> fvacio
    Texto x documento -> fTexto x (rec documento)
    Linea y documento -> fLinea y (rec documento)
  where rec = foldDoc fvacio fTexto fLinea 
{-
  Es un fold de un tipo algebraico siguiendo la estructura tal cual
  vimos en las clases prácticas. Recibe una función para el constructor
  Texto, una función para el constructor Linea, y otro para Vacio.
-}


-- | Ejercicio 2 |


infixr 6 <+>
(<+>) :: Doc -> Doc -> Doc
(<+>) doc1 doc2 = foldDoc doc2 fTexto Linea doc1
  where fTexto s1 rec  = case rec  of
                          Texto s2 rec' -> Texto (s1++s2) rec'
                          _ -> Texto s1 rec  


{-
Suponemos que tanto doc1 como doc2 cumplen el invariante dado, 
esto significa que no tienen dos Textos seguidos, que la indentacion de una linea sea >= 0, 
que cada String de ambos documentos no tiene el caracter del salto de linea y que ambos no son el String vacio.

Nosotros foldeamos el doc1, en cada constructor aplicamos una funcion para que devuelva los dos documentos concatenados,

flinea, deja la linea como estaba originalmente en el doc1, por lo tanto la indetacion va a ser >= 0, se mantiene el invariante.

ftexto, deja el texto como estaba en caso de que el proximo no sea texto, 
pero como el documento original cumple el invariante de que no hay textos seguidos, lo deja tal como esta, manteniendo el invariante.

fvacio, como el objetivo es concatenar documentos, remplazo el Vacio por el doc2, asi quedando ambos documentos concatenados
y como doc2 cumple el invariante, la concatenacion va a cumplir el invariante. Caso contrario el invariante podria llegar a romperse si
doc1 termina con Texto "s1" Vacio y doc2 inicia con Texto.
Esto lo solucionamos con la ftexto que concatena ambos Strings. Tanto s1 y s2 cumplen el invariante 
por esta razon la concatenacion de ambos Strings tambien cumpliran el invariante. Notar que esto solamente sucede si hay dos textos seguidos,
que como el doc1 y el doc2 cumplen el invariante, esto solo va suceder en el caso mencionado, y en ningun otra parte del doc1.

El doc2 se mantiene igual a no ser que el doc1 termina con Texto "s1" Vacio y doc2 inicia con Texto. en tal caso sucede lo explicado anteriormente
y se modifica el primer texto del doc2, esto cumple el invariante por la justificacion dada y como el resto del documento no se modifica
seguiria manteniendo el invariante

Dado todos los casos cubiertos, demostramos que siempre se cumple el invariante basandonos en que doc1 y doc2 cumplen el invariante
entonces nuestra funcion de concatenacion, va a devolver un documento concatenado que tambien cumple el invariante.

-}


-- | Ejercicio 3 |

indentar :: Int -> Doc -> Doc
indentar n  = foldDoc Vacio Texto (\n1 rec -> Linea (n1+n) rec) 
{-
Supongamos que el doc que nos pasan cumple el invariante.
esto significa que no tiene dos Textos seguidos, que la indentacion de cada Linea es >= 0,
que cada String no tiene el caracter del salto de linea y no es String vacio.

Nosotros foldeamos el doc, en cada constructor aplicamos una funcion para que devuelva el documento indentado,

fvacio, devolvemos Vacio. Cumple el invariante

ftexto, dejamos el Texto como estaba originalmente en el doc, al no tener ninguna modificacion se sigue manteniendo el invariante

flinea, la unica modificacion realizada al documento se encuentra en la indentacion de cada Linea, donde sumamos n con la indentacion de la Linea.
Por precondicion sabemos que n >= 0, y por invariante la indentacion es >= 0, por lo tanto la suma tambien lo va a ser.

como cada funcion solo hace modificaiones adentro de su constructor, no se va a modificar la estructura del documento, por lo tanto
se mantiene el invariante de que no hay dos Textos seguidos, y al no modificarse ningun String se sigue manteniendo que no hay String vacio y 
que no tienen el caracter de salto de linea.

-}


-- | Ejercicio 4 |


mostrar :: Doc -> String
mostrar = foldDoc [] (++) (\ n rec -> "\n" ++ replicate n ' ' ++ rec)


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
