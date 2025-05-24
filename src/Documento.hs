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

-- Por hacer: cambiar justificación 6 (fácil), cambiar justificación 7, hacer justificación 8.
-- | Ejercicio 1 |

foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc fVacio fTexto fLinea doc =
  case doc of
    Vacio -> fVacio
    Texto x documento -> fTexto x (rec documento)
    Linea y documento -> fLinea y (rec documento)
  where rec = foldDoc fVacio fTexto fLinea 
{-
  Es un fold de un tipo algebraico siguiendo la estructura tal cual
  vimos en las clases prácticas. Recibe una función para el constructor
  Texto, una función para el constructor Linea, y otro para Vacio.
-}


-- | Ejercicio 2 |

infixr 6 <+>
(<+>) :: Doc -> Doc -> Doc
(<+>) doc1 doc2 = foldDoc doc2 fTexto Linea doc1
  where fTexto s1 rec  = case rec of
                          Texto s2 rec' -> Texto (s1++s2) rec'
                          _ -> Texto s1 rec  

{-
  Asumimos que tanto doc1 como doc2 cumplen el invariante:
  * Ningún String de ambos docs es el String vacio;
  * Ningún String de ambos docs contiene saltos de linea;
  * No tienen dos Textos seguidos;
  * La indentación de sus Lineas es >= 0;

  Foldeamos el doc1, y para cada constructor aplicamos una función que devuelva los documentos unidos correctamente.

  * fLinea: deja la Linea como estaba originalmente en el doc1.
  Como ya se cumplía el invariante, la indentación va a ser >= 0, y se mantiene el invariante.

  * fTexto: deja el texto como estaba EN CASO DE QUE NO SEA TEXTO.
  Como el documento original cumple el invariante de que no hay textos seguidos, lo deja tal como esta, manteniendo el invariante.

  * fVacio: reemplazamos el Vacio del final de doc1 por el doc2, quedando ambos documentos concatenados.
  Como doc2 cumple el invariante, la concatenación va a cumplir el invariante.
  Caso límite: el invariante podría romperse si doc1 termina con "Texto s1 Vacio" y doc2 inicia con Texto.
  Lo solucionamos con el caso de la ftexto que concatena ambos Strings. Como s1 y s2 cumplen el invariante, la concatenación de ambos Strings tambien cumple el invariante.
  Notar que esto solamente sucede si hay dos textos seguidos, y como el doc1 y el doc2 cumplen el invariante, solo va suceder en el caso mencionado.

  El doc2 se mantiene igual a no ser que el doc1 termine con "Texto s1 Vacio" y doc2 inicie con Texto. En tal caso sucede lo explicado anteriormente
  y se modifica el primer texto del doc2. Esto cumple el invariante por la justificación dada, y como el resto del documento no se modifica se sigue manteniendo el invariante.

  Dados todos los casos cubiertos, demostramos que nuestra función de concatenación va a devolver el documento resultante de
  concatenar doc1 y doc2 que también cumple el invariante, asumiendo que doc1 y doc2 cumplen el invariante.
-}


-- | Ejercicio 3 |

indentar :: Int -> Doc -> Doc
indentar n  = foldDoc Vacio Texto (\n1 rec -> Linea (n1+n) rec) 
{-
  Asumimos que el doc que nos pasan cumple el invariante.

  Foldeamos el doc, en cada constructor aplicamos una funcion para que devuelva el documento indentado:

  * fVacio: devolvemos Vacio. Cumple el invariante.

  * ftexto: dejamos el Texto como estaba originalmente en el doc. Al no tener ninguna modificación, sigue manteniendo el invariante.

  * flinea: la única modificacion realizada al documento es sumar n con la indentación de la Linea.
  Por precondición sabemos que n >= 0, y por invariante la indentacion previa es >= 0, por lo tanto la suma tambien será >=0.

  Como cada función solo trabaja dentro de su constructor, no se va a modificar la estructura general del documento.
  Por lo tanto, se mantiene el invariante:
  * No hay dos Textos seguidos.
  * Al no modificarse ningún String, se sigue manteniendo que no hay Texto con String vacio.
  * No hay String con el caracter de salto de linea.
  * No hay Linea con indentación < 0.
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
