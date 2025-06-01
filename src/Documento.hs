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
 
  Queremos ver que `∀doc1::Doc.∀doc2::Doc. doc1 <+> doc2` devuelve un Doc que mantiene el invariante, siempre y cuando doc1 y doc2 cumplan el invariante.
  Haremos inducción estructural sobre doc1:

  CASO BASE: doc1 = Vacio
    Vacio <+> doc2 = foldDoc doc2 fTexto Linea Vacio = doc2
    * Como doc2 cumple el invariante, se mantiene el invariante.

  CASOS INDUCTIVOS:
    HI: Para el documento d, se tiene que `d <+> doc2` cumple el invariante.
    QVQ: `Texto s d <+> doc2` y `Linea n d <+> doc2` cumplen el invariante.

    Caso doc1 = Texto s d:
        Texto s d <+> doc2 =
        foldDoc doc2 fTexto Linea (Texto s d) =
        fTexto s (rec d) =
        fTexto s (foldDoc doc2 fTexto Linea d) =
        fTexto s (d <+> doc2)
      Llamemos rec' a (d <+> doc2):
      Si rec' empieza con Texto, tal que tiene la forma Texto s2 r:
        fTexto s (Texto s2 r) =
        Texto (s1 ++ s2) r
        * Como por invariante ni s1 ni s2 son strings vacíos ni contienen saltos de línea, su concatenación tampoco.
        * Evitamos tener dos Textos consecutivos.
        * r cumple el invariante porque es subdocumento de rec', que por HI cumple el invariante.
      Sino:
        Texto s1 rec'
        * s1 no es string vacío ni tiene saltos de línea por precondición.
        * Como rec' no empieza con Texto, no tenemos dos Textos seguidos.
        * Por HI, rec' cumple el invariante.
    
    Caso doc1 = Linea n d:
        Linea n d <+> doc2 =
        foldDoc doc2 fTexto Linea (Linea n d) =
        Linea n (rec d) =
        Linea n (foldDoc doc2 fTexto Linea d) =
        Linea n (d <+> doc2)
        * Preservamos la línea original, sin modificar su indentación. Como n >= 0 por invariante de doc1, se mantiene esta propiedad.
        * Por HI, d <+> doc2 cumple el invariante.

  Conclusión: `∀doc1::Doc.∀doc2::Doc. doc1 <+> doc2` cumple el invariante, siempre y cuando doc1 y doc2 cumplan el invariante.
  -}


-- | Ejercicio 3 |

indentar :: Int -> Doc -> Doc
indentar n  = foldDoc Vacio Texto (\n1 rec -> Linea (n1+n) rec) 

{-
  Asumimos que el doc que nos pasan cumple el invariante:
  * Ningún String es el String vacio;
  * Ningún String contiene saltos de linea;
  * No tiene dos Textos seguidos;
  * La indentación de sus Lineas es >= 0;
 
  Queremos ver que `∀doc::Doc. indentar n doc` devuelve un Doc mantiene el invariante, siempre y cuando doc cumpla el invariante y n > 0. 
  Haremos inducción estructural sobre doc:

  CASO BASE: doc = Vacio. 
    indentar n Vacio =
    foldDoc Vacio Texto (\n1 rec -> Linea (n1+n) rec) Vacio = 
    Vacio
    * El documento Vacio siempre cumple el invariante.

  CASOS RECURSIVOS:
    HI: Para el documento d, se tiene que `indentar n d` cumple el invariante.
    QVQ: `indentar n (Texto s d)` e `indentar n (Linea m d)` cumplen el invariante.

    Caso doc = Texto s d:
      indentar n (Texto s d) =
      foldDoc Vacio Texto (\n1 rec -> Linea (n1+n) rec) (Texto s d) = 
      Texto s (rec d) = 
      Texto s (foldDoc Vacio Texto (\n1 rec -> Linea (n1+n) rec) d) =
      Texto s (indentar n d)
      * s no es string vacio ni salto de linea.
      * No vamos a tener dos textos seguidos porque asumimos como precondición que doc1 = `Texto s d` cumple el invariante.
      * Por HI, `indentar n d` cumple el invariante.

    Caso doc = Linea m d
      indentar n (Linea m d) =
      foldDoc Vacio Texto (\n1 rec -> Linea (n1+n) rec) (Linea m d) =
      (\n1 rec -> Linea (n1+n) rec) m (rec d) = 
      (\n1 rec -> Linea (n1+n) rec) m (foldDoc Vacio Texto (\n1 rec -> Linea (n1+n) d) =
      (\n1 rec -> Linea (n1+n) rec) m (indentar n d) =
      Linea (m+n) (indentar n d)
      * Como por precondición m >= 0 y n > 0, entonces m+n > 0, cumpliendo la condición del entero del invariante del constructor Linea.
      * Por HI, `indentar n d` cumple el invariante.

  Conclusión: `∀doc::Doc. indentar n doc` devuelve un Doc mantiene el invariante, siempre y cuando doc cumpla el invariante y n > 0.
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
