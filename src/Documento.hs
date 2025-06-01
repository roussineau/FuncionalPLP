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
 
  Queremos ver que ∀doc1::Doc.∀doc2::Doc.la funcion devuelve los dos documentos concatenados. Y, a su vez, que cumple el invariante. 
  Como operamos recursivamente sobre el doc1, hacemos induccion sobre la estrucutra de doc1.
  
  Caso base: doc1 = Vacio. 
    La concatenacion de un documento vacio con otro documento debería de dar el otro documento, por eso devolvemos el doc2. Notar que, como el doc2 “entra” cumpliendo el invariante, va a salir cumpliendo el invariante. 
    Con esto probamos que en el caso base se cumple el invariante.
  
  Caso recursivo: doc1 = Texto s d/Linea n d
  Como hipótesis inductiva tenemos que d cumple el invariante. Debemos verificar que la extensión lo sigue cumpliendo.

    Veamos primero el caso doc1 = Linea n d
      La fLinea deja la Linea como estaba originalmente en el doc1. Como suponemos que el doc1 cumple el invariante, la indentación va a ser >= 0. Por lo tanto, al unirla a d, va a seguir cumpliendo el invariante. 
      Asimismo, al concatenar ese pedazo del doc1 a d logramos que se siga manteniendo la estructura del doc1. Entonces en el caso de doc1 = Linea n d, la flinea provoca que se siga cumpliendo el invariante.
    
    Veamos el caso doc1 = Texto s d
      La fTexto deja el texto como estaba originalmente en doc1 (EN CASO DE QUE d NO SEA TEXTO). Como suponemos que el doc1 cumple el invariante, s no va a estar vacio o tener caracter de saltos de línea. 
      Tambien, como suponemos que d no es Texto, puedo unirlo a d, y esto va a seguir cumpliendo el invariante. Asimismo al concatenar ese pedazo del doc1 a d logramos que se siga manteniendo la estructura del doc1.

      Sin embargo, en el caso de que d sea texto, esto solo va a ocurrir si doc1 termina con Texto s Vacio y doc2 empieza con Texto s'' d''. Esto se debe a lo siguiente: 
      Cuando llegamos al Vacio del doc1 y ponemos el doc2, quedaria Texto s (Texto s'' d''). De esta forma se estaría incumpliendo el invariante. Notar que no pasaria en ninguna otra parte porque doc 1 y doc 2 cumple el invariante. 
      Por esta razón, no podemos simplemente unir doc1 a d evitándolo, justamente, en el case d. Aquí, nos fijamos en el caso que d sea Texto. En tal caso, reescribimos a d como Texto s' d'. 
      Sabemos por HI que si d cumplía el invariante d' y s' también lo van a hacer. Para evitar el incumplimiento del invariante devolvemos Texto s++s' d'. 
      Esto funciona porque s y s' no tienen saltos de linea ni son vacios. En consecuencia, la concatenación tampoco los va a tener. Ademas de concatenar ambos Strings juntamos ambos Textos y quedaria algo que cumple el invariante. 
      Por ende, en el caso de doc1 = Texto s d, la ftexto provoca que se siga cumpliendo el invariante. 

  El doc2 se mantiene igual a no ser que el doc1 termine con "Texto s1 Vacio" y doc2 inicie con Texto. En tal caso sucede lo explicado anteriormente y se modifica el primer texto del doc2. 
  Esto cumple el invariante por la justificación dada, y como el resto del doc2 no se modifica, se sigue manteniendo el invariante.

  Dados todos los casos cubiertos, demostramos que nuestra función de concatenación va a devolver el documento resultante d concatenar doc1 y doc2 que también cumple el invariante, asumiendo que doc1 y doc2 cumplen el invariante.
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
  
 
  Queremos ver que ∀doc::Doc.la funcion devuelve el documento indentado que sigue cumpliendo el invariante. 
  Debido a que operamos recursivamente sobre el doc, hacemos induccion sobre su estrucutra.

  Caso base: doc1 = Vacio. 
    La indentacion de algo que no tiene Linea n d debe de mantenerse igual. Por lo tanto un Doc vacio tiene que mantenerse igual, por eso devolvemos vacio.
    Como un documento vacio cumple el invariante, comprobamos que el caso base lo cumple.

  Caso recursivo: doc = Texto s d/Linea n d
  Como hipotesis inductiva tenemos que d cumple el invariante, debemos ver que la extension lo sigue cumpliendo.

    Para ello, observemos el caso doc = Texto s d
      La ftexto deja el texto como estaba originalmente en el doc ya que indentacion solo debe de modificar las lineas. 
      Al suponer que este cumple el invariante, s no va a estar vacio ni contener el caracter de salto de linea. Asimismo, como no se toca la estrucutrura del doc, este se mantiene igual. 
      Por último, como suponemos que doc cumple invariante, sabemos que el d no va a comenzar con Texto y, al mantenerlo todo igual, sigue manteniendo el invariante.

    Luego, observemos el caso doc = Linea n d
      En indentar, a cada linea, debemos sumarle un m que se nos pasa como parametro en la funcion. 
      Como requiere sabemos que m >= 0, entonces, al suponer que doc cumple el invariante, en particular n va a ser => 0. Por lo tanto la suma tambien va a ser >= 0. 
      Como en el lugar donde estaba Linea n d, queda Linea n+m d, no se modifica la estructura del documento. En consecuencia, no va a pasar que queden dos textos seguidos. Gracias a esto, se sigue cumpliendo el invariante.

  Resumiendo, debido a que en el caso base, como en las extensiones, se sigue cumpliendo el invariante, si doc lo cumple y m >= 0, indentar(doc,m) va a cumplir el invariante

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
