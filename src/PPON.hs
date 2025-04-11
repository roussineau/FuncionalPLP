module PPON where
  
import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)


-- | Ejercicio 5 |

pponAtomico :: PPON -> Bool
pponAtomico (ObjetoPP _) = False
pponAtomico _ = True
-- Si usa el constructor ObjetoPP da False, sino da True.


-- | Ejercicio 6 |

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple (ObjetoPP xs) = sonTodosAtomicos xs
pponObjetoSimple _ = False
-- En caso de ser ObjetoPP llama a sonTodosAtomicos con su lista asociada, sino devuelve False.

sonTodosAtomicos :: [(String, PPON)] -> Bool
sonTodosAtomicos = foldr (\x rec -> pponAtomico (snd x) && rec) True
{-
  Se fija si los segundos elementos de las tuplas son PPONes atómicos, y los pasa todos por (&&)

  Así se vería desarmando el foldr, o sea, con recursión explícita:
  sonTodosAtomicos [] = True
  sonTodosAtomicos ((s,p):xs) = pponAtomico p && sonTodosAtomicos xs
-}


-- | Ejercicio 7 |

entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds =
  texto "{"
    <+> indentar
      2
      ( linea
          <+> intercalar (texto "," <+> linea) ds
      )
    <+> linea
    <+> texto "}"
-- Definida tal cual como en el PDF

intercalarPrima :: [Doc] -> Doc -> Doc
intercalarPrima = foldr (\x rec -> \sep -> x <+> sep <+> rec sep) (const vacio)
{-
  Función que espera una lista de Docs, un separador, y se lo agrega al final
  a todos los elementos del arreglo.

  No colapsamos las lambdas porque de esta forma se ve mejor lo que estamos haciendo:
  devolviendo una función que espera un separador para concatenárselo a cada elemento.

  Así se vería desarmando el foldr, o sea, con recursión explícita:
  intercalarPrima [] = const vacio -- Para no concatenar un separador al Doc Vacio del caso base
  intercalarPrima (x:xs) = (\sep -> x <+> sep <+> intercalarPrima xs sep)
-}

intercalar :: Doc -> [Doc] -> Doc
intercalar _ [] = vacio
intercalar sep docs = intercalarPrima docs sep 
{-
  Intercalamos todos los elementos del inicio de la lista con un separador,
  y el último lo concatenamos para que no tenga ese separador sin sentido al final.
-}


-- | Ejercicio 8 |

empiezaConLinea :: Doc -> Bool
empiezaConLinea = foldDoc False (\s rec -> False) (\n rec -> True)
-- Predicado auxiliar hecho con foldDoc porque no podemos hacer pattern matching con los constructores.

juntarLineas :: Doc -> Doc
juntarLineas = foldDoc vacio (\s rec -> texto s <+> rec) (\n rec -> if empiezaConLinea rec then rec else linea <+> rec)
-- Función auxiliar que dado un Doc, si tiene dos lineas seguidas o más las junta en una sola.

lineasAEspacios :: Doc -> Doc
lineasAEspacios = foldDoc vacio (\s rec -> texto s <+> rec) (\n rec -> texto " " <+> rec)
-- Función auxiliar que dado un Doc con una Linea, la reemplaza a esta por un espacio " ".

aplanar :: Doc -> Doc
aplanar d = lineasAEspacios (juntarLineas d)


-- | Ejercicio 9 |

pponADoc :: PPON -> Doc
pponADoc ppon =
  case ppon of 
    TextoPP s -> texto (show s)
    IntPP n -> texto (show n)
    ObjetoPP xs -> if sonTodosAtomicos xs then aplanar(casoObjeto xs) else casoObjeto xs
  where casoObjeto = entreLlaves . map (\x ->texto (show (fst x)) <+> texto ": " <+> pponADoc (snd x))

{-
  En esta funcion el esquema de recursion corresponde a la recursion primitiva.
  Esto es debido a que cada caso base se escribe en funcion de los parametros de los constructores base
  y el caso recursivo se escribe en funcion de cada parametro del constructor que no es de tipo PPON,
  el llamado recursivo con cada parametro de tipo PPON y el mismo parametro del constructor recursivo (de tipo [(String, PPON)])
  Por esto ultimo es recursion primitiva y no estructural, necesitamos tambien del parametro del constructor recursivo
  para resolver el problema.
-}
