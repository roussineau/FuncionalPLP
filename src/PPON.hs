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


-- | Ejercicio 6 |

sonTodosAtomicos :: [(String, PPON)] -> Bool
sonTodosAtomicos = foldr (\x rec -> pponAtomico (snd x) && rec) True
{-
  Se fija si los segundos elementos de las tuplas son PPONes atómicos, y los pasa todos por (&&)

  Así se vería desarmando el foldr, o sea, con recursión explícita:
  sonTodosAtomicos [] = True
  sonTodosAtomicos ((s,p):xs) = pponAtomico p && sonTodosAtomicos xs
-}

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple (ObjetoPP xs) = sonTodosAtomicos xs
pponObjetoSimple _ = False


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


intercalar ::  Doc -> [Doc]  -> Doc
intercalar _ [] = texto ""
intercalar sep xs= foldr1 (\x rec -> x <+> sep <+> rec )  xs

{-
  Función que espera una lista de Docs, un separador, y se lo agrega al final
  a todos los elementos del arreglo.

  No colapsamos las lambdas porque de esta forma se ve mejor lo que estamos haciendo:
  devolviendo una función que espera un separador para concatenárselo a cada elemento.

  Así se vería desarmando el foldr, o sea, con recursión explícita:
  intercalarPrima [] = const vacio -- Para no concatenar un separador al Doc Vacio del caso base
  intercalarPrima (x:xs) = (\sep -> x <+> sep <+> intercalarPrima xs sep)
-}


{-
  Intercalamos todos los elementos del inicio de la lista con un separador,
  y el último lo concatenamos para que no tenga ese separador sin sentido al final.
  Notar que no es recursión explícita.
-}


-- | Ejercicio 8 |

aplanar :: Doc -> Doc
aplanar doc = if empiezaConLinea doc then texto " " <+> foldDoc vacio fTexto fLinea doc else foldDoc vacio fTexto fLinea doc
    where fTexto s recDoc  = if recDoc == vacio then texto s else texto (s ++ " ") <+> recDoc
          fLinea n recDoc  = recDoc
          empiezaConLinea = foldDoc False (\s rec -> False) (\n rec -> True)

-- ver el tema del if del princupio y hacer justificacion

-- | Ejercicio 9 |

pponADoc :: PPON -> Doc
pponADoc ppon =
  case ppon of 
    TextoPP s -> texto (show s)
    IntPP n -> texto (show n)
    ObjetoPP xs -> if sonTodosAtomicos xs then aplanar (casoObjeto xs) else casoObjeto xs
  where casoObjeto = entreLlaves . map (\(fst,snd) -> texto (show fst) <+> texto ": " <+> pponADoc snd)

  -- fijarse si cambiar el nombre de fst snd
  --haría más hincapié en que se utiliza la subestructura sin aplicarle la recursión cuando se quiere verificar que es un pponObjetoSimple.

{-
  En esta función, el esquema de recursión corresponde a la recursión primitiva
  porque cada caso base se escribe en función de los parámetros de los constructores
  base, y el caso recursivo se escribe en función de:
     1) cada parámetro del constructor que no es de tipo PPON,
     2) el llamado recursivo con cada parámetro de tipo PPON, y
  -> 3) el mismo parámetro del constructor recursivo (de tipo [(String, PPON)]) <-
  Por esto último es recursión primitiva y no estructural, ya que necesitamos
  también del parámetro del constructor recursivo para resolver el problema.
-}
