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

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple (ObjetoPP xs) = all (\(x,y) -> pponAtomico y) xs
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


intercalar ::  Doc -> [Doc]  -> Doc
intercalar _ [] = texto ""
intercalar sep xs = foldr1 (\x rec -> x <+> sep <+> rec )  xs

-- | Ejercicio 8 |

aplanar :: Doc -> Doc
aplanar = foldDoc vacio (\s rec -> texto s <+> rec) (\n rec -> texto " " <+> rec)

-- | Ejercicio 9 |

pponADoc :: PPON -> Doc
pponADoc ppon =
  case ppon of 
    TextoPP s -> texto (show s)
    IntPP n -> texto (show n)
    ObjetoPP xs -> if pponObjetoSimple (ObjetoPP xs) then aplanar (casoObjeto xs) else casoObjeto xs
  where casoObjeto = entreLlaves . map (\(x,y) -> texto (show x) <+> texto ": " <+> pponADoc y)


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
