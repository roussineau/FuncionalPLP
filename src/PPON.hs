module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

pponAtomico :: PPON -> Bool
pponAtomico (ObjetoPP _) = False
pponAtomico _ = True

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple (TextoPP _) = False
pponObjetoSimple (IntPP _) = False
pponObjetoSimple (ObjetoPP xs) = sonTodosAtomicos xs

sonTodosAtomicos :: [(String, PPON)] -> Bool
--sonTodosAtomicos [] = True
--sonTodosAtomicos ((s,p):xs) = pponAtomico p && sonTodosAtomicos xs
sonTodosAtomicos = foldr (\x rec -> pponAtomico (snd x) && rec) True

intercalar :: Doc -> [Doc] -> Doc
intercalar = error "PENDIENTE: Ejercicio 7"

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

aplanar :: Doc -> Doc
aplanar = error "PENDIENTE: Ejercicio 8"

pponADoc :: PPON -> Doc
pponADoc = error "PENDIENTE: Ejercicio 9"
