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
intercalar _ [] = vacio
intercalar sep docs = intercalarPrima (init docs) sep <+> last docs

intercalarPrima :: [Doc] -> Doc -> Doc
--intercalarPrima [] = const vacio
--intercalarPrima (x:xs) = (\sep -> x <+> sep <+> intercalarPrima xs sep)
intercalarPrima = foldr (\x rec -> \sep -> x <+> sep <+> rec sep) (const vacio)

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

{-
--el siguiente aplanar esta mal,
--consultar sobre Linea 0 (Texto " " Vacio) = Linea 1 Vacio

aplanar :: Doc -> Doc
aplanar d = lineasAEspacios (juntarLineas d)

lineasAEspacios :: Doc -> Doc
lineasAEspacios = foldDoc (vacio) (\s rec -> texto s <+> rec) (\n rec -> texto " " <+> rec)

juntarLineas :: Doc -> Doc
juntarLineas = foldDoc (vacio) (\s rec -> texto s <+> rec) (\n rec -> if empiezaConLinea rec then rec else linea <+> rec)

empiezaConLinea :: Doc -> Bool
empiezaConLinea = foldDoc (False) (\s rec -> False) (\n rec -> True)

DESDE ACA ES COMENTERAIO
Explicacion:
Si definieramos aplanar en el Modulo de Doc (con recursion explicita) podria ser asi:

aplanar :: Doc -> Doc
aplanar d = lineasAEspacios (juntarLineas d)

lineasAEspacios :: Doc -> Doc
lineasAEspacios Vacio = Vacio
lineasAEspacios (Texto s d) = Texto s Vacio <+> (lineasAEspacios d)
lineasAEspacios (Linea n d) = Texto " " Vacio <+> lineasAEspacios d

juntarLineas :: Doc -> Doc
juntarLineas Vacio = Vacio
juntarLineas (Texto s d) = Texto s Vacio <+> (juntarLineas d)
juntarLineas (Linea n d) = if empiezaConLinea (juntarLineas d)
                           then juntarLineas d
                           else Linea 0 Vacio <+> juntarLineas d

empiezaConLinea :: Doc -> Bool
empiezaConLinea Vacio = False
empiezaConLinea (Texto s d) = False
empiezaConLinea (Linea n d) = True


Falta escribir estas funcions con lo exportado por Doc:

Escribimos lineasAEspacios con foldDoc:
lineasAEspacios = foldDoc (Vacio) (\s rec -> Texto s Vacio <+> rec) (\n rec -> Texto " " Vacio <+> rec)

Escribimos juntarLineas con foldDoc:
juntarLineas = foldDoc (Vacio) (\s rec -> Texto s Vacio <+> rec) (\n rec -> if empiezaConLinea rec then rec else Linea 0 Vacio <+> rec)

Escribimos empiezaConLinea con foldDoc:
empiezaConLinea = foldDoc (False) (\s rec -> False) (\n rec -> True)


Reemplazamos toda instancia de (Texto s Vacio) por (texto s), (Linea 0 Vacio) por (linea) y (Vacio) por (vacio):

lineasAEspacios = foldDoc (vacio) (\s rec -> texto s <+> rec) (\n rec -> texto " " <+> rec)

juntarLineas = foldDoc (vacio) (\s rec -> texto s <+> rec) (\n rec -> if empiezaConLinea rec then rec else linea <+> rec)

empiezaConLinea = foldDoc (False) (\s rec -> False) (\n rec -> True)

-}

pponADoc :: PPON -> Doc
pponADoc = error "PENDIENTE: Ejercicio 9"
