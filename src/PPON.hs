module PPON where

import Documento

data PPON
  = TextoPP String
  | IntPP Int
  | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)


-- Ejercicio 5

pponAtomico :: PPON -> Bool
pponAtomico (ObjetoPP _) = False
pponAtomico _ = True

-- Si es un ObjetoPP da False, en los otros casos es True


-- Ejercicio 6

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple (TextoPP _) = False
pponObjetoSimple (IntPP _) = False
pponObjetoSimple (ObjetoPP xs) = sonTodosAtomicos xs

-- En caso de ser TextoPP o IntP es falso, En el caso de ser ObjetoPP llama a sonTodosAtomicos a su lista asociada


sonTodosAtomicos :: [(String, PPON)] -> Bool
sonTodosAtomicos = foldr (\x rec -> pponAtomico (snd x) && rec) True

{-
Se fija si los segundos elementos de las tuplan son PPONes atomicos y  los pasa todos por  &&

Asi se veria desarmado el foldr
sonTodosAtomicos [] = True
sonTodosAtomicos ((s,p):xs) = pponAtomico p && sonTodosAtomicos xs
-}

-- Ejercicio 7

intercalar :: Doc -> [Doc] -> Doc
intercalar _ [] = vacio
intercalar sep docs = intercalarPrima (init docs) sep <+> last docs

intercalarPrima :: [Doc] -> Doc -> Doc
--intercalarPrima [] = const vacio
--intercalarPrima (x:xs) = (\sep -> x <+> sep <+> intercalarPrima xs sep)
intercalarPrima = foldr (\x rec -> \sep -> x <+> sep <+> rec sep) (const vacio)

-- tal como esta en el PDF
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

-- Ejercicio 8

aplanar :: Doc -> Doc
aplanar d = lineasAEspacios (juntarLineas d)

lineasAEspacios :: Doc -> Doc
lineasAEspacios = foldDoc (vacio) (\s rec -> texto s <+> rec) (\n rec -> texto " " <+> rec)

juntarLineas :: Doc -> Doc
juntarLineas = foldDoc (vacio) (\s rec -> texto s <+> rec) (\n rec -> if empiezaConLinea rec then rec else linea <+> rec)

empiezaConLinea :: Doc -> Bool
empiezaConLinea = foldDoc (False) (\s rec -> False) (\n rec -> True)

-- Ejercicio 9

pponADoc :: PPON -> Doc
pponADoc ppon = case ppon of 
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
