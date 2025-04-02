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



foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b) -> Doc -> b
foldDoc  vacio fTexto fLinea doc = case doc of
          Vacio -> vacio
          Texto x documento -> fTexto x (rec documento)
          Linea y documento -> fLinea y (rec documento)
          where rec = foldDoc vacio fTexto fLinea 



-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
(<+>) = foldDoc id (\ x rec -> \doc -> if esProximoTexto (rec doc) then añadirTextoAlPrimero doc x else Texto x (rec doc)) (\ n rec -> \doc -> Linea n (rec doc) ) 

esVacio:: Doc -> Bool
esVacio Vacio = True
esVacio _ = False

esProximoTexto:: Doc -> Bool
esProximoTexto (Texto _ _) = True
esProximoTexto _ = False

añadirTextoAlPrimero :: Doc ->String ->  Doc 
añadirTextoAlPrimero doc s1 = case doc of 
                            Vacio -> Vacio
                            Linea n d -> Linea n d
                            Texto s2 d -> Texto (s1++s2) d


-- consultar ejercicio
indentar :: Int -> Doc -> Doc
indentar _ Vacio = Vacio
indentar n (Texto s d) = Texto s (indentarPrima d n)
indentar n1 doc = indentarPrima doc n1

indentarPrima :: Doc -> Int -> Doc
indentarPrima = foldDoc (const Vacio) (\s rec -> \n -> Texto s (rec n)) (\n1 rec -> \n2 -> Linea (n1+n2) (rec n2))

mostrar :: Doc -> String
mostrar = foldDoc ([]) (\s rec -> s ++ rec) (\n rec ->"\n" ++ nEspacios n ++ rec)

nEspacios:: Int -> String
nEspacios n = [const ' ' x | x <- [1..n] ]
convetirLineaAString :: Doc -> String
convetirLineaAString (Linea n _) = [] 

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
