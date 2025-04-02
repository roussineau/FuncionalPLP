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
--infixr 6 <+>

--(<+>) :: Doc -> Doc -> Doc
--(<+>) = foldDoc id (\ x rec -> \doc -> Texto x (rec doc)) (\ n rec -> \doc -> Linea n (rec doc) ) 
concat2 :: Doc -> Doc -> Doc
concat2 Vacio = id
concat2 (Texto s d) = \d2 -> case d2 of 
                              Vacio -> Texto s d 
                              Texto s2 doc -> Texto (s ++ s2) (concat2 d doc)
                              Linea n2 doc -> Texto s (concat2 d (Linea n2 doc))                            
concat2 (Linea n d) = \d2 -> Linea n concat2 d d2


indentar :: Int -> Doc -> Doc
indentar i = error "PENDIENTE: Ejercicio 3"

mostrar :: Doc -> String
mostrar = error "PENDIENTE: Ejercicio 4"

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
