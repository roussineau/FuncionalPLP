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

--(texto "a" <+> linea) <+> texto "b" ~?= texto "a" <+> (linea <+> texto "b")

--(<+>) :: Doc -> Doc -> Doc
--(<+>) = foldDoc id (\ x rec -> \doc -> Texto x (rec doc)) (\ n rec -> \doc -> Linea n (rec doc) ) 

--concat2:: Doc -> Doc -> Doc
--concat2 Vacio = id 
--concat2 (Texto s d) = \d2 -> Texto s (concat2 d d2)
--concat2 (Linea n d) = \d2 -> Linea n (concat2 d d2)


--concat3:: Doc -> Doc -> Doc
--concat3 Vacio = id 
--concat3 (Texto s d) = \d2 ->  case d2 of 
--                              Vacio ->  Texto s (concat3 d d2)
--                              Texto s2 doc -> if esVacio d then concat3 d (Texto (s++s2) doc) else Texto s (concat3 d d2)
--                              Linea n2 doc -> Texto s (concat3 d d2)
--concat3 (Linea n d) = \d2 -> Linea n (concat3 d d2)

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
