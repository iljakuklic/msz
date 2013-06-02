Vizualizace
===========

Modul pro podporu vizualizace. Může vyžadovat extení nástroje apod.

> module ZZZ_Visualise where

Knihovny pro spouštění externích nástrojů, dočasné soubory a podobné pro interakci se systémem:

> import System.Process
> import System.IO
> import System.Directory
> import Control.Monad

Moduly pro stavbu textových dokumentů.

> import qualified Text.PrettyPrint as P
> import Text.PrettyPrint(($$), ($+$), (<+>), (<>))

Zobrazení vizualizací
---------------------

Cokoli je možno zobrazit (vizualizovat) by mělo implementovat následující třídu:

> class Display t where
>     display :: t -> IO ()

Zobrazení seznamu položek je zobrazí v sekvenci:

> instance (Display t) => Display [t] where
>     display xs = mapM_ display xs

K použití GraphViz nástroje `dot` pro vizualisaci grafových dat
nejdříve definujeme datový typ pro representaci .dot dokumentů.

> newtype Dot = Dot P.Doc
> instance Show Dot where show (Dot d) = show d

A pak způsob zobrazení, co zavolá nástroj `dot` a pošle mu dokument na stdin.
Je použit výstupní formát xlib, který daný graf rovnou vykreslí na obrazovku
do nového okna (funguje pravděpodobnně jen na Linux / MacOS a to navíc za 
předpokladu, že je graphviz zkompilován s podporou xlib, patches welcome).

> instance Display Dot where
>     display dot = void $ readProcess "dot" ["-Txlib"] (show dot)

Získání vizualizací
-------------------

Datové typy podporující grafovou vizualizaci přes `dot` implementují třídu `ToDot`:

> class ToDot t where
>     toDot :: t -> Dot

Pomocná funkce pro části dokumentu 
