TIN: Konečné Automaty
=====================

Obligatoární importy:

> import Data.List(nub)
> import qualified Data.Set as S
> import qualified Data.Map as M
> import ZZZ_Visualise
> import qualified Text.PrettyPrint as P
> import Text.PrettyPrint(($$), ($+$), (<+>), (<>))
> import Control.Monad
> import Data.Foldable as F
> import Prelude hiding (or, any, concat, elem, foldr, foldl)

Nedeterministický KA
--------------------

*Nedeterministický konečný automat* je pětice:

 * Konečná abeceda Σ
 * Konečná množina stavů Q
 * Počáteční stav q<sub>0</sub> ∈ Q
 * Přechodová relace δ ∈ S×Σ×S
 * množina koncových stavů F ⊆ Q

> data NDFSM ste alpha = NDFSM {               -- ste = stavy, alpha = abeceda
>     nd_q0     :: ste,                        -- poč. stav
>     nd_delta  :: S.Set (ste, alpha, ste),    -- přechodová relace
>     nd_fini   :: ste -> Bool                 -- koncové stavy dané charakteristickou funkcí
>   }

(Tato reprezentace nijak nevynucuje konečnost příslušných množin.)

KA akceptuje řetězec, pokud existuje alespoň jedna posloupnost přechodů
z daného stavu do koncového během zpracování vstupního řetězce.
Nedeterminismus reprezentujeme pomocí monády seznam (bacha, může být neefektivní).

> -- prázdný řetězec je přijat, jestliže se nacázíme v koncovém stavu
> ndAcceptsFrom fsm state [] = return (nd_fini fsm state)
> -- jinak vyzkoušíme všechny možné následné stavy
> ndAcceptsFrom fsm state (x:xs) = do
>     nextState <- nub [ s' | (s, a, s') <- S.toList (nd_delta fsm), s == state, x == a ]
>     ndAcceptsFrom fsm nextState xs

Je vstup akceptován z počátečního stavu?

> ndAccepts :: (Eq alpha, Eq t) => NDFSM t alpha -> [alpha] -> Bool
> ndAccepts fsm input = or $ ndAcceptsFrom fsm (nd_q0 fsm) input

Získání použitých stavů (ne všechny musí být dosažitelné) a abecedy (ne všechny symboly
z množiny alpha musí být použity):

> ndStates fsm = S.fromList $ [nd_q0 fsm] ++ concat [ [q,r] | (q,_,r) <- S.toList (nd_delta fsm) ]
> ndAlphabet fsm = S.fromList $ [ a | (_, a, _) <- S.toList (nd_delta fsm) ]

Lze také vytvořit automat příjmající komplement daného jazyka:

> ndComplement fsm = fsm { nd_fini = not . nd_fini fsm }

Následující nedeterministický konečný automat příjimá čísla od 0 do 255 zapsané pomocí desítkových
číslic a ukončené znakem `#`. Jako množina stavů je použita podmnožina přirozených čísel.

> fsm00 = NDFSM {
>     nd_q0 = (0 :: Int),
>     nd_fini = \q -> (q == 1),
>     nd_delta = S.fromList ( [(0,d,2) | d <- digits ]                               -- 0-9
>         ++ [ (0,d,3) | d <- tail digits ] ++ [ (3,d,4) | d <- digits ]             -- 10-99
>         ++ [(0,'1',5)] ++ [ (5,d,6) | d <- digits ] ++ [ (6,d,7) | d <- digits ]   -- 100-199
>         ++ [(0,'2',8)] ++ [(8,d,9) | d <- ['0'..'4']] ++ [(9,d,13) | d <- digits] -- 200-249
>         ++ [(0,'2',10), (10,'5',11)] ++ [(11,d,12) | d <- ['0'..'5']]              -- 250-255
>         ++ [ (q, '#', 1) | q <- [2,4,7,13,12] ] )  -- EOS
>     }
>   where digits = ['0'..'9']

Pár ukázek:

> fsm00_test01 = ndAccepts fsm00 "7#"
> fsm00_test02 = ndAccepts fsm00 "33#"
> fsm00_test03 = ndAccepts fsm00 "255#"
> fsm00_test04 = ndAccepts fsm00 "260#"  -- odmítne
> fsm00_test05 = ndAccepts fsm00 "#"     -- odmítne

Vizualizaci automatu je možno provést takto:

> fsm00_display = display $ toDot fsm00

Deterministický konečný automat
-------------------------------

*Deterministický KA*
se liší především tvarem přechodové relace, která je funkcí ze starého stavu a symbolu na vstupu
do nového stavu. Zbytek definice je stejný.

 * Přechodová funkce δ : S×Σ→S

> data DFSM ste alpha = DFSM {              -- ste = stavy, alpha = abeceda
>     d_q0     :: ste,                      -- poč. stav
>     d_delta  :: M.Map (ste, alpha) ste,   -- přechodová funkce (reprezentována MAPem)
>     d_fini   :: ste -> Bool               -- koncové stavy dané charakteristickou funkcí
>   }

Každý deterministický KA je speciálním případem nedeterministického a je možné jej triviálně převést.

> unDeterminize :: (Ord alpha, Ord ste) => DFSM ste alpha -> NDFSM ste alpha
> unDeterminize fsm@(DFSM q0 delta fini) = NDFSM q0 delta' fini
>     where delta' = S.fromList [ (q,a,r) | ((q,a),r) <- M.toList delta ]

Akceptující funkci recyklujeme od nedeterministického:

> dAccepts fsm input = ndAccepts (unDeterminize fsm) input

Příklad deterministického konečného automatu (stavy označíme čísly, opora: příklad 3.1):

> fsm01 = DFSM {
>     d_q0 = (0 :: Int),
>     d_fini = \q -> (q == 1),
>     d_delta = M.fromList ( [ ((0, 'z'), 8), ((4, 'z'), 3) ]
>         ++ [ ((q, 'c'), r) | (q,r) <- [(0,7),(2,2),(3,2),(4,2),(5,5),(6,5),(7,7),(8,7)] ]
>         ++ [ ((q, '.'), 6) | q <- [0,7,8] ]
>         ++ [ ((q, 'e'), 4) | q <- [0,5,7,8] ]
>         ++ [ ((q, '#'), 1) | q <- [2,5,7] ] )
>   }

Těmito funkcemi jej lze převést na graphviz a vizualisovat:

> fsm01_dot = toDot fsm01
> fsm01_display = display fsm01_dot

Vyzkoušíme, zda příjmá řetězec *zc.cezc#* (příklad v opoře) a pár dalších následovně:

> fsm01_test01 = dAccepts fsm01 "zc.cezc#"  -- OK
> fsm01_test02 = dAccepts fsm01 "c.ccc#"    -- OK
> fsm01_test03 = dAccepts fsm01 ".cezccc#"  -- OK
> fsm01_test04 = dAccepts fsm01 "zc.cezc"   -- chybí konec (odmítne)
> fsm01_test05 = dAccepts fsm01 "#"         -- prázdné číslo (odmítne)
> fsm01_test06 = dAccepts fsm01 "c.cc.cc#"  -- více desetinných teček (odmítne)

Tento automat je podobný předchozímu, nicméně příjimá skutečná čísla ve vědeckém formátu (zakončené znakem `#`).
Má tedy mnohem více přechodů.

> fsm02 = DFSM {
>     d_q0 = (0 :: Int),
>     d_fini = \q -> (q == 1),
>     d_delta = M.fromList ( concat [ [((0, z), 8), ((4, z), 3)] | z <- "+-" ]
>         ++ [ ((q, c), r) | (q,r) <- [(0,7),(2,2),(3,2),(4,2),(5,5),(6,5),(7,7),(8,7)], c <- ['0'..'9'] ]
>         ++ [ ((q, '.'), 6) | q <- [0,7,8] ]
>         ++ [ ((q, 'e'), 4) | q <- [0,5,7,8] ]
>         ++ [ ((q, '#'), 1) | q <- [2,5,7] ] )
>   }

Masochisti můžou vyzkoušet:

> fsm02_display = display $ toDot fsm02

Determinizace KA
----------------

Pro každý nedeterministický KA lze sestavit ekvivalentní (přijímající stejný jazyk) deterministický KA.
Tento svými stavy simuluje podmnožinu stavů, ve kterých můze nedeterministický KA v jeden moment být.

> determinize :: (Ord alpha, Ord ste) => NDFSM ste alpha -> DFSM (S.Set ste) alpha
> determinize fsm@(NDFSM q0 delta fini) = DFSM (S.singleton q0) delta' fini'
>     where
>         -- Přechodová funkce (pouze pro dosažitelné stavy)
>         delta' = mks M.empty (S.singleton q0)
>         mks acc s | S.null s = acc
>         mks acc s = foldl mkt acc [ (s, a) | a <- toList $ ndAlphabet fsm ]
>         mkt acc k | k `M.member` acc = acc
>         mkt acc k@(s, a) = if S.null tgt then acc else mks (M.insert k tgt acc) tgt
>             where tgt = S.fromList [r | (q,a',r) <- S.toList delta, a' == a, q `elem` s]
>         next s = [(s, a') | a' <- toList $ ndAlphabet fsm ]
>         -- stav je konecový, jestliže obsahuje některý z původních stavů
>         fini' q' = any (\q -> fini q) q'

Deterministická verze automatu `fsm00` vypadá takto:

> fsm03 = determinize fsm00
> fsm03_display = display $ toDot fsm03

Přestože v tomto konkrétním případě počet stavů mírně klesne, obecně jich může vzniknout až *O(2<sup>|Q|</sup>)*.

Boring stuff
------------

Vykreslení FSM do formátu .dot:

> instance (Show a, ToDoc s, Ord s) => ToDot (NDFSM s a) where
>     toDot fsm@(NDFSM q0 delta fini) = Dot dot where
>         states = zip (S.toList $ ndStates fsm) [ P.text ('n':show n) | n <- [1..] ]
>         getSte q = maybe (P.text "x") id $ lookup q states
>         dot = P.text "digraph {" $$ P.nest 4 (top $$ nodes $$ edges) $$ P.text "}"
>         top = P.text "node [shape=\"circle\"]" $$ P.text "s [shape=\"plaintext\",label=\"\"]"
>               $$ P.text "s ->" <+> getSte q0
>         nodes = P.vcat $ map node states
>         node (q,d) = d <+> P.brackets (
>                      P.text "label=" <> P.doubleQuotes (toDoc q) <> P.comma <+>
>                      if fini q then P.text "shape=\"doublecircle\"" else P.empty)
>         edges = P.vcat [ getSte q <+> P.text "->" <+> getSte r <+>
>                          P.brackets (P.text "label=" <> P.doubleQuotes (P.text $ show a))
>                          | (q,a,r) <- S.toList delta ]
> instance (Show a, Ord a, ToDoc s, Ord s) => ToDot (DFSM s a) where
>     toDot = toDot . unDeterminize

Links
-----

 * [Finite-state machine (wiki)](https://en.wikipedia.org/wiki/Finite-state_machine)
