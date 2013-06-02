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

Příklad nedeterministického konečného automatu (stavy označíme čísly, opora: příklad 3.1):

> fsm01 = NDFSM {
>     nd_q0 = 0,
>     nd_fini = \q -> (q == 1),
>     nd_delta = S.fromList ( [ (0, 'z', 8), (4, 'z', 3) ]
>         ++ [ (q, 'c', r) | (q,r) <- [(0,7),(2,2),(3,2),(4,2),(5,5),(6,5),(7,7),(8,7)] ]
>         ++ [ (q, '.', 6) | q <- [0,7,8] ]
>         ++ [ (q, 'e', 4) | q <- [0,5,7,8] ]
>         ++ [ (q, '#', 1) | q <- [2,5,7] ] )
>   }

Těmito funkcemi jej lze převést na graphviz a vizualisovat:

> fsm01_dot = toDot fsm01
> fsm01_display = display fsm01_dot

Vyzkoušíme, zda příjmá řetězec *zc.cezc#* (příklad v opoře) a pár dalších následovně:

> fsm01_test01 = ndAccepts fsm01 "zc.cezc#"  -- OK
> fsm01_test02 = ndAccepts fsm01 "c.ccc#"    -- OK
> fsm01_test03 = ndAccepts fsm01 ".cezccc#"  -- OK
> fsm01_test04 = ndAccepts fsm01 "zc.cezc"   -- chybí konec (odmítne)
> fsm01_test05 = ndAccepts fsm01 "#"         -- prázdné číslo (odmítne)
> fsm01_test06 = ndAccepts fsm01 "c.cc.cc#"  -- více desetinných teček (odmítne)

Tento automat je podobný předchozímu, nicméně příjimá skutečná čísla ve vědeckém formátu (zakončené znakem `#`).
Má tedy mnohem více přechodů.

> fsm02 = NDFSM {
>     nd_q0 = 0,
>     nd_fini = \q -> (q == 1),
>     nd_delta = S.fromList ( concat [ [(0, z, 8), (4, z, 3)] | z <- "+-" ]
>         ++ [ (q, c, r) | (q,r) <- [(0,7),(2,2),(3,2),(4,2),(5,5),(6,5),(7,7),(8,7)], c <- ['0'..'9'] ]
>         ++ [ (q, '.', 6) | q <- [0,7,8] ]
>         ++ [ (q, 'e', 4) | q <- [0,5,7,8] ]
>         ++ [ (q, '#', 1) | q <- [2,5,7] ] )
>   }

Masochisti můžou vyzkoušet:

> fsm02_display = display $ toDot fsm02

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

Pro každý nedeterministický KA lze sestavit ekvivalentní (přijímající stejný jazyk) deterministický KA.
Tento svými stavy simuluje podmnožinu stavů, ve kterých můze nedeterministický KA v jeden moment být.

Boring stuff
------------

Vykreslení FSM do formátu .dot:

> instance (Show a, Show s, Ord s) => ToDot (NDFSM s a) where
>     toDot fsm@(NDFSM q0 delta fini) = Dot dot where
>         states = zip (S.toList $ ndStates fsm) [ P.text ('n':show n) | n <- [1..] ]
>         getSte q = maybe (P.text "x") id $ lookup q states
>         dot = P.text "digraph {" $$ P.nest 4 (top $$ nodes $$ edges) $$ P.text "}"
>         top = P.text "node [shape=\"circle\"]" $$ P.text "s [shape=\"plaintext\",label=\"\"]"
>               $$ P.text "s ->" <+> getSte q0
>         nodes = P.vcat $ map node states
>         node (q,d) = d <+> P.brackets (
>                      P.text "label=" <> P.doubleQuotes (P.text $ show q) <> P.comma <+>
>                      if fini q then P.text "shape=\"doublecircle\"" else P.empty)
>         edges = P.vcat [ getSte q <+> P.text "->" <+> getSte r <+>
>                          P.brackets (P.text "label=" <> P.doubleQuotes (P.text $ show a))
>                          | (q,a,r) <- S.toList delta ]
> instance (Show a, Ord a, Show s, Ord s) => ToDot (DFSM s a) where
>     toDot = toDot . unDeterminize

Links
-----

 * [Finite-state machine (wiki)](https://en.wikipedia.org/wiki/Finite-state_machine)
