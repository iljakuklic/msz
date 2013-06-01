TIN: Konečné Automaty
=====================

Obligatoární importy:

> import Data.List(nub)
> import qualified Data.Set as S

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

