TIN: Klasifikace gramatik
=========================

Importy:

> import ZZZ_Visualise
> import qualified Text.PrettyPrint as P
> import Text.PrettyPrint(($$), ($+$), (<+>), (<>))

Nejdříve je třeba definovat několik pomocných datových typů.

Bude-li potřeba uvažovat prázdné symboly, využujeme následujícího typu,
který reprezentuje buď konkrétní symbol nebo epsilon (ε).

> data WithEps alphabet = Sym alphabet | Eps deriving Eq

Podpora pro tisk na konsoli:

> instance (Show a) => Show (WithEps a) where
>     show (Sym x) = show x
>     show Eps = "ε"


Dále množiny terminálů a neterminálů bývají disjunktní, bude je třeba odlišit:

> data TN t n = T t | N n deriving (Eq)

Bezkontextová gramatika
-----------------------

*Bezkontextová gramatika* (Context-free grammar) je čtveřice *G=(N,Σ,P,S)*, kde

 * *N* je konečná množina neterminálů (`n`)
 * *Σ* je konečná množina terminálů (`t`)
 * *P* je konečná množina pravidel *P ∈ N×(N∪Σ)<sup>*</sup>*
 * *S ∈ N* je výchozí symbol

> data CFG t n = CFG {
>     cfgRules :: [(n, [TN t n])],
>     cfgStart :: n
> }

Pṙíklad gramatiky pro aritmetické výrazy zde (opora, příklad 4.3):

> -- množina nonterminálů
> data CFG01N = Term | Vyraz | Faktor deriving (Eq, Show)
> -- gramatika
> cfg01 :: CFG Char CFG01N  -- terminály = znaky
> cfg01 = CFG {
>     cfgRules = [
>         (Vyraz, [N Term]), (Vyraz, [N Vyraz, T '+', N Term]), (Vyraz, [N Vyraz, T '-', N Term]),
>         (Term, [N Faktor]), (Term, [N Term, T '*', N Faktor]), (Term, [N Term, T '/', N Faktor]),
>         (Faktor, [T '(', N Vyraz, T ')']), (Faktor, [T 'i'])
>       ],
>     cfgStart = Vyraz
> }

Podobná gramatika bez levé rekurze (příjmá stejný jazyk, ale mění asociativitu operací
při pokusu o interpretaci derivačního stromu):

> cfg02 = CFG {
>     cfgRules = [
>         (Vyraz, [N Term]), (Vyraz, [N Term, T '+', N Vyraz]), (Vyraz, [N Term, T '-', N Vyraz]),
>         (Term, [N Faktor]), (Term, [N Faktor, T '*', N Term]), (Term, [N Faktor, T '/', N Term]),
>         (Faktor, [T '(', N Vyraz, T ')']), (Faktor, [T 'i'])
>       ],
>     cfgStart = Vyraz
> }

Tuto gramatiku je možno interpretovat jednoduchým parserem (viz níže).

> cfg02Parse = cfgParse cfg02
> test01 = cfg02Parse "i+i*(i-i)"

### Derivační strom

*Derivační strom* (parse tree) je kořenový strom s uspořádanými větvemi. Listy jsou anotovány terminály,
vnitřní uzly nonterminály, kořen odpovídá startovacímu symbolu.

> data PTree t n = PTreeT t | PTreeN n [PTree t n]

Nedeterministický parser pro bezkontextové gramatiky. **Pozor**, pokud gramatika obsahuje levou rekurzi, zacyklí se!
Jinak vrátí všechny derivační stromy vstupu vzhledem k dané gramatice.

> cfgParse :: (Eq t, Eq n) => CFG t n -> [t] -> [PTree t n]
> cfgParse (CFG rules start) input = [ tree | (tree, []) <- doTN (N start) input ]
>   where
>     doTN (T t) (i:inp) | i == t = [(PTreeT t, inp)]
>     doTN (T t) inp | otherwise  = []
>     doTN (N n) inp = concat $ map (\p -> map (treeify n) $ doSeq p inp) (rulesFor n)
>     rulesFor n = [ prod | (nt', prod) <- rules, nt' == n ]
>     treeify n (nodes, inp) = (PTreeN n nodes, inp)
>     doSeq [] inp = [([], inp)]
>     doSeq (tn:tns) inp0 = do
>          (tree,  inp1) <- doTN tn inp0
>          (trees, inp2) <- doSeq tns inp1
>          return (tree:trees, inp2)

Boring stuff
------------

Výpis (non)terminálu na terminál (sic!):

> instance (Show t, Show n) => Show (TN t n) where
>     show (T x) = show x
>     show (N x) = "<" ++ show x ++ ">"  -- neterminály se zobrazují v ostrých závorkách

Výpis gramatiky:

> instance (Show t, Show n) => Show (CFG t n) where
>     show (CFG rules start) = unlines (("Start: <" ++ show start ++ ">") : ruleLines)
>       where
>         ruleLines = map showRule rules
>         showRule (a, str) = "  <" ++ show a ++ ">  --> " ++ showProduction str
>         showProduction [] = " ε"
>         showProduction xs = concat $ map (\s -> ' ' : show s) xs

Tisknutí derivačního stromu:

> instance (Show t, Show n) => ToDoc (PTree t n) where
>     toDoc (PTreeT term) = P.text (show term)
>     toDoc (PTreeN nt children) = P.char '<' <> P.text (show nt) <> P.char '>' $+$ (P.nest 2 $ P.vcat $ map toDoc children)
> instance (Show t, Show n) => Show (PTree t n) where
>     show = show . toDoc
>     showList ts = showString . unlines $ map show ts

Links
-----

 * [Context-free grammar (wiki)](http://en.wikipedia.org/wiki/Context-free_grammar)
 * [Parse tree (wiki)](http://en.wikipedia.org/wiki/Parse_tree)
