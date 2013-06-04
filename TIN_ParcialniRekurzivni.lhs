TIN: Parciální rekurzivní funkce
================================

*Parciální rekurzivní funkce* budou reprezentovány tímto typem:

> newtype PRF = PRF ([Int] -> [Int])

Pro jednoduchost nebudeme kontrolovat, zda odpovídají délky vektorů.
Pokud tomu tak nebude, jednoduše selže pattern matching.

Pro vyhodnocení primitivně rekurzivní funkce je možno použít tuto funkci:

> evalPRF :: PRF -> [Int] -> [Int]
> evalPRF (PRF f) x = f x

Počáteční funkce
----------------

*Nulová funkce* (zero function)

> z :: PRF
> z = PRF (\[] -> [0])

*Funkce následníka* (successor)

> s :: PRF
> s = PRF (\[x] -> [x+1])

*Projekce* (projection).
Rodina funkcí vybírající *k*-tou složku vektoru.

> p :: Int -> PRF
> p 0 = PRF (\_ -> [])  -- speciální případ
> p k = PRF (\xs -> [xs !! (k-1)])

Primitivně rekurzivní funkce
----------------------------

Primitivně rekurzivní funkce je možno skládat z počátečních
a jiných primitivně rekurzivních funkcí pomocí následujících kombinátorů.

*Kombinace:* z funkcí *f : ℕ<sup>k</sup>→ℕ<sup>m</sup>* a *g : ℕ<sup>k</sup>→ℕ<sup>n</sup>*
získáme funkci *f×g : ℕ<sup>k</sup>→ℕ<sup>m+n</sup>*.
Znaménko '×' budeme zapisovat `><`.

> infix 7 ><
> (><) :: PRF -> PRF -> PRF
> (PRF f) >< (PRF g) = PRF (\xs -> f xs ++ g xs)

Pṙíklad:

> test01 = evalPRF (p 1 >< p 3) [5,71,3,6]  -- => [5,3]

*Kompozice* dvou funkcí *f : ℕ<sup>k</sup>→ℕ<sup>m</sup>* a *g : ℕ<sup>m</sup>→ℕ<sup>n</sup>*
je funkce *g∘f : ℕ<sup>k</sup>→ℕ<sup>n</sup>*. Značíme `<>`.

> infix 6 <>
> (<>) :: PRF -> PRF -> PRF
> (PRF g) <> (PRF f) = PRF (g . f)

Kombinátor *primitivní rekurze* vytvoří na základě funkcí *g : ℕ<sup>k</sup>→ℕ<sup>m</sup>* a *h : ℕ<sup>k+m+1</sup>→ℕ<sup>m</sup>*
funkci *prec g h : ℕ<sup>k+1</sup>→ℕ<sup>m</sup>*.

> prec :: PRF -> PRF -> PRF
> prec (PRF g) (PRF h) = PRF (\xs -> f (init xs) (last xs))
>   where
>     f xs 0 = g xs
>     f xs n = h (xs ++ [n-1] ++ f xs (n-1))

Příklady
--------

Sčítání:

> plus = prec (p 1) (s <> p 3)
> evalPlus x y = evalPRF plus [x,y]

Rodina konstantních funkcí (dá se definovat i na základě počátečních funkcí a primitivní rekurze, tady to ošmelíme zkratkou):

> konst :: Int -> PRF
> konst m = PRF (\_ -> [m])

Předchůdce *decr* (předchůdce nuly je 0, jinak předchůdce *n* je *n-1*):

> decr = prec (z <> p 0) (p 1)

Násobení:

> mult = prec (z <> p 0) (plus <> (p 1 >< p 3))

Parciálně rekurzivní funkce
---------------------------

*Minimalizace* vytvoří z funkce *f : ℕ<sup>n+1</sup>→ℕ* funkci *µf : ℕ<sup>n</sup>→ℕ*,
přičemž poslední složka vektoru funkce *f* je nejmenší taková, že tato funkce se vyhodnotí na 0.

> mu :: PRF -> PRF
> mu (PRF g) = PRF (\xs -> return $ head [ y | y <- [0..], g (xs ++ [y]) == [0] ])

Links
-----

 * [Primitive recursive function (wiki)](http://en.wikipedia.org/wiki/Primitive_recursive_function)
