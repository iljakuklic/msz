MAT: Algebraické struktury
==========================

Algebraickým strukturám odpovídají v Haskellu typové třídy, nosná množina je typová proměnná dané třídy.
Požadované zákony začínají na `prop` (od property) a vrací `True` je-li daný zákon splněn.

> propAssociative  (.%.) a b c   =   ((a .%. b) .%. c) == (a .%. (b .%. c))
> propLeftNeutral  (.%.) e x     =   (e .%. x) == x
> propRightNeutral (.%.) e x     =   (x .%. e) == x
> propNeutral      (.%.) e x     =   propLeftNeutral (.%.) e x && propRightNeutral (.%.) e x
> propInverse      (.%.) e x x_1 =   ((x .%. x_1) == e) && ((x_1 .%. x) == e)

Rovnou můžeme otestovat (nikoli dokázat!) asociativitu a neutrálnost jedničky u násobení:

> test01 = and [ propAssociative (*) a b c | a <- [1..10], b <- [(-5)..4], c <- [0..9] ]
> test02 = and [ propNeutral (*) 1 x | x <- [(-100)..100] ]

Grupoid
-------

Algebra (A,\*) typu (2) se nazývá *grupoid*.

V následujícḿ kódu je `a` množina A. Operace * je `.*.`.

> infix 7 .*.
> class Groupoid a where
>     -- jedna binární operace, zapisujeme ji .*.
>     (.*.) :: a -> a -> a

Pologrupa
---------

*Pologrupa* vyžaduje, aby `.*.` byla asociativní.

> class (Groupoid a) => Semigroup a where
>     -- žádné operace navíc, pouze test na asociativitu (.*.)
>     propSemigroup :: Eq a => a -> a -> a -> Bool
>     propSemigroup a b c = propAssociative (.*.) a b c

Příklad pologrupy (a tedy i grupoidu) jsou například celá čísla s operací maximum.

> newtype MaxInt = MaxInt Integer deriving (Ord, Eq)
> instance Groupoid  MaxInt where a .*. b = max a b
> instance Semigroup MaxInt where
> test03 = and [ propSemigroup (MaxInt a) (MaxInt b) (MaxInt c) | a <- r, b <- r, c <- r ]
>                                                                where r = [1..5]

Nebo celá čísla s násobením

> instance Groupoid  Integer where a .*. b = a * b
> instance Semigroup Integer where
> test03_1 = and [ propSemigroup a b c | a <- r, b <- r, c <- r]
>                              where r = [(-5)..10] :: [Integer]

Monoid
------

*Monoid* (M, je pologrupa s neutrálním prvkem `m1`.

> class (Semigroup m) => Monoid m where
>     -- neutrální prvek
>     m1 :: m
>     -- test neutrality
>     propMonoid :: Eq m => m -> Bool
>     propMonoid x = propNeutral (.*.) m1 x

Typickým příkladem monoidu jsou řetězce symbolů (slova) s operací konkatenace a neutrálním prvkem prázdný řetězec.

> newtype Str = Str String deriving Eq
> -- má operaci spojení řetězců
> instance Groupoid  Str where (Str a) .*. (Str b) = Str (a ++ b)
> instance Semigroup Str where              -- která je asociativní
> instance Monoid    Str where m1 = Str ""  -- a má neutrální prvek
> -- a dá se to demonstraovat
> test04_1 = and [ propSemigroup (Str a) (Str b) (Str c) | a <- ["x", "zz", "", "quak", "howg"],
>                                b <- ["hi", "", "omg"], c <- ["noes", "y", "end"] ]
> test04_2 = and [ propMonoid (Str x) | x <- ["some", "random", "words"] ]

Nebo také celá čísla s násobením. Neutrální prvek je 1.

> instance Monoid Integer where m1 = 1
> test04_3 = and [ propMonoid x | x <- [(-1000 :: Integer)..1000] ]

Celá čísla s operací maximum netvoří monoid, neboť `max` nemá neutrální prvek.

Grupa
-----

Algebra (G,\*,e,<sup>-1</sup>) typu (2,0,1) je *grupa*, když (G,\*,e) je monoid a každý prvek *x*<sup>-1</sup> je inverzí *x* a obráceně.

> class (Monoid g) => Group g where
>     inv :: g -> g
>     propGroup :: Eq g => g -> Bool
>     propGroup x = propInverse (.*.) m1 x (inv x)


Testy
-----

> tests = [test01, test02, test03, test03_1, test04_1, test04_2, test04_3]
