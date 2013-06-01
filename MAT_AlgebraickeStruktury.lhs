MAT: Algebraické struktury
==========================

Bude se hodit balík `Monoid` ze standardní knihovny. Importujeme jako qualified, abychom předešli kolizím
s našimi definicemi.

> import qualified Data.Monoid as M
> import Data.Monoid(Sum(Sum), Product(Product))

Algebraickým strukturám odpovídají v Haskellu typové třídy, nosná množina je typová proměnná dané třídy.
Požadované zákony začínají na `prop` (od property) a vrací `True` je-li daný zákon splněn.

> propAssociative  (.%.) a b c       =   ((a .%. b) .%. c) == (a .%. (b .%. c))
> propLeftNeutral  (.%.) e x         =   (e .%. x) == x
> propRightNeutral (.%.) e x         =   (x .%. e) == x
> propNeutral      (.%.) e x         =   propLeftNeutral (.%.) e x && propRightNeutral (.%.) e x
> propInverse      (.%.) e x x_1     =   ((x .%. x_1) == e) && ((x_1 .%. x) == e)
> propCommutative  (.%.) x y         =   (x .%. y) == (y .%. x)
> propDistributes  (.%.) (.+.) a b c =   (a .%. (b .+. c)) == ((a .%. b) .+. (a .%. c)) &&
>                                        ((b .+. c) .%. a) == ((b .%. a) .+. (c .%. a))

Rovnou můžeme otestovat (nikoli dokázat!) asociativitu a neutrálnost jedničky u násobení:

> test01 = and [ propAssociative (*) a b c | a <- [1..10], b <- [(-5)..4], c <- [0..9] ]
> test02 = and [ propNeutral (*) 1 x | x <- [(-100)..100] ]

Grupoid
-------

Algebra (A,\*) typu (2) se nazývá *grupoid*.

V následujícḿ kódu je `a` množina A. Operace * je `.*.`.

> infix 7 .*.
> class Eq a => Groupoid a where
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
>     -- test neutrality m1 vůči operaci .*.
>     propMonoid :: Eq m => m -> Bool
>     propMonoid = propNeutral (.*.) m1

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

Mějme nyní modulární aritmetiku modulo 17-ti, Z<sub>17</sub>.

> newtype Z17 = Z17 Integer deriving Eq
> -- chytrý konstruktor, co zaručí, že jsme v rozsahu 0..16
> z17 x = Z17 $ let x' = x `mod` 17 in if x' < 0 then x' + 17 else x'
> -- tisk
> instance Show Z17 where show (Z17 x) = show x ++ " (mod 17)"
> -- numerická instance
> instance Num Z17 where
>     (Z17 x) + (Z17 y) = z17 (x + y)
>     (Z17 x) * (Z17 y) = z17 (x * y)
>     negate (Z17 x) = z17 (negate x)
>     abs x = x
>     signum (Z17 x) = z17 (signum x)
>     fromInteger x = z17 x

Modulární aritmetika je grupa s operací sčítání:

> instance Groupoid  Z17 where a .*. b = a + b
> instance Semigroup Z17 where
> instance Monoid    Z17 where m1 = z17 0
> instance Group     Z17 where inv = negate

Demonstrovat funkčnost lze testem:

> test05_1 = 5 + 14 == z17 2

Abelovská grupa
---------------

Komutativní neboli symetrická neboli *abelovská grupa* přidává požadavek komutativity.

> class Group g => AbelGroup g where
>     propAbelGroup :: g -> g -> Bool
>     propAbelGroup = propCommutative (.*.)

Okruh
-----

Algebra (R,+,0,-,*) typu (2,0,1,2) se nazývá *okruh*, jsou-li spněny následující podmínky:

 * (R,+,0,-) je abelovská grupa
 * (R,*) je pologrupa
 * * je distributivní nad +

Nadefinujeme potřebné operátory, násobení má přednost a dostane tedy vyšší prioritu:

> infix 7 .*
> infix 6 .+
> infix 6 .-

> class Eq r => Ring r where
>     (.+) :: r -> r -> r    -- aditivní operace
>     r0   :: r              -- neutrální prvek pro aditivní operaci
>     rneg :: r -> r         -- negace
>     (.*) :: r -> r -> r    -- multiplikativní operace
>     -- distributivita * nad +
>     propRing :: r -> r -> r -> Bool
>     propRing = propDistributes  (.*) (.+)

Aditivní operace má být abelovská grupa:

> instance (Ring r) => Groupoid  (Sum r) where (Sum x) .*. (Sum y) = Sum (x .+ y)
> instance (Ring r) => Semigroup (Sum r) where
> instance (Ring r) => Monoid    (Sum r) where m1 = (Sum r0)
> instance (Ring r) => Group     (Sum r) where inv (Sum x) = Sum (rneg x)
> instance (Ring r) => AbelGroup (Sum r) where

Multiplikativní operace má být pologrupa:

> instance (Ring r) => Groupoid  (Product r) where (Product x) .*. (Product y) = Product (x .* y)
> instance (Ring r) => Semigroup (Product r) where

Odčítání pro aditivní operaci je definováno takto:

> x .- y = x .+ rneg y

Algebra s celými čísly (Z,+,0,-,*) tvoří okruh:

> instance Ring Integer where
>     a .+ b = a + b    -- sčítání
>     r0     = 0        -- neutrální prvek sčítání
>     rneg x = 0 - x    -- negace
>     a .* b = a * b    -- násobení


Linky
-----

 * [Monoid (wiki)](http://en.wikipedia.org/wiki/Monoid)
 * [Group (wiki)](http://en.wikipedia.org/wiki/Group_%28mathematics%29)
 * [Ring (wiki)](http://en.wikipedia.org/wiki/Ring_%28mathematics%29)
