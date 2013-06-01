TIN: Klasifikace gramatik
=========================

Nejdříve je třeba definovat několik pomocných datových typů.

Bude-li potřeba uvažovat prázdné symboly, využujeme následujícího typu,
který reprezentuje buď konkrétní symbol nebo epsilon (ε).

> data WithEps alphabet = Sym alphabet | Eps deriving Eq

Podpora pro tisk na konsoli:

> instance (Show a) => Show (WithEps a) where
>     show (Sym x) = show x
>     show Eps = "ε"


Dále množiny terminálů a neterminálů bývají disjunktní, bude je třeba odlišit:

> data TN t n = T t | N n deriving (Eq, Show)
