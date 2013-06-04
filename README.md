FIT Státnice 2013
=================

Některé státnicové otázky jakožto [literate code](http://en.wikipedia.org/wiki/Literate_programming) převážně v [Haskell](http://www.haskell.org/haskellwiki/Haskell)u.
Soubory .lhs je možno normálně zpracovat v GHC/GHCi, přičemž řádky uvozené zobáčkem `>` jsou kód a zbytek jsou komentáře.
GitHub to taky přežvýká a ozobáčkovaným řádkům zvýrazní syntaxi, zatímco zbytek považuje za [Markdown](http://daringfireball.net/projects/markdown/syntax) dokument.

Jmenné konvence: `ZKR_KratkyNazevOtazky.lhs`, kde `ZKR` je zkratka předmětu, která je následována podtržítkem a zkráceným názvem otázky.
Číslo otázky uvedeno není, stejně to má každý obor jinak...
Pokud by se nějaká otázka příliš rozlezla, bude rozělena na několik částí následovně: `ZKR_KratkyNazevOtazky_Podotazka.lhs`.

Příklad interakce
-----------------

1. Spuštění GHCi: `ghci TIN_KonecneAutomaty.lhs`
2. Vyzkoušení definovaných funkcí:

```Haskell
*Main> -- zobrazíme si předdefinovaný konečný automat
*Main> display $ toDot fsm00
*Main> -- a jeho deterministickou podobu
*Main> display $ determinize toDot fsm00
*Main> -- zkusíme, zda akceptuje nějaký vstup
*Main> ndAccepts fsm00 "42#"
True
*Main> ndAccepts fsm00 "420#"
False
*Main> :q
```

Závěrem
-------

Přispivatelé, patche a forky vítány. Enjoy.
