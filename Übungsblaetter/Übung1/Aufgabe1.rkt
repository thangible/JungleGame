#lang racket
 ( define lilly 'Katze )
 ( define lechat lilly )
 ( define minou 'lilly )

 ( define ( welcherNameGiltWo PersonA PersonB )
 (let (( PersonA 'Sam )
 ( PersonC PersonA ) )
 PersonC ) )

( define xs1 '(0 1 2 lilly 'lechat ) )
( define xs2 ( cons lechat 'lilly ) )
( define xs3 ( list lechat 'lilly ) )



;1. lilly
; evaluiert zu Katze; Speicherung des Strings Katze in lilly
;2. lechat
;evaluiert zu Katze; ruft die "Funktion" lilly auf
;3. minou
; evaluiert zu lilly; Speichert den String lilly in minou
;4. ( quote lechat )
;evaluiert zu lechat
;5. ( eval minou )
;evaluiert zu Katze; Eval nimmt eine Expression oder Definition auf und evaluiert diese dann dynamisch als quote
;6. ( eval lechat )
;evaluiert zu einem Fehler; (eval (lechat)) wäre möglich, da lechat quasi eine funktion ist
;7. ( eval 'lechat )
;evaluiert zu Katze, Siehe 5
;8. ( welcherNameGiltWo 'Ich 'Du )
;evaluiert zu Ich; 
;9. ( cdddr xs1 )
;evaluiert zu (lilly 'lechat), cdr wählt das zweite element aus einem Pair cdddr = (cdr (cdr (cdr xsl)))
;10. ( cdr xs2 )
;evaluiert zu ´lilly
;11. ( cdr xs3 )
;evaluiert zu ´(lilly)
;12. ( eval ( sin 3) )
;evaluiert zu 0.1411200080598672; sin 3 berechnet den sinus von 3 eval siehe 5
;13. ( eval '( welcherNameGiltWo 'minou 'lechat ) )
;evaluiert zu ´minou ; welcherNameGiltWo gibt minou wieder minou wird nicht weiter evaluiert sondern `minou
;14. ( eval ( welcherNameGiltWo 'minou 'lechat ) )
;evaluiert zu `lilly; welcherNameGiltWo gibt minou wieder minou wird nun weiter evaluiert zu ´lilly