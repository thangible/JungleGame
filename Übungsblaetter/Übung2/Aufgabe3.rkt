#lang racket





;1 money als Struct definiert
(struct money (amount currency))

;Für 4 die Liste definiert
(define *Kurse* '( (USD  0.8773)
                   ( Y  0.0074)
                   (  Euro  1.0)
                   (  CHF  1.2)
                   (  NOK  0.5)
                   (  GBP  1.5)))

;Für 5 die Liste definiert
(define *reisekasse* (list (money 10 'Euro)
                           (money 5 'Euro)
                           (money 500 'CHF)
                           (money 200 'Y)
                           (money 20 'NOK)
                           (money 100 'NOK)
                           (money 200 'USD)
                           (money 100 'GBP)
                           (money 300 'USD)))

;Für Testzwecke definiert
(define my-money (money 20 'Euro))

;2 selector
(define (amount-of money)
           (money-amount money))

(define (currency-of money)
           (money-currency money))

;3 Multiplikation von einem Geldbetrag
(define (multiply-depostit money x)
  (* (amount-of money) x))

;4 Rechnet Money in Euro um
(define (deposit->Euro money)
  (apply * (amount-of money) (cdr(assoc (currency-of money) *Kurse*))))


;5 Addiert zwei deposits
(define (deposit+ deposit1 deposit2)
  (+(deposit->Euro deposit1) (deposit->Euro deposit2))

  )

;5 Funktionen zur einzahlung und auszahlung
(define (einzahlen money)
  (append *reisekasse* money)
  )

(define (auszahlen money)
  (append *reisekasse* (money (- 0 (amount-of money) (currency-of money)))))

;6 Berechnet die Einauszahlung etc in Euro um
(define (everything-in-Euro konto)
  (map deposit->Euro konto)
  )
;7 Berechnet den Gesamtkontostand in Euro
(define (summeEuro konto)
  (foldl + 0 (everything-in-Euro konto)))
;8
(define (bestand konto waehrung)
  (filter (curry same-currency? waehrung) konto)
  )

  (define (same-currency? currency x)
    (equal? currency (currency-of x)  ))

;9
(define (summeWaehrung reisekasse waehrung)
(foldl + 0 (map amount-of (bestand reisekasse waehrung)
          )))



;Testen der Funktionen
(multiply-depostit my-money 5)
(deposit->Euro my-money)
(deposit+ my-money my-money)
(everything-in-Euro *reisekasse*)

(summeEuro *reisekasse*)

(summeEuro(bestand *reisekasse* 'Euro))

(summeWaehrung *reisekasse* 'NOK)