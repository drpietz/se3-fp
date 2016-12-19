#lang racket

(require test-engine/racket-tests)

; SE3:FP Hausaufgaben 03
; 
; Gruppe 11
; 6808046 Tim Pietz
; 6792069 Hung Quan Vu
;
; Aufgabe 1
; Teilaufgabe 1.1
; 
; Wir haben uns dafür entschieden, eine Liste von Pairs zu erstellen, die somit im Prinzip eine mit
; assoc durchsuchbare Map darstellt. Das vorere Element von Typ char stellt das zu kodierende Zeichen
; dar (Buchstabe), während das zugehörige Symbol den Code darstellt. 

(define seefunk-map
  '([#\A . Alfa]
    [#\B . Bravo]
    [#\C . Charlie]
    [#\D . Delta]
    [#\E . Echo]
    [#\F . Foxtrott]
    [#\G . Golf]
    [#\H . Hotel]
    [#\I . India]
    [#\J . Juliett]
    [#\K . Kilo]
    [#\L . Lima]
    [#\M . Mike]
    [#\N . November]
    [#\O . Oscar]
    [#\P . Papa]
    [#\Q . Quebec]
    [#\R . Romeo]
    [#\S . Sierra]
    [#\T . Tango]
    [#\U . Uniform]
    [#\V . Viktor]
    [#\W . Whiskey]
    [#\X . X-ray]
    [#\Y . Yankee]
    [#\Z . Zulu]
    [#\0 . Nadazero]
    [#\1 . Unaone]
    [#\2 . Bissotwo]
    [#\3 . Terrathree]
    [#\4 . Kartefour]
    [#\5 . Pantafive]
    [#\6 . Soxisix]
    [#\7 . Setteseven]
    [#\8 . Oktoeight]
    [#\9 . Novenine]
    [#\, . Decimal]
    [#\. . Stop]))

;>> Die Begründung, warum ihr genau diese Datenstruktur gewählt habt kommt etwas zu kurz.
;>> Vor Nachteile, Assoziationslisten, vs Vektoren, ...
;>> Liste von Paaren vs Liste von Listen.
;>> Wie lässt sich die Liste erweitern?
;>> 3 Pkt.

; Teilaufgabe 1.2
; 
; char->seefunk ruft die Hilfsfunktion char-encode mit der seefunk-map als Parameter auf, die wiederum
; die mithilfe der assoc Funktion den zum Zeichen gehörigen Schlüssel ausliest.

(check-expect (char->seefunk #\p) 'Papa)
(check-expect (char->seefunk #\5) 'Pantafive)

(define (char->seefunk char)
  (char-encode (zeichen->grossbuchstabe char) seefunk-map))

(define (char-encode char map)
  (cdr (assoc char map)))

;>> Gut, 4 Pkt.

; Teilaufgabe 1.3

(check-expect (zeichen->grossbuchstabe #\a) #\A)
(check-expect (zeichen->grossbuchstabe #\y) #\Y)
(check-expect (zeichen->grossbuchstabe #\G) #\G)
(check-expect (zeichen->grossbuchstabe #\1) #\1)

#|
(define (zeichen->grossbuchstabe char)
  (char-upcase char))
|#
; Alternative Implementation ohne Schummeln:
;

(define (zeichen->grossbuchstabe char)
  (let ((char-val (char->integer char)))
    (if [and (>= char-val (char->integer #\a))
             (<= char-val (char->integer #\z))]
        [integer->char (+ (- char-val (char->integer #\a))
                          (char->integer #\A))]
        char)))

;>> Die Implementation "ohne Schummeln" interessiert mich mehr :-)
;>> 5 Pkt.

; Teilaufgabe 1.4
;
; string-encode und die Hilfsfunktion wandeln einen String in eine Liste um, und wenden anschließend
; auf jedes Zeichen die Funktion an, die als char-encoder angegeben wurde.

(check-expect (seemannsgarn "David") '(Delta Alfa Viktor India Delta))

;>> Bitte? :-o

(define (seemannsgarn str)
  (string-encode str char->seefunk))

(define (string-encode str char-encoder)
  (string-encode-helper (string->list str) char-encoder))

(define (string-encode-helper lst char-encoder)
  (if [null? lst]
      '()
      [cons (char-encoder (car lst)) (string-encode-helper (cdr lst) char-encoder)]))

;>> 8 Pkt.

; Aufgabe 2

(require se3-bib/flaggen-module)

; Teilaufgabe 2.1
;
; Begründung? Siehe oben.

(define flaggenalphabet
  '([#\A . A]
    [#\B . B]
    [#\C . C]
    [#\D . D]
    [#\E . E]
    [#\F . F]
    [#\G . G]
    [#\H . H]
    [#\I . I]
    [#\J . J]
    [#\K . K]
    [#\L . L]
    [#\M . M]
    [#\N . N]
    [#\O . O]
    [#\P . P]
    [#\Q . Q]
    [#\R . R]
    [#\S . S]
    [#\T . T]
    [#\U . U]
    [#\V . V]
    [#\W . W]
    [#\X . X]
    [#\Y . Y]
    [#\Z . Z]))

;>> 3 Pkt.

; Teilaufgabe 2.2
; Alternative um eval zu umgehen, bestünde darin, das Flaggenalphabet als (list (cons #\A A) ...) zu
; initialisieren

;>> Warum macht ihr das nicht? Findet ihr das besser?

; Testeingaben:
; (char->flagge #\w)

(define (char->flagge char)
  (eval (char-encode (zeichen->grossbuchstabe char) flaggenalphabet)))

;>> 3 Pkt.

; Teilaufgabe 2.3

; Testeingaben:
; (string->flaggen "Racket")

(define (string->flaggen str)
  (string-encode str char->flagge))

;>> 8 Pkt.
;>> Gesamt: 34 Pkt.

(test)