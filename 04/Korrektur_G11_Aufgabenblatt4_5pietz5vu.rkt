#lang racket

(require test-engine/racket-tests)
(require se3-bib/tools-module)

; SE3:FP Hausaufgaben 04
; 
; Gruppe 11
; 6808046 Tim Pietz
; 6792069 Hung Quan Vu
;
; Aufgabe 1
; 
; 1. (max (min 5 (- 6 7)) 8)
;    -> 8
;
; 2. '(+ (- 11 13) 17)
;    -> '(+ (- 11 13) 17)
;
; 3. (cadr '(Alle Jahre wieder))
;    -> 'Jahre
;
; 4. (cddr '(kommt (das Christuskind)))
;    -> '(Christuskind)
;
; 5. (cons 'Auf '(die Erde nieder))
;    -> '(Auf die Erde nieder)
;
; 6. (cons 'Wo 'wir)
;    -> '(Wo . Wir)
;
; 7. (equal? (list 'Menschen 'sind) '(Menschen sind))
;    -> #t
;
; 8. (eq? (list 'Rudolph 'Das 'Rentier) (cons 'Rudolph '(Das Rentier)))
;    -> #f
;

;>> Bitte solche Aufgaben _immer_ mit Begründung, abtippen ist nicht die Kunst.
;>> 3 Pkt.

;
; Aufgabe 2
;
; Copy & Paste aus letzter Woche:

(define seefunk-map '([#\A . Alfa] [#\B . Bravo] [#\C . Charlie] [#\D . Delta] [#\E . Echo] [#\F . Foxtrott] [#\G . Golf] [#\H . Hotel] [#\I . India] [#\J . Juliett] [#\K . Kilo] [#\L . Lima] [#\M . Mike] [#\N . November] [#\O . Oscar] [#\P . Papa] [#\Q . Quebec] [#\R . Romeo] [#\S . Sierra] [#\T . Tango] [#\U . Uniform] [#\V . Viktor] [#\W . Whiskey] [#\X . X-ray] [#\Y . Yankee] [#\Z . Zulu] [#\0 . Nadazero] [#\1 . Unaone] [#\2 . Bissotwo] [#\3 . Terrathree] [#\4 . Kartefour] [#\5 . Pantafive] [#\6 . Soxisix] [#\7 . Setteseven] [#\8 . Oktoeight] [#\9 . Novenine] [#\, . Decimal] [#\. . Stop]))

(define (seemannsgarn str)
  (string-encode str char->seefunk))

(define (string-encode str char-encoder)
  (string-encode-helper (string->list str) char-encoder))

(define (string-encode-helper lst char-encoder)
  (if [null? lst]
      '()
      [cons (char-encoder (car lst)) (string-encode-helper (cdr lst) char-encoder)]))

(define (char->seefunk char)
  (char-encode (zeichen->grossbuchstabe char) seefunk-map))

(define (char-encode char map)
  (cdr (assoc char map)))

(define (zeichen->grossbuchstabe char)
  (char-upcase char))


; Teilaufgabe 2.1
;
; <Notfallmeldung>  ::= <Überschrift> <Notfallposition> <Notfallzeit> <Informationen>
;                       'ICH SENDE DEN TRÄGER --' <Unterschrift> 'OVER'
; <Überschrift>     ::= 'MAYDAY MAYDAY MAYDAY' <Ausruf> <Schiffsname> <Schiffsname> <Schiffsname>
;                       <B-Rufzeichen> 'MAYDAY' <Schiffsname> 'ICH BUCHSTABIERE' <B-Schiffsname>
;                       'RUFZEICHEN' <B-Rufzeichen>
; <Ausruf>          ::= 'HIER IST' | 'DELTA ECHO'
; <Notfallposition> ::= 'NOTFALLPOSITION' <Position>
; <Notfallzeit>     ::= 'NOTFALLZEIT' <Zeit>
; <Unterschrift>    ::= <Schiffsname> <B-Rufzeichen>
;

;>> Die Grammatik ist noch nicht fertig.
;>> z.B. gibt es keine Ableitungsregel für <B-Rufzeichen> und <Schiffsname>
;>> Irgendwie muss man von allen Nonterminalen zu Terminalsymbolen kommen.
;>> Das könnte man z.B. wie folgt machen:
;>> <Schiffsname> ::= <Zeichenkette>
;>> <Zeichenkette> ::= <Buchstabe> | <Buchstabe> <Zeichenkette>
;>> <Buchstabe> ::= A | B | C | ...

;>> 2 Pkt.

; Teilaufgabe 2.2
;
; Entspricht grundsätzlich der Vorgabe in der Vorlesung

(define (notfallmeldung schiffsname rufzeichen position zeit art)
  (string-upcase
   (verbinden "\n"
    (ueberschrift schiffsname rufzeichen)
    (notfallposition position)
    (notfallzeit zeit)
    (informationen art)
    "ICH SENDE DEN TRÄGER --"
    (unterschrift schiffsname rufzeichen)
    "OVER")))
  
(define (ueberschrift schiffsname rufzeichen)
  (verbinden "\n"
   "MAYDAY MAYDAY MAYDAY"
   (ausruf)
   (verbinden " " schiffsname schiffsname schiffsname (buchstabiere rufzeichen))
   (verbinden " " "MAYDAY" schiffsname "ICH BUCHSTABIERE" (buchstabiere schiffsname))
   (verbinden " " "RUFZEICHEN" (buchstabiere rufzeichen))))

(define (ausruf)
  (car (one-of (list "HIER IST"
                     (buchstabiere "DE")))))

(define (notfallposition position)
  (verbinden " " "NOTFALLPOSITION" position))

(define (notfallzeit zeit)
  (verbinden " " "NOTFALLZEIT" zeit))

(define (informationen art)
  (apply verbinden (cons "\n" art)))

(define (unterschrift schiffsname rufzeichen)
  (verbinden " " schiffsname (buchstabiere rufzeichen)))

(define (buchstabiere str)
  (apply verbinden (cons " " (seemannsgarn str))))

(define (verbinden . args)
  (let ((separator (car args))
        (head (str (cadr args)))
        (rlist (cddr args)))
    
    (if (empty? rlist)
        head
        (string-append head
                       separator
                       (apply verbinden (cons separator rlist))))))

(define (str obj)
  (cond
    [(string? obj) obj]
    [(symbol? obj) (symbol->string obj)]
    [else (error "Nicht erlaubtes Objekt")]))

;>> Ich finde eure Lösung gut.
;>> Bitte den letztem Teil besser kommentieren.
;>> 6 Pkt.

; Teilaufgabe 2.3
#|
(display (notfallmeldung "Seaside"
                         "SSDE"
                         "Ungefähr 10 sm nordöstlich Leuchtturm Kiel"
                         "1000 UTC"
                         '("Schwerer Wassereinbruch Wir sinken"
                           "Keine verletzten"
                           "Vier Mann gehen in die Rettungsinsel"
                           "Schnelle Hilfe erforderlich")))
|#
#|
(display (notfallmeldung "Amira"
                         "AMRY"
                         "50°46'N, 006°31'E"
                         "0640 UTC"
                         '("Kenterung in schwerer See"
                           "9 Mann an Bord"
                           "Schiff ist 15m lang"
                           "Grüner Rumpf")))
|#

;>> 3 Pkt.

; Aufgabe 3
; Teilaufgabe 3.1
;
; Innere Reduktion wertet den Term von innen nach außen aus, während äußere Reduktion genau umgekehrt
; die Auswertung mit der äußeren Ausdrücken beginnt.
;
; Innere Reduktion:
;
;    (hoch4 (* 3 (+ 1 (hoch4 2))))
; -> (hoch4 (* 3 (+ 1 (* 2 2 2 2))))
; -> (hoch4 (* 3 (+ 1 16))))
; -> (hoch4 (* 3 17))
; -> (hoch4 51)
; -> (* 51 51 51 51)
; -> 6765201
;
; Äußere Redunktion:
;
;    (hoch4 (* 3 (+ 1 (hoch4 2))))
; -> (* (* 3 (+ 1  (hoch4 2) )) (* 3 (+ 1  (hoch4 2) )) (* 3 (+ 1  (hoch4 2) )) (* 3 (+ 1  (hoch4 2) )))
; -> (* (* 3 (+ 1 (* 2 2 2 2))) (* 3 (+ 1  (hoch4 2) )) (* 3 (+ 1  (hoch4 2) )) (* 3 (+ 1  (hoch4 2) )))
; -> (* (* 3 (+ 1     16     )) (* 3 (+ 1  (hoch4 2) )) (* 3 (+ 1  (hoch4 2) )) (* 3 (+ 1  (hoch4 2) )))
; -> (* (* 3        17        ) (* 3 (+ 1  (hoch4 2) )) (* 3 (+ 1  (hoch4 2) )) (* 3 (+ 1  (hoch4 2) )))
; -> (*           51            (* 3 (+ 1  (hoch4 2) )) (* 3 (+ 1  (hoch4 2) )) (* 3 (+ 1  (hoch4 2) )))
; -> (*           51            (* 3 (+ 1 (* 2 2 2 2))) (* 3 (+ 1  (hoch4 2) )) (* 3 (+ 1  (hoch4 2) )))
; -> (*           51            (* 3 (+ 1     16     )) (* 3 (+ 1  (hoch4 2) )) (* 3 (+ 1  (hoch4 2) )))
; -> (*           51            (* 3        17        ) (* 3 (+ 1  (hoch4 2) )) (* 3 (+ 1  (hoch4 2) )))
; -> (*           51                      51            (* 3 (+ 1  (hoch4 2) )) (* 3 (+ 1  (hoch4 2) )))
; -> (*           51                      51            (* 3 (+ 1 (* 2 2 2 2))) (* 3 (+ 1  (hoch4 2) )))
; -> (*           51                      51            (* 3 (+ 1     16     )) (* 3 (+ 1  (hoch4 2) )))
; -> (*           51                      51            (* 3        17        ) (* 3 (+ 1  (hoch4 2) )))
; -> (*           51                      51                      51            (* 3 (+ 1  (hoch4 2) )))
; -> (*           51                      51                      51            (* 3 (+ 1 (* 2 2 2 2))))
; -> (*           51                      51                      51            (* 3 (+ 1     16     )))
; -> (*           51                      51                      51            (* 3        17        ))
; -> (*           51                      51                      51                      51           )
; ->                                               6765201
;

;>> Was sind die Vor-/Nachteile der beiden Varianten?

; Teilaufgabe 3.2
;
; Speziell in Racket werden normale Funktionen über innere Reduktion ausgewertet, während Spezial-
; formen über äußere Reduktion ausgewertet werden.
;
; Teilaufgabe 3.3
;
; Bei Verwendung der eigenen Implementation von If ensteht das Problem, dass die Funktion, und damit
; auch der cond-Ausdruck erst dann ausgeführt wird, wenn die Argumente bereits komplett ausgewertet
; wurden. Dies hat im Endeffekt zur Folge, dass der cond-Ausdruck nicht mehr entscheidet, welche der
; beiden Varianten ausgewertet werden, sondern nur noch, welcher der beiden Werte zurückgegeben wird,
; während beide Zweige zuvor immer komplett ausgewertet werden. Besonders bei rekursiven Aufrufen
; stellt das ein Problem dar, da in der Rekursion immer tiefer abgestiegen wird, während die Abbruch-
; bedingung (nie) überprüft wird, da hierfür der rekursive Aufruf ein Endergebnis erreichen müsste,
; was offensichtlich nicht geschieht.
; Genau dieses Problem tritt auch bei dem Beispiel auf, welches somit ebenfalls kein Ergebnis liefert.

;>> 9 Pkt
;>> Gesamt: 23 Pkt.