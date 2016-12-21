#lang racket

(require swindle/extra
         se3-bib/tools-module
         se3-bib/setkarten-module
         test-engine/racket-tests)

; SE3:FP Hausaufgaben 08
; 
; Gruppe 11
; 6808046 Tim Pietz
; 6792069 Hung Quan Vu
;
; Aufgabe 1
;
; Teilaufgabe 1.1
;
; Eine Funktion wird als Funktion höherer Ordnung bezeichnet, wenn sie selber Funktionen als Parameter
; annimmt, oder eine Funktion zurück gibt.
;
; 
; Teilaufgabe 1.2
;
; foldr ist eine Funktion höherer Ordnung, da diese an erster Stelle eine Funktion benötigt, (die
; anschließend auf andere Parameter angewandt wird).
;
; plus-oder-minus ist keine Funktion höherer Ordnung, da diese nur eine Zahl als Parameter erwartet,
; und ein Symbol zurück gibt.
;
; masala ist eine Funktion höherer Ordnung, da der erste Parameter als Funktion verwendet wird, (und
; zusätlich auch noch der Rückgabewert eine Funktion ist)
;
; flip ist eine Funktion höherer Ordnung, da der erste Parameter als Funktion verwendet wird, und sie
; zusätzlich eine Funktion zurück gibt.
;
;
; Teilaufgabe 1.3
;
; Bei Ausführung der Funktion masala zum Beispiel über den Aufruf (masala / 1), wird über den Lambda-
; Ausdruck intern eine "anonyme" Funktion erstellt, in welcher Variablen verwendet werden, die sich zu
; diesem Zeitpunkt in der lokalen Umgebung befinden. Nachdem die Auswertung von masala nun
; abgeschlossen wurde, würde die lokale Umgebung nun eigentlich "aufgelöst" werden, bleibt jedoch
; tatsächlich aufgrund des funtionalen Abschlusses für die zurückgegebene Funktion erhalten, damit
; diese auch noch bei späterer Ausführung auf die damaligen Werte der Parameter f und arg1 zurück-
; greifen kann.
; Wenn die Funktion dann also in dem konkreten Beispiel im Anschluss mit dem Parameter 3 aufgerufen
; wird, so wird intern über den funktionalen Abschluss auf die damaligen Bindungen der Variablen f
; und arg1 zugegriffen, während arg2 an die 3 gebunden wird.
;
;
; Teilaufgabe 1.4
;
; > (foldl (curry * 3) 1 '(1 2 3))
; (curry * 3) erstellt eine Funktion, die zusätzlich zu den anderen Parametern immer noch den
; konstanten Wert 3 als faktor hinzunimmt, wodurch ihre Ergebnisse immer mit 3 multipliziert werden.
; Das foldl wendet nun Elementweise einen Eintrag der Liste zusammen mit dem gespeicherten Zwischen-
; ergebnis auf die von curry zurück gegebene Funktion. Das Zwischenergebnis wird hierbei mit 1 ini-
; tialisiert. In dem ersten Schritt wird dementsprechend im Prinzip (* 3 1 1) -> 3 ausgewertet, im
; zweiten dann (* 3 2 3) -> 18, (* 3 3 18) -> 162, wobei an dieser Stelle die gesamte Liste
; abgearbeitet wurde, und die 162 somit der endgültige Rückgabewert ist.
;
; > (map (flip cons) '(1 2 3) '(3 2 1))
; Ein einfaches (map cons ...) würde immer das ein Element aus der ersten Liste nehmen, jeweils das
; entsprechende aus der zweiten Liste (gleicher Index), und somit die Pairs '(1 . 3) usw. bilden.
; Das flip gibt jedoch eine Funktion, in der die beiden Parameter in vertauschter Reihenfolge auf
; die gegebene Funktion (cons) angewandt werden. Dies hat zu Folge, dass die Elemente der Pairs
; jeweils vertauscht werden. Der Rückgabewert ist somit '((3 . 1) (2 . 2) (1 . 3)).
;
; > (filter list? '((a b) () 1 (())))
; Filter reduziert die gegebene Liste auf die Elemente, die die Prädikatsfunktion an erster Stelle
; erfüllen. list? gibt #t genau dann, wenn es sich bei dem Parameter um eine gültige Liste handelt.
; '(a b) ist eine gültige Liste, wird somit in die Ergebnisliste aufgenommen. '() ist ebenso eine
; gültige (leere) Liste, wird somit aufgenommen. 1 erfüllt das list?-Prädikat nicht, wird somit nicht
; aufgenommen, die Liste mit der Liste '(()) hingegen schon. Das Ergebnis ist somit '((a b) () (())).
;
; > (map (compose (curryr / 1.8) (curry - 32))
;        '(9941 212 32 -459.67))
; map bildet jedes Element einer Liste entsprechend der an erster Stelle gegebenen Funktion ab, und
; gibt eine Liste mit den Ergebnissen all dieser Abbildungen zurück. Die Funktion, die map an dieser
; Stelle mitgegeben wird, setzt sich hierbei aus zwei anderen Funktionen zusammen. Die erste dieser
; beiden Funktionen wird durch curryr gebildet, was in diesem konkreten Fall eine Funktion basierend
; auf / ist, an die dann zusätlich zu den jeweils aktuellen Parametern auch noch den Wert 1.8 (rechts)
; anhängt. Dies hat den Effekt, dass zusätzlich immer noch durch 1.8 geteilt wird.
; Die zweite Funktion hingegen basiert auf der - Funktion, und als erster Parameter wird immer der
; Wert 32 genutzt. Ein Aufruf von ((curry - 32) 1 2 3) würde somit zu (- 32 1 2 3) -> 26 übersetzt
; werden. Diese beiden Funktionen werden zusätlich noch über compose verbunden, was eine Hinter-
; einanderschaltung zur Folge hat, wobei zuerst die - 32 Funktion aufgerufen wird, und ihr Rückgabe-
; wert im Anschluss als Parameter für die / 1.8 Funktion verwendet wird. Das gesamte Konstrukt ist
; somit für einen einzigen Parameter also Äquivalent zu dem Ausdruck
; (lamda (x) (/ (- 32 x) 1.8))
; Bei der ersten tatsächlichen Auswertung mit dem Parameter 9941 wird also (- 32 9941) -> -9909
; berechnet, und dieser Wert im Anschluss in (/ -9909 1.8) -> -5505.0 wiederverwendet.
; Analog dazu werden auch die Werte für die anderen Listenelemente ausgerechnet, was die Ergebnisliste
; '(-5505.0 -100.0 0 273.15) zur Folge hat.
;
;
; Aufgabe 2
;
; Teilaufgabe 2.1
;
; 1. Liste der Absolutbeträge:
; > (map abs xs)
;
; 2. Liste aller glatt durch 13 teilbaren Zahlen:
; > (filter (compose (curry = 0) (curryr modulo 13)) xs)
;
; 3. Summe der geraden Zahlen größer 3:
; > (apply + (filter (cojoin even? (curryr > 3)) xs))
;
; 4. Anhand eines Prädikates (z.B. odd?) in zwei Teillisten aufspalten:
; > (list (filter odd? xs) (filter (negate odd?) xs))
;
;
; Aufgabe 3
;
; Teilaufgabe 3.1
;
; Genau wie auch bei den Schmetterlingen haben wir uns hier wieder dazu entschieden, die möglichen
; Ausprägungen einer Eigenschaft als einfache Liste darzustellen, und diese für alle Eigenschaften
; wiederum in einer Gesamtliste zu speichern.

(define eigenschaften '((1 2 3)
                        (waves oval rectangle)
                        (outline solid hatched)
                        (red green blue)))

; Eine Spielkarte wiederum wird ebenso als Liste dargestellt, in der einfach von jeder der
; Eigenschaften eine der möglichen Ausprägungen gewählt wird (siehe unten).


; Teilaufgabe 3.2

(define karten (amb-collect (map amb-car eigenschaften)))

; (show-set-cards karten)
(define (show-set-cards lst)
  (map (curry apply show-set-card) lst))


; Teilaufgabe 3.3

(define (is-a-set? a b c)
  (andmap (disjoin all-equal? all-different?)
          a b c))

(define (all-equal? . lst)
  (cond
    [(or (empty? lst)
         (empty? (cdr lst))) #t]
    [(equal? (car lst) (cadr lst)) (apply all-equal? (cdr lst))]
    [else #f]))

(define (all-different? . lst)
  (not (check-duplicates lst)))


; - Tests für Teilaufgabe 3.3 -
(check-expect #t (is-a-set? '(2 oval hatched red)
                            '(2 rectangle hatched red)
                            '(2 waves hatched red)))

(check-expect #f (is-a-set? '(2 rectangle outline red)
                            '(2 rectangle outline green)
                            '(1 rectangle solid green)))

(check-expect #t (all-equal? 'a 'a 'a 'a))
(check-expect #f (all-equal? 'a 'b 'a 'a))

(check-expect #f (all-different? 'a 'a 'a 'a))
(check-expect #f (all-different? 'a 'b 'a 'a))
(check-expect #t (all-different? 'a 'b 'c 'd))


; Teilaufgabe 3.4

(define (pick-cards)
  (take (shuffle karten) 12))

(define (all-sets cards)
  (amb-collect
   (let* ([lst-a (amb-cdr cards)]       [a (car lst-a)]
          [lst-b (amb-cdr (cdr lst-a))] [b (car lst-b)]
                                        [c (amb-car (cdr lst-b))])
     (amb-assert (is-a-set? a b c))
     (list a b c))))

; Grundidee: Gegeben eine Liste von Karten:
; '(1 2 3 4 5 6 7 8 9 10 11 12)
; Wählt man nichtdeterministisch einen Suffix (mit amb-cdr)
;       '(4 5 6 7 8 9 10 11 12)
; Das erste Element davon (4) ist die Karte a, aus dem Rest (5 6 ...) wieder nichtdeterm einen Suffix
;                 '(9 10 11 12)
; Auch hier ist das erste Element (9) wieder die zu betrachtende Karte b, und mit amb-car wählt man
; nichtdeterministisch einfach ein Element der Restliste '(10 11 12) als Karte c.
; Damit hat man dann aus allen nichtdeterm. 3 Karten a, b, c ausgewählt, und durch das Beschränken auf
; die Restlisten verhindert, dass zusätzlich zu '(4 9 12) auch noch '(9 4 12) als möglicherweise
; korrektes Set betrachtet wird.

; (define test-cards (pick-cards))
; (show-set-cards test-cards)
; (map show-set-cards (all-sets test-cards))

(test)