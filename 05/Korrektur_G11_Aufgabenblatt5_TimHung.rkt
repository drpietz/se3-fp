#lang racket

(require test-engine/racket-tests)
(require se3-bib/butterfly-module)
(require se3-bib/tools-module)

; SE3:FP Hausaufgaben 05
; 
; Gruppe 11
; 6808046 Tim Pietz
; 6792069 Hung Quan Vu
;
; Aufgabe 1
;
; Wir haben den Großteil der Dokumentation und einige Teile des Entwurfs der Datenstruktur o.ä. an die
; passenden Stellen im Quelltext verschoben. Die Planung geschah tatsächlich bevor wir angefangen
; haben zu programmieren. Fürs Verständnis erschien es uns jedoch sinnvoller, entsprechende Teile dort
; zu erklären, wo sie von nöten sind.
;
; Als erstes fällt auf, dass die möglichen Werte für die Merkmale der Schmetterlinge eine strenge
; Totalordnung darstellen. Solche eigenschaften lassen sich generell gut mit (geordneten) Listen
; darstellen, in denen die Ausprägungen, die weiter vorne in der Liste stehen dominanter sind.
; Wir haben uns dann dazu entschieden, alle vier dieser Ordnungslisten, jeweils für ein Merkmal,
; zusammen in einer Liste zu speichern. Die Reihenfolge orientiert sich dabei an der Reihenfolge der
; Merkmale, wie sie für die show-butterfly Funktion notwendig sind. Das hat den Vorteil, dass wir
; durch die äußere Liste iterieren können, aus jeder der inneren Listen ein (zufälliges) Element
; wählen, und das Ergebnis direkt über apply show-butterfly anzeigen lassen können. Sollte sich die
; Anzahl der Merkmale aus irgend einem Grund später mal verändern, so müssen derartige Funktionen
; nicht weiter angepasst werden, sondern lediglich das neue Mermal zu der Ordnungsliste hinzugefügt
; werden.

(define reccessive-order '((green red blue yellow)
                           (star dots stripes)
                           (curved curly straight)
                           (rhomb ellipse hexagon)))

; Ein weiterer Vorteil davon, sich auf reine Listen zu beschränken liegt darin, dass es relativ
; einfach ist, aus den dominanten Genen eines Schmetterlings die rezessiven zu berechnen. Wenn die
; Flügel eines Schmetterlings zum Beispiel 'red sind, so wären die möglichen Belegungen für das
; rezessive Gen '(red blue yellow), was einfach der Farbliste ab dem Element 'red entspricht. Wir
; haben uns dafür entschieden, ein einzelnes so berechnetes Merkmal wiederum in einer Liste zu
; speichern, in der das eine Element das bekannte dominante Gen darstellt, und das andere Element
; einfach die Liste der möglichen rezessiven Gene ist.
; Dies kann durch iterieren durch alle domianten Merkmale erweitert werden.

; Erweitert eine Liste dominanter Gene mit den möglichen rezessiven Varianten nach dem Schema
; '(red [...]) -> '((red (red blue yellow)) [...])
(define (from-dominant traits [rorder reccessive-order])
  (if (empty? traits)
      '()
      (cons (full-trait (car traits)
                        (car rorder))
            (from-dominant (cdr traits)
                           (cdr rorder)))))

(check-expect (from-dominant '(red stripes curved ellipse))
              '((red (red blue yellow))
                (stripes (stripes))
                (curved (curved curly straight))
                (ellipse (ellipse hexagon))))

; Erweitert ein einzelnes dominantes Gen durch eine Liste möglicher rezessiver Varianten bei Angabe
; der Rezessivitätsordnung für das entsprechende Gen.
; 'red, (green red blue yellow) -> (red (red blue yellow))
(define (full-trait dominant rorder)
  (list dominant
        (member dominant rorder)))

(check-expect (full-trait 'red '(green red blue yellow)) '(red (red blue yellow)))


; Mit der gewählten Darstellungsweise '(blue (blue yellow)) ist es relativ einfach möglich, das zu
; vererbende Gen zufallig auszuwählen, in dem jede Liste als Knoten eines Entscheidungsbaumes
; angesehen wird, und jedes enthaltene Element die selbe Wahrscheinlichkeit hat, gewählt zu werden.
; Bei dem Beispiel '(blue (blue yellow)) hat dies den Effekt, dass in der äußersten Liste das
; dominate Gen 'blue gewählt wird, während das rezessive '(blue yellow) ebenfalls zu 50% gewählt wird.
; Sollte das rezessive gewählt werden, so wird rekursiv zwischen den rezessiven Möglichkeiten gewählt,
; da das tatsächliche Gen ja nicht bekannt ist.
; Die Gene des Kindes setzen sich dann einfach aus einem zufälligen der Mutter sowie einem zufälligem
; des Vaters zusammen.

; Kombiniert die Gene zweier Schmetterlinge zufällig
(define (mendel a b)
  (if (empty? a)
      '()
      (cons (mendel-trait (car a) (car b))
            (mendel (cdr a) (cdr b)))))

; Kombiniert ein einzelnes Gen zweier Schmetterlinge zufällig
(define (mendel-trait a b)
  (list (random-leaf a)
        (random-leaf b)))

; Interpretiert eine (geschachtelten) Liste als Entscheidungsbaum und wählt ein zufälliges Blatt aus
(define (random-leaf tree)
  (if (list? tree)
      (random-leaf (random-elt tree))
      tree))


; Um aus dem Genom eines Schmetterlings die dominanten Gene auszulesen, gehen wir einfach jeweils die
; obige Rezessivitätsordnung von dominant nach rezessiv durch, und geben das erste Element, das in
; dem "Genombaum" des Schmetterlings gefunden wird, zurück.

; Gibt die domianten Gene eines Schmetterlings als Liste zurück
(define (get-dominant genome [rorder reccessive-order])
  (if (empty? genome)
      '()
      (cons [get-dominant-trait (car genome) (car rorder)]
            [get-dominant (cdr genome) (cdr rorder)])))

(check-expect (get-dominant antonia) '(blue stripes curved hexagon))

; Gibt die nach der Rezessivitätsordnung dominateste vorhandene Ausprägung eines Gens aus dem Genom
; eines Schmetterlings zurück
(define (get-dominant-trait trait rorder)
  (get-dominant-trait-helper (flatten trait) rorder))

(define (get-dominant-trait-helper trait rorder)
  (cond
    [(empty? rorder) (error "Kein gültiges Gen")]
    [(member (car rorder) trait) (car rorder)]
    [else (get-dominant-trait-helper trait (cdr rorder))]))

(check-expect (get-dominant-trait '(blue red) '(green red blue yellow)) 'red)

; Stellt einen Schmetterling der hier verwendeten Datenstruktur dar
(define (show-butterfly2 b)
  (apply show-butterfly (get-dominant b)))

; Mendelt mehrfach, gibt Bilder zurück
(define (mendeln a b n)
  (append (list (show-butterfly2 a)
                (show-butterfly2 b))
          (mendeln-helper a b n)))

(define (mendeln-helper a b n)
  (if (> n 0)
      (cons (show-butterfly2 (mendel a b))
            (mendeln-helper a b (- n 1)))
      '()))

(define antonia (from-dominant '(blue stripes curved hexagon)))
(define anton (from-dominant '(green star curly rhomb)))
(define toni (from-dominant '(red star curved hexagon)))
(define tini (from-dominant '(green dots straight rhomb)))
(define tina (from-dominant '(yellow stripes curly ellipse)))


; Tests?
; 
; (mendeln (from-dominant '(yellow stripes straight hexagon))
;          (from-dominant '(yellow stripes straight hexagon))
;          5)
;
; Zwei Schmetterlinge, deren dominante die rezessivesten Gene sind.
; Sollte nur gleiche Nachfahren geben.
;
; 
; Alle anderen Testfälle für mendeln sind aufgrund der Zufälligkeit nur bedingt hilfreich.
; Möglich wäre allerdings zum Beispiel auch noch:
;
; 
; (mendeln (from-dominant '(yellow dots straight hexagon))
;          (from-dominant '(yellow stripes straight hexagon))
;          5)
;
; Ca. 3/4 der Nachfahren sollten gepunktet sein, der Rest Streifen und ansonsten gleich
;
; 
; (mendeln (from-dominant '(green star curved rhomb))
;          (from-dominant '(yellow stripes straight hexagon))
;          5)
;
; Alle Kinder sind möglich
;
; Aufgabe 2

; Testet (rekursiv), ob alle Gene des Kinds von den Eltern kommen könnten
(define (ptest p1 p2 kind)
  (cond
    [(empty? p1) #t]
    [(not (ptest-trait (car p1) (car p2) (car kind))) #f]
    [else (ptest (cdr p1) (cdr p2) (cdr kind))]))

(check-expect (ptest antonia anton toni) #t)
(check-expect (ptest antonia anton tini) #t)
(check-expect (ptest antonia anton tina) #t)
; Einzeln können Toni, Tini und Tina von anton und antonia stammen, jedoch nicht alle 3

; Testet, ob ein einzelnes Gen-paar (Dominant + Rezessiv) von den Genpaaren der Eltern kommen kann
; Kurze Erklärung: Entweder das erste Gen kommt von p1 und das zweite von p2, oder anders herum
(define (ptest-trait p1 p2 kind)
  (let ((first-gene (list (car kind)))
        (second-gene (cdr kind)))
    (or (and (gene-from first-gene p1)
                 (gene-from second-gene p2))
            (and (gene-from first-gene p2)
                 (gene-from second-gene p1)))))


; Gibt true, wenn das Gen k von dem Elternteil a vererbt worden sein kann
(define (gene-from p k)
  (not (empty? (intersection (flatten (list k)) (flatten (list p))))))
;                                    ^   Tut uns leid   ^
; Die Alternative wäre es gewesen, alle kombinationen von list / symbol als Parameter einzeln zu
; behandeln, so wandeln wir einfach beide in listen um, und das flatten wäre wegen der relativ
; uneingeschränkten Eingabemöglichkeiten mit geschachtelten listen so wie so notwendig gewesen.

(check-expect (gene-from '(red (red blue yellow)) 'red) #t)
(check-expect (gene-from '(red (red blue yellow)) 'green) #f)
(check-expect (gene-from '(red blue) 'yellow) #f)
(check-expect (gene-from '(red (red blue yellow)) '(blue yellow)) #t)
(check-expect (gene-from '(red (red blue yellow)) '(green red blue yellow)) #t)

; Letzte zu schreibende Funktionen, hier ist uns aufgefallen, dass es teilweise schlauer gewesen wäre,
; Sets zu verwenden.. Kannten wir aber halt noch nicht, und die se3-tools die wir nutzen sind für lstn
; Berechnet den Durchschnitt zweier Listen
(define (intersection lsta lstb)
  (cond
    [(null? lsta) '()]
    [(member (car lsta) lstb) (cons (car lsta)
                                    (intersection (cdr lsta) lstb))]
    [else (intersection (cdr lsta) lstb)]))

(check-expect (intersection '(b c d e f) '(g e c a)) '(c e))
(check-expect (intersection '(d a v i d) '(m o s t l l e r)) '())

(test)

;>> Klasse Lösung!
;>> Gesamt: 36 Pkt.