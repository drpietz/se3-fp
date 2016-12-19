#lang racket

(require 2htdp/image
         2htdp/universe
         test-engine/racket-tests
         (only-in se3-bib/tools-module random-elt))

; SE3:FP Hausaufgaben 07
; 
; Gruppe 11
; 6808046 Tim Pietz
; 6792069 Hung Quan Vu
;
; Aufgabe 1

(define (produkt-a n f)
  (if (empty? n)
      '()
      (cons (* (car n) f) (produkt-a (cdr n) f))))


(define (produkt-b n f [akk '()])
  (if (empty? n)
      (reverse akk)
      (produkt-b (cdr n) f (cons (* (car n) f) akk))))


(define (produkt-c n f)
  (map (lambda (x)
         (* x f))
       n))


(check-expect (produkt-a '(2 4 3) 3) '(6 12 9))
(check-expect (produkt-b '(2 4 3) 3) '(6 12 9))
(check-expect (produkt-c '(2 4 3) 3) '(6 12 9))


; Aufgabe 2
;
; Teilaufgabe 2.1
;
;  -0-
; 1   2
;  -3-
; 4   5
;  -6-

(define-struct segment (pos-x pos-y width height))


(define segments (list (make-segment 20 10 80 10)    ; 0: Oben mitte
                       (make-segment 10 20 10 80)    ; 1: Oben links
                       (make-segment 100 20 10 80)   ; 2: Oben rechts
                       (make-segment 20 100 80 10)   ; 3: Mitte
                       (make-segment 10 110 10 80)   ; 4: Unten links
                       (make-segment 100 110 10 80)  ; 5: Unten rechts
                       (make-segment 20 190 80 10))) ; 6: Unten mitte


(define digits '((#t #t #t #f #t #t #t)   ; 0
                 (#f #f #t #f #f #t #f)   ; 1
                 (#t #f #t #t #t #f #t)   ; 2
                 (#t #f #t #t #f #t #t)   ; 3
                 (#f #t #t #t #f #t #f)   ; 4
                 (#t #t #f #t #f #t #t)   ; 5
                 (#t #t #f #t #t #t #t)   ; 6
                 (#t #f #t #f #f #t #f)   ; 7
                 (#t #t #t #t #t #t #t)   ; 8
                 (#t #t #t #t #f #t #t))) ; 9


; Teilaufgabe 2.2

; Gibt eine 7-Segment Anzeige als Bild entsprechend der Parameter zurück
; Jeder der 7 Parameter steht entsprechend des Index für den Zustand eines Segmentes
; (show-7segment #t #t #t #f #t #t #t)
; (map (lambda (s) (apply show-7segment s)) digits)
(define (show-7segment . states)
  (foldl add-segment
         (rectangle 120 210 "solid" "black")
         segments
         states))

; Fügt ein einzelnes Segment mit State #t/#f in ein Bild (schwarzer Hintergrund) ein
(define (add-segment segment state image)
  (let ((x (segment-pos-x segment))
        (y (segment-pos-y segment))
        (width (segment-width segment))
        (height (segment-height segment))
        (color (segment-color state)))
  (underlay/xy image
               x y
               (rectangle width height "solid" color))))

; Gibt die Segmentfarbe entspechend des Zustandes #t/#f
(define (segment-color is-on)
  (if is-on
      "red"
      (make-color 40 40 40)))

; Gibt eine Ziffer als 7-Segment Bild zurück
; (show-7segment-digit 6)
; (build-list 10 show-7segment-digit)
(define (show-7segment-digit digit)
  (apply show-7segment (list-ref digits digit)))


; Teilaufgabe 2.3

; (animate stimer)
(define (stimer t)
  (show-7segment-digit (ticks->time/limit t 1 10)))


; Rechnet eine Tickzahl in ein Vielfaches von Sekunden um. Siehe Tests
(define (ticks->time ticks second-val)
  (quotient ticks (* 28 second-val)))

; Rechnet eine Tickzahl in ein Vielfaches von Sekunden, begrenzt durch ein Limit um. Siehe Tests
(define (ticks->time/limit ticks second-val limit)
  (remainder (ticks->time ticks second-val) limit))


(check-expect (ticks->time 0 1) 0)   ; 0 Ticks = 0 Sekunden
(check-expect (ticks->time 27 1) 0)  ; 27 Ticks = 0 Sekunden
(check-expect (ticks->time 28 1) 1)  ; 28 Ticks = 1 Sekunde
(check-expect (ticks->time 279 1) 9) ; 279 Ticks = 9 Sekunden
(check-expect (ticks->time 280 1) 10) ; 280 Ticks = 10 Sekunden
(check-expect (ticks->time/limit 0 1 10) 0)   ; 0 Ticks = 0 Sekunden
(check-expect (ticks->time/limit 280 1 10) 0) ; 280 Ticks = 10 Sekunden -> 0 wegen Limit 10

(check-expect (ticks->time (- (* 60 28) 1) 60) 0) ; 60 Sekunden - 1 Tick = 0 Minuten
(check-expect (ticks->time (* 60 28) 60) 1)       ; 60 Sekunden = 1 Minute
(check-expect (ticks->time (* 45 60 28) 60) 45)   ; 45 * 60 Sekunden = 45 Minuten
(check-expect (ticks->time (* 60 60 28) 60) 60)   ; 60 * 60 Sekunden = 60 Minuten
(check-expect (ticks->time/limit (* 45 60 28) 60 60) 45) ; 45 * 60 Sekunden = 45 Minuten 
(check-expect (ticks->time/limit (* 60 60 28) 60 60) 0) ; 60 * 60 Sekunden = 60 Minuten -> 0 Limit 60


; Teilaufgabe 2.4

(define doppelpunkt (underlay/xy (underlay/xy (rectangle 120 210 "solid" "black")
                                              55 55
                                              (rectangle 20 20 "solid" "red"))
                                 55 125
                                 (rectangle 20 20 "solid" "red")))

; (animate ltimer)
; Start bei 99:59:50: (animate (lambda (t) (ltimer (+ t 10079720))))
(define (ltimer t)
  (beside
   (show-number (ticks->time/limit t 3600 100) 2)
   doppelpunkt
   (show-number (ticks->time/limit t 60 60) 2)
   doppelpunkt
   (show-number (ticks->time/limit t 1 60) 2)))

; Zeigt eine (mit 0en aufgefüllte) Zahl als Hintereinanderschaltung mehrerer 7-Segmentanzeige
; 131: (show-number 131)
; 131 mit 4 Ziffern: (show-number 131 4)
(define (show-number n [digit-count 1])
  (let* ((digit-list (number->digit-list n digit-count))
         (digit-count (length digit-list)))
    (if (= digit-count 1)
        (show-7segment-digit (car digit-list))
        (apply beside
               (map show-7segment-digit digit-list)))))

; Wandelt eine Zahl in eine Liste von Ziffern um, die ggf. mit 0en aufgefüllt ist. Siehe Tests
(define (number->digit-list n [min-length 1] [akk '()])
  (if (= 0 n)
      (list-padding akk min-length 0)
      (number->digit-list (quotient n 10)
                          min-length
                          (cons (remainder n 10) akk))))

(check-expect (number->digit-list 0) '(0))
(check-expect (number->digit-list 1) '(1))
(check-expect (number->digit-list 123) '(1 2 3))
(check-expect (number->digit-list +4940428832093) '(4 9 4 0 4 2 8 8 3 2 0 9 3))

(check-expect (number->digit-list 123 6) '(0 0 0 1 2 3))

; Fügt in eine Liste vorne ein Füll-element ein, so dass die Liste eine Mindestlänge erreicht
(define (list-padding lst min-length fill)
  (let* ((padding-len (max 0 (- min-length (length lst))))
         (padding (make-list padding-len fill)))
    (append padding lst)))

(check-expect (list-padding '(1 2 3) 1 0) '(1 2 3))
(check-expect (list-padding '(1 2 3) 6 0) '(0 0 0 1 2 3))
(check-expect (list-padding '(b c d) 6 'a) '(a a a b c d))


; Teilaufgabe 2.5

; Verwendet normale ltimer Funktion zum Anzeigen der verbleibenden Zeit
; Addition von 27 zu den Ticks sorgt für Aufrunden statt abrunden, was bei Timern gewünscht ist
; Wenn Endzeit erreicht werden alle Segmente zufällig geschaltet.
; (animate (curry zeige-timer 5))
(define (zeige-timer seconds t)
  (if (> t (* 28 seconds))
      (beside (random-segment) (random-segment) doppelpunkt
              (random-segment) (random-segment) doppelpunkt
              (random-segment) (random-segment))
      (ltimer (+ 27 (- (* 28 seconds) t)))))

; Gibt ein einzelnes 7-Segment-Display, in dem die Segmente zufällig beleuchtet werden
(define (random-segment)
  (apply show-7segment (build-list 7 (lambda (x) (random-elt '(#t #f))))))

(test)