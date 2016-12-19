#lang racket

; SE3:FP Hausaufgaben 01
; 
; Gruppe 11
; 6808046 Tim Pietz
; 6792069 Hung Quan Vu

; Aufgabe 1
; Teilaufgabe 1.1

(define (degrees->radians deg)
  (* (/ deg 180) pi))

(define (radians->degrees rad)
  (* (/ rad pi) 180))

;>> Bitte kommentieren und Testeingaben mit liefern.
;>> 3 Pkt.

; Teilaufgabe 1.2

(define (cos->sin x)
  (sqrt(- 1 (sqr x))))

;>> Soweit, so gut.

(define (cos->tan x)
  (/ x (cos->sin x)))

;>> Hier sind die Argumente vertauscht:
;>> (define (cos->tan x)
;>>  (/ (cos->sin x) x))

(define (my-acos x)
  (atan (cos->tan x)))

;>> Mit der kleinen Änderung ist es dann korrekt.
;>> Es fehlen mir aber auch hier die Kommentare, um eure Schritte nachvollziehen zu können.
;>> 2 Pkt.

; Teilaufgabe 1.3

(define (nm->km nm)
  (* nm 1.852))

;>> 1 Pkt.

; Aufgabe 2
; Teilaufgabe 2.1

(define oslo '("59.32N" "10.75E"))
(define hongkong '("22.2N" "114.1E"))
(define san-francisco '("37.75N" "122.45W"))
(define honolulu '("21.32N" "157.83W"))
(define osterinsel '("27.1S" "109.4W"))
(define lima '("12.1S" "77.05W"))

;>> An dieser Stelle mit Strings zu arbeiten ist etwas "umständlich"
;>> Kleiner Tippfehler: (define oslo '("59.32N" "10.75E")) -> (define oslo '("59.93N" "10.75E"))

; (distanzAB oslo hongkong) -> 8589.412217586054
; (distanzAB san-francisco honolulu) -> 3844.6880504870555
; (distanzAB osterinsel lima) -> 3757.6222188100564
(define (distanzAB A B)
  (nm->km (grad->minute (radians->degrees (grosskreisentfernung A B)))))

(define (grosskreisentfernung A B)
  (acos (+ (* (sin (lat A))
              (sin (lat B)))
           (* (cos (lat A))
              (cos (lat B))
              (cos (diff (long A) (long B)))))))

(define (lat A)
  (geostring->rad (first A)))

(define (long A)
  (geostring->rad (second A)))

(define (geostring->rad geostring)
  (if (or (string-suffix? geostring "N") (string-suffix? geostring "E"))
      (degrees->radians (geostring-value geostring))
      (- (degrees->radians (geostring-value geostring)))))

(define (geostring-value geostring)
  (string->number (substring geostring 0 (- (string-length geostring) 1))))

(define (grad->minute grad)
  (* 60 grad))

(define (diff a b)
  (abs (- a b)))

;>> 9 Pkt.

; Teilaufgabe 2.2
; Kaputt

(define (kurs A B)
  (if (eq? (long-direction A B) #\E)
      (radians->degrees (winkel A B))
      (- 360 (radians->degrees (winkel A B)))))

(define (winkel A B)
  (acos (/ (- (sin (lat B))
              (* (cos (grosskreisentfernung A B))
                 (sin (lat A))))
           (* (cos (lat A))
              (sin (grosskreisentfernung A B))))))

(define (long-direction A B)
  (if (xor (positive? (- (long B) (long A))) (< (diff (long B) (long A)) pi))
      #\W
      #\E))

;>> 1 Pkt.

; Teilaufgabe 2.3
; Teilaufgabe 2.3.1

(define himmelsrichtungen '("N" "NNO" "NO" "ONO" "O" "OSO" "SO" "SSO" "S" "SSW" "SW" "WSW" "W" "WNW" "NW" "NNW"))

(define (grad->himmelsrichtung grad)
  (cond
    ((or (< grad 11.25) (> grad 348.75)) (first himmelsrichtungen))
    (else (grad->himmelsrichtung-hilfs (- grad 11.25) (rest himmelsrichtungen)))))

(define (grad->himmelsrichtung-hilfs offset liste)
  (cond
    ((< offset 22.5) (first liste))
    (else (grad->himmelsrichtung-hilfs (- offset 22.5) (rest liste)))))

; Teilaufgabe 2.3.2

(define (himmelsrichtung->grad richtung)
  (himmelsrichtung->grad-hilf richtung himmelsrichtungen))

(define (himmelsrichtung->grad-hilf richtung liste)
  (cond
    ((eq? richtung (first liste)) 0)
    (else (+ 22.5 (himmelsrichtung->grad-hilf richtung (rest liste))))))

;>> Nette Idee, das rekursiv zu lösen.
;>> Ich empfehle euch aber mit Symbolen anstatt mit Strings zu arbeiten.

;>> 5 Pkt.
;>> Gesamt: 21 Pkt.

(require test-engine/racket-tests)

;1.1
(check-within (degrees->radians 180) 3.141592653589793 0.0)
(check-within (degrees->radians 360) 6.283185307179586 0.0)
(check-within (degrees->radians 90) 1.5707963267948966 0.0)

;(radians->degrees)
(check-within (radians->degrees pi) 180.0 0.0)
(check-within (radians->degrees (* pi 2)) 360.0 0.0)
(check-within (radians->degrees (/ pi 2)) 90.0 0.0)

;1.2
(check-within (my-acos (cos 1)) 1 0.001)
;(check-expect (my-acos 0) 0)

;1.3
(check-within (nm->km 1) 1.852 0.0)

;2.1
;(check-within (distanzAB 59.93 10.75 22.20 114.10) 8589.412217586058 0.0) ;Oslo -> Hongkong
;(check-within (distanzAB 37.75 122.45 21.32 157.83) 3844.6880504870555 0.0) ;San Francisco -> Honolulu
;(check-within (distanzAB 27.10 109.40 12.10 77.05) 3757.622218810056 0.0) ;Osterinsel -> Lima
;(check-within (distanzAB 41.1578 -8.6333 0.6722 -61.5333) 6944.307574004419 0.0) ;Porto -> Port of Spain

;2.2
;(check-within (kurs 59.93 10.75 22.20 114.10) 67.43567317710463 0.0) ;Oslo -> Hongkong
;(check-within (kurs 37.75 -122.45 21.32 -157.83) 251.78216746132105 0.0) ;San Francisco -> Honolulu
;(check-within (kurs -27.10 -109.40 -12.10 -77.05) 70.07186231064308 0.0) ;Osterinsel -> Lima
;(check-within (kurs 41.1578 -8.6333 0.6722 -61.5333) 244.04927957054304 0.0) ;Porto -> Port of Spain

;2.3
;(check-expect (grad->himmelsrichtung 267) 'W)
;(check-expect (grad->himmelsrichtung 1) 'N)

;Himmelsrichtung->Grad
;(check-expect (himmelsrichtung->grad 'N) 0)
;(check-within (himmelsrichtung->grad 'NNE) 22.5 0.0)

(test)