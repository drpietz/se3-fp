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

; Teilaufgabe 1.2

(define (cos->sin x)
  (sqrt(- 1 (sqr x))))

(define (cos->tan x)
  (/ x (cos->sin x)))

(define (my-acos x)
  (atan (cos->tan x)))

; Teilaufgabe 1.3

(define (nm->km nm)
  (* nm 1.852))


; Aufgabe 2
; Teilaufgabe 2.1

(define oslo '("59.32N" "10.75E"))
(define hongkong '("22.2N" "114.1E"))
(define san-francisco '("37.75N" "122.45W"))
(define honolulu '("21.32N" "157.83W"))
(define osterinsel '("27.1S" "109.4W"))
(define lima '("12.1S" "77.05W"))

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