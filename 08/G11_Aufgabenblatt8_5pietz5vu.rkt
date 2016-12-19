#lang racket

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
; annimmt, oder eine Funktion zurück geben kann
;
; 
; Teilaufgabe 1.2
;
; foldr ist eine Funktion höherer Ordnung, da diese an erster Stelle eine Funktion benötigt, die
; anschließend auf andere Parameter angewandt wird.
;
; plus-oder-minus ist keine Funktion höherer Ordnung, da sie

(define (potenzmenge lst)
  (if (empty? lst)
      '(())
      (let ((rptm (potenzmenge (cdr lst))))
      (append rptm
              (map (curry cons (car lst))
                   rptm)))))

(define (interleave x lst)
  (if (empty? lst)
      (list (list x))
      (cons (cons x lst)
            (map (curry cons (car lst)) (interleave x (cdr lst))))))

(define (permute lst)
  (if (empty? lst)
      '(())
      (apply append (map (curry interleave (car lst))
                         (permute (cdr lst))))))

(define (wechselgeld münzen betrag)
  (filter (lambda (x) (= betrag (apply + x)))
          (wechselgeld_ münzen betrag)))

(define (wechselgeld_ münzen betrag)
  (if (or (empty? münzen)
          (<= betrag 0))
      '(())
      (let ((münze (car münzen))
            (rmünzen (cdr münzen)))
        (append (wechselgeld_ (cdr münzen) betrag)
                (map (curry cons (car münzen)) (wechselgeld_ münzen (- betrag münze)))))))