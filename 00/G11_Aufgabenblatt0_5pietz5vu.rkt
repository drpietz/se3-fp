#lang racket

(define (zinsen betrag zinssatz)
  (* betrag zinssatz)
)

(define g 9.8)

(define (t s)
  (sqrt (/ (* 2 s) g))
  )

(define (v s)
  (* g (t s))
  )

(define (energie s m)
  (* 0.5 m (sqr (v s)))
  )