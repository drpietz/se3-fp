#lang racket

(require racket/format)
(require lang/posn)
(require 2htdp/image)
(require 2htdp/universe)

; SE3:FP Hausaufgaben 06
; 
; Gruppe 11
; 6808046 Tim Pietz
; 6792069 Hung Quan Vu
;
; Aufgabe 1
;
; kopfstueck
;   linear rekursiv: Ja, da sie sich in jedem Fall nur höchstens ein mal selber aufruft (else-Zweig).
;   Baumrekursion: Nein, da sie sich nie in einem Fall mehrfach aufruft.
;   geschachtelte rekursion: Nein, da sie beim rekursiven Aufruf nicht selber als Parameter verwendet.
;   direkte rekursion: Ja, da sie nur sich selber rekursiv aufruft.
;   indirekte rekursion: Nein, da sie nur sich selber rekursiv aufruft (keine Zyklen anderer Funkt.)
;
; endstueck
;   linear rekursiv: Ja, da sie sich in jedem Fall nur höchstens ein mal selber aufruft (else-Zweig).
;   Baumrekursion: Nein, da sie sich nie in einem Fall mehrfach aufruft.
;   geschachtelte rekursion: Nein, da sie beim rekursiven Aufruf nicht selber als Parameter verwendet.
;   direkte rekursion: Ja, da sie nur sich selber rekursiv aufruft.
;   indirekte rekursion: Nein, da sie nur sich selber rekursiv aufruft (keine Zyklen anderer Funkt.)
;
; merge
;   linear rekursiv: Nein, da sie sich im else-Zweig zweifach selber aufruft.
;   Baumrekursion: Ja, da sie sich im else-Zweig zweifach selber aufruft.
;   geschachtelte rekursion: Nein, da sie beim rekursiven Aufruf nicht selber als Parameter verwendet.
;   direkte rekursion: Ja, da sie nur sich selber rekursiv aufruft.
;   indirekte rekursion: Nein, da sie nur sich selber rekursiv aufruft (keine Zyklen anderer Funkt.)

;>> merge ist keine Baumrekursion, da nur ein rekursiver Aufruf tatsächlich stattfindet.
;>> In der if-Verzweigung wird nur eine der beiden Alternativen ausgewertet.

;
; merge-sort
;   linear rekursiv: Nein, da sie sich im else-Zweig zweifach selber aufruft.
;   Baumrekursion: Ja, da sie sich im else-Zweig zweifach selber aufruft.
;   geschachtelte rekursion: Nein, da sie beim rekursiven Aufruf nicht selber als Parameter verwendet.
;   direkte rekursion: Ja, da sie nur sich selber rekursiv aufruft.
;   indirekte rekursion: Nein, da sie nur sich selber rekursiv aufruft (keine Zyklen anderer Funkt.)
;                        Sie ruft zwar andere Rekursive funktionen auf, jedoch rufen diese nicht
;                        wieder Mergesort auf (kein gegenseitiger Aufruf).
;

;>> 8 Pkt.

; Aufgabe 2
;





; 1. Bild: Weihnachts-scene
; (animate weihnachts-scene)

(define (weihnachts-scene t)
  (scale 0.5
  (place-images
   (list  snowstorm baum1 baum2 haus baum1 strasse)
   (list
         (make-posn 0 t)
         (make-posn  t y)
         (make-posn (+ 300 t) y)
         (make-posn (+ 600 t) y)
         (make-posn (+ 900 t) y)
         (make-posn t 700)
         )
   (rectangle 1920 1080 "solid" "DeepSkyBlue"))))

;y-position der Elemente
(define y 300)

;Baum1 wird von die rekursive Funktion draw-ellipse erzeugt.
;draw-ellipse wird am Anfang ein ganz klein Elipse erzeugen, und danach immer größer nach ein
; bestimmte Faktor (1.5)
;bis wann eine bestimmte Größe erreicht ist dann hört die auf
(define (draw-ellipse image s)
(cond [(> s 150) image]
      [else (draw-ellipse
             (above/align
               "center"
               image
               (ellipse s (/ s 2) "solid" "Darkgreen"))
             (* s 1.5))]))
(define baum1 (above/align
               "center"
;; der Stern an der Spitze
               (star-polygon 40 5 2 "solid" "gold")
;; die Zweige
               (draw-ellipse (ellipse 10 20 "solid" "Darkgreen") 30)
;; der Stamm
               (rectangle 20 30 "solid" "brown")
))

;Baum2 wird von die rekursive Funktion draw-triangle erzeugt.
;analog wie draw-ellipse
(define (draw-triangle image s)
(cond [(> s 200) image]
      [else (draw-triangle
             (overlay/offset image 0 (/ s 1.5) (isosceles-triangle s 110 "solid" "Darkgreen"))
             (* s 1.2))]))

(define baum2 (above/align
               "center"
               (star-polygon 60 5 2 "solid" "gold")
               (draw-triangle (isosceles-triangle 0 0 "solid" "Darkgreen") 1)
          
               
               (rectangle 40 60 "solid" "brown")
               ))

;snowflake
;1/3 der snowflake
;erzeugt 1/3 der Snowflake durch lokale rekursive aufruf
(define (kurve n)
    (cond
      [(zero? n) (square 1 "solid" "black")]
      [else
       (local [(define kleiner (kurve (- n 1)))]
         (beside/align "bottom"
                       kleiner
                       (rotate 60 kleiner)
                       (rotate -60 kleiner)
                       kleiner))]))
;komplett snowflake
;trivial
(define (snowflake x) (above
   (beside
    (rotate 60 (kurve x))
    (rotate -60 (kurve x)))
   (flip-vertical (kurve x))))

;Snowstorm erzeugen
;analog zu draw-ellipse & draw-triangle 
(define (draw-snowstorm image s)
  (cond
    [(zero? s) image]
    [else
     (draw-snowstorm(overlay/offset (snowflake (random 4))
                                 (random 1700)
                                 (random 100)
                                 image)
                    (- s 1))]))
;hier wird die Dichte der Stürm initialisiert
(define stormdichte 50)
(define snowstorm (draw-snowstorm (snowflake (random 4)) stormdichte))

;ein Hause
;dient nur als Dekor
;Code ist trivial
(define haus (above/align
               "center"
               (isosceles-triangle 200 120 "solid" "DarkRed")               
               (overlay/offset (rectangle 60 80 "solid" "brown")
                              -30 -30
                               (rectangle 200 150 "solid" "orange"))
))

;streifen
;rekursive Aufruf
(define (streifen image s)
  (cond [(= s 0) image]
      [else (cond [(odd? s)(streifen
             (beside image (rectangle 50 20 "solid" "white"))
             (- s 1))]
                  [else
                   (streifen
                    (beside image (rectangle 50 20 "solid" "gray"))
                    (- s 1))])]))

;straße
;Wirksamkeit wie das Haus
;einbisschen weniger trivial
(define strasse (overlay/offset (streifen  (rectangle 50 20 "solid" "white") 100)
                                -1000 0
                                (rectangle 5000 200 "solid" "gray")))












; 2. Bild: Rotierende Mandarine
; (animate mandarine-scene)

(define hintergrund (make-color 255 135 66))
(define schicht (make-color 255 181 106 100))

; Erstellt eine Mandarine mit 150px Radius und 10 "Schichten", die sich dreht
(define (mandarine-scene t)
  (mandarine t 150 10))

; Erstellt eine Mandarine mit einem bestimmten Radius, die aus einer unteren dunklen Schicht besteht,
; sowie der angegebenen Anzahl an überliegenden 
(define (mandarine t radius schichten)
  (rotate t (underlay (circle radius "solid" hintergrund)
                     (mandarine_ radius schichten))))

(define (mandarine_ radius schichten)
  (if (<= schichten 1)
      (circle 5 "solid" schicht)
      (overlay (circle radius "solid" schicht)
               (mandarine_ (- radius (* radius (/ 1 schichten))) (- schichten 1)))))












; 3. Bild: Jingle Bells?
; (animate glocken-scene-crop)

;(define glocken-farbe (make-color 250 195 0))
;(define glocken-farbe (make-color 204 66 55))
;(define hintergrund-farbe (make-color 241 235 219))
(define glocken-farbe (make-color 241 235 219))
(define hintergrund-farbe (make-color 204 66 55))
(define breite 100)
(define höhe 110)
(define stab-len 120)
(define ausschlag 0.3)
(define skalierung 0.8)
(define dauer 6)

(define stabstift (make-pen glocken-farbe 10 "solid" "round" "round"))

(define glockenform
  (scale/xy breite höhe
            (polygon (list (make-pulled-point 1/5 5 0.5 1 1/5 -5)
                           (make-pulled-point 1/8 35 0 0.95 1/3 0)
                           (make-pulled-point 1/3 15 0.01 0.87 1/8 -20)
                           (make-pulled-point 1/2 -10 0.15 0.7 1/3 25)
                           (make-pulled-point 1/4 -10 0.23 0.2 1/3 -60)
                           (make-pulled-point 1/3 15 0.35 0.14 1/2 20)
                           (make-pulled-point 1/5 -55 0.43 0.09 4/5 -30)
                           (make-posn 0.5 0)
                           (make-pulled-point 4/5 30 0.57 0.09 1/5 55)
                           (make-pulled-point 1/2 -20 0.65 0.14 1/3 -15)
                           (make-pulled-point 1/3 60 0.77 0.2 1/4 10)
                           (make-pulled-point 1/3 -25 0.85 0.7 1/2 10)
                           (make-pulled-point 1/8 20 0.99 0.87 1/3 -15)
                           (make-pulled-point 1/3 0 1 0.95 1/8 -35))
                     "solid"
                     glocken-farbe)))

; Bitte gar nicht erst versuchen es zu verstehen.
; Es tut uns leid.
(define (glocken t n)
  (let* ((aktueller-winkel (* ausschlag (sin (* 2 pi (/ t 28 dauer)))))
         (stab-winkel (+ (/ pi 2) aktueller-winkel))
         (stab-ende-x (+ (/ breite 2) (* stab-len (cos stab-winkel))))
         (stab-ende-y (* stab-len (sin stab-winkel)))
         (baumsel (if (> n 0)
                      (rotate (- (* aktueller-winkel (/ 180 pi)))
                              (scale skalierung (glocken t (- n 1))))
                      (circle 10 "solid" glocken-farbe)))
         (baumsel-rad (/ (image-width baumsel) 2))
         (rahmen-diam (max (+ stab-len baumsel-rad) (/ höhe 2) (/ breite 2)))
         (rahmen (circle rahmen-diam "solid" (color 0 0 0 0)))
         (glocke-x (+ (min 0 stab-ende-x) (- rahmen-diam (/ breite 2))))
         (glocke-y rahmen-diam)
         (baumsel-x (+ glocke-x (- stab-ende-x baumsel-rad (min 0 stab-ende-x))))
         (baumsel-y (+ glocke-y (- stab-ende-y baumsel-rad))))
    (underlay/xy (underlay/xy rahmen
                              glocke-x glocke-y
                              (add-line glockenform
                                        (/ breite 2) (* höhe 0.2) stab-ende-x stab-ende-y stabstift))
                 baumsel-x baumsel-y
                 baumsel)))

(define (glocken-scene t)
  (let* ((img (glocken t 15))
         (w (image-width img))
         (h (image-height img)))
    (scale 1 (underlay (rectangle w h "solid" hintergrund-farbe)
                         img))))

(define (glocken-scene-crop t)
  (let* ((img (glocken-scene t))
         (w (image-width img))
         (h2 (/ (image-height img) 2)))
    (crop 0 h2 w h2 img)))

; Schönere 60FPS Version
; https://gfycat.com/DeliriousOddballCommabutterfly

(define (save-anim n)
  (when (>= n 0)
    (save-image (glocken-scene-crop n) (string-append "render/img" (~a n) ".png"))
    (save-anim (- n 1))))

;>> Nette Sammlung.
;>> Nett wäre noch, wenn ihr ein paar Beispielaufrufe mitliefert.
;>> Aber das könnt ihr ja dann in der Übung vorführen.
;>> Eine Baumrekursion habe ich bei euch nicht gesehen, korrigiert mich, wenn ich etwas übersehen habe.
;>> Die Einrückung ist teils etwas krude.
;>> 16+5 Pkt.

;>> Gesamt: 29 Pkt.