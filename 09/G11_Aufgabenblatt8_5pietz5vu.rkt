#lang swindle

(require swindle/setf
         swindle/misc)

; SE3:FP Hausaufgaben 09
; 
; Gruppe 11
; 6808046 Tim Pietz
; 6792069 Hung Quan Vu
;
; Aufgabe 1
;
; Teilaufgabe 1.1

(defclass literature ()
  (lid
   :reader lid
   :writer set-lid!
   :initarg :lid
   :type <integer>)
  (authors
   :reader authors
   :writer set-authors!
   :initarg :authors
   :type <list>)
  (year
   :reader year
   :writer set-year!
   :initarg :year
   :type <integer>)
  (title
   :reader title
   :writer set-title!
   :initarg :title
   :type <string>)
  :autoped #t
  :printer #t)

(defclass book (literature)
  (publisher
   :reader publisher
   :writer set-publisher!
   :initarg :publisher
   :type <string>)
  (publoc
   :reader publocation
   :writer set-publocation!
   :initarg :publocation
   :type <string>)
  (series
   :reader series
   :writer set-series!
   :initarg :series
   :type <string>)
  (part-num
   :reader part
   :writer set-part!
   :initarg :part
   :type <integer>)
  :autoped #t
  :printer #t)

(defclass anthology (book)
  (editor
   :reader editor
   :writer set-editor!
   :initarg :editor
   :type <string>)
  (page
   :reader page
   :writer set-page!
   :initarg :page
   :type <integer>)
  :autoped #t
  :printer #t)

(defclass article (literature)
  (magazine
   :reader magazine
   :writer set-magazine!
   :initarg :magazine
   :type <string>)
  (volume
   :reader volume
   :writer set-volume!
   :initarg :volume
   :type <integer>)
  (issue
   :reader issue
   :writer set-issue!
   :initarg :issue
   :type <integer>)
  (month
   :reader month
   :writer set-month!
   :initarg :month
   :type <integer>)
  :autoped #t
  :printer #t)


(define a (make book
                :lid 1
                :year 1790
                :title "Mein Leben im Loch Ness: Verfolgt als Ungeheuer"
                :authors '("Nessie")
                :publisher "Minority-Verlag"
                :publocation "Inverness"
                :series "Die besondere Biographie"
                :part 1))

(define b (make anthology
                :lid 2
                :year 1979
                :title "Mostly harmless - some observations concerning the third planet of the solar sytem"
                :authors '("Prefect, F.")
                :publisher "Galactic Press"
                :publocation "Vega-System, 3rd planet"
                :series "The Hitchhiker’s Guide to the Galaxy"
                :part 5
                :editor "Adams, D."
                :page 500))

(define c (make article
                :lid 3
                :year 3200
                :title "Zeitmaschinen leicht gemacht"
                :authors '("Wells, H. G.")
                :magazine "Heimwerkerpraxis für Anfänger"
                :volume 3
                :issue 500))

(defgeneric cite ((literature))
  )

(defmethod cite ((lit literature))
  (string-append (string-join ", " (authors lit)) " "
                 "(" (number->string (year lit)) ")"))

(defmethod cite ((book book))
  (string-append (call-next-method) ", "
                 "Band " (number->string (part book)) " der Reihe: "
                 (series book) ". "
                 (publisher book) ", "
                 (publocation book) "."))

(defmethod cite ((anth anthology))
  (string-append (call-next-method) ". "
                 (title anth) ". "
                 "In " (editor anth) ", editor, "
                 (series anth) ", "
                 "volume " (number->string (part anth)) " of "
                 "\"" "Das passt nirgends" "\". "
                 (publocation anth) ", "
                 "was" " edition, "
                 "p. " (number->string (page anth)) "."))

(defmethod cite ((art article))
  (string-append (call-next-method) ". "
                 (title art) ". "
                 (magazine art) ", "
                 (number->string (issue art))
                 "(" (number->string (volume art)) ")."))

; Kein string-join, kein curry, nicht mal default Werte für Parameter in swindle?
(define (string-join sep lst)
  (cond
    [(empty? lst) ""]
    [(empty? (cdr lst)) (car lst)]
    [else (string-append (car lst) sep (string-join sep (cdr lst)))]))


; Teilaufgabe 1.3
;
; Ergänzungsmethoden erlauben das Ausführen von zusätzlichen Funktionen um die Implementation einer
; der Oberklasse herum. Über die Stichwörter ":before" ":around" und ":after" ist es möglich
; beim Aufruf einer Methode zum entsprechenden Zeitpunkt zusätzlich initialisierungen o.ä. auszu-
; führen, die entweder direkt von der überliegenden Implementation mitverwendet werden, oder z.B. das
; Exemplar zum Schluss weiter bearbeiten. Ein expliziter super-call fällt somit weg.
; 
; Unserer Meinung nach ist die Anwendung von Ergänzungsmethoden in diesem Fall nicht besonders
; sinnvoll, da zumindest aus den gegebenen Materialien nicht hervorgeht, dass man mithilfe einer
; Nachmethode den Rückgabewert der Primärmethode zu verändern. Somit sähe die wahrscheinlich einzige
; Implementation unter Verwendung von Ergänzungsmethoden so aus, dass man in einer :before Methode
; einen Slot auf den klassenspezifischen Wert hinter der Jahresangabe setzt, der dann in der Primär-
; methode Abgefragt und angehängt wird. Das ist unserer Meinung nach jedoch keine schöne Lösung.
; Eine Alternative wäre es natürlich auch noch, die Ausgabe über (display "...") zu machen, was jedoch
; auch keine besonders schöne Lösung ist. </meinung>


; Aufgabe 2

; Wenn es ein Wasser- und Landfahrzeug ist, sollte es beide Medien beherrschen, dementsprechend eine
; Liste aus beiden
(defgeneric medien ((fahrzeug))
  :combination generic-list-combination)

; Wasser 5 km/h und Luft 500 km/h -> Höchstgeschwindigkeit 500
(defgeneric geschwindigkeit ((fahrzeug))
  :combination generic-max-combination)

; Wie oben. Wenn man auf Land mehr transportieren kann als in der Luft, muss man ggf. halt auf Land
; bleiben
(defgeneric zuladung ((fahrzeug))
  :combination generic-max-combination)

; Wahrscheinlich eigentlich Durchschnitt, das gibt es aber nicht vorgefertigt, und selber schreiben...
(defgeneric verbrauch ((fahrzeug))
  :combination generic-max-combination)

; Es macht für uns keinen Sinn, dass ein Fahrzeug auf dem Wasser mehr oder weniger Plätze haben sollte
; als auf Land. Höchstens Einschränkungen in Sachen gewicht. Deshalb haben wir uns entschieden, diese
; eine Eigenschaft einfach direkt in Fahrzeug einzutragen.
(defgeneric passagierzahl ((fahrzeug)))

(defclass fahrzeug ()
  (passagierzahl
   :accessor passagierzahl
   :initarg :passagierzahl)
  :printer #t)

(defclass landfahrzeug (fahrzeug)
  (medien-land
   :initvalue 'land
   :accessor medien)
  (geschwindigkeit-land
   :accessor geschwindigkeit
   :initarg :geschwindigkeit-land)
  (zuladung-land
   :accessor zuladung
   :initarg :zuladung-land)
  (verbrauch-land
   :accessor verbrauch
   :initarg :verbrauch-land))

(defclass schienenfahrzeug (landfahrzeug)
  (geschwindigkeit-schiene
   :accessor geschwindigkeit
   :initarg :geschwindigkeit-schiene)
  (zuladung-schiene
   :accessor zuladung
   :initarg :zuladung-schiene)
  (verbrauch-schiene
   :accessor verbrauch
   :initarg :verbrauch-schiene))

(defclass straßenfahrzeug (landfahrzeug)
  (geschwindigkeit-straße
   :accessor geschwindigkeit
   :initarg :geschwindigkeit-straße)
  (zuladung-straße
   :accessor zuladung
   :initarg :zuladung-straße)
  (verbrauch-straße
   :accessor verbrauch
   :initarg :verbrauch-straße))

(defclass wasserfahrzeug (fahrzeug)
  (medien-wasser
   :initvalue 'wasser
   :accessor medien)
  (geschwindigkeit-wasser
   :accessor geschwindigkeit
   :initarg :geschwindigkeit-wasser)
  (zuladung-wasser
   :accessor zuladung
   :initarg :zuladung-wasser)
  (verbrauch-wasser
   :accessor verbrauch
   :initarg :verbrauch-wasser))

(defclass luftfahrzeug (fahrzeug)
  (medien-luft
   :initvalue 'luft
   :accessor medien)
  (geschwindigkeit-luft
   :accessor geschwindigkeit
   :initarg :geschwindigkeit-luft)
  (zuladung-luft
   :accessor zuladung
   :initarg :zuladung-luft)
  (verbrauch-luft
   :accessor verbrauch
   :initarg :verbrauch-luft))


(defclass amphibienfahrzeug (wasserfahrzeug landfahrzeug))
(defclass amphibienflugzeug (straßenfahrzeug wasserfahrzeug luftfahrzeug))
(defclass zweiwegefahrzeug (straßenfahrzeug schienenfahrzeug))
(defclass zeitzug (schienenfahrzeug luftfahrzeug))

(define wala (make amphibienfahrzeug
                 :passagierzahl 4
                 :geschwindigkeit-land 150
                 :geschwindigkeit-wasser 15
                 :verbrauch-land 7
                 :verbrauch-wasser 12
                 :zuladung-land 700
                 :zuladung-wasser 400))


(define walu (make amphibienflugzeug
                 :passagierzahl 2
                 :geschwindigkeit-luft 15
                 :geschwindigkeit-wasser 30
                 :verbrauch-luft 17
                 :verbrauch-wasser 23
                 :zuladung-luft 270
                 :zuladung-wasser 530))

(define stsch (make zweiwegefahrzeug
                 :passagierzahl 7
                 :geschwindigkeit-schiene 150
                 :geschwindigkeit-straße 40
                 :verbrauch-schiene 15
                 :verbrauch-straße 10
                 :zuladung-schiene 700
                 :zuladung-straße 1000))

(define schluft (make zeitzug
                 :passagierzahl 18
                 :geschwindigkeit-luft 750
                 :geschwindigkeit-schiene 500
                 :verbrauch-luft 70
                 :verbrauch-schiene 30
                 :zuladung-luft 1200
                 :zuladung-schiene 3500))

; Teilaufgabe 2.3
;
; Die Auswertung einer Funktion, die abgesehen von der Operation nur durch Akzessoren definiert ist,
; wie es bei medien, geschwindigkeit, zuladung und verbrauch der Fall ist, kann durch Kobinatoren
; geschehen. So wird zum Beispiel beim Aufruf von (medien wala) die Klassenpräzedenzliste
; abgearbeitet, jeweils das Ergebniss des Akzessors gesammelt, und anschließend entsprechend einer
; bestimmten Aktion zusammengefasst. Bei medien handelt es sich dabei um die generic-list-combination,
; bei der aus den Ergebnissen eine Liste gebildet wird.
; Die Klassenpräzedenzliste stellt dabei die Reihenfolge dar, in der die Klassen nach
; implementierenden Methoden / Akzessoren durchsucht werden. Dabei steht die erbende Klasse immer an
; erster Stelle, danach (rekursiv) jeweils die Präzedenzlisten der Oberklassen in der Reihenfolge, wie
; sie bei Erstellung der Klasse genannt wurden. Wenn eine der (ggf. indirekten) Oberklassen auf
; mehreren Pfaden erreicht werden kann (Diamantvererbung), so wird diese erst bei dem letzten solcher
; Pfade besucht.
; Eine eindeutige Präzedenzliste spielt besonders bei nicht kommutativen Kombinationsoperationen wie
; list, append oder begin eine größere Rolle, da somit die Reihenfolge der Auswertung fest bestimmt
; ist. Eine ggf. noch größere Rolle spielt sie bei solchen Funktionen, die nicht kombiniert werden,
; sondern in denen die Präzedenzliste bestimmt, welche Methode überhaupt ausgeführt werden soll.
; 