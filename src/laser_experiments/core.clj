                                        ;
                                        ; Laser auf github:  https://github.com/Raynes/laser
                                        ;
                                        ; Laser 1.0.0 API documentation:  http://raynes.github.io/laser/docs/

                                        ; I wrote a fairly large and thorough guide to laser:
                                        ; https://github.com/Raynes/laser/blob/master/docs/guide.md


(ns laser-experiments.core
  (:require [clj-http.client :as client]
            [me.raynes.laser :as l])
  (:use [clojure.java.io :only [file]]))

;;sorgt dafür, dass alles ausgegeben wird
(set! *print-length* nil)

(def f (clojure.java.io/file "/home/mn/clojure/laser/METS-Formateproblem/export_mets_AUS__urn+nbn+de+gbv+46+1-908-VL282158_20130802T124213__VOM_2013-08-02.xml" ))

; f parsen --> document erstellen
(def d (l/parse f :parser :xml))

; (spit "/home/mn/clojure/output.edn" d)

(def q (l/and (l/element= :mets:div) (l/attr= :type "section")))

(def res (l/select d q ))
; Evaluierung eines Ausdrucks hier: C-x C-e
; dann im REPL zunaechst: (in-ns 'laser-experiments.core)

;-----------------------------------------------------------
; anderer Ansatz der Definition von q:
(defn create-selector [element [attrkey attrval]]
  (l/and (l/element= element) (l/attr= attrkey attrval)))
(def q (create-selector :mets:div [:type "section"]))
(def res (l/select d q ))

(map #(get-in % [:attrs :label]) res)

(map prn (map #(get-in % [:attrs :label]) res))

(use '[clojure.string :only (join split)])
(map #(split % #"\s") (map #(get-in % [:attrs :label]) res))
; weiter mit ... alles in eine Collection ... sort | unique ...

(apply concat (map  #(split % #"\s") (map #(get-in % [:attrs :label]) res)))
                                        ; oder:
(reduce concat (map  #(split % #"\s") (map #(get-in % [:attrs :label]) res)))
                                        ; Beispiel:
(reduce (fn [l r] (list '+ l r)) (range 10))
; OUT:     (+ (+ (+ (+ (+ (+ (+ (+ (+ 0 1) 2) 3) 4) 5) 6) 7) 8) 9)

(sort (reduce concat (map  #(split % #"\s") (map #(get-in % [:attrs :label]) res))))
(distinct  (sort (reduce concat (map  #(split % #"\s") (map #(get-in % [:attrs :label]) res)))))

; -----------------------------------------------------
;          v---- METS files crawler ----v
; -----------------------------------------------------
(defn doc-from-url [url]
  (l/parse (:body (client/get url)) :parser :xml))

; constant URL = top level METS file of the "Die Grenzboten"
; Output = laser data structure
(def start-document (doc-from-url "http://brema.suub.uni-bremen.de/grenzboten/oai/?verb=GetRecord&metadataPrefix=mets&identifier=282153"))

; Input = laser select result ;  Output = List of URLs
(defn get-href [mptr]
  (get-in mptr [:attrs :xlink:href]))

; Input = laser data structure ;  Output = laser select result
(defn mptr-query [doc]
  (l/select doc
            (l/and (l/element= :mets:mptr)
                   (l/attr= :loctype "URL"))))

; quasi-Input = function call with constant URL ;  Output = List of URLs
(def jg-urls (->> (mptr-query start-document)
                  rest
                  (map get-href)))
; Input = List of URLs ;  Output = List of laser data structures
(def jg-docs
  (map doc-from-url jg-urls))

; Input = laser data structure ;  Output = List of URLs
(defn get-volume-urls [jg-doc]
  (->> (mptr-query jg-doc)
       rest
       rest
       (map get-href)))

; Input = ??? ;  Output = List of laser data structures
(defn volume-docs [jg]
  (map doc-from-url (get-volume-urls jg)))

; Input = <der Output von select-article !?!?> ;   Output = Ein Kapitel-Titel !?
(defn get-label [node]
  (get-in node [:attrs :label]))

; Input = laser data structure ;   Output = laser select result
(defn select-article [doc]
  (l/select doc (l/element= :mets:div) (l/attr= :type "article")))

; Input = laser data structure ;   Output = list of <Kapitel Titel>
(defn get-articles [volume]
  (->> ( select-article volume)
       ( map get-label)))

; somewhat like a MAIN routine ;  Output = List of <Kapitel Titel>
; Die for-function ist ein 2-fach kartesisches Produkt ueber alle Jahrgaenge und
; die zugehoerigen volumes.
;; Tipp: Der Aufruf zu flatten ist hier unnötig wenn man die for schleife zum
;; 3-fachen kartesischen Produkt umwandelt
;;; ok - siehe unten - articles2 ... hat aber eine Exception geschmissen ...
(def articles
  (flatten
   (for [jg jg-docs
         volume (volume-docs jg)
         ]
     (get-articles volume))))

; test: Ist "jg-docs" ein "function call" ohne runde Klammern?
      ; (for [jg jg-docs] jg)
; ... oops - das hat viel ausgegeben  :-|   ... fast 9 Mio. Zeilen
; Ergebnis: ja, es ist ein "function call" ohne runde Klammern
;; Nein, es ist kein "function call" ohne runde Klammern, in clojure gibt es
;; das nicht. jd-docs ist eine Variable, die eine Sequenz enthält, deshalb
;; klappt das hier. Für ein function call hätte man (jg-docs) schreiben müssen
;; Das ist auch ein Vorteil von clojure, dass man diese Fälle immer ganz einfach
;; unterscheiden kann.
(def articles2
  (for [jg jg-docs
        volume (volume-docs jg)
        article (get-articles volume)
        ]
    article))
; ClassCastException clojure.lang.LazySeq cannot be cast to clojure.lang.IFn  laser-experiments.core/eval5037 (NO_SOURCE_FILE:1)
;;Das ist genau die version, an die ich gedacht habe!
;;Bei mir funktioniert sie auch problemlos.
;;Es kann sein, dass bei dir irgendwas an definitionen von der repl übrig war und
;;das gestört hat. Ich habe den Test hinzugefügt. Wenn keine Exception beim Laden
;;der Datei geworfen wird, funktioniert articles2
(doall (map #(assert (= %1 %2)) (take 1000 articles) (take 1000 articles2)))


(defn text-from-link [link]
  (as-> link x
        (doc-from-url x)
        (l/select x (l/element= :charparams))
        (map :content x)
        (flatten x)
        (apply str x)))

(defn abby-plaintext [vlid]
  (text-from-link (str "http://brema.suub.uni-bremen.de/grenzboten/download/fulltext/fr/" vlid)))
