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

; somewhat like a MAIN routine ;  Output = List of URLs
(def jg-urls (->> (mptr-query start-document)
                  rest
                  (map get-href)))
; Input = List of URLs ;  Output = List of laser data structures
(def jg-docs
  (map doc-from-url jg-urls))

; Input =  ;  Output =
(defn get-volume-urls [jg-doc]
  (->> (mptr-query jg-doc)
       rest
       rest
       (map get-href)))

; Input =  ;  Output =
(def volume-doc (doc-from-url (first (get-volume-urls jg-doc))))

; Input =  ;  Output =
(defn volume-docs [jg]
  (map doc-from-url (get-volume-urls jg)))

(defn get-label [node]
  (get-in node [:attrs :label]))

(defn select-article [doc]
  (l/select doc (l/element= :mets:div) (l/attr= :type "article")))

(defn get-articles [volume]
  (->> ( select-article volume)
       ( map get-label)))

(def articles
  (flatten
   (for [jg jg-docs
         volume (volume-docs jg)
         ]
     (get-articles volume))))
