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

(def start-document (doc-from-url "http://brema.suub.uni-bremen.de/grenzboten/oai/?verb=GetRecord&metadataPrefix=mets&identifier=282153"))

(defn get-href [mptr]
  (get-in mptr [:attrs :xlink:href]))

(defn mptr-query [doc]
  (l/select doc
            (l/and (l/element= :mets:mptr)
                   (l/attr= :loctype "URL"))))

(def jg-urls (->> (mptr-query start-document)
                  rest
                  (map get-href)))
(def jg-docs
  (map doc-from-url jg-urls))


(defn get-band-urls [jg-doc]
  (->> (mptr-query jg-doc)
       rest
       rest
       (map get-href)))

(def band-doc (doc-from-url (first (get-band-urls jg-doc))))

(defn band-docs [jg]
  (map doc-from-url (get-band-urls jg)))

(defn get-label [node]
  (get-in node [:attrs :label]))

(defn select-article [doc]
  (l/select doc (l/element= :mets:div) (l/attr= :type "article")))

(defn get-articles [band]
  (->> ( select-article band)
       ( map get-label)))

(def articles
  (flatten
   (for [jg jg-docs
         band (band-docs jg)
         ]
     (get-articles band))))
