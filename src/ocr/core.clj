(ns ocr.core
  (:use [clojure.java.shell :only (sh)]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

;; Base conversion
(defn convert [in out]
  (sh "convert" in "-colorspace" "gray" "+dither" "-colors" "2" "-normalize" "-resize" "32x32!" out))

(convert "5.jpg" "5-out.png")

(map #(convert (str "data/in/" % ".png") (str "data/out/" % ".txt")) (range 0 9))


;;

(use '[clojure.string :only (join split trim)])
(defn parse-char-row [row]
  (map #(Integer/parseInt %) (filter #(or (= % "1") (= % "0")) (split row #""))))

(defn parse-char-data [element]
  (let [label (trim (last element))
        rows  (take 32 element)]
    [label (vec (flatten (map parse-char-row rows)))]))

(defn load-training-data
  [filename]
  (let [lines (drop 21 (split (slurp filename) #"\n"))
        elements (partition 33 lines)]
    (map parse-char-data elements)))

(def training-set (load-training-data "data/optdigits-orig.tra"))
