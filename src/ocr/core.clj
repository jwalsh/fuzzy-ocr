(ns ocr.core
  (:use [clojure.java.shell :only (sh)]))

(defn load-char-file [file]
  (let [filename file
        tokens   (split filename #"[_/\.]")
        label    (nth tokens 2)
        contents (parse-char-row (slurp file))]
    [label contents]))

(load-char-file "data/in/5.png")
(classify  (load-char-file "data/in/7.png"))

(def temp-outfile "/tmp/clj-converted.txt")

(defn classify-image [filename]
  (convert-image filename temp-outfile)
  (classify (load-char-file temp-outfile)))

(classify-image "data/in/3.png")

(defn sample [n]
  (println "I think that is the number" (classify-image (str "data/in/" n ".png"))))

(sample "5")

(defn -main [& args]
  (doseq [filename args]
    (println "I think that is the number" (classify-image filename))))

;; Base Conversion
(defn convert-image
  [in out]
  (sh "convert" in "-colorspace" "gray" "+dither" "-colors" "2"
      "-normalize" "-resize" "32x32!" out)
  (spit out (apply str (load-text-image out))))

(convert-image "data/in/5.png" "/tmp/5.txt")

(map #(convert (str "data/in/" % ".png") (str "data/out/" % ".txt")) (range 0 9))

;; Conversion for ImageMagik text files
(defn read-text-image-line [line]
  (if (= "white" (last (split line #"[,:\s]+"))) "0" "1"))

(defn load-text-image
  [filename]
  (let [lines (vec (drop 1 (split (slurp filename) #"\n")))
        converted (map read-text-image-line lines) ]
    (map #(apply str %) (partition 32 converted))))

(load-text-image "data/out/5.txt")

;; Training Data

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

;; Classifying Digits

(defn minus-vector [& args]
  (map #(apply - %) (apply map vector args)))

(defn sum-of-squares [coll]
  (reduce (fn [a v] (+ a (* v v))) coll))

(defn calculate-distances [in]
  (fn [row]
    (let [vector-diff (minus-vector (last in) (last row))
          label       (first row)
          distance    (Math/sqrt (sum-of-squares vector-diff))]
    [label distance])))

(defn classify [in]
  (let [k                  10
        diffs              (map (calculate-distances in) training-set)
        nearest-neighbours (frequencies (map first (take k (sort-by last diffs))))
        classification     (first (last (sort-by second nearest-neighbours)))]
    classification))
