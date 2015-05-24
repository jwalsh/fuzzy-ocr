(ns ocr.core
  (:use [clojure.java.shell :only (sh)]
        [clojure.string :only [trim split join]]))

;; Core entities
(defrecord Sample [digit vect])

;; Base Conversion
(defn convert-image
  [in out]
  (sh "convert" in "-colorspace" "gray" "+dither" "-colors" "2"
      "-normalize" "-resize" "32x32!" out))

;; Conversion for ImageMagik text files
(defn read-text-image-line [line]
  ;;  (println line)
  (if (= "#FFFFFF" (nth (split line #"[\s]+") 2)) "0" "1"))

;; This consumes the exported format used by ImageMagik
(defn load-text-image [filename]
  (let [lines (vec (drop 1 (split (slurp filename) #"\n")))
        converted (map read-text-image-line lines) ]
    ;; (println converted)
    (map #(apply str %) (partition 32 converted))))

;; Debugging only
(defn export-text-image [loaded-text-image exported-text-image]
  (spit exported-text-image (apply str (join "\n" (load-text-image loaded-text-image)))))

;; Training Data
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
  ;; (println in)
  (fn [row]
    (let [vector-diff (minus-vector (last in) (last row))
          label       (first row)
          distance    (Math/sqrt (sum-of-squares vector-diff))]
      ;; (println "Processing" label "with a distance of" distance)
    [label distance])))

;; Application
(defn load-char-file [filename]
  (println (slurp filename))
  (let [tokens   (split filename #"[_/\.]")
        label    (nth tokens 2)
        contents (parse-char-row (slurp filename))]
    [(str label) contents]))

(defn classify [in]
  (let [k                  1000
        diffs              (map (calculate-distances in) training-set)
        nearest-neighbours (frequencies (map first (take k (sort-by last diffs))))
        classification     (first (last (sort-by second nearest-neighbours)))]
    (println "Frequency of nearest neighbors: "  nearest-neighbours)
    classification))

(defn classify-image [filename]
  (let [temp-outfile "/tmp/clj-converted.txt"
        temp-png "/tmp/clj-converted.png"
        temp-exported "/tmp/clj-exported.txt"]
    (convert-image filename temp-outfile)
    (convert-image filename temp-png)
    (export-text-image temp-outfile temp-exported)
    (classify (load-char-file temp-exported))))

;; (quote (classify-image "data/in/2.png"))

(defn -main [& args]
  (doseq [filename args]
    (println filename "is the number" (classify-image filename)))
  (System/exit 0))
