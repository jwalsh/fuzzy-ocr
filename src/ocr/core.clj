(ns ocr.core
  (:use [clojure.java.shell :only (sh)]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))


(defn convert [in out]
  (sh "convert" in "-colorspace" "gray" "+dither" "-colors" "2" "-normalize" "-resize" "32x32!" out))

(convert "5.jpg" "5-out.png")

(map #(convert (str "in/" % ".png") (str "out/" % ".png")) (range 0 9))
