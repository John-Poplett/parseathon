(ns examples.telephone-number
  [:use [parseathon :only [ndigits]]
   name.choi.joshua.fnparse])

(defstruct telno-s :areacode :exchange :suffix)

(def #^{:doc "Define a parser for area-codes"}
  areacode (complex [_ (lit \()
                     result (ndigits 3)
                     _ (lit \))]
                    result))

(def exchange (ndigits 3))
(def suffix (ndigits 4))
(def space (lit \space))
(def hyphen (lit \-))

(def #^{:doc "A complete parser for a telephone-number."}
  telno
  (complex [areacode areacode
            _ space
            exchange exchange
            _ hyphen
            suffix suffix]
           (struct telno-s areacode exchange suffix)))

(def telno-pattern "\\((\\d{3})\\) (\\d{3})-(\\d{4})")

(defn telno-regex [input]
  "Demonstration of a telno parser using plain-vanilla regular expressions."
  (let [result (re-find (re-pattern telno-pattern) input)]
    (if result
      (let [areacode (nth result 1)
            exchange (nth result 2)
            suffix (nth result 3)]
        (struct telno-s areacode exchange suffix))
      nil)))
