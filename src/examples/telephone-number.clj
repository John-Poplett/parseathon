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

(def #^{:doc "A complete parser for a telephone-number."}
  telno
  (complex [areacode areacode
            _ (lit \space)
            exchange exchange
            _ (lit \-)
            suffix suffix]
           (struct telno-s areacode exchange suffix)))
