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


;; (defn parse-tmpl-var [data]
;;   (let [result (re-find (re-pattern "^<!--\\s+TMPL_VAR\\s+([a-z][a-z0-9]*)\\s+-->") data)]
;;     (if result
;;       [ [ :tmpl-var (keyword (second result)) ], (subs data (count (first result))) ]
;;       nil)))

(defn telno-regex [input]
  (let [result (re-find (re-pattern "\\((\\d\\d\\d)\\) (\\d{3,3})-(\\d{4,4})") input)]
    (if result
      (let [areacode (nth result 1)
            exchange (nth result 2)
            suffix (nth result 3)]
        (struct telno-s areacode exchange suffix))
      nil)))
