(ns parseathon
  [:use name.choi.joshua.fnparse clojure.algo.monads
   [slingshot.slingshot :only [throw+]]])

;; From Brehaut's fnparse article
(defn run-p
  [parser input]
  (let [result (rule-match parser
			   #(prn "fail:" %&)
			   #(prn "incomplete:" %&)
			   {:remainder input})]
    (cond (nil? result) nil
	  (vector? result) (apply str result)
	  :else (str result))))

(defn run-p
  [parser input]
  (let [result (rule-match parser
			   #(prn "fail:" %&)
			   #(prn "incomplete:" %&)
			   {:remainder input})]
    result))

(defn- test-set [& args]
  (set (mapcat (fn [value]
		 (cond (sequential? value)
		       (map char (range (int (first value)) (inc (int (second value)))))
		       (char? value) [value])) args)))

;(def digit (term #(contains? (test-set [\0 \9]) %)))

(def
  #^{:doc "Defines a parser that succeeds when
     the input matches a single digit."}
  digit (lit-alt-seq "0123456789"))

(defmacro ndigits
  "Creates a parser for the specified number of digits. The
  parser returns a string of the matching digits on success."
  [n] `(semantics (rep= ~n digit) #(apply str %)))

(defn rep>=<=
  "A similiar function to rep=, only the rule succeeds if the number
  of times that the subrule is fulfilled is within the specified range."
  [low high subrule]
  (rep-predicate #(and (>= % low) (<= % high)) subrule))

(defmacro range-digits
  "Creates a parser for a number of digits within the specified range.
  The parser returns a string of the matching digits on success."
  [low high] `(semantics (rep>=<= ~low ~high digit) #(apply str %)))

(defstruct telno-s :areacode :exchange :suffix)

(def areacode (complex [_ (lit \()
			result (ndigits 3)
			_ (lit \))]
		      result))

(def exchange (ndigits 3))
(def suffix (ndigits 4))

(def telno
  (complex [areacode areacode
	    _ (lit \space)
	    exchange exchange
	    _ (lit \-)
	    suffix suffix]
	   (struct telno-s areacode exchange suffix)))

(defn stringify [v]
  (map #(apply str %) v))

;(defn m-result [output input]
;  [output input])



;; hexInt ::= /0[xX]([A-Fa-f0-9])+/

(defn foo []
  (domonad sequence-m
	   [letters ['a 'b 'c]
	    numbers [1 2 3]]
	   [letters numbers]))

(defn foox []
  (clojure.algo.monads/with-monad sequence-m
    (m-bind [(quote a) (quote b) (quote c)]
	    (fn [letters]
	      (m-bind [1 2 3]
		      (fn [numbers] (m-result [letters numbers])))))))

(defn fooy []
  (let* [name__111__auto__ sequence-m
	 m-bind (:m-bind name__111__auto__)
	 m-result (:m-result name__111__auto__)
	 m-zero (:m-zero name__111__auto__)
	 m-plus (:m-plus name__111__auto__)]
	(clojure.tools.macro/with-symbol-macros
	  (m-bind [(quote a) (quote b) (quote c)] (fn [letters] (m-bind [1 2 3] (fn [numbers] (m-result [letters numbers]))))))))