(ns parseathon
  [:use name.choi.joshua.fnparse clojure.algo.monads
   [slingshot.slingshot :only [throw+]]])

;; Derived from version in Brehaut's fnparse article
(defn run-p
  "Utility function to execute a given parser with the given
  input. Good for REPL experiments."
  [parser input]
  (let [result (rule-match parser
                           #(prn "fail:" %&)
                           #(prn "incomplete:" %&)
                           {:remainder input})]
    result))

(defn test-set [& args]
  (set (mapcat (fn [value]
                 (cond (sequential? value)
                       (map char (range (int (first value)) (inc (int (second value)))))
                       (char? value) [value])) args)))

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
