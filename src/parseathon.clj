(ns parseathon
  [:use name.choi.joshua.fnparse clojure.algo.monads
   [slingshot.slingshot :only [throw+]]])

(def ^:dynamic *parser-state-constructor* #(assoc {} :remainder %))

;;
;; Derived from version in Andrew Brehaut's fnparse article
;; This version has overrides to allow it to work with custom
;; parser state structures. It is possible to supply it with
;; a custom constructor for the parser state and to override
;; the *remainder-accessor* var by passing in an appropriate
;; map.
;;
(defn run-p
  "Utility function to execute a given parser with the given
  input. Good for REPL experiments."
  ([parser input]
     (run-p parser input {:constructor *parser-state-constructor* :accessor *remainder-accessor*}))
  ([parser input {:keys [constructor accessor]}]
     (binding [*remainder-accessor* accessor]
              (let [result (rule-match parser
                                       #(prn "fail:" %&)
                                       #(prn "incomplete:" %&)
                                       (constructor input))]
                result))))

;;
;; Original source now converted to a macro. This is a case where the runtime efficiency of
;; a macro is justified. Otherwise, this code executes each time you parse
;; a single character.
;;
;; (defn test-set [& args]
;;   (set (mapcat (fn [value]
;;                  (cond (sequential? value)
;;                        (map char (range (int (first value)) (inc (int (second value)))))
;;                        (char? value) [value])) args)))

(defmacro test-set [& args]
  "From args generate a set that can be used with the idiom
  '(def <non-terminal> (term #(contains? (test-set <args>) %)))'
  args can be either ranges specified by a two-value vector or
  indivdual characters."
  (let [mapper (fn [value]
                 (cond (sequential? value)
                       (map char (range (int (first value)) (inc (int (second value)))))
                       (char? value) [value]))
        result# (set (mapcat mapper args))]
    `(do ~result#)))

;;
;; Define some character classes that share meanings
;; with the Posix character classes.
;;
(def
  #^{:doc "Defines a parser that succeeds when
     the input matches a single digit."}
  digit (term #(contains? (test-set [\0 \9]) %)))

(def blank (alt (lit \space) (lit \tab)))
(def lower (term #(contains? (test-set [\a \z]) %)))
(def upper (term #(contains? (test-set [\A \Z]) %)))
(def xdigit (term #(contains? (test-set [\a \f] [\A \F] [\0 \9]) %)))
(def alpha (alt lower upper))
(def alnum (alt alpha digit))
(def word (alt alnum (lit \_)))

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
