(ns examples.protobuf
  [:use [parseathon :only [digit alpha test-set]]
   name.choi.joshua.fnparse clojure.algo.monads
   [slingshot.slingshot :only [throw+]]])

(defstruct parser-state :remainder :column :line)

(defstruct field :qualifier :name :type :tag)

(def remainder-a
  (accessor parser-state :remainder))

(defn run-p [parser input]
  (parseathon/run-p parser input {:accessor remainder-a :constructor #(struct parser-state % 0 0)}))

(def alpha-plus (term #(contains? (test-set [\a \z] [\A \Z] \- \_) %)))

(def lower-plus-hyphen (term #(contains? (test-set [\a \z] \-) %)))

(defn- b-char [subrule]
  (invisi-conc subrule (update-info :line inc)))

(def space (lit \space))

(def tab (lit \tab))

(def line-break (b-char (rep+ (alt (lit \newline) (lit \return)))))

(def ws (rep+ (alt space tab line-break)))

(def optional-ws (rep* (alt space tab line-break)))

(defn- protobuf-keyword [string]
  "Match a literal string and return it as a keyword."
  (constant-semantics (lit-conc-seq string) (keyword string)))

(def protobuf-qualifier (alt (protobuf-keyword "required") (protobuf-keyword "optional") (protobuf-keyword "repeated")))

(def protobuf-type (alt (protobuf-keyword "int32") (protobuf-keyword "string")))

(def protobuf-identifier (semantics (conc alpha (rep* alpha-plus))
                                (fn [result] (apply str (flatten result)))))

(def protobuf-tag (semantics (rep+ digit)
                             (fn [result] (Integer/parseInt (apply str result)))))

(def type-specifier
  (complex [qualifier protobuf-qualifier
            _ ws
            type protobuf-type
            _ ws
            name protobuf-identifier
            _ ws
            _ (lit \=)
            _ ws
            tag protobuf-tag
            _ optional-ws
            _ (lit \;)]
           (struct field qualifier name type tag)))

(def type-specifiers
  (conc type-specifier (rep* (conc ws type-specifier))))

(declare message-body)

(def message
  (complex
   [_ (lit-conc-seq "message")
    _ ws
    _ protobuf-identifier
    _ ws
    body message-body]
   body))

(def message-body-element
  (alt message type-specifier))

(def message-body
  (complex
   [_ (lit \{)
    _ ws
    body (conc
          message-body-element
          (rep* (conc ws message-body-element)))
    _ ws
    _ (lit \})]
   body))
