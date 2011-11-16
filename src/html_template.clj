(ns html-template)
    
(defn- first-match [m]
  (if (coll? m) (first m) m))

(defn- match [regex text]
  "Return the startr index of the match, length of the match and the match itself."
  (let [r (re-find (re-pattern regex) text)
        m (first-match r)]
    (if (nil? m)
      [0 0 nil]
      (let [ind (.indexOf text m) len (.length m)]
        [ind (+ ind len) r]))))

(defn- parse-tmpl-directive [data result tag identifier]
  "Create tokenized representation of HTML-TEMPLATE tags. The regex expression
returns two or three values: the complete match, the tag name and an optional
identifier. The function uses a map lookup to convert the tag name into its
tokenized representation, .e.g. the string \"<!-- TMPL_VAR hello -->\" is
converted to the vector [ :tmpl-var :hello ]."
  (if result
    (let [value ({ "TMPL_VAR" [ :tmpl-var (keyword identifier) ]
                   "TMPL_IF" [ :tmpl-if (keyword identifier) ]
                   "TMPL_ELSE" [ :tmpl-else nil ]
                   "/TMPL_IF" [ :tmpl-end-if nil ]
                   "TMPL_UNLESS" [ :tmpl-unless (keyword identifier) ]
                   "/TMPL_UNLESS" [ :tmpl-end-unless nil ]
                   "TMPL_LOOP" [ :tmpl-loop (keyword identifier) ]
                   "/TMPL_LOOP" [ :tmpl-end-loop nil ] } (.toUpperCase tag)) ]
      [ value (subs data (count result)) ])
    nil))

(defn- ^String triml-newline
  "Removes newlines from the left side of string."
  [^CharSequence s]
  (loop [index (int 0)]
    (if (= (.length s) index)
      ""
      (let [ch (.charAt s index)]
        (if (or (= ch \newline) (= ch \return))
          (recur (inc index))
          (.. s (subSequence index (.length s)) toString))))))

(comment
  "Pattern to identify all valid instances of an HTML-TEMPLATE tag. It returns the whole matching string,
the tag and optionally an identifier.")
(def tmpl-pattern  "<!--\\s+((?i)/?TMPL_(?:IF|ELSE|LOOP|UNLESS|VAR))(?:\\s+([a-z][a-z\\-0-9]*))?\\s++-->")

(defn parse-template [init-template-data]
  "Parse template data into a tokenized intermediate form."
  (loop [result []
         data init-template-data]
    (if (> (count data) 0)
      (let [[start finish [match-result match-tag match-identifier]] (match tmpl-pattern data)]
        ;(println data " " start " " finish)
        (cond (> start 0)    ; collect plain text up to tmpl directive
              (recur (conj result [ :text (subs data 0 start) ])
                     (subs data start))
              (and (= start 0) (= finish 0)) ; remainder is plain text
              (recur (conj result [ :text data ])
                     "")
              (and (= start 0) (> finish 0))
              (let [[directive, next-data] (parse-tmpl-directive data match-result match-tag match-identifier)]
                (recur (conj result directive) (triml-newline next-data)))
              :else (assert false "Oops!")))
      result)))

(defstruct context :compiled-output :context :context-symbol)

(defn- create-context-stack []
  [(struct context [] (gensym) nil)])

(declare peek-context)

(defn- create-code-frame [ctx context-symbol]
  "Like push-context-stack only preserves \"context\" from previous
stack position."
  (conj ctx (struct context [] (peek-context ctx) context-symbol)))
  
(defn- push-context-stack [ctx context-symbol]
  (conj ctx (struct context [] (gensym) context-symbol)))

(defn- pop-context-stack [context]
  (pop context))

(defn- peek-compiled-output [context]
  (:compiled-output (peek context)))

(defn- peek-context [context]
  (:context (peek context)))

(defn- peek-previous-context [context]
  (peek-context (pop-context-stack context)))

(defn- peek-context-symbol [context]
  (:context-symbol (peek context)))

(defn- push-compiled-output [context value]
  (let [ele (peek-compiled-output context)
        old-struct (peek context)]
    (conj (pop context) (assoc old-struct :compiled-output (conj ele value)))))

(defn compile-template
  ([init-tokens] (compile-template init-tokens {}))
  ([init-tokens {evaluate? :eval, :or { evaluate? true }}]
  "Compile tokenized, intermediate form into executable Clojure code"
  (loop [context (create-context-stack)
         tokens init-tokens]
    (if tokens
      (let [[key value] (first tokens)]
        ;(println "compiled-output:" (peek-compiled-output context) "key:" key ", value:" value)
        (cond (= key :text)
              (recur (push-compiled-output context `(print ~value)) (next tokens))
              (= key :tmpl-var)
              (recur (push-compiled-output context `(print (get ~(peek-context context) ~value))) (next tokens))
              (= key :tmpl-if)
              (recur (create-code-frame context value) (next tokens))
              (= key :tmpl-else)
              (recur (create-code-frame context :tmpl-else) (next tokens))
              (= key :tmpl-end-if)
              (let [context-symbol (peek-context-symbol context)]
                (cond (= context-symbol :tmpl-else)
                      (let [if-context (pop context)
                            compiled-output
                            `(if (get ~(peek-context context) ~(peek-context-symbol if-context))
                               (do
                                 ~@(peek-compiled-output if-context))
                               (do
                                 ~@(peek-compiled-output context)))]
                        (recur (push-compiled-output (pop-context-stack if-context) compiled-output) (next tokens)))
                      :else
                      (let [compiled-output
                            '(if (get ~(peek-context context) ~(peek-context-symbol context))
                               (do
                                 ~@(peek-compiled-output context)))]
                        (recur (push-compiled-output (pop-context-stack context) compiled-output) (next tokens)))))
              (= key :tmpl-unless)
              (recur (create-code-frame context value) (next tokens))
              (= key :tmpl-end-unless)
              (let [context-symbol (peek-context-symbol context)]
                (cond (= context-symbol :tmpl-else)
                      (let [if-context (pop context)
                            compiled-output
                            `(if (not (get ~(peek-context context) ~(peek-context-symbol if-context)))
                               (do
                                 ~@(peek-compiled-output if-context))
                               (do
                                 ~@(peek-compiled-output context)))]
                        (recur (push-compiled-output (pop-context-stack if-context) compiled-output) (next tokens)))
                      :else
                      (let [compiled-output
                            '(if (not (get ~(peek-context context) ~(peek-context-symbol context)))
                               (do
                                 ~@(peek-compiled-output context)))]
                        (recur (push-compiled-output (pop-context-stack context) compiled-output) (next tokens)))))

              (= key :tmpl-loop)
              (recur (push-context-stack context value) (next tokens))
              (= key :tmpl-end-loop)
              (let [context-symbol (peek-context-symbol context)
                    compiled-output `(doseq [~(peek-context context) (get ~(peek-previous-context context) ~(peek-context-symbol context))]
                                       ~@(peek-compiled-output context))]
                (recur (push-compiled-output (pop-context-stack context) compiled-output) (next tokens)))
              :else (recur context (next tokens))))
      (let [compiled-output `(fn [~(peek-context context)] ~@(peek-compiled-output context))]
        (if evaluate?
          (eval compiled-output)
          compiled-output))))))

(defn parse-and-compile [^CharSequence template]
  "Compile template to Clojure code."
  (compile-template (parse-template template)))

(defn print-template [^CharSequence template context]
  "Parse, compile and evaluate the compiled template with the given context."
  ((parse-and-compile template) context))

