(ns html-template)
    
(defn- first-match [m]
  (if (coll? m) (first m) m))

(defn- match [regex text]
  (let [m (first-match (re-find (re-pattern regex) text))]
    (if (nil? m)
      [0 0]
      (let [ind (.indexOf text m) len (.length m)]
        [ind (+ ind len)]))))

(defn- parse-tmpl-directive [data]
  "Create tokenized representation of HTML-TEMPLATE tags. The regex expression
returns two or three values: the complete match, the tag name and an optional
identifier. The function uses a map lookup to convert the tag name into its
tokenized representation, .e.g. the string \"<!-- TMPL_VAR hello -->\" is
converted to the vector [ :tmpl-var :hello ]."
  (let [[result tag identifier] (re-find (re-pattern "^<!--\\s+(/?TMPL_(?:IF|ELSE|LOOP|VAR))(?:\\s+([a-z][a-z\\-0-9]*))?\\s++-->") data)]
    (if result
      (let [value ({ "TMPL_VAR" [ :tmpl-var (keyword identifier) ]
                     "TMPL_IF" [ :tmpl-if (keyword identifier) ]
                     "TMPL_ELSE" [ :tmpl-else nil ]
                     "/TMPL_IF" [ :tmpl-end-if nil ]
                     "TMPL_LOOP" [ :tmpl-loop (keyword identifier) ]
                     "/TMPL_LOOP" [ :tmpl-end-loop nil ] } tag) ]
        [ value (subs data (count result)) ])
      nil)))

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

(defn parse-template [init-template-data]
  "Parse template data into a tokenized intermediate form."
  (loop [result []
         data init-template-data]
    (if (> (count data) 0)
      (let [[start, finish] (match "<!--\\s+(TMPL_(ELSE|((IF|VAR|LOOP)\\s+[a-z][a-z\\-0-9]*))|/TMPL_(IF|LOOP))\\s+-->" data)]
        ;(println data " " start " " finish)
        (cond (> start 0)    ; collect plain text up to tmpl directive
              (recur (conj result [ :text (subs data 0 start) ])
                     (subs data start))
              (and (= start 0) (= finish 0)) ; remainder is plain text
              (recur (conj result [ :text data ])
                     "")
              (and (= start 0) (> finish 0))
              (let [[directive, next-data] (parse-tmpl-directive data)]
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

(defn parse-and-compile [template]
  "Compile template to Clojure code."
  (compile-template (parse-template template)))

(defn print-template [template context]
  "Parse, compile and evaluate the compiled template with the given context."
  ((parse-and-compile template) context))



