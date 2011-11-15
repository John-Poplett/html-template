(ns html-template
  [:use name.choi.joshua.fnparse clojure.algo.monads])

(defn run-p
  [parser input]
  (let [result (rule-match parser
                           #(prn "fail:" %&)
                           #(prn "incomplete:" %&)
                           {:remainder input})]
    (cond (nil? result) nil
          (vector? result) (apply str result)
          :else (str result))))

(defn- test-set [& args]
  (set (mapcat (fn [value]
                 (cond (sequential? value)
                       (map char (range (int (first value)) (inc (int (second value)))))
                       (char? value) [value])) args)))

(def lower-case (term #(contains? (test-set [\a \z]) %)))

(def lower-case-plus-hyphen (term #(contains? (test-set [\a \z] \-) %)))

(def tmpl-identifier (semantics (conc lower-case (rep* lower-case-plus-hyphen))
                                (fn [result] (apply str (flatten result)))))

(def comment-start (lit-conc-seq "<!--"))

(def comment-end (lit-conc-seq "-->"))

(def ws (rep+ (lit-alt-seq [\space \tab \newline])))

(defn- lit-ic
  "Like lit only is case insensitive."
  [literal-token]
  (term (fn [char] (= (java.lang.Character/toUpperCase literal-token) (java.lang.Character/toUpperCase char)))))

(defn- tmpl-expr-with-parameter [tag key]
  (complex [_ comment-start
            _ ws
            _ (lit-conc-seq tag lit-ic)
            _ ws
            identifier tmpl-identifier
            _ ws
            _ comment-end
            _ (rep* (lit \newline))]
           [key (keyword identifier)]))

(defn- tmpl-expr-no-parameter [tag key]
  (complex [_ comment-start
            _ ws
            _ (lit-conc-seq tag lit-ic)
            _ ws
            _ comment-end
            _ (rep* (lit \newline))]
           [key nil]))

(declare stmts)

(def loop-start (tmpl-expr-with-parameter "TMPL_LOOP" :tmpl-loop))
(def loop-end (tmpl-expr-no-parameter "/TMPL_LOOP" :tmpl-end-loop))
(def loop-stmt
  (complex [loop-start-value loop-start
            values stmts
            loop-end-value loop-end]
           [loop-start-value values loop-end-value]))

(def if-start (tmpl-expr-with-parameter "TMPL_IF" :tmpl-if))
(def if-end (tmpl-expr-no-parameter "/TMPL_IF" :tmpl-end-if))
(def else (tmpl-expr-no-parameter "TMPL_ELSE" :tmpl-else))
(def if-simple-stmt
  (complex [if-start-value if-start
            values stmts
            if-end-value if-end]
           [if-start-value values if-end-value]))
(def if-else-stmt
  (complex [if-start-value if-start
            if-values stmts
            else-value else
            else-values stmts
            if-end-value if-end]
           [if-start-value if-values else-value else-values if-end-value]))
(def if-stmt (alt if-simple-stmt if-else-stmt))

(def unless-start (tmpl-expr-with-parameter "TMPL_UNLESS" :tmpl-unless))
(def unless-end (tmpl-expr-no-parameter "/TMPL_UNLESS" :tmpl-end-unless))
(def unless-simple-stmt
  (complex [unless-start-value unless-start
            values stmts
            unless-end-value unless-end]
           [unless-start-value values unless-end-value]))
(def unless-else-stmt
  (complex [unless-start-value unless-start
            unless-values stmts
            else-value  else
            else-values stmts
            unless-end-value unless-end]
           [unless-start-value unless-values else-value else-values unless-end-value]))
(def unless-stmt (alt unless-simple-stmt unless-else-stmt))

(def tmpl-var (tmpl-expr-with-parameter "TMPL_VAR" :tmpl-var))

(def tmpl-expr (alt loop-start loop-end if-start if-end else unless-start unless-end tmpl-var))

(def text
  (complex [value (rep+ (except anything tmpl-expr))]
           [ :text (apply str (flatten value)) ]))

(def stmt
  (complex [value (alt text tmpl-var if-stmt loop-stmt unless-stmt)]
           value))

(def stmts
  (complex [values (rep+ stmt)]
            values ))

(defn- flatter [data]
  "Partially flatten results. A vector of vectors is allowed but further nesting is not."
  (letfn [(work-horse [data acc]
            (if data
              (cond (and (vector? (first data)) (vector? (first (first data))))
                    (work-horse (next data) (work-horse (first data) acc))
                    :else
                    (work-horse (next data) (conj acc (first data))))
              acc))]
    (work-horse data [])))

(defn- call-flatter []
   (flatter [[:text "xxx"] [[:tmpl-loop "foo"] [[[:tmpl-unless "hello"] [[:tmpl-var "hello"]] [:tmpl-end-unless nil]]] [:tmpl-end-loop nil]]]))

(defn parse-template [data]
  (flatter (rule-match stmts prn prn {:remainder data})))

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

