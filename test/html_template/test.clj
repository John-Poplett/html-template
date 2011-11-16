(ns html-template.test
  (:use [html-template])
  (:use [clojure.test]))

(defn w []
  (let [tmpl "<html><head><!-- this is foo --></head><body><!-- TMPL_VAR hello --></body></html>"]
    (parse-template tmpl)))

(defn v []
  (let [tmpl "<html><head><!-- this is foo --></head><body>
<!-- TMPL_LOOP foo -->
<!-- TMPL_IF hello -->
<p><!-- TMPL_VAR hello --></p>
<!-- TMPL_ELSE -->
<p>hello world!</p>
<!-- /TMPL_IF -->
<!-- /TMPL_LOOP -->
</body></html>"]
    (parse-template tmpl)))

(defn vv []
  (compile-template (v) {:eval false}))

(deftest tmpl-noop
  (let [tmpl "<html><body><!-- this is foo --></body></html>"
        context { :hello "hello world!" }
        expected-result tmpl]
    (is (= (with-out-str (print-template tmpl context)) expected-result))))

(deftest tmpl-var
  (let [tmpl "<html><body><!-- TMPL_VAR hello --></body></html>"
        context { :hello "hello world!" }
        expected-result "<html><body>hello world!</body></html>"]
    (is (= (with-out-str (print-template tmpl context)) expected-result))))

(deftest tmpl-var-mixed-case
  (let [tmpl "<html><body><!-- Tmpl_Var hello --></body></html>"
        context { :hello "hello world!" }
        expected-result "<html><body>hello world!</body></html>"]
    (is (= (with-out-str (print-template tmpl context)) expected-result))))

(deftest tmpl-loop
  (let [tmpl "<html><body><!-- TMPL_LOOP foo --><!-- TMPL_VAR hello --><!-- /TMPL_LOOP --></body></html>"
        context { :foo [ { :hello "hello " } { :hello "world!" }]}
        expected-result "<html><body>hello world!</body></html>"]
    (is (= (with-out-str (print-template tmpl context)) expected-result))))

(deftest tmpl-if
  (let [tmpl "<html><body><!-- TMPL_IF foo --><!-- TMPL_VAR foo --><!-- TMPL_ELSE -->hi, shirley!<!-- /TMPL_IF --></body></html>"
        test-pairs [[ { :foo "hello world!" } "<html><body>hello world!</body></html>"]
                    [ {} "<html><body>hi, shirley!</body></html>"]]]
    (doseq [pair test-pairs]
      (is (= (with-out-str (print-template tmpl (first pair))) (second pair))))))

(deftest tmpl-unless
  (let [tmpl "<html><body><!-- TMPL_UNLESS foo -->hi, shirley!<!-- TMPL_ELSE --><!-- TMPL_VAR foo --><!-- /TMPL_UNLESS --></body></html>"
        test-pairs [[ { :foo "hello world!" } "<html><body>hello world!</body></html>"]
                    [ {} "<html><body>hi, shirley!</body></html>"]]]
    (doseq [pair test-pairs]
      (is (= (with-out-str (print-template tmpl (first pair))) (second pair))))))

