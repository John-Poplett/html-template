(ns html-template.test
  (:use [html-template])
  (:use [clojure.test]))

(deftest tmpl-noop
  (let [tmpl "<html><body><!-- this is foo --></body></html>"
        context { :hello "hello world!" }
        expected-result tmpl]
    (is (= (with-out-str (print-template tmpl context)) expected-result))))

(deftest tmpl-var-test
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

(def sample-tmpl
"<table border=1>
  <!-- TMPL_LOOP rows -->
    <tr>
      <!-- TMPL_LOOP cols -->
        <!-- TMPL_IF colorful-style -->
          <td align=\"right\" bgcolor=\"pink\"><!-- TMPL_VAR content --></td>
        <!-- TMPL_ELSE -->
          <td align=\"right\" ><!-- TMPL_VAR content --></td>
        <!-- /TMPL_IF -->
      <!-- /TMPL_LOOP -->
    </tr>
  <!-- /TMPL_LOOP -->
</table>")

(deftest sample-parse-test
  (let [result (parse-template sample-tmpl)]
    (is (>= (.indexOf ((first result) 1) "<table") 0))
    (is (= ((second result) 0) :tmpl-loop))
    (is (= ((second result) 1) :rows))
    (is (= ((last (butlast result)) 0) :tmpl-end-loop))
    (is (>= (.indexOf ((last result) 1) "</table>") 0))))


