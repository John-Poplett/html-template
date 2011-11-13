(ns html-template.example
 (:use html-template clojure.pprint))

;; <table border=1>
;;   <!-- TMPL_LOOP rows -->
;;     <tr>
;;       <!-- TMPL_LOOP cols -->
;;         <!-- TMPL_IF colorful-style -->
;;           <td align="right" bgcolor="pink"><!-- TMPL_VAR content --></td>
;;         <!-- TMPL_ELSE -->
;;           <td align="right" ><!-- TMPL_VAR content --></td>
;;         <!-- /TMPL_IF -->
;;       <!-- /TMPL_LOOP -->
;;     </tr>
;;   <!-- /TMPL_LOOP -->
;; </table>

;(declare parse-template)

(defn sample-parse []
  (parse-template
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
</table>"))

;; (let* ((rows (loop for i below 49 by 7
;;                    collect (list :cols
;;                                  (loop for j from i below (+ i 7)
;;                                        for string = (format nil "~R" j)
;;                                        collect (list :content string
;;                                                      :colorful-style (oddp j))))))
;;        (values (list :rows rows)))
;;   (fill-and-print-template #p"/tmp/foo.tmpl" values))

(defn- range-by-old [start end step]
  (reverse (reduce (fn [x y] (if (or (= 0 y) (= 0 (mod y step))) (conj x y) x)) nil (range start end))))

(defn- range-by [start end step]
  (remove #(and (> % 0) (not= 0 (mod % step))) (range start end)))

(defn- build-row [start end]
  `{ :cols ~(vec (map (fn [n] `{ :content ~(cl-format nil "~R" n) :colorful-style ~(odd? n)}) (range start end)))})

(defn build-rows [start end]
  `{ :rows ~(vec (map (fn [row-start] (build-row row-start (+ row-start 7))) (range-by start end 7)))})

(defn template-code [values]
  (println "<table border=1>")
  (doseq [rows (get values :rows)]
    (println "<tr>")
    (doseq [cols (get rows :cols)]
      (if (get cols :colorful-style)
        (do 
          (print "          <td align=\"right\" bgcolor=\"pink\">")
          (print (get cols :content))
          (println "</td>"))
        (do
          (print "          <td align=\"right\" >")
          (print (get cols :content))
          (println "</td>"))))
    (println "</tr>"))
  (println "</table>"))

(defn sample-output []
  ((compile-template (sample-parse)) (build-rows 0 49)))

