;;;; plotly.lisp

(in-package #:plotly-cl)

(defparameter *windows-browser* "firefox")

(cl-interpol:enable-interpol-syntax)

(defun generate-plot (plot-code width height)
  (let ((style (cl-css:css `((html :height 100%)
                             (body :height 100%
                                   :display flex
                                   :justify-content center
                                   :align-items center)
                             ("#plot" :width ,#?"${width}px"
                                      :height ,#?"${height}px")))))
    (who:with-html-output-to-string (_)
      (:html
       (:head
        (:script :src "https://cdn.plot.ly/plotly-latest.min.js")
        (:style (who:str style)))
       (:body
        (:div :id "plot")
        (:script (who:str plot-code)))))))

(defun open-plot (plot-code width height)
  "Write output to the file and open browser"
  (uiop/stream:with-temporary-file (:pathname pn :stream stream :direction :output :keep t :type "html")
    (write-string (generate-plot plot-code width height) stream)
    (if (uiop:os-windows-p)
        (sb-ext:run-program *windows-browser* (list (cl-ppcre:regex-replace-all "/" (namestring pn) "\\")) :wait nil :search t)
        ;; Both below works by themselves but error code means that graph is not opened when wrapped in open-plot
                                        ;(uiop:run-program  (cl-ppcre:regex-replace-all "/" (namestring pn) "\\") :ignore-error-status t)
                                        ;(uiop:run-program (namestring pn) :ignore-error-status t) ; https://bugs.launchpad.net/asdf/+bug/1470519
        (sb-ext:run-program (or (uiop:getenv "BROWSER") "xdg-open") (list (namestring pn)) :wait nil :search t))))

(defun pl-plot (traces &key layout (width 1000) (height 700))
  "Plot the data (list of traces)"
  (let* ((json-traces (format nil "[~{~a~^,~}]" (mapcar #'json:encode-json-alist-to-string traces)))
         (json-layout (json:encode-json-alist-to-string layout))
         (plot-code (ps:ps
                      (let ((div ((ps:@ document get-element-by-id) "plot")))
                        (*plotly.plot div ((ps:@ *json* parse) (ps:lisp json-traces))
                                      ((ps:@ *json* parse) (ps:lisp json-layout)))))))
    (open-plot plot-code width height)))
