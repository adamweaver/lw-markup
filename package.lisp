(defpackage :css
  (:use :cl)
  (:export "PARSE" "INFLATE-XML-WITH-STYLES"))

(defpackage :html
  (:use :cl)
  (:export "ESC" "MAPCAT" "HTML" "XML"))

(defpackage :markdown
  (:use :cl)
  (:export "PARSE" "MAKE-HTML" "HTML"))

(defpackage :rtf
  (:use :cl))

(defpackage :xml
  (:use :cl)
  (:export "NODE" "NODE-TAG" "NODE-ATTRS" "NODE-CONTENT" "HAS-ATTR" "HAS-ATTR-P" "UPSERT-ATTR" "DELETE-ATTR"
           "XML-PARSE-ERROR" "PRINT-XML" "DECODE" "ENCODE"))
