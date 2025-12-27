(defpackage :css
  (:use :cl :utils)
  (:export "PARSE" "INFLATE-XML-WITH-STYLES"))

(defpackage :html
  (:use :cl)
  (:export "ESC" "MAPCAT" "HTML" "XML"))

(defpackage :markdown
  (:use :cl :utils)
  (:export "PARSE" "MAKE-HTML" "HTML"))

(defpackage :rtf
  (:use :cl))

(defpackage :xml
  (:use :cl :utils)
  (:export
   "DECODE"
   "DELETE-ATTR"
   "ENCODE"
   "HAS-ATTR"
   "HAS-ATTR-P"
   "NODE"
   "NODE-ATTRS"
   "NODE-CONTENT"
   "NODE-TAG"
   "PRINT-XML"
   "UPSERT-ATTR"
   "XML-PARSE-ERROR"))
