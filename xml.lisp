(in-package :xml)

;;; Read XML from a file or string.
;;; if HTML-P is set, some tags like "<li>", "<p>", "<br>", "<hr>", and "<link>" are self-closing.
;;; Otherwise we expect valid XHTML / XML to be sent*.
;;;
;;; * For ease of use in the RXML dialect, quotation marks in attributes or inside strings we can use
;;;   #\{ and #\} delimiters to denote that the contents are (in rxml-mode: BASIC for interpolation,
;;;   otherwise: just a quotation mark)
;;;
;;; * Oh and #\: is a valid token in a tag name or attribute, not a namespace delimiter


(defvar *default-entities* '(("quot" . "\"") '("amp" . "&") '("apos" .  "'") '("lt" . "<") '("gt".  ">")
                             ("nbsp" . #.(string #\No-Break-Space)) ("copy" . #.(string (code-char 169)))))

(defvar *self-closing-tags*  '("!DOCTYPE"))

(defconstant +spaces+ '(#\Space #\Tab #\No-Break-Space #\Zero-Width-No-Break-Space #\Return #\Newline #\Null))

(defstruct node
  tag attrs content)

(defstruct braced
  text compiled)

(defun self-closing-p (tag)
  (find tag *self-closing-tags* :test #'string-equal))

(defun has-attr (xml coercer attr &optional default)
  (or (lw:when-let (value (cdr (assoc attr (xml:node-attrs xml) :test #'string-equal)))
        (if coercer (funcall coercer value) value))
      default))

(defun has-attr-p (xml attr)
  (assoc attr (xml:node-attrs xml) :test #'string-equal))

(defun upsert-content (xml content)
  (setf (node-content xml) content))

(defun upsert-attr (xml name value)
  (lw:if-let (attr (assoc name (xml:node-attrs xml) :test #'string-equal))
             (setf (cdr attr) value)
             (setf (xml:node-attrs xml) (acons name value (xml:node-attrs xml)))))

(defun delete-attr (xml name)
  (setf (xml:node-attrs xml) (delete name (xml:node-attrs xml) :test #'string-equal :key #'car)))

(define-condition xml-parse-error ()
  ((string :initarg :string :reader string-of)
   (position :initarg :position :reader position-of))
  (:report (lambda (condition stream)
             (let* ((string (string-of condition)) (position (position-of string)) (length (length string)))
               (format stream "XML parse error in ~S~@[...~] starting at ~D ~A" (subseq string 0 (min length 20)) (< length 20)
                       position (subseq string position (min (+ position 20) length)))))))

(defvar %indent% 0)
(defmethod print-xml ((xml node) out)
  (format out "~&~v,0T<~A~@[ [~{~A=~A~^; ~}]~]>" %indent% (node-tag xml) (loop for (k . v) in (node-attrs xml) collect k collect v))
  (when (and (node-content xml) (not (equal "" (node-content xml))))
    (let ((%indent% (+ %indent% 2)))
      (print-xml (node-content xml) out)))
  (format out "~&~v,0T</~A>" %indent% (node-tag xml)))

(defmethod print-xml ((list cons) out)
  (loop for c in list do (print-xml c out)))

(defmethod print-xml ((string string) out)
  (format out "~&~v,0T~A" %indent% string))

(defparameter *prologue* nil)
(defparameter *entities* *default-entities*)

;;;============================================================================
;;; PUBLIC INTERFACE
;;;============================================================================

(defun decode (string)
  (let ((result (decode* string)))
    (if (and (node-p result) (string-equal (node-tag result) "?xml"))
        (node-content result)
        result)))

(defun decode* (string)
  "Convert our STRING into an XML:NODE and children"
  (if (pathnamep string)
      (decode* (hcl:file-string string :external-format '(:utf-8 :eol-style :lf)))
      (let (*prologue* (*entities* *default-entities*))
        (values (parse (lex string)) *prologue* *entities*))))

(defun encode-entities (string)
  (let ((entities (mapcar (lambda (s) (char (cdr s) 0)) *default-entities*)))
    (apply #'strcat (loop for p = 0 then (when i (1+ i)) for i = (position-if (lambda (c) (find c entities :test #'char=)) string :start p)
                          collect (subseq string p i)
                          while i collect "&"
                          collect (car (rassoc (subseq string i (1+ i)) *default-entities* :test #'string=))
                          collect ";"))))

(defun encode (root)
  "Convert our ROOT node back into an XML string"
  (flet ((attribute (cons) (list (car cons) (encode-entities (cdr cons)))))
    (cond
      ((stringp root) (encode-entities root))
      ((consp root) (format nil "~{~A~}" (mapcar #'encode root)))
      ((null (node-content root)) (format nil "<~A~@[ ~{~{~A=~S~}~^ ~}~] />" (node-tag root) (mapcar #'attribute (node-attrs root))))
      (t (format nil "<~A~@[ ~{~{~A=~S~}~^ ~}~]>~{~A~}</~A>"
                 (node-tag root) (mapcar #'attribute (node-attrs root)) (mapcar #'encode (node-content root)) (node-tag root))))))

(defun xml-p (e)
  (node-p e))

(defun attrib (node key)
  (dict:dictionary key (node-attrs node)))

(defun value (node)
  (node-content node))

(defun child (node key)
  (lw:when-let (child (find key (node-content node) :test #'string= :key #'node-tag))
    (if (cdr (node-content child))
        (node-content child)
        (car (node-content child)))))

;;;============================================================================
;;; PARSING
;;; We should only have #\< #\= #\/ #\>, :END and strings in our stream.
;;; :END is equivalent to #\< #\/ - avoids parser complaints by making it one token
;;; VAR("hx-get") is necessary to stop "hx-get" being parsed as "hx - get" i.e. 0
;;;============================================================================

(parsergen:defparser xml-parser ((<toplevel> <content*>) $1)
  (<attr*>
   ((:string #\= <basic> <attr*>) (cons (cons $1 $3) $4))
   ((:string <attr*>) (cons (maybe-brace $1) $2))
   ((:array <attr*>) (cons (cons (aref $1 0) (make-braced :text (format nil "VAR(~S)" (aref $1 0)))) $2))
   (() nil))
  (<basic>
   ((:string) $1)
   ((:array) (make-braced :text (aref $1 0))))
  (<tag>
   ((#\< :string <attr*> #\/ #\>) (make-node :tag $2 :attrs $3 :content nil))
   ((#\< :string <attr*> #\> <content*> :end :string #\>)
    (if (string-equal $2 $7)
        (make-node :tag $2 :attrs $3 :content $5)
        (error "Unbalanced XML tag <~A ~A> matched with </~A>" $2 $3 $5))))
  (<content*>
   ((<tag> <content*>) (cons $1 $2))
   ((<basic> <content*>) (cons $1 $2))
   (() nil)))

(defun parse (tokens)
  (xml-parser (lambda () (values (typecase (car tokens) (string :string) (array :array) (t (car tokens))) (pop tokens)))))

(defun maybe-brace (key)
  (cond ((or (starts-with key "class:") (starts-with key "style:"))
         (cons (if (char= (char key 6) #\@) (concatenate 'string (subseq key 0 6) (subseq key 7)) key) (make-braced :text (subseq key 6))))
        ((char= (char key 0) #\@)
         (cons (subseq key 1) (make-braced :text key)))
        (t (cons key :single))))

;;;============================================================================
;;; LEXING
;;;============================================================================

(defun whitespace-p (char)
  (find char #(#\Zero-Width-No-Break-Space #\No-Break-Space #\Space #\Tab #\Newline #\Return) :test #'char=))

(defun delimiter-p (char)
  (or (whitespace-p char) (find char #(#\/ #\> #\= #\<) :test #'char=)))

(defun trim (string start end)
  (lw:when-let (start (position-if-not #'whitespace-p string :start start :end end))
    (subseq string start (1+ (position-if-not #'whitespace-p string :start start :end end :from-end t)))))

(defun lex (string)
  (loop with end = (length string)
        for i below end
        for char = (char string i)
        if (whitespace-p char)
          do (progn)
        else
          if (string-equal "<!CDATA[[" string :start2 i :end2 (min (+ i 9) end))
            collect (let ((eoc (search "]]>" string :start2 (+ i 9) :test #'char=)))
                      (prog1 (subseq string (+ i 9) eoc)
                        (setf i (if eoc (+ eoc 2) end))))
        else
          if (string-equal "<Script>" string :start2 i :end2 (min (+ i 8) end))
            nconc (let ((eoc (search "</Script>" string :start2 (+ i 8) :test #'char-equal)))
                    (prog1 (list #\< "Script" #\> (subseq string (+ i 8) eoc) :end "Script" #\>)
                      (setf i (if eoc (+ eoc 8)))))
        else
          if (string-equal "<style>" string :start2 i :end2 (min (+ i 7) end))
            nconc (let ((eoc (search "</style>" string :start2 (+ i 7) :test #'char-equal)))
                    (prog1 (list #\< "Style" #\> (subseq string (+ i 7) eoc) :end "Style" #\>)
                      (setf i (if eoc (+ eoc 7)))))
        else
          if (and (char= char #\<) (< (1+ i) end) (char= (char string (1+ i)) #\/))
            collect :end and do (incf i)
        else
          if (or (char= char #\<) (char= char #\=) (char= char #\/))
            collect char
        else
          if (char= char #\")
            collect (let ((eoq (position #\" string :start (1+ i) :test #'char=)))
                      (prog1 (subseq string (1+ i) eoq)
                        (setf i (or eoq end))))
        else
          if (char= char #\{)
            collect (let ((eob (position #\} string :start (1+ i) :test #'char=)))
                      (prog1 (vector (subseq string (1+ i) eob))
                        (setf i (or eob end))))
        else
          if (char= char #\>)
            nconc (let ((sos (position #\< string :start i :test #'char=)))
                    (cons #\> (when sos
                                (prog1 (decode-braced-delimiters (trim string (1+ i) sos))
                                  (setf i (1- sos))))))
        else
          nconc (let ((eow (position-if #'delimiter-p string :start i)))
                  (prog1 (unless (eql i eow) (list (subseq string i eow)))
                    (setf i (1- (or eow end)))))))

(defun decode-entities (string)
  (flet ((replace-entity (string start end match-start match-end reg-starts reg-ends)
           (declare (ignore start end match-start match-end))
           (let ((value (subseq string (aref reg-starts 0) (aref reg-ends 0))))
             (or (cdr (assoc value *entities* :test #'string-equal))
                 (let ((number (ignore-errors (parse-integer value :radix 16))))
                   (and number (string (code-char number))))))))
    (when string
      (decode-braced-delimiters (cl-ppcre:regex-replace-all "&(?:[\\w\\d]+);" string #'replace-entity)))))

(defun decode-braced-delimiters (string)
  (loop with start = 0
        with end = (length string)
        for pos = (when (< start end) (position #\{ string :start start :test #'char=))
        if (null pos)
          nconc (unless (= start end) (list (subseq string start))) and do (loop-finish)
        else
          if (/= start pos)
            collect (subseq string start pos)
        collect (let ((eow (position #\} string :start (1+ pos) :test #'char=)))
                  (prog1 (vector (subseq string (1+ pos) eow))
                    (setf start (1+ (or eow end)))))))
