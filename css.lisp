(in-package :css)

;;;============================================================================
;;; DEFAULT RULES

(defstruct rule
  tag classes id attrs parent ancestors sibling subsequent-siblings (specificity 0) styles)

(defparameter *rules* nil)
(defparameter *root* '(("font-family" . "Helvetica") ("line-height" . 1) ("font-size" . 12) ("color" . "#000000")))

(defun fixate (value base)
  "Apply VALUE against BASE to determine an actual number in points.
110% of 12pt == 13.12pt.
650pt of 12pth == 650pt"
  (if (numberp value)
      (* value base)
      (destructuring-bind (&optional number format) (#~m/^(\d+)(\D*)$/ value)
        (let ((value (or (ignore-errors (hcl:parse-float number)) 0)))
          (case* #'string-equal (or format "pt")
                 ("pt" value)   ;  a point is 1/72 of an inch
                 ("pc" (* value 6/72))          ; a pica is 1/6 of an inch
                 ("%" (* value (/ base 100)))
                 (("vmin" "vw") (* value 595/100))
                 (("vmax" "vh") (* value 842/100))
                 ;; ("em" (* value (pdf:line-height (value "font-family")) (value "font-size")))
                 ;; ("rem" (* value (pdf:line-height (cdr (assoc "font-family" *root* :test #'string-equal))
                 ;;                                  (cdr (assoc "font-size" *root* :test #'string-equal)))))
                 ("px" (* value 72/96))         ; https://www.w3.org/Style/Examples/007/units.en.html - 1px = 1/96 of an inch
                 ("cm" (* value 1/72 254/100))
                 ("mm" (* value 1/72 254/1000))
                 ("in" (* value 1/72))
                 (t value))))))                 ; points

;;;============================================================================
;;; INFLATE XML
;;;============================================================================

(defparameter *left* nil)
(defparameter *right* nil)
(defparameter *ancestry* nil)

(defun inflate-xml-with-styles (xml)  
  (labels ((%inflate (x)
             "Add our computed CSS to xml node X under attr 'css'"
             (unless (or (null x) (stringp x))
               (let* ((*ancestry* (cons x *ancestry*))
                      (rules (sort (remove-if-not #'%match-p *rules*) #'> :key #'rule-specificity))
                      (kids (xml:node-content x))
                      (style (xml:has-attr x 'db::string "style"))
                      (styles (when style (parse style)))
                      (css (apply #'append styles (mapcar #'rule-styles rules))))
                 (xml:upsert-attr x "css" css)
                 (when kids
                   (let ((*left* nil) (*right* (cdr kids)))
                     (%inflate (car kids))))
                 (loop for k on kids
                       do (let ((*left* (car k)) (*right* (cddr k)))
                            (%inflate (cadr k)))))))
           (%match-p (r)
             "Does rule R apply to *ancestry*?"
             (lw:when-let (me (car *ancestry*))
               (every (lambda (f) (funcall f r me)) (list #'%tag-p #'%classes-p #'%attrs-p #'%parent-p #'%ancestors-p #'%sibling-p #'%sub-p))))

           (%tag-p (r x)
             (lw:if-let (tag (rule-tag r))
                        (or (string= (rule-tag r) "*") (string-equal (rule-tag r) (xml:node-tag x)))
                        t))

           (%classes-p (r x)
             (let ((my-classes (xml:has-attr x 'db::string "class")))
               (or (null (rule-classes r))
                   (null (set-difference (rule-classes r) (lw:split-sequence " " my-classes :test #'char=) :test #'string=)))))

           (%attrs-p (r x)
             (declare (ignore r x))
             t)

           (%parent-p (r x)
             (declare (ignore x))
             (lw:if-let (pater (rule-parent r))
                        (and (cadr *ancestry*) (let ((*ancestry* (cdr *ancestry*))) (%match-p pater)))
                        t))

           (%ancestors-p (r x)
             (declare (ignore x))
             (lw:if-let (grampa (rule-ancestors r))
                        (loop for parentage on (cdr *ancestry*) thereis (let ((*ancestry* parentage)) (%match-p grampa)))
                        t))

           (%sibling-p (r x)
             (declare (ignore x))
             (lw:if-let (sib (rule-sibling r))
                        (when *left*
                          (let ((*ancestry* (cons *left* (cdr *ancestry*)))) (%match-p r)))
                        t))

           (%sub-p (r x)
             (declare (ignore x))
             (lw:if-let (sub (rule-subsequent-siblings r))
                        (when *right*
                          (let ((*ancestry* *right*)) (%match-p r)))
                        t)))

    (let ((*rules* (list (make-rule :tag "*" :styles *root*))) (*left* nil) (*right* nil))
      (%inflate xml))))

;;;============================================================================
;;; PARSING
;;;============================================================================

(defun parse (file)
  (if (pathnamep file)
      (parse (hcl:file-string file :external-format '(:utf-8 :eol-style :lf)))
      (build-ruleset (lex file))))

(defun lex (string)
  (loop with end = (length string)
        with delims = #.(format nil "~~>+&{}:;,() ~C~C~C" #\Tab #\Return #\Linefeed)
        for start below end
        for char = (char string start)          
        nconc (cond ((find char '(#\Space #\Tab #\Return #\Linefeed) :test #'char=) nil)
                    ((find char "{}:;()," :test #'char=) (list char))
                    ((find char "~>+&" :test #'char=) (list (string char)))
                    (t (let ((eow (position-if (lambda (c) (find c delims :test #'char=)) string :start start)))
                         (prog1 (list (unquote (subseq string start eow)))
                           (setf start (if eow (1- eow) end))))))))

(defun unquote (string)
  (if (and (plusp (length string)) (char= (char string 0) #\"))
      (string-trim #(#\") string)
      string))

(parsergen:defparser css-parser ((<toplevel> <rules>))
  (<selectors>
   ((<selector> #\, <selectors>) (cons $1 $3))
   ((<selector>) (cons $1 nil)))
  (<selector>
   ((:string <selector>) (cons $1 $2))
   ((:string) (cons $1 nil)))
  (<attributes>
   ((:string #\: <values> #\; <attributes>) (cons (cons $1 $3) $5))
   ((:string #\: <values> #\;) (cons (cons $1 $3) nil)))
  (<value>
   ((:hsl #\( :string #\, :string #\, :string #\)) (hsl-to-rgb (fixate $3 1) (fixate $5 100) (fixate $7 100)))
   ((:rgb #\( :string #\, :string #\, :string #\)) (rgb-to-rgb $3 $5 $7))
   ((:weight) (+ 2 (* 2 (position $1 '("thin" "medium" "thick") :test #'string-equal))))
   ((:border) (if (string-equal $1 "none") "none" "solid"))
   ((:string) $1))
  (<values>
   ((<value> <values>) (cons $1 $2))
   ((<value>) (cons $1 nil)))
  (<rule>
   ((<selectors> #\{ <attributes> #\}) (let ((attrs (make-styles $3))) (loop for s in $1 collect (make-rule* (nreverse s) attrs)))))
  (<rules>
   ((<rule> <rules>) (cons $1 $2))
   ((<rule>) (cons $1 nil))
   ((<attributes>) (cons $1 nil)))) ;; Used for inline styles only

(defun make-rule* (selectors styles)
  "Parse SELECTORS list into a tree of RULEs"
  (labels ((delimiter-p (char)
             (or (char= char #\#) (char= char #\.) (char= char #\[)))
           
           (classify (string)
             (let ((tag "") id classes attributes)
               (loop with length = (length string)
                     for i below length
                     for c = (char string i)
                     do (case c
                          (#\# (let ((eow (position-if #'delimiter-p string :start (1+ i))))
                                 (setf id (subseq string (1+ i) eow) i (if eow (1- i) length))))
                          (#\. (let ((eow (position-if #'delimiter-p string :start (1+ i))))
                                 (setf classes (cons (subseq string (1+ i) eow) classes) i (if eow (1- i) length))))
                          (#\[ (let ((eow (position #\] string :start (1+ i))))
                                 (setf attributes (cons (subseq string (1+ i) eow) attributes) i (if eow (1- i) length))))
                          (t (setf tag (concatenate 'string tag (string c))))))
               (list (unless (string= tag "") tag) id classes attributes)))

           (specificity (rule)
             (+ (if (rule-id rule) 100 0)
                (if (rule-attrs rule) 10 0)
                (* 10 (length (rule-classes rule)))
                (if (rule-tag rule) 1 0)
                (if (rule-parent rule) (specificity (rule-parent rule)) 0)
                (if (rule-ancestors rule) (specificity (rule-ancestors rule)) 0))))

    
    (destructuring-bind (tag id classes attributes) (classify (car selectors))
      (let ((parent (when (and (cadr selectors) (string= (cadr selectors) ">")) (make-rule* (cddr selectors) nil)))
            (sibling (when (and (cadr selectors) (string= (cadr selectors) "+")) (make-rule* (cddr selectors) nil)))
            (subsequent (when (and (cadr selectors) (string= (cadr selectors) "~")) (make-rule* (cddr selectors) nil)))
            (ancestors (when (and (cadr selectors) (not (find (cadr selectors) '(">" "+" "~") :test #'string=)))
                         (make-rule* (cdr selectors) nil))))
        (let ((rule (make-rule :tag tag
                               :classes classes
                               :id id
                               :styles styles
                               :attrs attributes
                               :parent parent
                               :sibling sibling
                               :subsequent-siblings subsequent
                               :ancestors ancestors)))
          (setf (rule-specificity rule) (specificity rule))
          rule)))))

(defun make-styles (alist)
  "Update shortcut styles - margin-*, border-*, padding-* for all four sides"
  (let (styles)
    (labels ((has-style-p (style)
               (assoc style styles :test #'string-equal))

             (maybe-update-sides (root values)
               (loop for side in '("top" "right" "bottom" "left")
                     for value in values
                     for attr = (format nil "~A-~A" root side)
                     unless (has-style-p attr) do (setf styles (acons attr value styles))))

             (update-single (attr value)
               (setf styles (acons attr value styles)))

             (update-border (prefix alist)
               (loop for (subkey . value) in alist
                     for key = (format nil "border-~A~A" prefix subkey)
                     do (setf styles (acons key value styles))))

             (maybe-update-borders (alist)
               (loop for side in '("top" "right" "bottom" "left")
                     do (loop for (tag . value) in alist
                              for key = (format nil "border-~A~A" side tag)
                              unless (has-style-p key)
                                do (setf styles (acons key value styles)))))

             (get-box-values (e)
               (case (length e)
                 (0 (list 0 0 0 0))
                 (1 (list (car e) (car e) (car e) (car e)))
                 (2 (list (car e) (cadr e) (car e) (cadr e)))
                 (3 (list (car e) (cadr e) (caddr e) (cadr e)))
                 (t e)))

             (get-border-values (args)
               (let ((width "1pt") (style "solid") (color "#000000"))
                 (loop for a in args
                       if (or (numberp a) (#~m/^\d+\w*$/ a)) do (setf width a)
                         else if (char= (char a 0) #\#) do (setf color a))
                 (list (cons "-width" width) (cons "-style" style) (cons "-color" color)))))

      (loop for (attr . args) in alist
            do (case* #'string-equal attr
                      ;; Do these unless we have a more specific attr set
                      ("border" (maybe-update-borders (get-border-values args)))
                      ("padding" (maybe-update-sides "padding" (get-box-values args)))
                      ("margin" (maybe-update-sides "margin" (get-box-values args)))

                      ;; Always do these though
                      ("border-top" (update-border "top" (get-border-values args)))
                      ("border-right" (update-border "right" (get-border-values args)))
                      ("border-bottom" (update-border "border" (get-border-values args)))
                      ("border-left" (update-border "left" (get-border-values args)))
                      ("margin-top" (update-single "margin-top" (car args)))
                      ("margin-right" (update-single "margin-right" (car args)))
                      ("margin-bottom" (update-single "margin-bottom" (car args)))
                      ("margin-left" (update-single "margin-left" (car args)))
                      ("padding-top" (update-single "padding-top" (car args)))
                      ("padding-right" (update-single "padding-right" (car args)))
                      ("padding-bottom" (update-single "padding-bottom" (car args)))
                      ("padding-left" (update-single "padding-left" (car args)))
                      (t (setf styles (acons attr (car args) styles)))))
      styles)))

(defparameter *css-tokens* nil)
(defun build-ruleset (list)
  (let ((*css-tokens* list)
        (border-styles '("none" "hidden" "dotted" "dashed" "solid" "double" "groove" "ridge" "inset" "outset"))
        (border-weights '("thin" "medium" "thick")))

    (flatten (css-parser (lambda () (let ((token (pop *css-tokens*)))
                                      (values (cond ((null token) :eoi)
                                                    ((characterp token) token)
                                                    ((string-equal token "hsl") :hsl)
                                                    ((string-equal token "rgb") :rgb)
                                                    ((find token border-styles :test #'string-equal) :border)
                                                    ((find token border-weights :test #'string-equal) :weight)
                                                    (t :string))
                                              token)))))))

;;;============================================================================
;;; Colour conversion
;;;============================================================================

(defun hsl-to-rgb (hue saturation lightness)
  ;; https://stackoverflow.com/questions/2353211/hsl-to-rgb-color-conversion
  (flet ((hue-to-rgb (p q tee)
           (setf tee (nth-value 1 (truncate (abs tee))))
           (cond ((< tee 1/6) (round (* 255 (+ p (* 6 tee (- q p))))))
                 ((< tee 1/2) (round (* 255 q)))
                 ((< tee 2/3) (round (* 255 (+ p (* 6 (- q p) (- 2/3 tee))))))
                 (t (round (* 255 p))))))
    (if (zerop saturation)
        "#ffffff"
        (let* ((hue (/ hue 360)) (saturation (/ saturation 100)) (lightness (/ lightness 100))
               (q (if (< lightness 1/2) (* lightness (1+ saturation)) (- (+ lightness saturation) (* lightness saturation))))
               (p (- (* 2 lightness) q)))
          (format nil "#~2,'0X~2,'0X~2,'0X" (hue-to-rgb p q (+ hue 1/3)) (hue-to-rgb p q hue) (hue-to-rgb p q (- hue 1/3)))))))

(defun rgb-to-rgb (r g b)
  (format nil "~{~2,'0X~}" (mapcar (lambda (i) (or (ignore-errors (conv:integer! i)) 0)) (list r g b))))
