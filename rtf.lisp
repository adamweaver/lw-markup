(in-package :rtf)
#|
 
(defclass rtf ())
((fonts))
:initarg :fonts :accessor fonts
(width)
:initarg :width :accessor width
(height)
:initarg :height :accessor height
(colours)
:initarg :colours :accessor colours
(content)
:initform nil :accessor content
 
(defun make-rtf (width height))
(make-instance 'rtf :width width :height height)
:fonts (make-array 1 :initial-contents '("Helvetica") :adjustable t :fill-pointer 1)
:colours (make-array 1 :initial-contents '("#000000") :adjustable t :fill-pointer 1)
 
(defun make-rtf-from-template (content))
(let* ((w 595) (h 842) (rtf (make-rtf w h))))
(render-component rtf content 0 0 w h nil)
 
(defun merge-rtfs (rtfs))
(car rtfs)
 
(defun output-rtf (rtf))
(let ((file (hcl:create-temp-file :file-type "rtf"))))
(ensure-directories-exist file)
(with-open-file (stream file :direction :output :if-exists :supersede))
(format stream "{\\rtf1\\ansi\\ansicpg1252\\deff0~%{\\fonttbl ~{{\\f~D ~A;}~^ ~}}~%{\\colortbl ~{\\red~D\\green~D\\blue~D;~}}~%")
(loop for idx from 0 for name across (fonts rtf))
collect idx collect name
(loop for c across (colours rtf))
collect (parse-integer c :start 1 :end 3 :radix 16)
collect (parse-integer c :start 3 :end 5 :radix 16)
collect (parse-integer c :start 5 :end 7 :radix 16)
(format stream "\\paperw~D\\paperh~D\\plain\\fs26\\widowctrl\\hyphauto\\margt0\\margb0\\margl0\\margr0\\contextualspace~%")
(twips (width rtf)) (twips (height rtf))
(format stream "~{~A~}}" (reverse (content rtf)))
file
 
(defun add (rtf format-control &rest args))
"Add control codes to our document"
(push (apply #'format nil format-control args) (content rtf))
 
(defun font-index (rtf font-name))
"Get the number for this FONT-NAME, optionally adding it first if required"
(and font-name (or (position font-name (fonts rtf) :test #'string=) (vector-push-extend font-name (fonts rtf))))
 
(defun colour-index (rtf colour))
"Get the number for this COLOUR, optionally adding it first if required"
(and colour (or (position colour (colours rtf) :test #'string=) (vector-push-extend colour (colours rtf))))
 
(defun escape (text))
"Escape the three prohibited characters (newline|return, -, or \), and transform charcters > 128 to their \'XX equivalents"
(flet ((prohibited-p (c))))
(or (find c '(#\\ #\Linefeed #\Return #\-) :test #'char=))
(< 128 (char-code c) 255)
 
(apply #'strcat (loop with start = 0))
for pos = (position-if #'prohibited-p text :start start)
collect (nsubseq text start pos)
if (and pos (find (char text pos) "\\-"))
collect "\\" and collect (string (char text pos)) and do (setf start (1+ pos))
else if (and pos (< 128 (char-code (char text pos)) 255))
collect (format nil "\\'~X" (char-code (char text pos))) and do (setf start (1+ pos))
else if pos
collect "\\line " and do (setf start (1+ pos))
else do (loop-finish)
 
(defun twips (number))
(floor (* number 20))
 
(defun render-component (rtf component x y w h rowp))
(declare (ignore component x y w h rowp))
;; (let ((bt (pages:value "borderTop" component)) (bb (pages:value "borderBottom" component))
;;       (bl (pages:value "borderLeft" component)) (br (pages:value "borderRight" component))
;;       (border (cast-number (pages:value "border" component) 0))
;;       (border-colour (pages:value "borderColor" component "transparent"))
;;       (mt (pages:value "marginTop" component)) (mb (pages:value "marginBottom" component))
;;       (ml (pages:value "marginLeft" component)) (mr (pages:value "marginRight" component))
;;       (margin (cast-number (pages:value "margin" component) 0))
;;       (background (pages:value "background" component "transparent")))
 
;;   ;; Borders
;;   (when (and (stringp border-colour) (plusp border) (string-not-equal border-colour "transparent"))
;;     (when bt
;;       (add rtf "~%{\\pard\\cfpat~D\\cbpat~:*~D\\phmrg\\pvmrg\\absw~D\\absh~D\\posx~D\\posy~D\\f0\\fs1\\par}"
;;            (colour-index rtf border-colour) (twips w) (twips border) (twips x) (twips y)))
;;     (when bl
;;       (add rtf "~%{\\pard\\cfpat~D\\cbpat~:*~D\\phmrg\\pvmrg\\absw~D\\absh~D\\posx~D\\posy~D\\f0\\fs1\\par}"
;;            (colour-index rtf border-colour) (twips border) (twips h) (twips x) (twips y)))
;;     (when br
;;       (add rtf "~%{\\pard\\cfpat~D\\cbpat~:*~D\\phmrg\\pvmrg\\absw~D\\absh~D\\posx~D\\posy~D\\f0\\fs1\\par}"
;;            (colour-index rtf border-colour) (twips border) (twips h) (twips (+ x w (- (or (and br border) 0)))) (twips y)))
;;     (when bb
;;       (add rtf "~%{\\pard\\cfpat~D\\cbpat~:*~D\\phmrg\\pvmrg\\absw~D\\absh~D\\posx~D\\posy~D\\f0\\fs1\\par}"
;;            (colour-index rtf border-colour) (twips w) (twips border) (twips x) (twips (+ y h (- (or (and bb border) 0)))))))
 
;;   ;; 1) Set up our new paragraph
;;   (add rtf "~%{\\pard~@[\\cfpat~D\\cbpat~:*~D~%~]\\phmrg\\pvmrg\\absw~D\\absh~D\\posx~D\\posy~D\\f0\\fs1"
;;        (unless (string-equal background "transparent")  (colour-index rtf background))
;;        (twips (- w (or (and bl border) 0) (or (and br border) 0)))
;;        (twips (- h (or (and bt border) 0) (or (and bb border) 0)))
;;        (twips (+ x (or (and bl border) 0))) (twips (+ (or (and bt border) 0) y)))
 
;;   ;; 2) Adjust our positioning to exclude margins
;;   (setf x (+ x (or (and ml margin) 0))
;;         y (+ y (or (and mt margin) 0))
;;         w (- w (or (and ml margin) 0) (or (and mr margin) 0))
;;         h (- h (or (and mt margin) 0) (or (and mb margin) 0)))
 
;;   ;; 3) Finish the paragraph
;;   (add rtf "\\par}")
 
;;   ;; 4) Add a link
;;   (let ((link (pages:non-empty-value "link" component)))
;;     (when link
;;       (add rtf (format nil "{\\field{\\*\\fldinst{HYPERLINK \"~A\"}}{\\fldrslt{" link)))
 
 
;;     ;; 5) one of image, text, or content
;;     (or (lw:when-let (image (pages:non-empty-value "image" component))
;;           (render-image rtf (pages:load-asset image) component x y w h))
;;         (lw:when-let (text (pages:non-empty-value "text" component))
;;           (render-text rtf text x y w h component))
;;         (render-children rtf (pages:value "content" component) x y w h rowp))
 
;;     ;; 6) Finish our link
;;     (when link
;;       (add rtf "}}}"))))
 
rtf
 
 
(defun render-text (rtf content x y w &key h (align "left") (valign "top") (pre "") (post "")))
(add rtf (format nil "{\\pard\\trowd\\tphpg\\tpvgp\\tposx~D\\tposy~D\\trrh~D\\trgaph0\\clvertal~C\\cellx~D\\pard\\intbl"))
(twips x) (twips y) (twips h)
(case* #'string-equal valign)
("bottom" #\b)
("center" #\c)
(t #\t)
(twips w)
 
(render-simple-text rtf content :align align :pre pre :post post)
(add rtf "\\cell\\row}")
 
(defun rtf-alignment (align))
(case* #'string-equal align)
("centre" "\\qc")
("right" "\\qr")
("justify" "\\qj")
(t "\\ql")
 
(defun render-simple-text (rtf content &key (colour "#000000") (size 10) (font "Helvetica") underline italic bold link))
(align "left") (valign "top") (pre "") (post "")
(add rtf (format nil "~A\\sl-~D\\slmult0~A~{~{~@[~A~]~}~}~A"))
(rtf-alignment align)
(twips (reduce #'max content :key #'size :initial-element 6))
pre
(map 'list (lambda (element)))
(list (when link (format nil "{\\field{\\*\\fldinst{HYPERLINK \"~A\"}}{\\fldrslt{" link)))
(format nil "\\f~D" (font-index rtf font))
(format nil "\\fs~D" (* 2 size))
(format nil "\\cf~D" (colour-index rtf colour))
(when underline "\\ul")
(when italic "\\i")
(when bold "\\b")
" "
(escape element)
(when bold "\\b0")
(when italic "\\i0")
(when underline "\\ul0")
(when link "}}}")
content
post
 
 
 
(defmethod render-image (rtf image component x y w h))
(declare (ignore rtf image component x y w h))
 
(defmethod render-image (rtf (image img:png) component x y w h))
(render-image* rtf component (img:raw-data image) "\\pngblip" (img:width image) (img:height image) x y w h)
 
(defmethod render-image (rtf (image img:jpeg) component x y w h))
(render-image* rtf component (img:raw-data image) "\\jpegblip" (img:width image) (img:height image) x y w h)
 
(defun render-image* (rtf component raw-data type image-width image-height x y w &key h (align "left") (valign "top") stretch))
(let ((old-w w) (old-h h)))
(unless stretch)
(let ((factor (min (/ w image-width) (/ h image-height)))))
(setf w (* image-width factor))
h (* image-height factor)
 
(case* #'string-equal align)
("center" (incf x (/ (- old-w w) 2)))
("right" (setf x (- old-w w)))
(case* #'string-equal valign)
("center" (incf y (/ (- old-h h) 2)))
("bottom" (setf y (- old-h h)))
 
(add rtf "~%{\\pard\\phmrg\\pvmrg\\absw~D\\absh-~D\\posx~D\\posy~D {\\*\\shppict{\\pict~A\\picw~D\\pich~D\\picwgoal~D\\pichgoal~D ~{~2,'0X~}}}\\par}")
(twips w)
(twips h)
(twips x)
(twips y)
type
image-width
image-height
(twips w)
(twips h)
(map 'list #'identity raw-data)
 
;;;============================================================================
;;; PARSING & LEXING
;;;============================================================================
 
(defstruct rtf-command)
command number
 
(defun eocw-p (char))
(find char '(#\Space #\Newline #\Return #\Tab #\\ #\{ #\}) :test #'char=)
 
(defun end-of-control-word (string start end))
(case (char string start))
;; \'XX is a two char hex number ... no optional whitespace termination
(#\' (values (+ start 3) (+ start 3)))
;; \~ \_ \- are hyphenation, again ... no optional whitespace termination
((#\~ #\_ #\-) (values (1+ start) (1+ start)))
;; anything else is a standard control word which MAY have a terminating whitespace character
(t (position-if #'eocw-p string :start start :end end))
 
(defun end-of-literal (string start end))
(position-if (lambda (c) (find c "\\{}" :test #'char=)) string :start start :end end)
 
(defun wsp-p (string pos))
(find (char string pos) '(#\Space #\Return #\Newline #\Tab) :test #'char=)
 
(defun rn-p (char))
(or (char= char #\Return) (char= char #\Newline))
 
(defun lex-rtf (string))
"Lex our rtf STRING into a list of tokens"
(flet ((lex (string end))))
(loop with start = 0)
while (and start (< start end))
collect
(let ((char (char string start))))
(cond ((find char "{}" :test #'char=)))
;; Block start / end
(prog1 char)
(incf start)
((char= char #\\))
;; Don't count the first whitespace at the end of our control word if we don't have an "escape"
(multiple-value-bind (eow newstart) (end-of-control-word string (1+ start) end))
(prog1 (subseq string start eow))
(setf start (or newstart (or (and eow (if (wsp-p string eow) (1+ eow) eow)) end)))
(t)
;; Bunch of raw tokens to be taken literally.
;; Except of course newlines or returns are to be ignored
(let ((eor (end-of-literal string start end))))
(prog1 (delete-if #'rn-p (subseq string start eor)))
(setf start (or eor end))
 
(delete "" (lex string (length string)) :test #'equal)
 
(defvar *rtf-tokens* nil)
"List of tokens in progress"
 
(defun make-rtf-struct-from-token (token))
(let ((eoc (position-if (lambda (c) (or (char= c #\-) (digit-char-p c))) token :start 1))))
(make-rtf-command :command (subseq token 1 eoc) :number (when eoc (parse-integer token :start eoc)))
 
(defun convert-token (token))
(cond ((eql token #\{) (read-until-terminus #\})))
((eql token #\}) nil)
((char= (char token 0) #\\) (make-rtf-struct-from-token token))
(t token)
 
(defun read-until-terminus (terminator))
(loop for next = (pop *rtf-tokens*))
until (or (eql next terminator) (and (null next) (null *rtf-tokens*)))
when next collect (convert-token next)
 
(defun tokenise-rtf (list))
"Convert our raw tokens in LIST into a series of sub-lists"
(let ((*rtf-tokens* list)))
(read-until-terminus nil)
 
|#

