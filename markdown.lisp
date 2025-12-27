(in-package :markdown)

(defvar *indented-code-p* t
  "Will \s{4,} blah be a code block?")

(defvar *inline-links-p* t
  "Will <adam@appsuite.com.au> be a link?")

(defvar *source* nil
  "Lines of our text split by newlines to be interpreted")

(defstruct span
  type link content)

(defun span (type &key link content)
  (make-span :type type :link link :content content))

(defun indented-p (line num)
  (or (and (> (length line) num) (every #'whitespace-p (nsubseq line 0 num)))
      (and (plusp (length line)) (char= (char line 0) #\Tab))))

(defun ltrim (leading-char line)
  (let ((string (if (char= (char line 0) leading-char) (subseq line 1) line)))
    (string-left-trim #(\Space #\Tab) string)))

(defun trim (line)
  (string-trim #(#\Space #\Tab) line))

(defun chomp (line)
  "Remove trailing \r and convert /^\s+$/ to ''"
  (if (every #'whitespace-p line)
      ""
      (string-right-trim #(#\Return) line)))

(defun whitespace-p (char)
  (or (char= char #\Space) (char= char #\Tab)))

(defmacro omit (&body body)
  `(prog1
       (progn ,@body)
     (when (and (car *source*) (every #'whitespace-p (car *source*)))
       (pop *source*))))

(defun parse (source &key (indented-code t) (inline-links t))
  (let ((*indented-code-p* indented-code) (*inline-links-p* inline-links))
    (if (pathnamep source)
        (parse (hcl:file-string source :external-format '(:utf-8 :eol-style :lf)) :indented-code indented-code :inline-links inline-links)
        (let ((*source* (mapcar #'chomp (lw:split-sequence '(#\Linefeed #\Return) source :coalesce-separators t))))
          (parse*)))))

(defun parse* ()
  (merge-strings (loop for line = (pop *source*)
                       for mdown = (when line (make-markdown line))
                       if (and mdown (consp mdown)) append mdown
                         else if mdown collect mdown
                                else do (loop-finish))))

(defun dedupe-whitespace (a b)
  (concatenate 'string (string-right-trim #(#\Space #\Tab) a) " " (string-left-trim #(#\Space #\Tab) b)))

(defun merge-strings (s)
  (cond ((and (stringp (car s)) (stringp (cadr s))) (merge-strings (cons (dedupe-whitespace (car s) (cadr s)) (cddr s))))
        ((cadr s) (cons (car s) (merge-strings (cdr s))))
        (t s)))

(defun make-markdown (line)
  (or (make-heading line)
      (make-blockquote line)
      (make-ordered-list line)
      (make-unordered-list line)
      (make-table line)
      (make-horizontal-rule line)
      (make-code-block line)
      (make-text line)
      (span :br :content "")))

(defun next-line-type (line)
  (cond ((string= line "") :empty)
        ((#~t/^#+/ line) :h1)
        ((#~t/^>/ line) :bq)
        ((#~t/^\d\.\s+/ line) :ol)
        ((#~t/^[-+*]\s+/ line) :ul)
        ((#~t/^[-_*]{3,}$/ line) :hr)
        ((#~t/^```\s*$/ line) :codeblock)
        ((and *indented-code-p* (indented-p line 4)) :codeblock)
        ((#~t/^!\[.*]\(.*\)/ line) :img)
        ((whitespace-p (char line 0)) :text)
        (t nil)))

(defun find-runs-of-type (type)
  (loop for line = (car *source*)
        for next = (when line (next-line-type line))
        while *source*
        if (null next)
          collect (strcat " " (pop *source*))
        else if (eq next type)
               collect (pop *source*)
        else do (loop-finish)))

;;;============================================================================
;;; Heading is either:
;;;
;;;  #{1,6} TEXT
;;;
;;; or
;;;
;;; TEXT
;;; ==== or -----


(defun make-heading (line)
  (let ((line (trim line)) (next (if (car *source*) (trim (car *source*)) "")))
    (destructuring-bind (&optional hash heading) (#~m/^(#+)\s*(\S.*)$/ line)
      (cond (hash (omit (span (aref #(() :h1 :h2 :h3 :h4 :h5 :h6) (max 1 (min (length hash) 6))) :content heading)))
            ((and (string/= next "") (every (lambda (c) (char= c #\=)) next)) (omit (pop *source*) (span :h1 :content line)))
            ((and (string/= next "") (every (lambda (c) (char= c #\-)) next)) (omit (pop *source*) (span :h2 :content line)))
            (t nil)))))

;;;============================================================================
;;; Blockquote
;;;
;;; > Blockquote

(defun make-blockquote (line)
  (when (and (plusp (length line)) (char= (char line 0) #\>))
    (let ((*source* (mapcar (lambda (line) (ltrim #\> line)) (cons line (find-runs-of-type :bq)))))
      (omit (span :bq :content (parse*))))))

;;;============================================================================
;;; Ordered Lists
;;; 1. First Item
;;; 1. Second Item
;;;    Continuation of second item
;;; 2. Third Item

(defun ordered-list-p (line)
  (#~t/^\d\.\s+/ line))

(defun make-ordered-list (line)
  (when (ordered-list-p line)
    (omit (push line *source*) (span :ol :content (parse-list #'ordered-list-p)))))

(defun parse-list (test)
  (loop for line = (car *source*)
        for item-p = (when line (funcall test line))
        while item-p
        collect (span :li :content (parse-list-continuation (pop *source*)))))

(defun parse-list-continuation (line)
  "List item continuation is a preceding 4 spaces or 1 tab"
  (let ((*source* (cons (string-left-trim #(#\Space #\Tab) (subseq line 2))
                        (loop for next = (car *source*)
                              for continuation = (and (indented-p next 1) (string-left-trim #(#\Space #\Tab) next))
                              while continuation
                              collect continuation
                              do (pop *source*)))))
    (parse*)))

;;;============================================================================
;;; Unordered Lists
;;; - First Item
;;; + Second Item
;;;   Continuation of second item
;;; * Third Item

(defun unordered-list-p (line)
  (#~t/^[-+*]\s+/ line))

(defun make-unordered-list (line)
  (when (unordered-list-p line)
    (omit (push line *source*) (span :ul :content (parse-list #'unordered-list-p)))))

;;;============================================================================
;;; Horizontal Rule
;;; ***
;;; ---
;;; ___

(defun make-horizontal-rule (line)
  (when (#~t/^[*_-]{3,}\s*$/ line)
    (omit (span :hr))))

;;;============================================================================
;;; Table
;;; | one | two | three | four |
;;; | --- | --: | :--: | :-- |
;;; | left aligned | right aligned | centre aligned | left aligned |

(defun table-p (line)
  (#~t/\s*\|.*\|\s*$/ line))

(defun make-table (line)
  (when (table-p line)
    (omit (push line *source*) (span :table :content (loop for next in *source* while (and next (table-p next))
                                                           collect (split-table-cells (pop *source*)))))))

(defun split-table-cells (line)
  (map 'vector #'make-heading-or-text (nbutlast (cdr (lw:split-sequence "|" line :test #'char=)))))

(defun make-heading-or-text (text)
  (let ((text (trim text)))
    (or (make-heading text) (make-text text))))

;;;============================================================================
;;;; Code block
;;;; ```
;;;; code goes here
;;;; ```
;;;;
;;;; or
;;;;     indented-by-four-spaces-or-by-one-tab
;;;;     so-is this

(defun make-code-block (line)
  (cond ((and *indented-code-p* (indented-p line 4)) (span "code" :content (get-code-block-lines line)))
        ((#~t/^```/ line) (span :codeblock :content (get-explicit-code-block-lines)))
        (t nil)))

(defun get-code-block-lines (line)
  (format nil "~{~A~^~%~}" (omit (cons (trim line)
                                       (loop for n = (car *source*) while (and n (indented-p n 4))
                                             collect (trim (pop *source*)))))))

(defun get-explicit-code-block-lines ()
  (format nil "~{~A~^~%~}" (omit (loop for str = (pop *source*) until (or (null str) (#~t/^```/ str)) collect str))))

;;;============================================================================
;;; Plain text
;;;
;;; Italic => _smith_ or *smith*
;;; Bold => __smith__ or *smith*
;;; Italic + bold => ___smith___ or ***smith**** or **_smith_** or __*smith*__
;;; Link [Duck Duck Go](https://duckduckgo.com) or <https://duckduckgo.com>
;;; Image ![Picture](link to picture)
;;; Code => `word`

(defun ensure-text-span (span &optional link)
  (cond ((span-p span) (when link (setf (span-link span) link)) (list span))
        ((consp span) (mapcan (lambda (line) (ensure-text-span line link)) span))
        (t (list (span :text :content span :link link)))))

(defun make-text (line)
  (mapcan #'ensure-text-span (merge-strings (parse-text-line line))))

(defun parse-text-line (line)
  (loop with start = 0 with fin = (length line)
        for pos = (position-if #'special-char-p line :start start)
        when (and pos (/= start pos))
          collect (subseq line start pos)
        if pos
          collect (multiple-value-bind (span eos) (find-text-part line pos) (setf start eos) span) and do (unless start (loop-finish))
        else
          if (/= start fin)
            collect (subseq line start) and do (loop-finish)
        else
          do (loop-finish)))

(defun special-char-p (char)
  (or (find char "_*[`" :test #'char=) (and *inline-links-p* (char= char #\<))))

(defun emphasis-char-p (char)
  (or (char= char #\_) (char= char #\*)))

(defun find-text-part (line start)
  (case (char line start)
    ((#\* #\_) (find-text-emphasis line start))
    (#\` (find-text-code line (1+ start)))
    (#\! (find-text-complex-img line (1+ start)))
    (#\[ (find-text-complex-link line (1+ start)))
    (#\< (find-text-simple-link line (1+ start)))))

(defun find-text-emphasis (line start)
  "_a_ => italic; __b__ => bold; ___c___ => emphasis"
  (let ((count (+ 1 (if (emphasis-char-p (char line (1+ start))) (if (emphasis-char-p (char line (+ start 2))) 2 1) 0))))
    (lw:if-let (eos (find-end-of-text-emphasis line (+ start count) count))
               (values (mapcar (lambda (s) (setf (span-type s) (svref #(:i :b :em) (1- count))) s) (make-text (subseq line (+ start count) eos))) (+ eos count))
               (subseq line start))))

(defun find-end-of-text-emphasis (line start count)
  (loop for idx from start upto (- (length line) count) thereis (and (every #'emphasis-char-p (subseq line idx (+ idx count))) idx)))

(defun find-text-code (line start)
  "`smith` => smith; `3 * 4` => 3 * 4"
  (lw:if-let (eos (position #\` line :start start :test #'char=))
             (values (span :type :code :content (subseq line start eos)) (1+ eos))
             (values "`" start)))

(defun find-text-simple-link (line start)
  "<apples> => link:apples"
  (lw:if-let (eos (position #\> line :start start :test #'char=))
             (values (span :text :content (subseq line start eos) :link (subseq line start eos)) (1+ eos))
             (values "<" start)))

(defun find-text-complex-img (line start)
  (multiple-value-bind (m r) (cl-ppcre:scan-to-strings"(!\[[^]]+)]\\s*\\((.?)\\)" line :start start)
    (if m
        (values (span :img :content (aref r 0) :link (aref r 1)) (+ start (length m)))
        (values "!" start))))

(defun find-text-complex-link (line start)
  (multiple-value-bind (m r) (cl-ppcre:scan-to-strings "([^]]+)]\\s*\\((.*?)\\)" line :start start)
    (if m
        (values (ensure-text-span (make-markdown (aref r 0)) (aref r 1)) (+ start (length m)))
        (values "[" start))))


;;;============================================================================
;;; MAKE HTML FROM SPAN
;;;============================================================================

(defun make-html (spans)
  (with-output-to-string (out)
    (labels ((decode (s)
               (cond ((consp s) (map nil #'decode s))
                     ((stringp s) (write-string s out))
                     ((arrayp s) (map nil #'decode s))
                     ((null s))
                     (t (despanify s))))

             (wrap (tag content)
               (format out "<~(~A~)>" tag)
               (decode content)
               (format out "</~(~A~)>" tag))

             (table-heading-p (spans)
               (#~t/^\s*[-:]+\s*$/ (span-content (nth 0 spans))))

             (write-table (content)
               (write-string "<table class=\"primary zebra\">" out)
               (when (and (> (length content) 1) (every #'table-heading-p (nth 1 content)))
                 (write-string "<thead><tr>" out)
                 (map nil (partial* #'wrap "th") (nth 0 content))
                 (write-string "</tr></thead>")
                 (setf content (subseq content 2)))
               (write-string "<tbody>" out)
               (map nil (lambda (row) (write-string "<tr>" out) (map nil (partial* #'wrap "td") row) (write-string "</tr>" out)) content)
               (write-string "</tbody></table>" out))

             (despanify (s)
               (let ((type (span-type s)) (link (span-link s)) (content (span-content s)))
                 (when (and link (not (eq type :img))) (format out "<a href=~S>" link))
                 (case type
                   (:em (wrap "strong" content))
                   (:br (write-string "<br>" out))
                   ((:h1 :h2 :h3 :h4 :h5 :h6 :b :i :ol :ul :li) (wrap type content))
                   (:bq (wrap "blockquote" content))
                   (:hr (write-string "<hr>" out))
                   (:codeblock (wrap "code" content))
                   (:img (format out "<img src=\"~A\" alt=\"\~A\">" link content))
                   (:text (write-string content out))
                   (:table (write-table content))
                   (t (error "Unimplemented tag ~S" type)))

                 (when (and link (not (eq type :img))) (write-string "</a>" out)))))

      (decode spans))))

(defun html (text)
  (make-html (parse text)))
