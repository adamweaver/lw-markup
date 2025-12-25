(in-package :html)

(defun esc (string)
  (flet ((escapable (char)
           (or (char= char #\<) (char= char #\>) (char= char #\")))
         (escape (char)
           (case char
             (#\< "&lt;")
             (#\> "&gt;")
             (#\" "&quot;")
             (t char))))
    (let ((pos (position-if #'escapable string)))
      (if pos
          (with-output-to-string (out)
            (write-sequence string out :end pos)
            (loop for i from pos below (length string) do (princ (escape (char string i)) out)))
          string))))

(defun mapcat (function &rest args)
  (apply #'concatenate 'string (apply #'map 'list function args)))

(defun maybe-write (stream arg)
  (cond ((consp arg) (map nil (lambda (a) (maybe-write stream a)) arg))
        (arg (princ arg stream))))

(defmacro html (&body children)
  (let ((xmlish nil))
    (labels ((stringify (el)
               (cond ((and (consp el) (keywordp (car el))) (elements el))
                     (el (list el))))

             (elements (el)
               (if (and xmlish (null (cddr el)))
                   (append (tag-start (car el)) (tag-attrs (cadr el)) (list " />"))
                   (append (tag-start (car el)) (tag-attrs (cadr el)) (list ">")
                           (mapcan #'stringify (cddr el))
                           (unless (void-p (car el)) (tag-end (car el))))))

             (tag-start (tag)
               (list "<" (string-downcase (symbol-name tag))))

             (tag-end (tag)
               (list "</" (string-downcase (symbol-name tag)) ">"))

             (tag-attrs (elements)
               (let* ((pairs (loop for (k v) on elements by #'cddr collect (cons k v)))
                      (static-classes (remove-if-not #'static-class-p pairs))
                      (dynamic-classes (remove-if-not #'dynamic-class-p pairs))
                      (static-attrs (remove-if-not #'static-attr-p pairs))
                      (dynamic-attrs (remove-if-not #'dynamic-attr-p pairs)))

                 (append (when (or static-classes dynamic-classes) (list (format nil " class=\"~{~A~^ ~}" (mapcar #'cdr static-classes))))
                         (when dynamic-classes (list `(intersperse " " (delete nil (list ,@(when static-classes '("")) ,@(mapcar #'make-dynamic-class dynamic-classes))))))
                         (when (or static-classes dynamic-classes) (list (if static-attrs "\" " "\"")))
                         (when (and static-attrs (not (or static-classes dynamic-classes))) (list " "))
                         (when static-attrs (intersperse " " (mapcar #'make-static-attr static-attrs)))
                         (when dynamic-attrs (list `(intersperse " " (delete nil (list "" ,@(mapcar #'make-dynamic-attr dynamic-attrs)))))))))

             (static-class-p (cons)
               (and (eq (car cons) :class) (stringp (cdr cons))))

             (dynamic-class-p (cons)
               (and (eq (car cons) :class) (not (stringp (cdr cons)))))

             (static-attr-p (cons)
               (and (not (eq (car cons) :class)) (constantp (cdr cons))))

             (dynamic-attr-p (cons)
               (and (not (eq (car cons) :class)) (not (constantp (cdr cons)))))

             (make-dynamic-class (cons)
               `(when ,(cdr cons) ,(if (symbolp (cdr cons)) (string-downcase (symbol-name (cdr cons))) (cdr cons))))

             (make-static-attr (cons)
               (if (eq (cdr cons) t)
                   (format nil "~(~A~)" (car cons))
                   (format nil "~A=\"~A\"" (if (symbolp (car cons)) (string-downcase (symbol-name (car cons))) (car cons)) (cdr cons))))

             (make-dynamic-attr (cons)
               (let ((result (gensym)) (key (if (symbolp (car cons)) (string-downcase (symbol-name (car cons))) (car cons))))
                 `(let ((,result ,(cdr cons)))
                    (when ,result
                      (if (eq ,result t)
                          ,key
                          (format nil ,(format nil "~A=\"~~A\"" key) ,result))))))

             (void-p (tag)
               (find tag '(:area :base :br :col :!doctype :embed :hr :img :input :link :meta :param :source :track :wbr) :test #'eq))

             (merge-strings (list)
               (cond ((null list) nil)
                     ((atom list) list)
                     ((and (stringp (car list)) (stringp (cadr list))) (merge-strings (cons (concatenate 'string (car list) (cadr list)) (cddr list))))
                     (t (cons (car list) (merge-strings (cdr list)))))))

      (when (eq (car children) :xml) (setf xmlish (pop children)))
      (let ((args (merge-strings (mapcan #'stringify children))) (out (gensym "HTML-OUTPUT")))
        `(with-output-to-string (,out)
           ,@(mapcar (lambda (s) (if (stringp s) `(write-string ,s ,out) `(maybe-write ,out ,s))) args))))))

(defmacro xml (&body children)
  `(html :xml ,@children))
