;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: SYSDEF.DOCUMENT-ACTION; Base: 10 -*-
;;; Copyright (c) 2006-2009, Sean Ross.  All rights reserved.
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


;; TODO
;; What about catch tags?
(in-package :sysdef.document-action)

;; Render Utilities
(defmacro html (&body body)
  `(with-html-output (*standard-output* *standard-output* )
     ,@body))

(defmacro html-string (&body body)
  `(with-html-output-to-string (*standard-output*)
     ,@body))

(defmacro with-doc-page ((&key title) &body body)
  `(with-html-output (*standard-output* *standard-output* :indent t :prologue t)
     (:head
      (:script :type "text/javascript" :src "../mb-artifacts/js/jquery-1.2.6.js")
      (:title (esc (string ,title))))
     (:body
      ,@body)))

(defmacro dictionary-entry ((type name) &body body)
  `(html (:div :class ,type
          (:a :class "none" :name ,name)
          ,@body)))

(defmacro section ((title &key oneline) &body body)
  (once-only (title oneline)
    (with-gensyms (inner-html)
      `(let ((,inner-html (html-string ,@body)))
         (html (:div :id (dashify ,title)
                (:p (:strong (esc ,title) ":")
                 (if (blankp ,inner-html)
                     (html " None.")
                     (html
                       (if ,oneline
                           (html " " (str ,inner-html))
                           (html (:br) (:p (str ,inner-html)))))))))))))


(defun render-arglist (arglist results)
  (html (:i (if (null arglist)
                (html " &lt;no arguments&gt; ")
                (html (esc (let ((*print-case* :downcase))
                             (format nil "~{~A~^ ~}" (plain-lambda-list arglist))))))
         "=> "  (html (esc (let ((*print-case* :downcase))
                             (if (null results)
                                 (format nil "<no results>")
                                 (format nil "~{~A~^, ~}" results))))))))

(defun render-symbol-links (symbols &key (docs *package-docs*) (package *documented-package*))
  (let ((strings (mapcar #'(lambda (sym)
                             (with-output-to-string (*standard-output*)
                               (render-link (string-downcase sym) docs package)))
                         symbols)))
    (format nil "~{~A~^, ~}" strings)))


(defun link-name (doc)
  (document-path (name-of doc) (car (rassoc (class-name (class-of doc)) *type-mapping*))
                 #P""))

(defun merge-page-path (name)
  (document-path name :doc-grouping (package-root *documented-package*)))

(defun generate-merge-page (docs)
  (let ((path (merge-page-path (name-of (first docs))))
        (name (name-of (first docs))))
    (unless (probe-file path)
      (ensure-directories-exist path)
      (with-open-file (*standard-output* path :direction :output :if-exists :supersede)
        (with-doc-page (:title (string name))
          (html
            (:p (:h3 (str (string-upcase name)) (:br)))
            (:span "Please select the reference to " (esc (string name))) " which you intended."
            (:ul
             (dolist (doc docs)
               (html (:li (:a :href (document-path name (applicable-to doc) (package-root *documented-package*))
                           (esc (pretty-name-of doc))))))))))
      path)))

(defun render-link (word docs package)
  (let ((docs (remove-if-not (lambda (doc) (eql (name-of doc)
                                                (find-symbol (string-upcase word) package)))
                             docs)))
    (let ((href (cond ((null docs) nil)
                      ((singlep docs) (link-name (first docs)))
                      (t (merge-page-path (name-of (first docs)))))))
      (html (:a :href href (:b (str word)))))))

(defun render-link-to (doc)
  (html (:a :href (link-name doc) (:b (esc (string (name-of doc)))))))

(defun linkify-string (string &key (docs *package-docs*) (uppercase-only nil) (package *documented-package*) )
  "Converts string into an html string with links to documentation."
  (flet ((exported-symbol-p (token)
           (exportedp (string-upcase token) :from package)))
    (let ((lines (split-lines string)))
      (with-html-output-to-string (*standard-output*)
        (dolist (line lines)
          (dolist (token (split-tokens line))
            (if (and (if uppercase-only
                         (uppercase-word-p token)
                         t)
                     (exported-symbol-p token))
                (render-link token docs package)
                (princ token)))
          (html (:br)))))))



(defun render-args-and-values (args values)
  (loop for (arg . value) in args :do
        (html (:i (esc (string-downcase arg))) " -- " (esc (string-downcase value)) (:br)))
  (loop for (arg . value) in values :do
        (html (:i (esc (string-downcase arg))) " -- " (esc (string-downcase value)) (:br))))

;; HINTS

(defmethod doc-hint-path (system)
  (merge-pathnames (make-pathname :name "doc-hints" :type "lisp")
                   (component-pathname system)))


(defun doc-hint (system type name hint package)
  (let ((hints (or *hints* (system-doc-hints system package))))
    (loop for (ttype tname . body) in hints :do
          (when (and (eql ttype type) (eql tname name))
            (return (second (assoc hint body)))))))

(defun system-doc-hints (system package)
  (let ((path (doc-hint-path system)))
    (when (probe-file path)
      (contents-of path package))))




;; RENDER
(defmethod render-documentation (sys doc &optional (package *documented-package*))
  (format t "~10@TGenerate Docs for ~S ~S~%" (name-of doc) doc))


(defun args-and-values (fn-doc)
  "Returns 2 values an alist mapping arguments to types and an alist mapping return values to types."
  (let ((args (extract-variables (plain-lambda-list (lambda-list-of fn-doc))))
        (decls (declaration-of fn-doc)))
    (values (compute-arg-types args decls (name-of fn-doc))
            (return-values-of fn-doc))))

(defun type-of-in (var decls)
  "Returns the type of var as prescribed by DECLS"
  (gethash var (types-of decls)))

(defun compute-arg-types (arglist decls name)
  (let ((result (pairlis arglist (make-list (length arglist) :initial-element t))))
    ;; process type declarations in body
    (dolist (arg arglist)
      (when-let (type (type-of-in arg decls))
        (setf (cdr (assoc arg result)) type)))
    ;; process ftype (which then takes precedence)
    (when-let (ftype (gethash name (ftypes-of decls)))
      (loop for arg in arglist
            for type in (second ftype) :do
            (setf (cdr (assoc arg result)) type)))
    result))

(defun ftypes-of (decl)
  (or (gethash 'ftype decl) (make-hash-table)))
(defun types-of (decl)
  (or (gethash 'type decl) (make-hash-table))) 
(defun values-of (decl)
  (gethash 'values decl))

(defun return-values-of (fn-doc) ;
  (let ((decl (declaration-of fn-doc)))
    (name-results
     (or2 (when-let (ftype (gethash (name-of fn-doc) (ftypes-of decl)))
            (cddr ftype))
          (values-of decl)
          '(t)))))

(defun name-results (results)
  (cond ((null results) nil)
        ((singlep results) `(("result" . ,(first results))))
        (t (loop for type in results
            for count from 1
            collect `(,(format nil "result~D" count) . ,type)))))

(defun extract-variables (lambda-list)
  (remove-if #'(lambda (sym) (find sym lambda-list-keywords)) lambda-list))

(defun safeness (decls)
  (let ((speed (gethash 'speed decls 2))
        (safety (gethash 'safety decls 2)))
    (cond ((and (> speed 2) (< safety 1)) :unsafe)
          ((and (> safety 2) (< speed 1)) :safe)
          (t nil))))

(defun safety-image (decls)
  (when-let (safeness (safeness decls))
    (html (:img :src (format nil "../mb-artifacts/images/~(~A~).png" safeness)))))

(defun setfable-image (name)
  (when (setfablep name)
    (html (:img :src (format nil "../mb-artifacts/images/setfable.png")))))
    

;; TODO: add default values of &optional variables in Arguments and Values
(defmethod render-documentation ((system system) (doc function-doc) &optional (package *documented-package*))
  (flet ((doc-hint (name hint)
           (doc-hint system :function name hint package)))
    (multiple-value-bind (affected-by uses) (uses-exported-symbols-from (source-of doc) package)
      (multiple-value-bind (args values) (args-and-values doc)
        (with-accessors ((name name-of)) doc
          (with-doc-page (:title name)
            (dictionary-entry ("function" name)
              (html
                (safety-image (declaration-of doc)) (setfable-image (name-of doc)) (:br)
                (:p (:i "Function") "&nbsp;" (:strong (esc (string name))))
                

                (section ("Syntax")
                  (:tt (:strong (esc (string-downcase name)))) "&nbsp;"
                  (render-arglist (plain-lambda-list (lambda-list-of doc)) (mapcar 'first values)))

                (section ("Arguments and Values")
                  (render-args-and-values args values))

                (section ("Description")
                  (str (linkify-string (docstring doc))))

                (when-let (example (doc-hint name :examples))
                  (section ("Examples") (str example)))

                (section ("Affected By")
                  (str (render-symbol-links
                        (remove name (append affected-by (doc-hint name :affected-by))))))
            
                (section ("Exceptional Situations")
                  (str (doc-hint name :exceptional-situations)))

                (section ("See Also")
                  (str (render-symbol-links
                        (remove name (append uses (doc-hint name :see-also))))))

                (when-let (notes (doc-hint name :notes))
                  (section ("Notes") (esc notes)))
        
                (section ("Source" :oneline t) (:a :id "clicky" :href "#" "Toggle")
                  (:div :id "code" :style "display:none;"
                   (:pre (esc (source-code-of doc package)))))
            
                (:script :type "text/javascript"
                 "$('a#clicky').click(function() { $('div#code').toggle('fast'); return false;});")))))))))



;; TODO
;; See Also should really be a link to functions which are affected by this variable
(defun initial-value-of (doc)
  (with-standard-io-syntax
    (let ((*print-pretty* t)
          (*print-right-margin* 60)
          (*package* (package-for doc))
          (*print-case* :downcase)
          (*print-readably* nil))
      (if (boundp (name-of doc))
          (prin1-to-string (symbol-value (name-of doc)))
          "<Unbound>"))))

(defun declared-type (doc)
  (prin1-to-string (or (type-of-in (name-of doc) (declaration-of doc))
                       t)))


(defmethod render-documentation ((system system) (doc special-var-doc) &optional (package *documented-package*))
  (flet ((doc-hint (name hint)
           (doc-hint system :special-var name hint package)))
    (multiple-value-bind (affected-by uses) (uses-exported-symbols-from (source-of doc) package)
      (declare (ignore affected-by))
      (with-accessors ((name name-of)) doc
        (with-doc-page (:title name)
          (dictionary-entry ("Variable" name)
            (:p (:i "Variable") "&nbsp;" (:strong (esc (string name))))
            (section ("Value Type" :oneline t) (str (declared-type doc)))
            (section ("Initial Value" :oneline t)
              (esc (initial-value-of doc)))
            (section ("Description") (str (linkify-string (docstring doc))))
            (when-let (example (doc-hint name :examples))
              (section ("Examples") (str example)))
            (section ("See Also")
              (str (render-symbol-links
                    (remove name (append uses (doc-hint name :see-also))))))

            (when-let (notes (doc-hint name :notes))
              (section ("Notes") (esc notes)))
        
            (section ("Source" :oneline t) (:a :id "clicky" :href "#" "Toggle")
              (:div :id "code" :style "display:none;"
               (:pre (esc (source-code-of doc package)))))
            
            (:script :type "text/javascript"
             "$('a#clicky').click(function() { $('div#code').toggle('fast'); return false;});")))))))


;; TODO: Add subclasses and links to methods
(defmethod render-documentation ((system system) (doc class-doc) &optional (package *documented-package*))
  (flet ((doc-hint (name hint)
           (doc-hint system :class name hint package)))
    (multiple-value-bind (affected-by uses) (uses-exported-symbols-from (source-of doc) package)
      (with-accessors ((name name-of)) doc
        (let ((precedence-list (mapcar 'class-name (mb.sysdef::class-precedence-list (find-class name)))))
        (with-doc-page (:title name)
          (dictionary-entry ((pretty-name-of doc) name)
            (:p (:i (esc (pretty-name-of doc))) "&nbsp;" (:strong (esc (string name))))
            (section ("Class Precedence List")
              (str (render-symbol-links precedence-list)))

            (section ("Affected By" :oneline t) 
              (str (render-symbol-links
                    (remove name (append affected-by (doc-hint name :affected-by))))))
          
            (section ("See Also" :oneline t)
              (str (render-symbol-links
                    (set-difference (remove name (append uses (doc-hint name :see-also)))
                                    precedence-list))))

            (section ("Description") (str (linkify-string (docstring doc))))

            (section ("Source" :oneline t) (:a :id "clicky" :href "#" "Toggle")
              (:div :id "code" :style "display:none;"
               (:pre (esc (source-code-of doc package)))))
            
            (:script :type "text/javascript"
             "$('a#clicky').click(function() { $('div#code').toggle('fast'); return false;});"))))))))


(defmethod render-documentation ((system system) (doc struct-doc) &optional (package *documented-package*))
  (flet ((doc-hint (name hint)
           (doc-hint system :struct name hint package)))
    (multiple-value-bind (affected-by uses) (uses-exported-symbols-from (source-of doc) package)
      (with-accessors ((name name-of)) doc
        (with-doc-page (:title name)
          (dictionary-entry ((pretty-name-of doc) name)
            (:p (:i (esc (pretty-name-of doc))) "&nbsp;" (:strong (esc (string name))))
            
            (section ("Affected By" :oneline t) 
              (str (render-symbol-links
                    (remove name (append affected-by (doc-hint name :affected-by))))))
          
            (section ("See Also" :oneline t)
              (str (render-symbol-links
                    (remove name (append uses (doc-hint name :see-also))))))

            (when-let (notes (doc-hint name :notes))
              (section ("Notes") (esc notes)))

            (section ("Description") (str (linkify-string (docstring doc))))
            
            (section ("Source" :oneline t) (:a :id "clicky" :href "#" "Toggle")
              (:div :id "code" :style "display:none;"
               (:pre (esc (source-code-of doc package)))))
            
            (:script :type "text/javascript"
             "$('a#clicky').click(function() { $('div#code').toggle('fast'); return false;});")))))))

(defmethod render-documentation ((system system) (doc type-doc) &optional (package *documented-package*))
  (flet ((doc-hint (name hint)
           (doc-hint system :type name hint package)))
    (multiple-value-bind (affected-by uses) (uses-exported-symbols-from (source-of doc) package)
      (with-accessors ((name name-of)) doc
        (with-doc-page (:title name)
          (dictionary-entry ((pretty-name-of doc) name)
            (:p (:i (esc (pretty-name-of doc))) "&nbsp;" (:strong (esc (string name))))
            
            (section ("Affected By" :oneline t) 
              (str (render-symbol-links
                    (remove name (append affected-by (doc-hint name :affected-by))))))
          
            (section ("See Also" :oneline t)
              (str (render-symbol-links
                    (remove name (append uses (doc-hint name :see-also))))))

            (when-let (notes (doc-hint name :notes))
              (section ("Notes") (esc notes)))

            (section ("Description") (str (linkify-string (docstring doc))))

            (section ("Source" :oneline t) (:a :id "clicky" :href "#" "Toggle")
              (:div :id "code" :style "display:none;"
               (:pre (esc (source-code-of doc package)))))
            
            (:script :type "text/javascript"
             "$('a#clicky').click(function() { $('div#code').toggle('fast'); return false;});")))))))


(defmethod render-documentation ((system system) (doc package-doc) &optional (package *documented-package*))
  (with-accessors ((name name-of)) doc
    (with-doc-page (:title name)
      (dictionary-entry ((pretty-name-of doc) (string name))
        (:p (:h3 "The " (esc (string name)) " Dictionary"))
        (dolist (entry (remove doc *package-docs*))
          (html (:p (:i (esc (pretty-name-of entry))) "&nbsp;"
                 (:strong (render-link-to entry)))))))))


;; TODO: Refactor all of the renders
(defmethod render-documentation ((system system) (doc macro-doc) &optional (package *documented-package*))
  (flet ((doc-hint (name hint)
           (doc-hint system :macro name hint package)))
    (multiple-value-bind (affected-by uses) (uses-exported-symbols-from (source-of doc) package)
      (multiple-value-bind (args values) (args-and-values doc)
        (with-accessors ((name name-of)) doc
          (with-doc-page (:title name)
            (dictionary-entry ("Macro" name)
              (html
                (safety-image (declaration-of doc)) (setfable-image (name-of doc)) (:br)
                (:p (:i "Macro") "&nbsp;" (:strong (esc (string name))))
                

                (section ("Syntax")
                  (:tt (:strong (esc (string-downcase name)))) "&nbsp;"
                  (render-arglist (plain-lambda-list (lambda-list-of doc)) (mapcar 'first values))

                  (when-let (notes (doc-hint name :syntax))
                    (section ("Notes") (esc notes))))

                (section ("Arguments and Values")
                  (render-args-and-values args values))

                (section ("Description")
                  (str (linkify-string (docstring doc))))

                (when-let (example (doc-hint name :examples))
                  (section ("Examples") (str example)))

                (section ("Affected By")
                  (str (render-symbol-links
                        (remove name (append affected-by (doc-hint name :affected-by))))))
            
                (section ("Exceptional Situations")
                  (str (doc-hint name :exceptional-situations)))

                (section ("See Also")
                  (str (render-symbol-links
                        (remove name (append uses (doc-hint name :see-also))))))

                (when-let (notes (doc-hint name :notes))
                  (section ("Notes") (esc notes)))
        
                (section ("Source" :oneline t) (:a :id "clicky" :href "#" "Toggle")
                  (:div :id "code" :style "display:none;"
                   (:pre (esc (source-code-of doc package)))))
            
                (:script :type "text/javascript"
                 "$('a#clicky').click(function() { $('div#code').toggle('fast'); return false;});")))))))))



;; TODO: add default values of &optional variables in Arguments and Values
(defmethod render-documentation ((system system) (doc generic-fuction-doc) &optional (package *documented-package*))
  (flet ((doc-hint (name hint)
           (doc-hint system :generic-function name hint package)))
    (multiple-value-bind (affected-by uses) (uses-exported-symbols-from (source-of doc) package)
      (multiple-value-bind (args values) (args-and-values doc)
        (with-accessors ((name name-of)) doc
          (with-doc-page (:title name)
            (dictionary-entry ("function" name)
              (html
                (safety-image (declaration-of doc)) (setfable-image (name-of doc)) (:br)
                (:p (:i "Generic Function") "&nbsp;" (:strong (esc (string name))))
                

                (section ("Syntax")
                  (:tt (:strong (esc (string-downcase name)))) "&nbsp;"
                  (render-arglist (plain-lambda-list (lambda-list-of doc)) (mapcar 'first values)))

                (section ("Method Signatures")
                  (render-signatures doc))

                (section ("Arguments and Values")
                  (render-args-and-values args values))

                (section ("Description")
                  (str (linkify-string (docstring doc))))

                (when-let (example (doc-hint name :examples))
                  (section ("Examples") (str example)))

                (section ("Affected By")
                  (str (render-symbol-links
                        (remove name (append affected-by (doc-hint name :affected-by))))))
            
                (section ("Exceptional Situations")
                  (str (doc-hint name :exceptional-situations)))

                (section ("See Also")
                  (str (render-symbol-links
                        (remove name (append uses (doc-hint name :see-also))))))

                (when-let (notes (doc-hint name :notes))
                  (section ("Notes") (esc notes)))
        
                (section ("Source" :oneline t) (:a :id "clicky" :href "#" "Toggle")
                  (:div :id "code" :style "display:none;"
                   (:pre (esc (source-code-of doc package)))))
            
                (:script :type "text/javascript"
                 "$('a#clicky').click(function() { $('div#code').toggle('fast'); return false;});")))))))))



(defun render-method-arglist (arglist results)
  (html (:i (if (null arglist)
                (html " &lt;no arguments&gt; ")
                (html (esc (let ((*print-case* :downcase))
                             (format nil "~{~A~^ ~}" (plain-lambda-list arglist))))))
         "=> "  (html (esc (let ((*print-case* :downcase))
                             (if (null results)
                                 (format nil "<no results>")
                                 (format nil "~{~A~^, ~}" results))))))))

(defun specializer-name (x)
  (if (typep x 'class)
      (class-name x)
      x))
      
(defmethod method-signature (method)
  (loop for var in (method-lambda-list method)
        for specializer on (method-specializers method)
        collect (if specializer (list var (specializer-name (car specializer))) var)))

(defun render-signatures (doc)
  (let ((gf (fdefinition (name-of doc)))
        (name (name-of doc)))
    (dolist (method (generic-function-methods gf))
      (html (:p (:tt (:strong (esc (string-downcase name)))) "&nbsp;"
             (:i (esc (format nil "~{~A~^ ~}" (mapcar 'as-human-lisp (method-signature method))))))))))


;; System Page
(defmethod render-system-page ((system system) packages)
  (let ((name (name-of system)))
    (with-doc-page (:title name)
      (dictionary-entry ("System" name)
        (:p (:i "System") "&nbsp;" (:strong (esc (string (name-of system)))))
        (section ("Description")
          (esc (documentation system 'system)))
        (section ("Packages")
          (dolist (package packages)
            (html (:p (:a :href (document-path (package-name package) 'package
                                               (make-pathname :directory (list :relative
                                                                               (string-downcase (package-name package)))))
                       (:strong (esc (package-name package))))
                   (:br)))))
        (when-let (notes (doc-hint system :system name :notes (find-package :sysdef-user)))
          (section ("Notes") (str notes)))))))


#|
(system-doc-hints (find-system :hunchentoot) (find-bpackage :sysdef-user))
(document (find-system :mb.sysdef))
|#