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

(in-package :sysdef.document-action)

(defmacro with-gensyms ((&rest vars) &body body)
  `(let ,(mapcar (lambda (x) `(,x (gensym))) vars)
     ,@body))

(defun blankp (string)
  (zerop (length string)))

(defun topmost-car (cons)
  "Returns the `leftmost` element in list. eg. (((1) 2) 3) => 1"
  (if (consp cons)
      (topmost-car (car cons))
      cons))

(defmacro or2 (&rest tests)
  (if (endp tests)
      nil
      (with-gensyms (val win)
        `(multiple-value-bind (,val ,win) ,(first tests)
           (if (or ,val ,win)
               ,val
               (or2 ,@(cdr tests)))))))

(defun defpackage-operator-p (info)
  (eql (operator-of info) 'defpackage))

(defun exportedp (symbol &key (from (symbol-package symbol)))
  (and (not (keywordp symbol))
       (eql (nth-value 1 (find-symbol (string symbol) from)) :external)))

(defun singlep (x)
  (and (consp x) (not (cdr x))))

(defun setfablep (place)
  (fboundp `(setf ,place)))

;;; Extracting documentable symbols


(defun symbol-defines (symbol)
  "Returns a list of the types that symbol defines. A subset of *all-symbol-defines*"
  (remove-if-not (lambda (type) (definesp symbol type)) *all-symbol-defines*))

(defgeneric definesp (symbol type)
  (:method ((symbol symbol) (type (eql :type)))
   ;; There isn't a portable way to determine if a symbol
   ;; specifies a type. (subtypep <name> t) is not portable.
   #+lispworks (ignore-errors (not (eql (type:expand-user-type symbol) symbol)))
   #+ccl (not (eql (ccl::type-expand symbol) symbol))
   #+sbcl (not (eql (sb-kernel:type-expand symbol) symbol))
   #+cmucl (not (eql (kernel:type-expand symbol) symbol))
   #+clisp (handler-case (not (eql symbol (ext:type-expand symbol)))
             (error (c) (and c t)))
   #-(or lispworks ccl sbcl cmucl clisp)
   (find-if #'(lambda (doc) (and (eql (name-of doc) symbol)
                                 (typep doc 'type-doc)))
            *current-docs*))
  (:method ((symbol symbol) (type (eql :macro)))
   (macro-function symbol))
  (:method ((symbol symbol) (type (eql :function)))
   (and (fboundp symbol)
        (not (definesp symbol :generic-function))
        (not (definesp symbol :macro))))
  (:method ((symbol symbol) (type (eql :generic-function)))
   (and (fboundp symbol) (typep (fdefinition symbol) 'generic-function)))
  (:method ((symbol symbol) (type (eql :special-variable)))
   (and (declared-special-p symbol) (not (symbol-constant-p symbol))))
  (:method ((symbol symbol) (type (eql :constant)))
   (symbol-constant-p symbol))
  (:method ((symbol symbol) (type (eql :class)))
   (and (find-class symbol nil)
        (not (definesp symbol :condition))
        (subtypep symbol 'standard-object)))
  (:method ((symbol symbol) (type (eql :condition)))
   (and (find-class symbol nil)
        (subtypep symbol 'condition)))
  (:method ((symbol symbol) (type (eql :structure)))
   (and (find-class symbol nil)
        (subtypep (find-class symbol) 'structure-object))))

(defun documentable-symbols (package)
  (loop :for symbol :being :the :external-symbols :of package
        :collect symbol))

(defun doc-type-ordinal (type)
  "Assigns ordinals to the different kinds of entries for sorting
purposes."
  (or (position type *all-symbol-defines*)
      (error "Invalid type ~S." type)))

(defun name= (name1 name2)
  "Two function names are equal if they are EQUAL - this covers
symbols as well as general function names."
  (equal name1 name2))

(defun name< (name1 name2)
  "Comparison function used for sorting - symbols are \"smaller\"
then SETF names, otherwise sort alphabetically."
  (or (and (consp name2)
           (atom name1))
      (and (consp name1)
           (consp name2)
           (string< (second name1) (second name2)))
      (and (atom name1)
           (atom name2)
           (string< name1 name2))))

(defun doc-entry< (entry1 entry2)
  "Comparions function used for sorting - sort by name and, if
the names are the same, by DOC-TYPE-ORDINAL."
  (or (name< (name-of entry1) (name-of entry2))
      (and (name= (name-of entry1) (name-of entry2))
           (< (doc-type-ordinal (applicable-to entry1))
              (doc-type-ordinal (applicable-to entry2))))))

(defun symbols-to-document (package)
  (mapcar #'(lambda (sym) (list sym (symbol-defines sym)))
          (documentable-symbols package)))

;; nicked from documentation-template (Sorry Edi)
(defun declared-special-p (symbol)
  "Returns true if SYMBOL is declared special."
  #+:lispworks (and (sys:declared-special-p symbol) (not (keywordp symbol)))
  #+:sbcl (eql :special (sb-int:info :variable :kind symbol))
  #+:allegro (eq (sys:variable-information symbol) :special)
  ;; if we don't use these lisps then look for *'s around the name.
  ;; yes i know it doesn't mean the same thing but it should do the
  ;; right thing most of the time
  #-(or :lispworks :sbcl :allegro) 
  (let ((name (symbol-name symbol)))
    (and (> (length name) 2)
         (eql (char name 0) #\*)
         (eql (char name (1- (length name))) #\*))))

(defun symbol-constant-p (symbol)
  "Returns true if SYMBOL is a constant."
  #+:lispworks (sys:symbol-constant-p symbol)
  #-:lispworks (constantp symbol))




(defun common-lisp-declaration-p (decl)
  (member decl *common-lisp-declarations*))

(defun process-declaration (decl &optional (into *current-declarations*))
  (case (first decl)
    (cl:optimize (register-optimizations (rest decl) into))
    (cl:ftype (register-ftype (rest decl) into))
    (cl:values (register-values (rest decl) into))
    (cl:type (register-type (rest decl) into)))
  (unless (common-lisp-declaration-p (first decl))
    ;; then this is probably a type declaration in the form (declare (fixnum most-positive-fixnum))
    ;; the cost if it isn't is quite low anyway so process it nonetheless
    (register-type decl into)))
  

(defun register-type (value into)
  (let ((types (or (gethash 'cl:type into)
                    (setf (gethash 'cl:type into) (make-hash-table)))))
    (destructuring-bind (spec . variables) value
      (dolist (var variables)
        (setf (gethash var types) spec)))))
    

(defun register-ftype (value into)
  (let ((ftypes (or (gethash 'cl:ftype into)
                    (setf (gethash 'cl:ftype into) (make-hash-table)))))
    (destructuring-bind (spec . functions) value
      (dolist (function functions)
        (setf (gethash function ftypes) spec)))))

(defun register-values (values into)
  (setf (gethash 'values into) values))

(defun register-optimizations (optimizations into)
  (dolist (opt optimizations)
    (destructuring-bind (name &optional (value 3)) (if (listp opt) opt (list opt))
      (setf (gethash name into) value))))

(defun in-package-form-p (form)
  (and (consp form) (eql (car form) 'cl:in-package)))

(defun in-package-name (form)
  (second form))

(defun merge-declarations (decls defaults)
  (let ((*current-declarations* (copy-declarations defaults)))
    (when decls
      (mapcar 'process-declaration decls))
    *current-declarations*))

(defun copy-declarations (decls)
  (let ((new (make-hash-table)))
    (maphash #'(lambda (key val) (setf (gethash key new) val))
             decls)
    new))


(defun package-root (package)
  (merge-pathnames (make-pathname :directory (list :relative (string-downcase (package-name package))))
                   *doc-root*))

(defun system-path (system)
  (merge-pathnames (make-pathname :name (remove-if-not (lambda (x) (or (alphanumericp x) (member x *valid-file-chars*)))
                                                       (format nil "~(~A~)" (name-of system)))
                                  :type "html" :version :newest)
                   *doc-root*))




(defun document-path (symbol type &optional (root *default-pathname-defaults*))
  (merge-pathnames (make-pathname :name (remove-if-not (lambda (x) (or (alphanumericp x) (member x *valid-file-chars*)))
                                                       (format nil "~(~A-~A~)" symbol type))
                                  :type "html" :version :newest)
                   root))

(defun package-path (name)
  (make-pathname :directory (remove-if-not (lambda (x) (or (alphanumericp x) (member x *valid-file-chars*)))
                                           (format nil "~(~A~)" name))
                 :type "html" :version :newest))


(defun dashify (string)
  "downcases string and replaces #\Space's with #\_"
  (substitute #\_ #\Space (string-downcase string)))

(defun plain-lambda-list (lambda-list)
  "Removes default values from keyword & optional values in lambda list"
  (mapcar #'(lambda (x)
              (if (consp x)
                  (topmost-car x)
                  x))
          lambda-list))

(defun uses-exported-symbols-from (body package)
  "Returns external symbols of package which are referenced by body as 2 lists. The
first value contains special variables and the second all other symbols"
  (let (exported specials)
    (dolist (sym (flatten body) (VALUES (remove-duplicates specials)
                                        (remove-duplicates exported)))
      (when (and (symbolp sym) (exportedp sym :from package))
        (if (declared-special-p sym)
            (push sym specials)
            (push sym exported))))))


(defun contents-of (file package)
  (let ((*package* package))
    (with-open-file (stream file)
      (loop for form = (read stream nil stream)
            until (eq stream form)
            collect form))))


(defun uppercase-word-p (word)
  (every #'(lambda (char)
             (if (alpha-char-p char)
                 (char< #\A char #\Z)
                 t))
         word))

(defun split-lines (string)
  (split "\\n" string))

(defun seperatorp (char)
  (find char '(#\newline #\return #\Space #\, #\. #\` #\' #\; #\: #\( #\))))

(defun split-tokens (line)      
  (let ((start 0)
        (res nil))
    (flet ((next-seperator (line start)
             (position-if #'seperatorp line :start start)))
      (loop for next-seperator = (next-seperator line start)
            :do
            (if next-seperator
                (progn
                  (push (subseq line start next-seperator) res)
                  ;; and add the seperator
                  (push (subseq line next-seperator (1+ next-seperator)) res)
                  (setf start (1+ next-seperator)))
                (progn (push (subseq line start) res) (return))))
      (delete "" (nreverse res) :test 'string=))))
 


(defun group-by (fn sequence &key (test #'eql) (key #'identity))
  "Groups SEQUENCE into an alist using (compose FN KEY) to extract the key from each element."
  (let ((result ()))
    (map nil #'(lambda (elt)
                 (let* ((elt-key (funcall key (funcall fn elt)))
                        (result-elt (assoc elt-key result :test test)))
                   (if result-elt
                       (setf (cdr result-elt) (cons elt (cdr result-elt)))
                       (setf result (acons elt-key (list elt) result)))))
         sequence)
    result))


(defun function-lambda-list (function)
  "Returns the lambda list of the function designator FUNCTION."
  #+:sbcl
  (sb-introspect:function-arglist function)
  #+:allegro
  (excl:arglist function)
  #+:lispworks
  (lw:function-lambda-list function)
  #-(or :sbcl :lispworks :allegro)
  '(:unknown))

(defun as-human-lisp (code &optional (package *package*))
  (with-standard-io-syntax
    (let ((*print-pretty* t)
          (*print-right-margin* 60)
          (*package* package)
          (*print-case* :downcase)
          (*print-readably* nil))
      (prin1-to-string code))))

;; EOF
