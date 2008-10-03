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
;; use hint to override what packages to document

(declaim (optimize safety debug (speed 0)))
(in-package :sysdef.document-action)

;; DOCUMENT ACTION
(defclass document-action (action)
  ()
  (:default-initargs :needs '(:load-action)))

(defmethod execute ((system system) (action document-action))
  (document system))

(defgeneric document (system)
  (:method ((system symbol))
   (document (find-system system)))
  (:method ((system system))
   (let* ((sysdata (system-data system))
          (packages (mapcar 'first sysdata))
          (*documented-system*  system))
     (generate-system-doc system packages)
     (loop for (package . data) in sysdata :do
           (generate-package-docs system package data)))))

  
(defmethod generate-system-doc ((system system) packages)
  (ensure-directories-exist (system-path system))
  (with-open-file (*standard-output* (system-path system) :direction :output :if-exists :supersede)
    (render-system-page system packages)))

(defun doc-applicable-p (symbol definition doc)
  (and (eql symbol (name-of doc))
       (member definition (ensure-list (applicable-to doc)))))


(defmethod generate-package-docs ((system system) (package package) (docs list))
  (format t "Generate package doc for package ~A with ~D symbols.~%" package (length docs))
  (let ((*documented-package* package)
        (*package-docs* docs)
        (*hints* (system-doc-hints system package)))
    (generate-merge-pages docs)
    (dolist (doc docs)
      (let ((doc-path (document-path (name-of doc) (applicable-to doc) (package-root package))))
        (ensure-directories-exist doc-path)
        (with-open-file (*standard-output* doc-path :direction :output :if-exists :supersede)
          (render-documentation system doc))))))


(defun generate-merge-pages (docs)
  (loop :for (nil . docs) in (group-by #'name-of docs) :do
        (unless (singlep docs)
          (format t "Generate merge page for ~A~%" (name-of (first docs)))
          (generate-merge-page docs))))

(defgeneric render-documentation (system doc &optional package))


;;; COLLECTOR CLASS
(defstruct (collector (:conc-name c-))
  (values (list)))

(defun add-data (collector data)
  (nconcf (c-values collector) data))


;;; INFO CLASS.
;; This holds the basic info for forms and is converted to doc's at a later stage.
(defclass info ()
  ((name :initarg :name :accessor name-of)
   (source :initarg :source :accessor source-of)
   (declaration :initarg :declaration :accessor declaration-of)))

(defmethod operator-of ((info info))
  (first (source-of info)))

(defmethod package-of ((info info))
  (let ((name (name-of info)))
    (cond ((symbolp name) (symbol-package name))
          ((and (consp name) (eql (car name) 'setf))
           (symbol-package (second name)))
          (t (error "Don't know how to find a name for ~S." name)))))

(defmethod type-for ((info info))
  (first (source-of info)))

(defmethod print-object ((info info) stream)
  (print-unreadable-object (info stream)
    (format stream "~A ~A" (type-for info) (name-of info))))

;; DOC CLASS


(defun create-source-doc (symbol type)
  (make-instance (cdr (assoc type *type-mapping*))
                 :name symbol))

(defclass doc ()
  ((name :initarg :name :accessor name-of)
   (source :initarg :source :accessor source-of :initform :unknown)
   (pretty-name :initarg :pretty-name :reader pretty-name-of)
   (declaration :initarg :declaration :accessor declaration-of :initform (make-hash-table)))
  (:documentation "The root documentation class."))

(defmethod  applicable-to ((doc doc))
  (car (rassoc (class-name (class-of doc)) *type-mapping*)))

(defmethod display-source-of ((doc doc))
  (source-of doc))

(defmethod source-code-of ((doc doc) &optional (package *package*))
  (as-human-lisp (display-source-of doc) package))

(defmethod print-object ((doc doc) stream)
  (print-unreadable-object (doc stream)
    (format stream "~A ~A" (class-name (class-of doc)) (name-of doc))))

(defmethod initialize-instance :after ((doc doc) &rest args &key info)
  (when info
    (setf (source-of doc) (source-of info)
          (declaration-of doc) (declaration-of info)
          (name-of doc) (name-of info))))

(defmethod package-for ((doc doc))
  (symbol-package (name-of doc)))

;;FUNCTIONS
(defclass funcallable-doc (doc)
  ((setfable :accessor setfable :initform nil :initarg :setfable)))

(defclass function-doc (funcallable-doc) ()
  (:default-initargs :pretty-name "Function"))

(defmethod display-source-of ((doc function-doc))
  (destructuring-bind (op name arglist . body) (source-of doc)
    `(,op ,name ,arglist ,@(parse-body body :documentation t))))

(defmethod lambda-list-of ((doc funcallable-doc))
  (if (eql (source-of doc) :unknown)
      (function-lambda-list (name-of doc))
      (third (source-of doc))))

(defmethod source-of ((doc funcallable-doc))
  (if (eql (call-next-method) :unknown)
      (list 'defun (name-of doc) '(:unknown) :unknown)
      (call-next-method)))
  

(defmethod declaration-of ((doc function-doc))
  (let ((*current-declarations* (copy-declarations (slot-value doc 'declaration))))
    (merge-all-declarations (multiple-value-bind (body declarations docstring)
                                (parse-body (cdddr (source-of doc)) :documentation t)
                              (declare (ignore body docstring))
                              declarations))))

(defclass generic-fuction-doc (funcallable-doc)
  ((methods :accessor methods-of :initarg :methods :initform nil))
  (:default-initargs :pretty-name "Generic Function"))

(defmethod docstring ((doc generic-fuction-doc))
  (let ((gf (fdefinition (name-of doc))))
    (format nil "~A~@[~%~{~A~^~%~}~]" (or (documentation gf t) "")
            (remove nil (mapcar (rcurry 'documentation t) (generic-function-methods gf))))))

(defclass method-doc (funcallable-doc) ())

(defun unknown-source-p (doc)
  (eql (slot-value doc 'source) :unknown))

(defun has-specializer-p (gf-doc)
  (symbolp (third (source-of gf-doc))))

(defmethod lambda-list-of ((doc generic-fuction-doc))
  (cond ((unknown-source-p doc) '(:unknown))
        ((has-specializer-p doc) (fourth (source-of doc)))
        (t (third (source-of doc)))))

(defclass macro-doc (funcallable-doc) ()
  (:default-initargs :pretty-name "Macro"))

;; HANDLES DEFVAR DEFPARAMATER AND DEFCONSTANT
(defclass special-var-doc (doc) ()
  (:default-initargs :pretty-name "Special Variable"))

(defclass constant-doc (special-var-doc) ()
  (:default-initargs :pretty-name "Constant"))

(defclass class-doc (doc) ()
  (:default-initargs :pretty-name "Class"))
(defclass defclass-doc (class-doc) ())
(defclass condition-doc (class-doc) ()
  (:default-initargs :pretty-name "Condition"))
(defclass type-doc (doc) ()
  (:default-initargs :pretty-name "Type"))
(defclass struct-doc (doc) ()
  (:default-initargs :pretty-name "Structure"))

;; internal usage
(defclass package-doc (doc) ()
  (:default-initargs :pretty-name "Package"))
(defmethod package-of ((doc package-doc))
  (find-package :cl-user))
(defmethod package-for ((doc package-doc))
  (find-package (name-of doc)))


;; DOCSTRING FROM DOC OBJECTS
(defgeneric docstring (doc)
  (:method ((doc doc))
   (documentation (name-of doc) (doc-type-of doc)))
  (:method ((doc package-doc))
   (documentation (find-package (name-of doc)) t))
  (:method ((doc class-doc))
   (documentation (find-class (name-of doc)) 'type)))

(defgeneric doc-type-of (doc)
  (:method ((doc type-doc)) 'type)
  (:method ((doc struct-doc)) 'structure)
  (:method ((doc special-var-doc)) 'variable)
  (:method ((doc funcallable-doc)) 'function))

(defun add-doc-method (generic-doc info)
  (pushnew (make-instance 'method-doc :info info)
           (methods-of generic-doc))
  generic-doc)

(defun ensure-generic-doc-info (info current-docs)
  (or (find-if (lambda (doc) (and (typep doc 'generic-fuction-doc)
                                  (eql (name-of doc) (name-of info))))
               current-docs)
      (make-instance 'generic-fuction-doc :info info)))

(defmethod info->doc ((info info)  current-docs)
  (let ((type (type-for info)))
    (when (or (defpackage-operator-p info) (exportedp (name-of info)))
      (case type
        (defpackage (make-instance 'package-doc :info info))
        (define-condition (make-instance 'condition-doc :info info))
        (defstruct (make-instance 'struct-doc :info info))
        (deftype (make-instance 'type-doc :info info))
        (defclass (make-instance 'defclass-doc :info info))
        (defvar (make-instance 'special-var-doc :info info))
        (defparameter (make-instance 'special-var-doc :info info))
        (defconstant (make-instance 'constant-doc :info info))
        (defun (make-instance 'function-doc :info info))
        (defmacro (make-instance 'macro-doc :info info))
        (defmethod (add-doc-method (ensure-generic-doc-info info current-docs)
                                   info))
        (defgeneric (make-instance 'generic-fuction-doc :info info))
        (t (format t "Ignoring ~S.~%" info))))))

(defun convert-info-to-docs (info-list)
  (let ((collected-docs ()))
    (dolist (info info-list)
      (when-let (doc (info->doc info collected-docs))
        (pushnew doc collected-docs)))
    (nreverse collected-docs)))



;;; MAPPING OVER COMPONENTS OF SYSTEMS
(defmacro do-components ((var system type) &body body)
  `(loop named ,(gensym) for ,var in (all-files ,system :type ,type)
         :do (progn ,@body)))

(defun walk-system (system)
  (let ((collected-data ()))
    (do-components (comp system 'lisp-source-file)
      (when (component-exists-p comp)
        (nconcf collected-data (c-values (process-file (input-file comp))))))
    collected-data))



(defmethod system-data ((system symbol))
  (system-data (find-system system)))

(defmethod system-data ((system system))
  (let* ((data (group-by #'package-for (convert-info-to-docs (walk-system system)))))
    (loop for (package . docs) in data
          :collect (cons package (sort (merge-missing-symbols package docs) 'doc-entry<)))))

(defun merge-missing-symbols (package docs)
  (let ((all-symbols (symbols-to-document package))
        (new ()))
    (loop for (symbol types) in all-symbols :do
          (dolist (type types)
            (unless (documentation-present symbol type docs)
              (format t "Add new doc for ~A ~A~%" symbol type)
              (push (create-source-doc symbol type) new))))
    (append new docs)))

(defun documentation-present (symbol type docs)
  "Returns true if the list DOCS contains an entry for SYMBOL and TYPE.
SYMBOL is expected to be an entry in *type-mapping* ."
  (check-type symbol symbol)
  (find-if #'(lambda (doc)
               (and (eql symbol (name-of doc))
                    (eql (applicable-to doc) type)))
           docs))


;; WALKING FILES

(defun collect (&rest data)
  (check-type *collector* collector "a collector. It should be bound to an instance of COLLECTOR")
  (add-data *collector* data))

(defmacro do-forms ((var file) &body body)
  (with-gensyms (gstream geof gloop gstart)
    `(let ((*package* *package*)
           (*current-declarations* (make-hash-table))
           (,geof (gensym)))
       (with-open-file (,gstream ,file)
         (tagbody
          ,gstart
          (loop :named ,gloop
                :for ,var = (restart-case (read ,gstream nil ,geof)
                              (contine () :report "Ignore form"
                                (go ,gstart)))
                :until (eql ,var ,geof)
                :when (in-package-form-p ,var)
                :do (setf *package* (find-package (in-package-name ,var))) :end
                :do (progn ,@body)))))))
 
(defun process-file (file)
  (let ((*collector* (make-collector)))
    (handler-bind ((condition (lambda (c) (when-let (restart (find-restart 'continue c))
                                            (invoke-restart restart)))))
      (do-forms (form file)
        (process-form form))
      *collector*)))


(defun process-form (form)
  (when-let (processor (doc-type-processor form))
    (invoke-processor processor form)))



;;; DOCUMENTATION PROCESSORS
;; OUR PROCESSOR CLASS.


(defstruct (doc-processor (:conc-name dp-)) name test processor)

(defmacro define-doc-processor (name (var) &body body)
  (destructuring-bind (&key processor matcher) body
    `(register-processor ',name
                         (make-doc-processor :name ',name
                                             :test #'(lambda (,var) (declare (ignorable ,var)) ,matcher)
                                             :processor #'(lambda (,var) (declare (ignorable ,var)) ,processor)))))

(defmacro define-simple-doc-processor (name (var) &body body)
  `(define-doc-processor ,name (,var) :matcher (eql (car ,var) ',name) ,@body ))

(defun register-processor (name processor)
  (setf (gethash name *registered-processors*) processor))

(defun doc-type-processor (form)
  (loop for processor being the hash-values of  *registered-processors*
        :thereis (processor-accepts-p processor form )))

(defun processor-accepts-p (processor form)
  (when (matchesp (dp-test processor) form)
    processor))

(defun matchesp (test form)
  (and (consp form)
       (funcall test form)))

(defun invoke-processor (processor form)
  (funcall (dp-processor processor) form))



;; THE PROCESSORS THEMSELVES
;; We want to descend into some forms
(defmacro define-toplevel-form (name accessor)
  "Defines a special form which, when appearing at the toplevel, has the forms it contains
processed. ACCESSOR is used to determine which forms are processed."
  `(define-simple-doc-processor ,name (form)
     :processor (let ((forms (funcall (coerce ,accessor 'function) form)))
                  (mapcar 'process-form forms))))

(define-toplevel-form eval-when 'cddr)
(define-toplevel-form progn 'cdr)
;; macro expansions should be toplevel forms too
;; add macrolet & symbol-macrolet.


(defun merge-all-declarations (all-decls)
  (reduce (lambda (a b)
            (merge-declarations b a))
          (mapcar 'rest all-decls)
          :initial-value *current-declarations*))

(define-simple-doc-processor locally (form)
  :processor (multiple-value-bind (body declarations) (parse-body (cdr form))
               (let ((*current-declarations* (merge-all-declarations declarations)))
                 (mapcar 'process-form body))))


(define-simple-doc-processor declaim (form)
  :processor (progn (mapcar 'process-declaration (rest form))
               (collect (make-instance 'info :name 'declaration
                                       :source form
                                       :declaration form))))

;; We process proclamations as well provided that the forms are constant.
(define-simple-doc-processor proclaim (form)
  :processor (progn
               (dolist (proclamation (rest form))
                 (when (constantp proclamation)
                   (process-declaration (if (eql (car proclamation) 'quote)
                                            (second proclamation)
                                            proclamation))))
               (collect (make-instance 'info :name 'declaration
                                       :source form
                                       :declaration form))))



(define-doc-processor known-definitions (form)
  :matcher (member (first form) *recognized-forms*)
  :processor (when (or (eql (car form) 'defpackage)
                       (and (symbolp (second form))  ;;exclude (defun (setf <name>))
                            (exportedp (second form) :from *package*)))
               (collect (make-instance 'info
                                       :name (second form)
                                       :source form
                                       :declaration (copy-declarations *current-declarations*)))))

;; EOF


