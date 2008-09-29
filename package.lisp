(in-package :cl-user)

(defpackage :sysdef.document-action
  (:use :cl :sysdef :alexandria :cl-who :cl-ppcre)
  (:export #:document-action)
  (:import-from  #.(package-name 
                    (or (find-package "CLOS")
                        (find-package "PCL")
                        (find-package "SB-PCL")
                        (find-package "MOP")
                        (find-package "OPENMCL-MOP")
                        (error "Can't find suitable CLOS package.")))
   :generic-function-methods
   :method-lambda-list
   :method-specializers
   :method-qualifiers))

(in-package :sysdef.document-action)


;; SPECIAL VARIABLES
(defvar *current-docs* nil "Bound to a list of the current docs when documenting a system.")
(defparameter *current-declarations* nil)
(defparameter *doc-root* (merge-pathnames (make-pathname :directory '(:relative "documentation"))
                                          *root-pathname*))

(defvar *hints* () "This variable is bound to a cached version of doc hints for the system currently being documented.")
(defvar *documented-system* nil "Bound to the current system that is being documented.")
(defvar *documented-package* nil "Bound to the current package that is being documented.")
(defvar *package-docs* nil)
(defvar *collector* nil)
(defparameter *type-mapping* '((:function . function-doc)
                               (:special-variable . special-var-doc)
                               (:constant . constant-doc)
                               (:generic-function . generic-fuction-doc)
                               (:macro . macro-doc)
                               (:class . defclass-doc)
                               (:condition . condition-doc)
                               (:type . type-doc)
                               (:structure . struct-doc)
                               (:package . package-doc)))
(defparameter *registered-processors* (make-hash-table) )
(defparameter *recognized-forms*
  '(deftype defmacro defun defgeneric defmethod defvar defparameter deftype defclass
     define-condition defconstant defstruct defpackage))
(defparameter *all-symbol-defines*
  '(:constant :special-variable :type :class :condition
    :structure :macro :function :generic-function))
(defparameter *common-lisp-declarations*
  '(declaration dynamic-extent ftype ignorable ignore inline optimize inline notinline optimize type))
(defparameter *valid-file-chars* '(#\- #\_ ))