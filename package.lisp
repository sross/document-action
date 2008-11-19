;; Copyright (c) 2008 Sean Ross
;; 
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use,
;; copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following
;; conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;; OTHER DEALINGS IN THE SOFTWARE.
;;
(in-package :cl-user)

(defpackage :sysdef.document-action
  (:use :cl :sysdef :alexandria :cl-who :cl-ppcre)
  (:export #:document-action #:document)
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
