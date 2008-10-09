(in-package :sysdef-user)

(define-system :document-action (serial-system)
  (:version 0 1 2)
  (:components "package" "utils" "action" "render")
  (:requires :alexandria :cl-who :cl-ppcre)
  (:preferences "~/.mudballs.prefs"))
