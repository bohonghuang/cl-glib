;;;; gio.lisp

;;;; Copyright (C) 2022-2023 Bohong Huang
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU Lesser General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public License
;;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(cl:defpackage gio
  (:use)
  (:export #:*ns*))

(in-package #:gio)

(cl:eval-when (:execute :compile-toplevel :load-toplevel)
  (cl:setf gir-wrapper:*quoted-name-alist* `((("MenuLinkIter" . "get_next") . menu-link-iter-peek)
                                             (("MenuAttributeIter" . "get_next") . menu-attribute-iter-peek)
                                             ("file_parse_name" . file-new-with-parse-name)
                                             (("Volume" . "mount") . volume-start-mount)
                                             (("Volume" . "mount_finish") . volume-finish-mount)
                                             (("Application" . "run") . %application-run)
                                             . ,(cl:mapcan (cl:lambda (definition)
                                                             (cl:let ((name (cl:second definition)))
                                                               (cl:when (cl:stringp name)
                                                                 (cl:multiple-value-bind (replaced-name dbus-replaced-p)
                                                                     (cl-ppcre:regex-replace-all "DBus" name "Dbus")
                                                                   (cl:multiple-value-bind (replaced-name vtable-replaced-p)
                                                                       (cl-ppcre:regex-replace-all "VTable" replaced-name "Vtable")
                                                                     (cl:when (cl:or dbus-replaced-p vtable-replaced-p)
                                                                       (cl:list (cl:cons name (gir-wrapper::camel-case->lisp-symbol replaced-name)))))))))
                                                           (cl:cdr (cl:macroexpand '(gir-wrapper:define-gir-namespace "Gio")))))))

(gir-wrapper:define-gir-namespace "Gio")

(cl:defvar *application* cl:nil)

(cl:export '*application*)

(cl:defun application-run (application argv)
  (cl:let ((*application* application))
    (%application-run application argv)))

(cl:export 'application-run)

(cl:eval-when (:execute :compile-toplevel :load-toplevel)
  (cl:setf gir-wrapper:*quoted-name-alist* cl:nil))
