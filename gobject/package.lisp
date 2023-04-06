;;;; gobject/package.lisp

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

(cl:defpackage gobject
  (:use)
  (:nicknames #:gobj)
  (:import-from #:gir-wrapper #:pointer-object #:object-pointer)
  (:export #:*ns* #:pointer-object #:object-pointer))

(cl:in-package #:gobj)

(gir-wrapper:define-gir-namespace "GObject")

(cl:defun coerce (object type)
  (pointer-object (object-pointer object) type))

(cl:export 'coerce)
