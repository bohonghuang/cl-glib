;;;; glib/package.lisp

;;;; Copyright (C) 2022 Bohong Huang
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

(cl:defpackage glib
  (:use)
  (:import-from #:gir-wrapper)
  (:export #:*ns*))

(cl:in-package #:glib)

(cl:eval-when (:execute :compile-toplevel :load-toplevel)
  (cl:setf gir-wrapper:*quoted-name-alist* '(("CSET_a_2_z" . +cset-a-z-lower-case+)
                                             ("CSET_A_2_Z" . +cset-a-z-upper-case+)
                                             ("t" . time)
                                             ("timeout_add")
                                             ("timeout_add_seconds")
                                             ("idle_add")
                                             (("Variant" . "new_variant") . make-variant))))

(gir-wrapper:define-gir-namespace "GLib")

(cl:defvar *objects* (cl:make-hash-table))

(cl:defvar *objects-lock* (bt:make-lock))

(cffi:defcallback funcall-object-callback :bool ((user-data :pointer))
  (cl:funcall (cl:gethash (cffi:pointer-address user-data) *objects*)))

(cffi:defcallback free-object-callback :void ((user-data :pointer))
  (bt:with-lock-held (*objects-lock*)
    (cl:remhash (cffi:pointer-address user-data) *objects*)))

(cl:defconstant +1+maxsizet+ (cl:min (cl:expt 2 (cl:* 8 (cffi:foreign-type-size :pointer))) (cl:1+ cl:most-positive-fixnum)))

(cl:deftype unsigned-unboxed-word ()
  `(cl:integer 0 ,(cl:1- +1+maxsizet+)))

(cl:defun put-object (object)
  (bt:with-lock-held (*objects-lock*)
    (cl:loop
       :with object-id :of-type unsigned-unboxed-word := 0
       :do (cl:setf object-id (cl:random +1+maxsizet+))
       :while (cl:gethash object-id *objects*)
       :finally
          (cl:setf (cl:gethash object-id *objects*) object)
          (cl:return object-id))))

(cl:defun get-object (id)
  (bt:with-lock-held (*objects-lock*)
    (cl:gethash id *objects*)))

(cl:defun timeout-add (interval function cl:&optional (priority +priority-default+))
  (gir:invoke (*ns* 'timeout-add)
              priority
              interval
              (cffi:callback funcall-object-callback)
              (cffi:make-pointer (put-object function))
              (cffi:callback free-object-callback)))

(cl:export 'timeout-add)

(cl:defun timeout-add-seconds (interval function cl:&optional (priority +priority-default+))
  (gir:invoke (*ns* 'timeout-add-seconds)
              priority
              interval
              (cffi:callback funcall-object-callback)
              (cffi:make-pointer (put-object function))
              (cffi:callback free-object-callback)))

(cl:export 'timeout-add-seconds)

(cl:defun idle-add (function cl:&optional (priority +priority-default+))
  (gir:invoke (*ns* 'idle-add)
              priority
              (cffi:callback funcall-object-callback)
              (cffi:make-pointer (put-object function))
              (cffi:callback free-object-callback)))

(cl:export 'idle-add)

(cl:eval-when (:execute :compile-toplevel :load-toplevel)
  (cl:setf gir-wrapper:*quoted-name-alist* cl:nil))
