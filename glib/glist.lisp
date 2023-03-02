;;;; glib/glist.lisp

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

(cl:in-package #:glib)

(cffi:defcstruct (%glist :class glist-type)
  "The GList struct is used for each element in a doubly-linked list."
  (data :pointer)
  (next (:pointer (:struct %glist)))
  (prev (:pointer (:struct %glist))))

(cffi:defcfun ("g_list_alloc" glist-alloc) (:pointer (:struct %glist))
  "Allocates space for one GList element.")

(cl:export 'glist-alloc)

(cffi:defcfun ("g_list_append" glist-append) (:pointer (:struct %glist))
  "Adds a new element on to the end of the list."
  (list (:pointer (:struct %glist)))
  (data :pointer))

(cl:export 'glist-append)

(cffi:defcfun ("g_list_concat" glist-concat) (:pointer (:struct %glist))
  "Adds the second GList onto the end of the first GList."
  (list1 (:pointer (:struct %glist)))
  (list2 (:pointer (:struct %glist))))

(cl:export 'glist-concat)

(cffi:defcfun ("g_list_copy" glist-copy) (:pointer (:struct %glist))
  "Copies a GList."
  (list (:pointer (:struct %glist))))

(cl:export 'glist-copy)

(cffi:defcfun ("g_list_delete_link" glist-delete-link) (:pointer (:struct %glist))
  "Removes the node link_ from the list and frees it."
  (list (:pointer (:struct %glist)))
  (link (:pointer (:struct %glist))))

(cl:export 'glist-delete-link)

(cffi:defcfun ("g_list_find" glist-find) (:pointer (:struct %glist))
  "Finds the element in a GList which contains the given data."
  (list (:pointer (:struct %glist)))
  (data (:pointer)))

(cl:export 'glist-find)

(cffi:defcfun ("g_list_foreach" %glist-foreach) :void
  "Calls a function for each element of a GList."
  (list (:pointer (:struct %glist)))
  (function :pointer)
  (user-data :pointer))

(cl:defun glist-foreach (list function)
  (cl:let ((address (put-object function)))
    (%glist-foreach list (cffi:callback funcall-object-callback) (cffi:make-pointer address))
    (cl:remhash address *objects*)))

(cl:export 'glist-foreach)

(cffi:defcfun ("g_list_free" glist-free) :void
  "Frees all of the memory used by a GList."
  (list (:pointer (:struct %glist))))

(cl:export 'glist-free)

(cffi:defcfun ("g_list_free_1" glist-free-1) :void
  "Frees one GList element."
  (list (:pointer (:struct %glist))))

(cl:export 'glist-free-1)

(cffi:defcfun ("g_list_index" glist-index) :int
  "Gets the position of the element containing the given data (starting from 0)."
  (list (:pointer (:struct %glist)))
  (data :pointer))

(cl:export 'glist-index)

(cffi:defcfun ("g_list_insert" glist-insert) (:pointer (:struct %glist))
  "Gets the position of the element containing the given data (starting from 0)."
  (list (:pointer (:struct %glist)))
  (data :pointer)
  (position :int))

(cl:export 'glist-insert)

(cffi:defcfun ("g_list_last" glist-last) (:pointer (:struct %glist))
  "Gets the last element in a GList."
  (list (:pointer (:struct %glist))))

(cl:export 'glist-last)

(cffi:defcfun ("g_list_length" glist-length) :uint
  "Gets the number of elements in a GList."
  (list (:pointer (:struct %glist))))

(cl:export 'glist-length)

(cffi:defcfun ("g_list_nth" glist-nth) (:pointer (:struct %glist))
  "Gets the element at the given position in a GList."
  (list (:pointer (:struct %glist)))
  (n :uint))

(cl:export 'glist-nth)

(cffi:defcfun ("g_list_nth_data" glist-nth-data) :pointer
  "Gets the data of the element at the given position."
  (list (:pointer (:struct %glist)))
  (n :uint))

(cl:export 'glist-nth-data)

(cffi:defcfun ("g_list_nth_prev" glist-nth-prev) (:pointer (:struct %glist))
  "Gets the element n places before list."
  (list (:pointer (:struct %glist)))
  (n :uint))

(cl:export 'glist-nth-prev)

(cffi:defcfun ("g_list_position" glist-position) :uint
  "Gets the position of the given element in the GList (starting from 0)."
  (list (:pointer (:struct %glist)))
  (llink (:pointer (:struct %glist))))

(cl:export 'glist-position)

(cffi:defcfun ("g_list_prepend" glist-prepend) (:pointer (:struct %glist))
  "Prepends a new element on to the start of the list."
  (list (:pointer (:struct %glist)))
  (data :pointer))

(cl:export 'glist-prepend)

(cffi:defcfun ("g_list_remove" glist-remove) (:pointer (:struct %glist))
  "Removes an element from a GList."
  (list (:pointer (:struct %glist)))
  (data :pointer))

(cl:export 'glist-remove)

(cffi:defcfun ("g_list_remove_all" glist-remove-all) (:pointer (:struct %glist))
  "Removes all list nodes with data equal to data."
  (list (:pointer (:struct %glist)))
  (data :pointer))

(cl:export 'glist-remove-all)

(cffi:defcfun ("g_list_remove_link" glist-remove-link) (:pointer (:struct %glist))
  "Removes an element from a GList, without freeing the element."
  (list (:pointer (:struct %glist)))
  (llink (:pointer (:struct %glist))))

(cl:export 'glist-remove-link)

(cffi:defcfun ("g_list_reverse" glist-reverse) (:pointer (:struct %glist))
  "Reverses a GList."
  (list (:pointer (:struct %glist))))

(cl:export 'glist-reverse)

(cl:defun glist-list (glist)
  (cl:loop
     :until (cffi:null-pointer-p glist)
     :collect (cffi:with-foreign-slots ((data next prev) glist (:struct %glist))
                (cl:setf glist next)
                (cl:values data))))

(cl:export 'glist-list)
