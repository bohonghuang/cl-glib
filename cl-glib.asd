(defsystem cl-glib
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "lgpl3"
  :description "GLib bindings for Common Lisp."
  :homepage "https://github.com/BohongHuang/cl-glib"
  :bug-tracker "https://github.com/BohongHuang/cl-glib/issues"
  :source-control (:git "https://github.com/BohongHuang/cl-glib.git")
  :pathname "glib"
  :components ((:file "package")
               (:file "glist" :depends-on ("package")))
  :depends-on (#:cl-gobject-introspection-wrapper #:bordeaux-threads))

(uiop:register-image-restore-hook
 (lambda ()
   (let* ((namespace "GLib")
          (package (find-package (string-upcase namespace))))
     (when package
       (setf (symbol-value (find-symbol "*NS*" package))
             (uiop:symbol-call :gir :require-namespace namespace))))))
