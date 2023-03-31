(defsystem cl-gio
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "lgpl3"
  :description "GIO bindings for Common Lisp."
  :homepage "https://github.com/BohongHuang/cl-glib"
  :bug-tracker "https://github.com/BohongHuang/cl-glib/issues"
  :source-control (:git "https://github.com/BohongHuang/cl-glib.git")
  :pathname "gio"
  :components ((:file "package"))
  :depends-on (#:cl-gobject-introspection-wrapper))

(uiop:register-image-restore-hook
 (lambda ()
   (let* ((namespace "Gio")
          (package (find-package (string-upcase namespace))))
     (when package
       (setf (symbol-value (find-symbol "*NS*" package))
             (uiop:symbol-call :gir :require-namespace namespace))))))
