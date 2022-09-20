(defsystem cl-glib.gobject
  :version "1.0.0"
  :author "Bohong Huang <1281299809@qq.com>"
  :maintainer "Bohong Huang <1281299809@qq.com>"
  :license "lgpl3"
  :description "GObject binding for Common Lisp."
  :homepage "https://github.com/BohongHuang/cl-gobject"
  :bug-tracker "https://github.com/BohongHuang/cl-gobject/issues"
  :source-control (:git "https://github.com/BohongHuang/cl-gobject.git")
  :serial t
  :components ((:file "gobject"))
  :depends-on (#:cl-gobject-introspection-wrapper))

(uiop:register-image-restore-hook
 (lambda ()
   (let* ((namespace "GObject")
          (package (find-package (string-upcase namespace))))
     (when package
       (setf (symbol-value (find-symbol "*NS*" package))
             (uiop:symbol-call :gir :require-namespace namespace))))))
