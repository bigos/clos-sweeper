;;;; clos-sweeper.lisp

(in-package #:clos-sweeper)



(defun window ()
  (let ((app (gtk:make-application :application-id "org.bigos.gtk4-example.better-menu"
                                   :flags gio:+application-flags-flags-none+)))
    (window-activation app)
    (setf *app* app)

    (let ((status (gtk:application-run app nil)))
      (setf *canvas* nil
            *app*    nil)

      (gobj:object-unref app)
      status)))

(defun main ()
  (when (eq :os-windows (uiop/os:detect-os))    
    (examine-windows-system))
  (init-model)
  (window))


(defun examine-library (path label)
  (if (uiop/filesystem:file-exists-p path)
      (format t "~A library found~%" label)
      (warn "could not find the expected ~A library" label)))

;;; that is useless because the system fails before the checks are made when we do not have the mingw64/bin in the PATH
(defun examine-windows-system ()
  (format t "~&~%~%running launcher on ~A~%" (uiop/os:detect-os))

  (if (eq :os-windows (uiop/os:detect-os))
      (progn
        (format t "OS check OK~%")
        (if (or
             (search "mingw64/bin"  (uiop/os:getenv "PATH") :test #'equalp)
             (search "mingw64\\bin" (uiop/os:getenv "PATH") :test #'equalp))
            (progn
              (format t "MSYS2 bin is found in environmet variable PATH~%")
              (if (uiop/filesystem:directory-exists-p #p "c:/msys64/mingw64/bin")
                  (progn
                    (format t "MSYS2 bin folder check OK~%~%")

                    (examine-library #p "c:/msys64/mingw64/bin/libgtk-4-1.dll" "gtk")
                    (examine-library "c:/msys64/mingw64/bin/libcairo-2.dll" "cairo")
                    (examine-library "c:/msys64/mingw64/bin/libglib-2.0-0.dll" "glib")
                    (examine-library "c:/msys64/mingw64/bin/libgio-2.0-0.dll" "gio")
                    (examine-library "c:/msys64/mingw64/bin/libgirepository-1.0-1.dll" "girrepository")
                    (examine-library "c:/msys64/mingw64/bin/libgobject-2.0-0.dll" "gobject"))
                  (warn "directory not found ~A" "C:\\msys64\\mingw64\\bin")))
            (warn "MSYS2 bin path not found in the PATH environmental variable")))
      (warn "Expected Windows but found ~A" (uiop/os:detect-os))))




