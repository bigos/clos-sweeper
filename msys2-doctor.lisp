;;; run it in the terminal with:
;;; sbcl --load  ~/Programming/Lisp/clos-sweeper/launch.lisp

(defun examine-library (path label)
  (if (uiop/filesystem:file-exists-p path)
      (format t "~A library found~%" label)
      (error "could not find the expected ~A library" label)))

(defun examine ()
  (format t "~&~%~%running launcher on ~A~%" (uiop/os:detect-os))

  (if (eq :os-windows (uiop/os:detect-os))
      (progn
        (format t "OS check OK~%")
        (if (search "msys64/mingw64/bin" (uiop/os:getenv "PATH"))
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
                  (error "directory not found ~A" "C:\\msys64\\mingw64\\bin")))
            (error "MSYS2 bin path not found in the PATH environmental variable")))
      (error "Expected Windows but found ~A" (uiop/os:detect-os)))
  ;; the end
  )

(examine)

(format t "~%done~%")
(sb-ext:quit)
