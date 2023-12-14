(in-package #:clos-sweeper)

;; ================================= MENU ======================================
;; https://docs.gtk.org/gio/ctor.MenuItem.new_section.html ; ===================

(defun prepare-radio-action (app action-name default)
  (let ((action
          (gio:make-stateful-simple-action :name action-name
                                           :parameter-type (glib:make-variant-type
                                                            :type-string "s")
                                           :state (glib:make-string-variant
                                                   :string default))))
    (gio:action-map-add-action app action)
    (gtk4:connect action "activate"
                  (lambda (event parameter)
                    (declare (ignore event))
                    (gio:action-change-state action parameter)

                    (apply 'de-menu-radio (list action-name
                                                (glib:variant-string
                                                 (gio:action-state action))))))
    (gobj:object-unref action)))

(defun prepare-item-radio (app menu label action-name string)
  (declare (ignore app))

  (format t "preparing radio item ~S~%" (list label action-name string))

  (let ((item (gio:make-menu-item :model menu
                                  :label label
                                  :detailed-action (format nil "app.~A" action-name))))
    (setf (gio:menu-item-action-and-target-value item)
          (list (format nil  "app.~A" action-name) (glib:make-string-variant
                                       :string string)))
    item))

(defun prepare-item-bool (app menu label action-name default &key (disabled nil))
  (let ((action (gio:make-stateful-simple-action :name action-name
                                                 :parameter-type nil
                                                 :state (glib:make-boolean-variant
                                                         :value default))))
    (when disabled (setf (gio:simple-action-enabled-p action) nil))
    (gio:action-map-add-action app action)

    (gtk:connect action "activate"
                 (lambda (event parameter)
                   (declare (ignore event parameter))
                   (gio:action-change-state action (glib:make-boolean-variant
                                                    :value (if (zerop (glib:variant-hash (gio:action-state action)))
                                                               T
                                                               nil)))
                   (apply 'de-menu-bool
                          (list action-name
                                (glib:variant-hash (gio:action-state action))))))
    (gobj:object-unref action))

  (gio:make-menu-item :model menu
                      :label label
                      :detailed-action (format nil  "app.~A" action-name)))

(defun prepare-item-simple (app menu label action-name &key (disabled nil))
  (let ((action (gio:make-simple-action :name action-name
                                        :parameter-type nil)))
    (when disabled (setf (gio:simple-action-enabled-p action) nil))
    (gio:action-map-add-action app action)

    (gtk4:connect action "activate"
                  (lambda (event parameter)
                    (declare (ignore event parameter))

                    (apply 'de-menu-simple (list action-name))))
    (gobj:object-unref action))

  (gio:make-menu-item :model menu
                      :label label
                      :detailed-action (format nil  "app.~A" action-name)))

(defun prepare-section (label section)
  (gio:make-section-menu-item
   :label label
   :section  section))

(defun prepare-submenu (label &rest submenu-items)
  (list :submenu label
        (apply 'build-items submenu-items)))

(defun build-items (&rest items)
  (let ((submenu (gio:make-menu)))
    (apply 'build-menu submenu items)
    submenu))

(defun build-menu (submenu &rest items)
  (loop for i in items
        for item-class-string = (when (typep i 'gir::object-instance)
                                  (format nil "~A"
                                          (gir:gir-class-of i)))
        do (cond
             ((equalp item-class-string "#O<MenuItem>")
              (gio:menu-append-item submenu i))
             ((and (null item-class-string)
                   (consp i)
                   (eq :submenu (first i)))
              (gio:menu-append-submenu submenu (second i) (third i)))
             (T (error "unexpected item-class-string or option ~S" item-class-string)))))

;; ================================= MENU code ends here =======================

(defun menu-bar-menu (app)
  (let ((menu (gio:make-menu)))
    (build-menu
     menu
     (prepare-submenu
      "Game"
      (prepare-section
       nil
       (build-items
        (prepare-item-bool app menu "Dark mode" "dark-mode" nil)))
      (prepare-submenu
       "New Game"
       (prepare-section
        nil
        (progn
          (prepare-radio-action app "new-game-size" "SMALL")
          (build-items
           (prepare-item-radio app menu "Small 8x8"    "new-game-size" "SMALL")
           (prepare-item-radio app menu "Medium 16x16" "new-game-size" "MEDIUM")
           (prepare-item-radio app menu "Large 32x32"  "new-game-size" "LARGE")))))
      (prepare-section
       nil
       (build-items
        (prepare-item-simple app menu "Quit" "quit"))))
     (prepare-submenu
      "Help"
      ;; for now I plan to have only the About menu item
      ;; (prepare-section
      ;;  nil
      ;;  (build-items
      ;;   (prepare-item-simple app menu "Help" "help")
      ;;   (prepare-item-simple app menu "Tutorial" "tutorial" :disabled T)))
      (prepare-section
       nil
       (build-items
        (prepare-item-simple app menu "About" "about")))))

    (values menu)))
