(defun rdg/minibuffer-setup-hook ()
  (set (make-local-variable 'line-spacing) 0)
  (setq resize-mini-windows nil))

(add-hook 'minibuffer-setup-hook
          'rdg/minibuffer-setup-hook)

(add-hook 'ido-minibuffer-setup-hook
          'rdg/minibuffer-setup-hook)

(setq redisplay-dont-pause t)

(mapc (lambda (mode)
        (when (fboundp mode) (funcall mode -1)))
      '(scroll-bar-mode tool-bar-mode blink-cursor-mode))

(defvar rdg/geometry-file
  (expand-file-name ".rdg-geometry" user-emacs-directory)
  "The file where frame geometry settings are saved.")

(defun rdg/load-frame-geometry ()
  "Load saved frame geometry settings."
  (if (file-readable-p rdg/geometry-file)
      (with-temp-buffer
        (insert-file-contents rdg/geometry-file)
        (read (buffer-string)))
    '(100 40 0 0)))

(defun rdg/save-frame-geometry ()
  "Save current frame geometry settings."
  (with-temp-file rdg/geometry-file
    (print (rdg/get-geometry) (current-buffer))))

(defun rdg/get-geometry ()
  "Get the current geometry of the active frame."
  (list (frame-width) (frame-height) (frame-parameter nil 'top) (frame-parameter nil 'left)))

(defun rdg/set-geometry ()
  "Set the default frame geometry using the values loaded from rdg/geometry-file."
  (let ((geom (rdg/load-frame-geometry)))
    (let ((f-width (nth 0 geom))
          (f-height (nth 1 geom))
          (f-top (nth 2 geom))
          (f-left (nth 3 geom)))
      (setq default-frame-alist
            (append default-frame-alist
                    `((width . ,f-width)
                      (height . ,f-height)
                      (top . ,f-top)
                      (left . ,f-left)))))))

(defun rdg/look-startup-after-init ()
  "Load defaults for the overall look -- to be called after loading the init file so as to pick up custom settings."
  (if window-system
      (progn
        (rdg/set-geometry)
        (add-hook 'kill-emacs-hook 'rdg/save-frame-geometry)
        (setq-default line-spacing 2)
        (add-to-list 'default-frame-alist `(font . "Menlo-12"))
        (set-face-font 'default "Menlo-12")
        (set-face-font 'variable-pitch "Lucida Grande-12")
        (set-face-font 'fixed-pitch "Menlo-12")
        (add-to-list 'default-frame-alist '(internal-border-width . 0))
        (set-fringe-mode '(8 . 0))
        (load-theme 'rdg t)
        (defadvice load-theme
          (after load-rdg-theme (theme &optional no-confirm no-enable) activate)
          (when (not (equal theme 'rdg))
            (load-theme 'rdg t))))
    (when (not (eq system-type 'darwin))
      (menu-bar-mode -1))
    ;; Menu bar always off in text mode
    (menu-bar-mode -1)))

(add-hook 'after-init-hook 'rdg/look-startup-after-init)

(provide 'rdg-look)
