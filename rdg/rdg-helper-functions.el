(defun kill-default-buffer ()
  "Kill the currently active buffer -- set to C-x k so that users are not asked which buffer they want to kill."
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(global-set-key (kbd "C-x k") 'kill-default-buffer)

(defun kill-buffer-if-file (buf)
  "Kill a buffer only if it is file-based."
  (when (buffer-file-name buf)
    (when (buffer-modified-p buf)
        (when (y-or-n-p (format "Buffer %s is modified - save it?" (buffer-name buf)))
            (save-some-buffers nil buf)))
    (set-buffer-modified-p nil)
    (kill-buffer buf)))

(defun kill-all-buffers ()
    "Kill all file-based buffers."
    (interactive)
    (mapc (lambda (buf) (kill-buffer-if-file buf))
     (buffer-list)))

(defun create-new-buffer ()
  "Create a new buffer named *new*[num]."
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*new*")))

(global-set-key (kbd "C-c n") 'create-new-buffer)

(defun comment-current-line-dwim ()
  "Comment or uncomment the current line."
  (interactive)
  (save-excursion
    (push-mark (beginning-of-line) t t)
    (end-of-line)
    (comment-dwim nil)))

(global-set-key (kbd "C-M-;") 'comment-current-line-dwim)

(defun newline-anywhere ()
  "Add a newline from anywhere in the line."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "M-RET") 'newline-anywhere)

(defun increase-window-height (&optional arg)
  "Make the window taller by one line. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window arg))

(global-set-key (kbd "C->") 'increase-window-height)

;; (defun decrease-window-height (&optional arg)
;;   "Make the window shorter by one line. Useful when bound to a repeatable key combination."
;;   (interactive "p")
;;   (enlarge-window (- 0 arg)))

;; (global-set-key (kbd "C-<") 'decrease-window-height)

;; (defun decrease-window-width (&optional arg)
;;   "Make the window narrower by one column. Useful when bound to a repeatable key combination."
;;   (interactive "p")
;;   (enlarge-window (- 0 arg) t))

;; (global-set-key (kbd "C-,") 'decrease-window-width)

;; (defun increase-window-width (&optional arg)
;;   "Make the window wider by one column. Useful when bound to a repeatable key combination."
;;   (interactive "p")
;;   (enlarge-window arg t))

;; (global-set-key (kbd "C-.") 'increase-window-width)

(when window-system
  (defun new-emacs-instance ()
    "Create a new instance of Emacs."
    (interactive)
    (let ((path-to-emacs
           (locate-file invocation-name
                        (list invocation-directory) exec-suffixes)))
      (call-process path-to-emacs nil 0 nil))))

(global-set-key (kbd "C-c N") 'new-emacs-instance)

(provide 'rdg-helper-functions)
