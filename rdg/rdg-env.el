;; Make buffer names unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Don't show the startup message
(setq inhibit-startup-message t)

;; Save backup files in the temporary directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Shorten yes/no answers to y/n
(setopt use-short-answers t)
(setopt use-dialog-box nil)

;; Stop asking me about killing active processes
(setopt confirm-kill-processes nil)

;; Automatically update buffers when files change
(global-auto-revert-mode t)

;; Enable 'power user' features
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq tags-add-tables nil)

;; I don't use bidi
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; defer fontification
(setq redisplay-skip-fontification-on-input t)

;; improve lsp perf
(setq read-process-output-max (* 4 1024 1024)) ; 4MB
;; ignore non-selected windows
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; kill
(setq save-interprogram-paste-before-kill t)
(setq kill-do-not-save-duplicates t)

(setq savehist-additional-variables
      '(search-ring regexp-search-ring kill-ring))

(add-hook 'savehist-save-hook
          (lambda ()
            (setq kill-ring
                  (mapcar #'substring-no-properties
                          (cl-remove-if-not #'stringp kill-ring)))))

;; save sh files executable
(add-hook 'after-save-hook
          #'executable-make-buffer-file-executable-if-script-p)

;; don't ping URL-like strings in ffap
(setq ffap-machine-p-known 'reject)

;; resize all windows
(setq window-combination-resize t)

;; hide inapplicable commands in M-x
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

(provide 'rdg-env)
