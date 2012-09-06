(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/graphene/")

(load-file "~/.emacs.d/private-vars.el")

;; add melpa, marmalade
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade"  . "http://marmalade-repo.org/packages/") t)

;; init packages
(package-initialize)

;; byte-compile .el files on saving
;(add-hook 'emacs-lisp-mode-hook '(lambda ()
;                                   (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t)))
(require 'graphene)

;; ;; mainly for sudo editing
;; (require 'tramp)

;; get flyspell working
(setq ispell-program-name "/usr/local/bin/ispell")

;; Use Alt-3 1o insert a #, unbind right alt
(fset 'insert-pound "#")
(define-key global-map "\M-3" 'insert-pound)
(setq ns-right-alternate-modifier nil)

;; ;; php+, 2-space indent
;; (require 'php+-mode)
;; (php+-mode-setup)
;; (setq php-basic-offset 2)

;; ;; Mark word, sexp, line, ...
;; (require 'expand-region)
;; (global-set-key (kbd "C-=")
;;                 'er/expand-region)
;; (global-set-key (kbd "C--")
;;                 'er/contract-region)

;; auto markdown-mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(if window-system
    (load-theme 'solarized-light)
  ;; Dark theme in text mode
  (load-theme 'solarized-dark t))
