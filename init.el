(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/graphene/")

;; add melpa, marmalade
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade"  . "http://marmalade-repo.org/packages/") t)

;; init packages
(package-initialize)

(setq warning-minimum-level :error)

(require 'graphene)

;; Use Alt-3 1o insert a #, unbind right alt
(fset 'insert-pound "#")
(define-key global-map "\M-3" 'insert-pound)
(setq ns-right-alternate-modifier nil)

;; php+, 2-space indent
(require 'php+-mode)
(php+-mode-setup)
(setq php-basic-offset 4)

;; Mark word, sexp, line, ...
(require 'expand-region)
(global-set-key (kbd "C-=")
                'er/expand-region)
(global-set-key (kbd "C--")
                'er/contract-region)

;; auto markdown-mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; don't compile sass/scss on saving
(setq scss-compile-at-save nil)

;; 2-space indent for CSS
(setq css-indent-offset 2)

;; Non-blinking cursor
(blink-cursor-mode -1)

;; Don't use tabs for indent; replace tabs with two spaces.
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(if window-system
    (load-theme 'solarized-light)
  ;; Dark theme in text mode
  (load-theme 'solarized-dark t))
