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

;; Add de facto prog-mode hooks
(setq graphene-prog-mode-hooks (append '(coffee-mode-hook css-mode-hook sgml-mode-hook html-mode-hook) graphene-prog-mode-hooks))

;; Use Alt-3 1o insert a #, unbind right alt
(fset 'insert-pound "#")
(define-key global-map "\M-3" 'insert-pound)
(setq ns-right-alternate-modifier nil)

;; Don't resize the minibuffer
(setq resize-mini-windows nil)

;; php+, 2-space indent
(require 'php+-mode)
(php+-mode-setup)
(setq php-basic-offset 2)

;; Add php+-mode to mweb-tags
(push '(php+-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>") mweb-tags)

;; YAS
(require 'yasnippet)
(yas-reload-all)
(add-hook 'graphene-prog-mode-hook (lambda () (yas-minor-mode)))

;; RSense
(setq rsense-home "/usr/local/Cellar/rsense/0.3/libexec")
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)

(add-hook 'ruby-mode-hook
          (lambda () (add-to-list 'ac-sources 'ac-source-rsense)))

;; Flycheck
(require 'flycheck)
(add-hook 'graphene-prog-mode-hook 'flycheck-mode)
(require 'flymake-cursor)

(defvar flycheck-checker-js
  '(:command
    ("jslint"
     "--terse"
     "--indent" "2"
     "--plusplus"
     "--white"
     "--continue"
     "--es5"
     "--maxerr" "100"
     "--sloppy"
     "--sub" source)
    :error-patterns
    (("^\\(.+\.js\\)\(\\([0-9]+\\)\):\\(.*\\)" 1 2 nil 3))
    :modes (javascript-mode js-mode js2-mode)))
(add-to-list 'flycheck-checkers 'flycheck-checker-js)

;; Mark word, sexp, line, ...
(require 'expand-region)
(global-set-key (kbd "C-=")
                'er/expand-region)
(global-set-key (kbd "C--")
                'er/contract-region)

;; auto markdown-mode
(push '("\\.md\\'" . markdown-mode) auto-mode-alist)

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
    (progn (message "loading solarized theme")
           (load-theme 'solarized-light))
  ;; Dark theme in text mode
  (load-theme 'solarized-dark t))
