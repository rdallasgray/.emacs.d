(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/graphene/")
;;(add-to-list 'load-path "~/.emacs.d/pallet/")
(add-to-list 'load-path "~/.emacs.d/readline-complete/")

;; add melpa, marmalade
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade"  . "http://marmalade-repo.org/packages/") t)

;; init packages
(package-initialize)

(setq warning-minimum-level :error)

(require 'graphene)
(require 'pallet)
(require 'uniquify)

(if (eq system-type 'windows-nt)
    (require 'smart-tab)
    (add-hook 'shell-mode-hook (lambda () (smart-tab-mode t)))
  ;; AC for shell-mode
  (setq explicit-shell-file-name "bash")
  (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
  (setq comint-process-echoes t)
  (require 'readline-complete)
  (add-to-list 'ac-modes 'shell-mode)
  (add-hook 'shell-mode-hook 'ac-rlc-setup-sources))

;; Uniquify buffers
(setq uniquify-buffer-name-style 'forward)

;; Add de facto prog-mode hooks
(setq graphene-prog-mode-hooks
      (append
       '(coffee-mode-hook sws-mode-hook css-mode-hook sgml-mode-hook html-mode-hook)
       graphene-prog-mode-hooks))

;; Use Alt-3 1o insert a #, unbind right alt
(fset 'insert-pound "#")
(define-key global-map "\M-3" 'insert-pound)
(setq ns-right-alternate-modifier nil)

;; Don't resize the minibuffer
(setq resize-mini-windows nil)

;; Sensible window movement
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; CoffeeScript 4-space tabs (for Huzu)
(setq coffee-tab-width 4)
(add-hook 'coffee-mode-hook (lambda ()
                              (setq default-tab-width 4)
                              (exec-path-from-shell-getenv "COFFEELINT_CONFIG")))

;; Add eco/jeco to mweb-filename-extensions
(setq mweb-filename-extensions
      (append '("eco" "jeco")
              mweb-filename-extensions))

; AC
(setq ac-disable-faces nil)

;; RSense
(setq rsense-home "/usr/local/Cellar/rsense/0.3/libexec")
(add-to-list 'load-path (concat rsense-home "/etc"))
(require 'rsense)

(add-hook 'ruby-mode-hook
          (lambda () (add-to-list 'ac-sources 'ac-source-rsense)))

;; Flycheck
(require 'flycheck)
(add-hook 'graphene-prog-mode-hook 'flycheck-mode)
(setq flycheck-highlighting-mode nil)
(require 'flymake-cursor)

;; Mark word, sexp, line, ...
(require 'expand-region)
(global-set-key (kbd "C-=")
                'er/expand-region)
(global-set-key (kbd "C--")
                'er/contract-region)

;; Easier sexp navigation
(global-set-key (kbd "M-n") 'forward-sexp)
(global-set-key (kbd "M-p") 'backward-sexp)

;; auto markdown-mode
(push '("\\.md\\'" . markdown-mode) auto-mode-alist)

;; auto stylus-mode
(push '("\\.styl\\'" . jade-mode) auto-mode-alist)

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
