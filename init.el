(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/non-elpa/")
(let ((default-directory "~/.emacs.d/non-elpa/"))
  (normal-top-level-add-subdirs-to-load-path))

(add-to-list 'custom-theme-load-path "~/.emacs.d/non-elpa/theme/")

;; byte-compile .el files on saving
(add-hook 'emacs-lisp-mode-hook '(lambda ()
                                   (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t)))

;; add melpa, marmalade
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade"  . "http://marmalade-repo.org/packages/") t)

;; init packages
(package-initialize)

;; mainly for sudo editing
(require 'tramp)

;; get flyspell working
(setq ispell-program-name "/usr/local/bin/ispell")

;; start flymake, autocomplete in prog modes
(add-hook 'prog-mode-hook (progn
                            (lambda () (flymake-mode t))
                            (lambda () (auto-complete))))

;; default fonts
(set-face-attribute 'variable-pitch nil :family "Lucida Sans")
(set-face-attribute 'fixed-pitch nil :family "Menlo")

;; scroll bars off by default (don't want them in speedbar)
(scroll-bar-mode -1)

;; no toolbar
(tool-bar-mode -1)

;; non-blinking cursor
(blink-cursor-mode -1)

;; docked speedbar
(require 'sr-speedbar)

(setq speedbar-hide-button-brackets-flag t
      speedbar-show-unknown-files t
      speedbar-smart-directory-expand-flag t
      speedbar-use-images nil
      speedbar-indentation-width 2
      speedbar-update-flag nil
      sr-speedbar-auto-refresh nil
      sr-speedbar-right-side nil)

(add-hook 'speedbar-reconfigure-keymaps-hook
          '(lambda ()
             (define-key speedbar-key-map [S-up] 'speedbar-up-directory)
             (define-key speedbar-key-map [right] 'speedbar-flush-expand-line)
             (define-key speedbar-key-map [left] 'speedbar-contract-line)
             (define-key speedbar-key-map (kbd "<kp-enter>") 'speedbar-item-rename)
             (define-key speedbar-key-map (kbd "<s-backspace>") 'speedbar-item-delete)
             (define-key speedbar-key-map (kbd "<s-i>") 'speedbar-item-info)
             (define-key speedbar-key-map (kbd "<s-r>") 'speedbar-refresh)
             ))

(add-hook 'speedbar-mode-hook '(lambda () (hl-line-mode 1)))

;; recent M-x commands a la ido
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(require 'auto-complete-config)
(ac-config-default)

;; nicer scrolling with mouse wheel/trackpad
(setq redisplay-dont-pause t)
(defun up-slightly (amt) "Scroll up a bit"  (scroll-up-command amt))
(defun down-slightly (amt) "Scroll down a bit" (scroll-down-command amt))
(global-set-key [wheel-down] (lambda () (interactive) (up-slightly 1)))
(global-set-key [wheel-up] (lambda () (interactive) (down-slightly 1)))
(global-set-key [double-wheel-down] (lambda () (interactive) (up-slightly 2)))
(global-set-key [double-wheel-up] (lambda () (interactive) (down-slightly 2)))
(global-set-key [triple-wheel-down] (lambda () (interactive) (up-slightly 4)))
(global-set-key [triple-wheel-up] (lambda () (interactive) (down-slightly 4)))

;; quickly create new buffers with s-n
(defun create-new-buffer ()
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*new*")))
(global-unset-key (kbd "s-n"))
(global-set-key (kbd "s-n") 'create-new-buffer)

;; mark word, sexp, line, ...
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;; add semicolon at end of line
(defun insert-semicolon-at-end-of-line ()
  (interactive)
  (end-of-line)
  (insert ";"))
(global-set-key (kbd "C-;") 'insert-semicolon-at-end-of-line)

;; sane forward-/backward-word (requires thingatpt)
;; (global-set-key "\M-f" 'forward-same-syntax)
;; (global-set-key [(meta right)] 'forward-same-syntax)

;; (global-set-key "\M-b" (lambda() (interactive) (forward-same-syntax -1)))
;; (global-set-key [(meta left)] (lambda() (interactive) (forward-same-syntax -1)))

;; (defun kill-syntax (&optional arg) "Kill ARG sets of syntax characters after point."
;;   (interactive "p")
;;   (let ((opoint (point)))
;;     (forward-same-syntax arg)
;;     (kill-region opoint (point))))

;; (global-set-key "\M-d" 'kill-syntax)
;; (global-set-key [(meta backspace)] (lambda() (interactive) (kill-syntax -1)))

;; sensible defaults
(setq inhibit-startup-message t
      color-theme-is-global t
      uniquify-buffer-name-style 'forward)

;; don't ask me what buffer to kill
(defun kill-buffer-with-fire ()
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))
(global-set-key (kbd "C-x k") 'kill-buffer-with-fire)
(global-set-key (kbd "s-w") 'kill-buffer-with-fire)


;; autopair gives closer-to-textmate functionality than the built-in electric modes
(require 'autopair)
(autopair-global-mode)

;; autorevert all buffers
(global-auto-revert-mode t)

;; cua-mode -- only for rectangles, and to have something like delete-selection-mode that's compatible with autopair
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; newline anywhere (after cua because of C-RET)
(defun newline-anywhere ()
  (interactive)
  (end-of-line)
  (newline))
(global-set-key [(meta return)] 'newline-anywhere)

;; always autoindent new lines 
(define-key global-map (kbd "RET") 'newline-and-indent)

;; hippie-expand everywhere
;; (setq hippie-expand-try-functions-list
;;       '(try-complete-file-name-partially
;;         try-complete-file-name
;;         try-expand-dabbrev
;;         try-expand-dabbrev-all-buffers
;;         try-expand-dabbrev-from-kill
;;         try-expand-all-abbrevs
;;         try-expand-list
;;         try-expand-line
;;         try-complete-lisp-symbol-partially
;;         try-complete-lisp-symbol))

;; (require 'smart-tab)
;; (global-smart-tab-mode 1)
;; (setq smart-tab-using-hippie-expand t)

;; Use Alt-3 1o insert a #, unbind right alt
(fset 'insert-pound "#")
(define-key global-map "\M-3" 'insert-pound)
(setq ns-right-alternate-modifier nil)

;; delete files by moving them to the OS X trash
(setq delete-by-moving-to-trash t)

;; Don't make me type out 'yes' and 'no'
(fset 'yes-or-no-p 'y-or-n-p)

;; show matching parens
(show-paren-mode 1)
(setq show-paren-delay 0)

;; stop creating backup~ files
(setq make-backup-files nil)

;; stop creating #autosave# files
(setq auto-save-default nil)

;; php+, 2-space indent
(require 'php+-mode)
(php+-mode-setup)
(setq php-basic-offset 2)

;; auto markdown-mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; tabs
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; apply syntax highlighting to all buffers
(global-font-lock-mode t)

;; line numbers
(add-hook 'find-file-hook (lambda () (progn
                                       (linum-mode 1)
                                       (setq linum-format " %4d "))))

;; ido
(setq ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-everywhere t)
(ido-mode 1)

;; allow disabled commands
(put 'ido-complete 'disabled nil)
(put 'ido-exit-minibuffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'autopair-newline 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; web view editing
(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("html" "phtml"))
(multi-web-global-mode 1)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(if window-system
    (progn
      (add-to-list 'default-frame-alist '(width .  180))
      (add-to-list 'default-frame-alist '(height .  70))
      (add-to-list 'default-frame-alist '(line-spacing . 2))
      (add-to-list 'default-frame-alist '(font . "-apple-Menlo-*-*-*-*-12-*-*-*-*-*-utf-8"))
      (load-theme 'solarized-light t)
      (sr-speedbar-open))
  (progn
    (load-theme 'solarized-dark t)
    (menu-bar-mode -1)))

(load-theme 'graphene t)
(visual-line-mode 1)

;; seems to fix graphical glitches with linum
(set-fringe-mode '(8 . 0))
