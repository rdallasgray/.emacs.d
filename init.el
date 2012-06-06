(add-to-list 'load-path "~/.emacs.d")

;; byte-compile .el files on saving
(add-hook 'emacs-lisp-mode-hook '(lambda ()
  (add-hook 'after-save-hook 'emacs-lisp-byte-compile t t))
)

;; add melpa, marmalade
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade"  . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)

;; init packages
(package-initialize)

;; mainly for sudo editing
(require 'tramp)

;; get flyspell working
(setq ispell-program-name "/usr/local/bin/ispell")

;; default fonts
(set-face-attribute 'variable-pitch nil :family "Lucida Sans")
(set-face-attribute 'fixed-pitch nil :family "Menlo")

;; scroll bars and right fringe off by default (don't want them in speedbar)
(scroll-bar-mode -1)
(fringe-mode 'left-only)

;; right scroll bar when visiting file
(add-hook 'find-file-hook (lambda () (setq vertical-scroll-bar 'right)))

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
      sr-speedbar-right-side nil
      sr-speedbar-max-width 21)

;(global-set-key (kbd "C-x C-x") (lambda () (interactive) (if (sr-speedbar-window-p)
;							     (other-window 1)
;							   (sr-speedbar-select-window))))

;; recent M-x commands a la ido
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

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

;; mark a line 
(defun mark-visual-line-anywhere ()
  "Mark an entire visual line, with point anywhere in the line."
  (interactive)
  (deactivate-mark)
  (push-mark (beginning-of-visual-line 1))
  (activate-mark)
  (beginning-of-visual-line 2))

(global-set-key (kbd "C-c l") 'mark-visual-line-anywhere)

;; mark a word
(require 'thingatpt)

(defun mark-word-anywhere ()
  "Mark a word, with point anywhere in the word."
  (interactive)
  (deactivate-mark)
  (unless (thing-at-point 'word) (forward-word)(backward-word))
  (push-mark (beginning-of-thing 'word))
  (activate-mark)
  (end-of-thing 'word))

(global-set-key (kbd "C-c w") 'mark-word-anywhere)

;; sane forward-/backward-word (requires thingatpt)
;; DOESN'T WORK WITH CURSOR KEYS 
(global-set-key "\M-f" 'forward-same-syntax)

(global-set-key "\M-b" (lambda() (interactive) (forward-same-syntax -1)))

(defun kill-syntax (&optional arg) "Kill ARG sets of syntax characters after point."
  (interactive "p")
  (let ((opoint (point)))
    (forward-same-syntax arg)
    (kill-region opoint (point))) )

(global-set-key "\M-d" 'kill-syntax)
(global-set-key [(meta backspace)] (lambda() (interactive) (kill-syntax -1)))

;; step into CamelCase
; (global-subword-mode 1) ; have to deal with the above first



;; sensible defaults
(setq inhibit-startup-message t
      color-theme-is-global t
      uniquify-buffer-name-style 'forward)

;; don't ask me what buffer to kill
(global-set-key (kbd "C-x k") 
        '(lambda () (interactive) 
           (let (kill-buffer-query-functions) (kill-buffer))))

;; autopair gives closer-to-textmate functionality than the built-in electric modes
(require 'autopair)
(autopair-global-mode)

;; cua-mode -- only for rectangles, and to have something like delete-selection-mode that's compatible with autopair
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; always autoindent new lines 
(define-key global-map (kbd "RET") 'newline-and-indent)

;; hippie-expand everywhere
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-all-abbrevs
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))


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

;; auto php-mode
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;; auto markdown-mode
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; tabs
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

; encoding
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

; apply syntax highlighting to all buffers
(global-font-lock-mode t)

;; line numbers 
(add-hook 'find-file-hook (lambda () (linum-mode 1)))
(setq linum-format " %4d ")

;; nicer line wrapping
(add-hook 'find-file-hook (lambda () (visual-line-mode 1)))

;; kill whole lines with CR
(setq kill-whole-line t)

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
(put 'downcase-region 'disabled nil)

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

(when window-system
  (setq initial-frame-alist '((width . 180)
			      (height . 70)))
  (setq default-frame-alist '((line-spacing . 1)
                              (left-fringe . 6)
                              (right-fringe . 0)
                              (internal-border-width . 0)
                              (font . "-apple-Menlo-*-*-*-*-12-*-*-*-*-*-utf-8")))
  (load-theme 'graphene t)
  (sr-speedbar-open))
