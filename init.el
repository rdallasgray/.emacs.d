;; add melpa, marmalade
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives  '("marmalade"  . "http://marmalade-repo.org/packages/") t)

;; init packages
(package-initialize)

;; my theme
(load-theme 'blunted t)

;; recent M-x commands a la ido
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(require 'tabbar)
(require 'sr-speedbar)

;; nicer scrolling
(setq redisplay-dont-pause t)
(defun up-slightly (amt) "Scroll up a bit"  (scroll-up amt))
(defun down-slightly (amt) "Scroll down a bit" (scroll-down amt))
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

(global-set-key (kbd "C-x l") 'mark-visual-line-anywhere)

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

(global-set-key (kbd "C-x w") 'mark-word-anywhere)


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
(setq cua-enable-cua-keys nil) ;; only for rectangles
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
(fset 'insertPound "#")
(define-key global-map "\M-3" 'insertPound)

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

;; step into CamelCase
(global-subword-mode 1)

;; line numbers 
(add-hook 'find-file-hook (lambda () (linum-mode 1)))

;; nicer line wrapping
(add-hook 'find-file-hook (lambda () (visual-line-mode 1)))

;; kill whole lines with CR
(setq kill-whole-line t)

;; icomplete
(icomplete-mode 1) 

;; turn off warnings
(setq warning-minimum-level :error)

;; no toolbar
(tool-bar-mode -1)

;; non-blinking cursor
(blink-cursor-mode -1)

;; flymake everywhere
(flymake-mode 1)

;; ido
(setq ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-everywhere t)
(ido-mode 1)

(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("html" "phtml"))
(multi-web-global-mode 1)


(when window-system
  ;; * -MAKER-FAMILY-WEIGHT-SLANT-WIDTHTYPE-STYLE-PIXELS-HEIGHT-HORIZ-VERT-SPACING-WIDTH-CHARSET *
  (setq default-frame-alist '((width . 200)(height . 80)(font . "-apple-Menlo-*-*-*-*-12-*-*-*-*-*-utf-8"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#f3f3f3" "#23d" "#834" "#288" "#56d" "#950" "#56d" "#333"])
 '(custom-safe-themes (quote ("dcfaff781574c2aae079365a8a9f9bdcb206acab1c3c841c00c9b3b4e78aba6d" "591ac6117f76fc697f613eb6d29510a890e1d376c86f40c1aa51b8f97898a781" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" default)))
 '(fci-rule-color "#ddd")
 '(fringe-mode (quote (nil . 0)) nil (fringe))
 '(nav-width 25))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'ido-complete 'disabled nil)
(put 'ido-exit-minibuffer 'disabled nil)
