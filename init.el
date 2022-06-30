(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package)
  (require 'use-package-ensure)
  (setq use-package-always-ensure t))

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package use-package-ensure-system-package)

(use-package dash)
(use-package s)

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; No pop-ups
(setq pop-up-frames nil
      pop-up-windows nil)

(setq create-lockfiles nil)

(setq tags-revert-without-query 1)

(setq-default bidi-display-reordering nil)

(setq kill-do-not-save-duplicates t)

;; Scroll up and down a line at a time
(global-set-key (kbd "C-S-v") 'scroll-up-line)
(global-set-key (kbd "M-V") 'scroll-down-line)

;; Use Alt-3 1o insert a #, unbind right alt
(fset 'insert-pound "#")
(define-key global-map "\M-3" 'insert-pound)
(setq ns-right-alternate-modifier nil)

;; Mac-port specific key settings
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)

(setq require-final-newline t
      warning-minimum-level :error
      ring-bell-function 'ignore)

(add-hook 'before-save-hook 'whitespace-cleanup)

;; midnight
(setq clean-buffer-list-delay-general 7)
(midnight-delay-set 'midnight-delay "12:00am")

;; text scaling
(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)

(defvar rdg/ignored-compilation-buffer-match '("*RuboCop"))

(defun rdg/remove-fringe-and-margin ()
  (fringe-mode nil)
  (set-window-margins nil 0))

(defun kill-zombie-buffers ()
  "Kill buffers no longer associated with a file."
  (interactive)
  (let ((buffers (buffer-list)))
    (mapc (lambda (buf)
            (let ((filename (buffer-file-name buf)))
              (when (and filename (not (file-exists-p filename)))
                (kill-buffer buf))))
          buffers)))

;; Sensible window movement
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <left>") 'windmove-left)

(defun buf-stack (direction)
  (let* ((other-win (windmove-find-other-window direction))
         (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No window in that direction")
      (set-window-buffer other-win buf-this-buf)
      (switch-to-prev-buffer (selected-window) t))))

(defun buf-stack-left ()
  (interactive)
  (buf-stack 'left))

(defun buf-stack-right ()
  (interactive)
  (buf-stack 'right))

(defun buf-stack-up ()
  (interactive)
  (buf-stack 'up))

(defun buf-stack-down ()
  (interactive)
  (buf-stack 'down))

(defun buf-stack-set-keys()
  (global-set-key (kbd "C-c S-<left>") 'buf-stack-left)
  (global-set-key (kbd "C-c S-<right>") 'buf-stack-right)
  (global-set-key (kbd "C-c S-<up>") 'buf-stack-up)
  (global-set-key (kbd "C-c S-<down>") 'buf-stack-down))

(buf-stack-set-keys)

(defun rdg/beginning-of-next-defun ()
  (interactive)
  (end-of-defun)
  (end-of-defun)
  (beginning-of-defun))

(global-set-key (kbd "M-<down>") 'rdg/beginning-of-next-defun)
(global-set-key (kbd "M-<up>") 'beginning-of-defun)

;; No visible region on C-x C-x
(defun exchange-point-and-mark-no-region ()
  "Suppress region visibility when exchanging point and mark."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-region)

(defun rdg/maybe-write-and-close-compilation-buffer (buf str)
  (if (-any? (lambda (it) (s-contains? it (buffer-name buf)))
             rdg/ignored-compilation-buffer-match)
      (progn
        (message "Compilation: %s" str)
        (kill-buffer buf))))

(-each rdg/ignored-compilation-buffer-match
  (lambda (it) (add-to-list 'display-buffer-alist
                            `(,it (display-buffer-no-window)))))

(add-hook 'compilation-finish-functions
          'rdg/maybe-write-and-close-compilation-buffer)

(use-package treemacs)

(use-package bury-successful-compilation)

(use-package server
  :config
  (if (server-running-p)
      (message "Server is running")
    (progn
      (message "Starting server")
      (server-start))))

(use-package smartparens
  :custom (sp-escape-quotes-after-insert nil)
  :config
  (defun rdg/unwrap-and-mark-sexp (&optional arg)
    (interactive)
    (let ((sexp-info (sp-unwrap-sexp arg)))
      (goto-char (plist-get sexp-info :beg))
      (push-mark (- (plist-get sexp-info :end) 2) t t)
      (setq deactivate-mark nil)))
  (sp-pair " " " " :actions '(wrap))
  (defun rdg/wrap-and-mark-region (orig &rest args)
    (apply orig args)
    (let ((sexp-info sp-last-wrapped-region))
      (goto-char (plist-get sexp-info :beg))
      (push-mark (plist-get sexp-info :end) t t)
      (setq deactivate-mark nil)))
  (advice-add 'sp-wrap :around 'rdg/wrap-and-mark-region)
  :bind
  (("C-M-<left>" . sp-backward-sexp)
   ("C-M-<right>" . sp-forward-sexp)
   ("C-M-<up>". sp-backward-up-sexp)
   ("C-M-<down>". sp-down-sexp)
   ("C-M-k". sp-kill-sexp)
   ("C-M-w". sp-copy-sexp)
   ("C-]". sp-select-next-thing)
   ("C-)". sp-forward-slurp-sexp)
   ("C-(". sp-backward-slurp-sexp)
   ("C-}". sp-forward-barf-sexp)
   ("C-{". sp-backward-barf-sexp)
   ("M-F". sp-forward-symbol)
   ("M-B". sp-backward-symbol)
   ("C-M-<backspace>" . rdg/unwrap-and-mark-sexp)))

(use-package web-mode)

(use-package exec-path-from-shell)

(use-package speedbar
  :config
  ;; Show all files in speedbar
  ;; TODO fix this to not show files ending in ~
  (let ((re "^\\.\\.?\\(\\(DS_Store\\)\\|\\(#.+\\)\\)?$"))
    (setq speedbar-directory-unshown-regexp re
          speedbar-file-unshown-regexp re))
  (add-hook 'speedbar-mode-hook 'rdg/remove-fringe-and-margin))

(use-package sr-speedbar)

(use-package prescient
  :config (prescient-persist-mode t))

(use-package so-long
  :straight t
  :custom
  (so-long-threshold 500)
  (so-long-max-lines nil)
  :config
  (defvar so-long-target-modes)
  (add-to-list 'so-long-target-modes 'shell-mode)
  (global-so-long-mode t))

(use-package visual-regexp)

(use-package anzu
  :custom
  (anzu-mode-lighter "")
  (anzu-deactivate-region t)
  (anzu-replace-to-string-separator " => ")
  :config
  (global-anzu-mode t)
  (set-face-attribute 'anzu-mode-line nil
                      :foreground 'unspecified
                      :inherit 'company-tooltip-search)
  (define-key (current-global-map) [remap query-replace]
    #'anzu-query-replace)
  (define-key (current-global-map) [remap query-replace-regexp]
    #'anzu-query-replace-regexg)
  (define-key isearch-mode-map [remap isearch-query-replace]
    #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp]
    #'anzu-isearch-query-replace-regexp))

(use-package native-complete
  :custom (native-complete-style-regex-alist '((".+*(pry|guard).*> " . tab))))
(use-package company-try-hard)
(use-package company-prescient)
(use-package company
  :config
  (defvar rdg/company-no-return-key-modes '(shell-mode))
  (defun rdg/company-complete-on-return-p ()
    (not (derived-mode-p 'comint-mode)))
  (defun rdg/company-maybe-try-hard (buf win tick pos)
    (when (and (not company-candidates)
               (looking-back "[A-Za-z0-9_\-\/\.]" 1))
      (company-try-hard)
      (let ((this-command 'company-try-hard))
        (company-post-command))))
  (defun rdg/company-maybe-complete-on-return ()
    (interactive)
    (if (rdg/company-complete-on-return-p)
        (company-complete-selection)
      (if (functionp 'comint-send-input)
          (comint-send-input)
        (newline))))
  (defun rdg/advise-company-try-hard ()
    (advice-remove 'company-idle-begin
                   #'rdg/company-maybe-try-hard)
    (advice-add 'company-idle-begin :after
                #'rdg/company-maybe-try-hard))
  (defvar rdg/company-default-backends
    '(company-files company-dabbrev-code company-etags
                    company-capf company-keywords
                    company-dabbrev))
  (defvar rdg/company-shell-backends
    '(company-files company-native-complete company-capf
                    company-dabbrev))
  (defun rdg/company-set-mode-backends (backends)
    "Set BACKENDS locally"
    (set (make-local-variable 'company-backends)
         (-uniq (append backends rdg/company-default-backends))))
  (setq company-backends rdg/company-default-backends
        company-idle-delay 0.15)
  (rdg/advise-company-try-hard)
  (company-prescient-mode t)
  (define-key company-active-map (kbd "<tab>")
    #'company-complete-selection)
  (define-key company-active-map (kbd "C-<tab>")
    #'company-try-hard)
  (define-key company-active-map [return]
    #'rdg/company-maybe-complete-on-return)
  (define-key company-active-map (kbd "RET")
    #'rdg/company-maybe-complete-on-return))
    (add-to-list 'display-buffer-alist '("*shell*" display-buffer-same-window))

(use-package wgrep)

(use-package counsel
  :bind
  (("C-s" . counsel-grep-or-swiper)
   ("C-r" . counsel-grep-or-swiper)
   ("C-x f" . counsel-recentf)
   ("C-c C-s" . isearch-forward)
   ("C-c C-r" . isearch-backward)
   ("C-c C-r" . ivy-resume)
   ("C-c u" . counsel-unicode-char)
   ("C-c g c" . counsel-git-checkout)
   ("C-c g f" . counsel-git)
   ("C-c g g" . counsel-git-grep)
   ("C-c e f" . counsel-etags-find-tag)
   ("C-c ! !" . counsel-flycheck)
   ("C-c //" . counsel-tramp)))

(use-package counsel-etags)

(use-package ivy-prescient)
(use-package ivy
  :config
  (ivy-prescient-mode t))

(use-package hydra)

(use-package expand-region
  :config
  (defun hydra-mark-begin (&optional arg)
    (interactive "p")
    (er/expand-region arg)
    (hydra-mark/body))
  (defhydra hydra-mark (global-map "C-M-SPC")
    "Mark"
    ("+" er/expand-region "Expand")
    ("-" er/contract-region "Contract"))
  (global-set-key (kbd "C-M-SPC") 'hydra-mark-begin))

;; (use-package shell-pop
;;   :custom
;;   (shell-pop-universal-key "C-c `")
;;   (shell-pop-window-position "bottom")
;;   :bind ("C-c `" . shell-pop))

(use-package multi-vterm
  :custom (multi-vterm-dedicated-window-height 20)
  :config
  (defun rdg/multi-vterm-dwim ()
    (interactive)
    "Toggle the dedicated window or create a new vterm."
    (if (multi-vterm-dedicated-exist-p)
        (multi-vterm)
      (multi-vterm-dedicated-open)))
  (global-set-key (kbd "C-c `") 'rdg/multi-vterm-dwim))

;; (use-package comint
;;   :custom
;;   (comint-prompt-read-only t)
;;   (comint-buffer-maximum-size 10000))

;; (use-package shell
;;   :config (native-complete-setup-bash)
;;   :init
;;   (add-to-list 'display-buffer-alist '("*shell*" display-buffer-same-window))
;;   (define-key shell-mode-map (kbd "<tab>") #'company-complete)
;;   (add-hook 'shell-mode-hook
;;             (lambda ()
;;               (set (make-local-variable
;;                     'completion-at-point-functions)
;;                    (append completion-at-point-functions
;;                            '(pcomplete-completions-at-point)))
;;               (rdg/company-set-mode-backends rdg/company-shell-backends)
;;               (rdg/remove-fringe-and-margin))))

(use-package sqlformat
  :custom (sqlformat-command 'pgformatter))

(use-package magit
  :custom (magit-status-buffer-switch-function 'switch-to-buffer))

(use-package scss-mode
  :custom (scss-compile-at-save nil))

(use-package flycheck
  :custom
  (flycheck-idle-change-delay 5)
  :config
  (setq flycheck-disabled-checkers (append flycheck-disabled-checkers
                                           '(javascript-jshint json-jsonlist)))
  (mapc (lambda (mode) (flycheck-add-mode 'javascript-eslint mode))
        '(js-mode js2-mode rjsx-mode)))

(use-package prettier-js)

(use-package js
  :config
  (defvar rdg/company-js-backends '())
  (let ((hook (lambda ()
                (setq js-indent-level 2
                      sgml-basic-offset 2)
                (subword-mode)
                (prettier-js-mode)
                (rdg/company-set-mode-backends rdg/company-js-backends))))
    (mapc (lambda (mode-hook) (add-hook mode-hook hook))
          '(js-mode-hook js2-mode-hook rjsx-mode-hook)))
  (defun rdg/use-eslint-from-node-modules (fn)
    (let ((root (locate-dominating-file
                 (or (buffer-file-name) default-directory)
                 (lambda (dir)
                   (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js" dir)))
                     (and eslint (file-executable-p eslint)))))))
      (when root
        (let ((eslint (expand-file-name "node_modules/eslint/bin/eslint.js" root)))
          (funcall fn eslint)))))
  (defun rdg/set-flycheck-eslint-executable ()
    (rdg/use-eslint-from-node-modules
     (lambda (eslint) (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'rdg/set-flycheck-eslint-executable))

(defun rdg/eslint-fix-buffer ()
  (rdg/use-eslint-from-node-modules
   (lambda (eslint)
     (let ((file (buffer-file-name (current-buffer))))
       (call-process eslint nil nil nil "--fix" file)
       (revert-buffer t t t)))))

(defun rdg/add-eslint-fix-buffer-hook ()
  (remove-hook 'after-save-hook 'rdg/eslint-fix-buffer t)
  (add-hook 'after-save-hook 'rdg/eslint-fix-buffer nil t))

(use-package rubocop
  :custom (rubocop-prefer-system-executable t)
  :config
  (defun rdg/rubocop-autocorrect-and-revert()
    (rubocop-autocorrect-current-file)
    (revert-buffer t t t))
  (defun rdg/rubocop-fix-layout-and-revert()
    (rubocop--file-command "rubocop --fix-layout --format emacs")
    (revert-buffer t t t))
  (defun rdg/add-rubocop-autocorrect-hook ()
    (remove-hook 'after-save-hook 'rdg/rubocop-autocorrect-and-revert t)
    (add-hook 'after-save-hook 'rdg/rubocop-autocorrect-and-revert nil t))
  (defun rdg/add-rubocop-fix-layout-hook ()
    (remove-hook 'after-save-hook 'rdg/rubocop-fix-layout-and-revert t)
    (add-hook 'after-save-hook 'rdg/rubocop-fix-layout-and-revert nil t)))

(use-package ruby-tools)
(use-package ruby-hash-syntax)

(use-package ruby-mode
  :config
  (defun rdg/ruby-update-tags ()
    (let ((root (locate-dominating-file default-directory ".git")))
      (when root
        (cd root)
        (call-process-shell-command "ripper-tags -R -e --exclude=vendor"))))
  (defun rdg/add-ruby-update-tags-hook ()
    (remove-hook 'after-save-hook 'rdg/ruby-update-tags t)
    (add-hook 'after-save-hook 'rdg/ruby-update-tags nil t))
  (exec-path-from-shell-copy-env "GEM_HOME")
  (defun rdg/ruby-mode-hook ()
    (subword-mode)
    (ruby-tools-mode)
    (setq ruby-insert-encoding-magic-comment nil
          ruby-align-chained-calls t
          ruby-use-smie t)
    (rdg/add-rubocop-fix-layout-hook)
    (rdg/add-ruby-update-tags-hook))
  (add-hook 'ruby-mode-hook 'rdg/ruby-mode-hook))

;; imenu
(use-package imenu
  :custom (imenu-auto-rescan t)
  :bind ("C-c ." . ivy-imenu-anywhere))

(use-package project-persist
  :load-path "project-persist-git/")

(use-package project-persist-drawer
  :load-path "project-persist-drawer/"
  :bind ("C-c d d" . project-persist-drawer-toggle))

(use-package ppd-sr-speedbar
  :load-path "ppd-sr-speedbar/")

(use-package graphene-meta-theme
  :load-path "graphene-meta-theme/"
  :config
  (add-to-list 'custom-theme-load-path
               "~/.emacs.d/graphene-meta-theme/"))

(use-package graphene
  :load-path "graphene/"
  :config
  (add-hook 'graphene-prog-mode-hook 'eldoc-mode))

(use-package cfn-mode
  :config
  (add-to-list 'graphene-prog-mode-hooks 'cfn-mode)
  (add-hook 'cfn-mode-hook (lambda () (flycheck-cfn-setup))))

(use-package dockerfile-mode)

(use-package feature-mode)

(use-package json-mode)

(use-package markdown-mode
  :config
  (define-key markdown-mode-map (kbd "C-c <up>") nil)
  (define-key markdown-mode-map (kbd "C-c <down>") nil)
  (define-key markdown-mode-map (kbd "C-c <left>") nil)
  (define-key markdown-mode-map (kbd "C-c <right>") nil))

(use-package yaml-mode)

(use-package smart-tab
  :config (global-smart-tab-mode 1))

(use-package solarized-theme
  :straight (solarized-theme :host github :repo "sellout/emacs-color-theme-solarized")
  :init
  (when window-system
    (load-theme 'solarized t)))
