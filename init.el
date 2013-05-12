(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/graphene/lib")
(add-to-list 'load-path "~/.emacs.d/pallet/lib")
(add-to-list 'load-path "~/.emacs.d/readline-complete/")

(require 'pallet)
(require 'graphene)

(setq warning-minimum-level :error)

(setq dropbox-directory "~/Dropbox")

;; org
(setq org-directory (expand-file-name "org" user-emacs-directory))
(setq org-completion-use-ido t)
(setq org-default-notes-file (expand-file-name "notes.org" org-directory))
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-agenda-files
      (mapcar (lambda (el) (expand-file-name el org-directory))
              '("notes.org" "personal.org" "projects.org" "blog.org" "ideas.org")))

;; org capture/refile
(defun capture-find-or-create-headline (headline)
  "Find or create HEADLINE in the current buffer"
  (goto-char (point-min))
  (if (re-search-forward
       (format org-complex-heading-regexp-format headline) nil t)
      (progn (end-of-line) (newline))
    (goto-char (point-max))
    (or (bolp) (newline))
    (insert "* " headline)
    (end-of-line) (newline)))

(defun capture-headline-current-project-name ()
  (capture-find-or-create-headline project-persist-current-project-name))

(setq org-capture-templates
      '(("n" "Note" entry
         (file org-default-notes-file)
         "** %U %?")
        ("p" "Project Note" entry
         (file+function (expand-file-name "projects.org" org-directory) capture-headline-current-project-name)
         "* %U %f: %?")
        ("d" "Personal Note" entry
         (file (expand-file-name "personal.org" org-directory))
         "* %U %?")
        ("b" "Blog Note" entry
         (file (expand-file-name "blog.org" org-directory))
         "* %U %?")
        ("i" "Idea" entry
         (file (expand-file-name "ideas.org" org-directory))
         "* %U %?")))

(setq org-refile-use-outline-path 'file)
(setq org-outline-path-complete-in-steps t)
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

;; org-mobile
(require 'org-mobile)
(setq org-mobile-inbox-for-pull (expand-file-name "inbox.org" org-directory))
(setq org-mobile-directory (expand-file-name "Apps/MobileOrg" dropbox-directory))
(add-hook 'after-init-hook 'org-mobile-pull)
(add-hook 'kill-emacs-hook 'org-mobile-push)

;; Shell completion if not on Windows
(if (eq system-type 'windows-nt)
    (progn
      (require 'smart-tab)
      (add-hook 'shell-mode-hook (lambda () (smart-tab-mode t))))
  ;; AC for shell-mode
  (setq explicit-shell-file-name "bash")
  (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
  (setq comint-process-echoes t)
  (require 'readline-complete)
  (add-to-list 'ac-modes 'shell-mode)
  (add-hook 'shell-mode-hook 'ac-rlc-setup-sources))

;; Uniquify buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Add de facto prog-mode hooks
(push 'sws-mode-hook graphene-prog-mode-hooks)

;; Use Alt-3 1o insert a #, unbind right alt
(fset 'insert-pound "#")
(define-key global-map "\M-3" 'insert-pound)
(setq ns-right-alternate-modifier nil)

;; Sensible window movement
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; Scroll up and down a line at a time
(global-set-key (kbd "C-S-v") 'scroll-up-line)
(global-set-key (kbd "M-V") 'scroll-down-line)

;; CoffeeScript 4-space tabs (for Huzu)
(setq coffee-tab-width 4)
(add-hook 'coffee-mode-hook
          (lambda ()
            (setq default-tab-width 4)
            (exec-path-from-shell-copy-env "COFFEELINT_CONFIG")))

;; Get rid of CoffeeREPL garbage
(add-to-list
         'comint-preoutput-filter-functions
         (lambda (output)
           (replace-regexp-in-string "\\[[0-9]+[GKJ]" "" output)))

;; Add eco/jeco to mweb-filename-extensions
(setq mweb-filename-extensions
      (append '("eco" "jeco")
              mweb-filename-extensions))

; AC everywhere
(setq ac-disable-faces nil)

;; RSense
;; (setq rsense-home "/usr/local/Cellar/rsense/0.3/libexec")
;; (add-to-list 'load-path (concat rsense-home "/etc"))
;; (require 'rsense)

;; (add-hook 'ruby-mode-hook
;;           (lambda () (add-to-list 'ac-sources 'ac-source-rsense)))

;; imenu
(require 'idomenu)
(require 'imenu-anywhere)
(global-set-key (kbd "C-c t") 'idomenu)
(global-set-key (kbd "C-c T") 'imenu-anywhere)

;; multi-occur
(defun multi-occur-in-open-buffers (regexp &optional allbufs)
  "Occur in all open buffers."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp))
(global-set-key (kbd "M-s O") 'multi-occur-in-open-buffers)

;; multiple-cursors
(global-set-key (kbd "C-c m") 'mc/mark-all-like-this-dwim)

;; ;; yasnippet
;; (require 'yasnippet)
;; (setq yas-snippet-dirs '("~/.emacs.d/yasnippets"))
;; (yas-reload-all)
;; (add-hook 'graphene-prog-mode-hook
;;           '(lambda()
;;              (add-to-list 'ac-sources 'ac-source-yasnippet)
;;              (yas-minor-mode)))

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

;; No visible region on C-x C-x
(defun exchange-point-and-mark-no-region ()
  "Suppress region visibility when exchanging point and mark."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-region)

;; visible-mark-mode
(require 'visible-mark)
(setq visible-mark-max 16
      visible-mark-inhibit-trailing-overlay nil)

;; Easier sexp navigation
(global-set-key (kbd "M-n") 'forward-sexp)
(global-set-key (kbd "M-<down>") 'forward-sexp)
(global-set-key (kbd "M-p") 'backward-sexp)
(global-set-key (kbd "M-<up>") 'backward-sexp)

;; auto markdown-mode
(push '("\\.md\\'" . markdown-mode) auto-mode-alist)
(push '("\\.markdown\\'" . markdown-mode) auto-mode-alist)
(add-hook 'markdown-mode-hook (lambda () (auto-fill-mode t)))

;; auto stylus-mode
(push '("\\.styl\\'" . jade-mode) auto-mode-alist)

;; auto json-mode
(push '("\\.json\\'" . json-mode) auto-mode-alist)

;; don't compile sass/scss on saving
(setq scss-compile-at-save nil)

;; 2-space indent for CSS
(setq css-indent-offset 2)

;; Non-blinking cursor
(blink-cursor-mode -1)

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Don't use tabs for indent; replace tabs with two spaces.
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Leave my minibuffer alone
(setq resize-mini-windows nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(if window-system
    (load-theme 'solarized-light))
