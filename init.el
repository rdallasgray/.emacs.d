(require 'server)
(if (server-running-p)
    (message "Server is running")
  (progn
    (message "Starting server")
    (server-start)))

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/graphene/lib")
(add-to-list 'load-path "~/.emacs.d/pallet/lib")
(add-to-list 'load-path "~/.emacs.d/readline-complete/")
(add-to-list 'load-path "~/.emacs.d/emacs-pry/")

(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'pallet)
(require 'graphene)

(setq warning-minimum-level :error)

(setq dropbox-directory "~/Dropbox")

;;(e)shell-mode
(setq explicit-shell-file-name "bash")
(setq shell-file-name explicit-shell-file-name)
(setenv "SHELL" shell-file-name)

;; Set up readline-complete if not on Windows
(unless (eq system-type 'windows-nt)
  (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
  (setq comint-process-echoes t)
  (require 'readline-complete)
  (add-hook 'shell-mode-hook
            (lambda ()
              (ac-rlc-setup-sources)
              (auto-complete-mode))))

;; Easily open/switch to a shell
(global-set-key (kbd "C-c `") 'shell-pop)

;; org -- ignore if org dir doesn't exist
(eval-after-load 'org-mode
  '(progn
    (let ((maybe-org-directory (expand-file-name "org" user-emacs-directory)))
      (when (file-exists-p maybe-org-directory)
        (setq org-directory maybe-org-directory)
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
          (when (not (re-search-forward
                      (format org-complex-heading-regexp-format headline) nil t))
            (insert "* " headline))
          (newline))

        (defun capture-headline-current-project-name ()
          (capture-find-or-create-headline project-persist-current-project-name))

        (setq org-capture-templates
              '(("n" "Note" entry
                 (file org-default-notes-file)
                 "* %U %?")
                ("p" "Project Note" plain
                 (file+function (expand-file-name "projects.org" org-directory) capture-headline-current-project-name)
                 "** %U %f: %?")
                ("d" "Personal Note" entry
                 (file (expand-file-name "personal.org" org-directory))
                 "* %U %?")
                ("b" "Blog Note" entry
                 (file (expand-file-name "blog.org" org-directory))
                 "* %U %?")
                ("i" "Idea" entry
                 (file (expand-file-name "ideas.org" org-directory))
                 "* %U %?")))

        (setq org-refile-allow-creating-parent-nodes t)
        (setq org-refile-use-outline-path 'file)
        (setq org-outline-path-complete-in-steps t)
        (setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

        ;; org-mobile -- ignore if no dropbox/org-mobile directory
        (let ((maybe-org-mobile-directory (expand-file-name "Apps/MobileOrg" dropbox-directory)))
          (when (file-exists-p maybe-org-mobile-directory)
            (require 'org-mobile)
            (setq org-mobile-inbox-for-pull (expand-file-name "inbox.org" org-directory))
            (setq org-mobile-directory (expand-file-name "Apps/MobileOrg" dropbox-directory))
            (org-mobile-pull)
            (add-hook 'kill-emacs-hook 'org-mobile-push)))))))


;; No pop-ups
(setq pop-up-frames nil
      pop-up-windows nil)

;; Uniquify buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Sane magit window creation
(setq magit-status-buffer-switch-function 'switch-to-buffer)

;; Add de facto prog-mode hooks
(push 'sws-mode-hook graphene-prog-mode-hooks)

;; Use Alt-3 1o insert a #, unbind right alt
(fset 'insert-pound "#")
(define-key global-map "\M-3" 'insert-pound)
(setq ns-right-alternate-modifier nil)
;; Mac-port specific key settings
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)

;; Sensible window movement
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; Scroll up and down a line at a time
(global-set-key (kbd "C-S-v") 'scroll-up-line)
(global-set-key (kbd "M-V") 'scroll-down-line)

; AC everywhere
(setq ac-disable-faces nil)

;; JS
(eval-after-load 'javascript-mode
  '(progn
     (setq js-indent-level 2)))

;; Ruby
(dolist (regex
         '("\\.watchr$" "\\.arb$" "\\.rake$" "\\.gemspec$" "\\.ru$" "Rakefile$" "Gemfile$" "Capfile$" "Guardfile$"))
  (add-to-list 'auto-mode-alist `(,regex . ruby-mode)))

(eval-after-load 'ruby-mode
  '(progn
    (exec-path-from-shell-copy-env "GEM_HOME")
    (setq ruby-deep-indent-paren nil)
    (add-hook 'ruby-mode-hook
              (lambda ()
                (robe-mode)
                (ruby-tools-mode)
                (require 'pry)))
    (add-hook 'robe-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-robe)))

    ;; RSense
    (setq rsense-home nil)
    (let ((rsense-home-val
           (cond ((eq system-type 'gnu/linux) "/usr/lib/rsense-0.3")
                 ((eq system-type 'darwin) "/usr/local/Cellar/rsense/0.3/libexec"))))
      (when (and rsense-home-val (file-exists-p rsense-home-val))
        (setq rsense-home rsense-home-val)
        (add-to-list 'load-path (concat rsense-home "/etc"))))

    (when rsense-home
      (require 'rsense)
      (add-hook 'ruby-mode-hook
                (lambda () (add-to-list 'ac-sources 'ac-source-rsense))))))

;; imenu
(add-hook 'graphene-prog-mode-hook
          (lambda ()
            (require 'imenu-anywhere)))

(eval-after-load 'imenu-anywhere
  '(progn
     (setq imenu-auto-rescan t)
     (global-set-key (kbd "C-c .") 'imenu-anywhere)))

;; multi-occur
(defun multi-occur-in-open-buffers (regexp &optional allbufs)
  "Occur in all open buffers."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp))

(global-set-key (kbd "M-s O") 'multi-occur-in-open-buffers)

;; multiple-cursors
(global-set-key (kbd "C-c m") 'mc/mark-all-like-this-dwim)

;; Flycheck
(add-hook 'graphene-prog-mode-hook
          (lambda()
            (require 'flycheck)
            (flycheck-mode)))

(eval-after-load 'flycheck
  '(progn
     (defun rdg/flycheck-display-errors-function (errors)
       (mapc (lambda (err)
               (message "FlyC: %s" (flycheck-error-message err)) (sit-for 1))
             errors))
     (setq flycheck-highlighting-mode nil
           flycheck-display-errors-function 'rdg/flycheck-display-errors-function)))


;; Mark word, sexp, line, ...
(require 'expand-region)
(global-set-key (kbd "C-=")
                'er/expand-region)
(global-set-key (kbd "C--")
                'er/contract-region)

;; easier sexp navigation
(defun beginning-of-next-defun ()
  (interactive)
  (end-of-defun)
  (end-of-defun)
  (beginning-of-defun))
(global-set-key (kbd "M-<down>") 'beginning-of-next-defun)
(global-set-key (kbd "M-<up>") 'beginning-of-defun)

;; No visible region on C-x C-x
(defun exchange-point-and-mark-no-region ()
  "Suppress region visibility when exchanging point and mark."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-region)

;; auto markdown-mode
(push '("\\.md\\'" . gfm-mode) auto-mode-alist)
(push '("\\.markdown\\'" . gfm-mode) auto-mode-alist)
(add-hook 'gfm-mode-hook (lambda () (auto-fill-mode t)))

;; auto stylus-mode
(push '("\\.styl\\'" . jade-mode) auto-mode-alist)

;; auto json-mode
(push '("\\.json\\'" . json-mode) auto-mode-alist)

;; auto feature-mode
(push '("\\.feature\\'" . feature-mode) auto-mode-alist)

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

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(if window-system
    (load-theme 'solarized-light))
