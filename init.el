(require 'server)
(if (server-running-p)
    (message "Server is running")
  (progn
    (message "Starting server")
    (server-start)))

(add-to-list 'load-path "~/.emacs.d/graphene/")
(add-to-list 'load-path "~/.emacs.d/pallet/")
(add-to-list 'load-path "~/.emacs.d/project-persist-git/")
(add-to-list 'load-path "~/.emacs.d/project-persist-drawer/")
(add-to-list 'load-path "~/.emacs.d/ppd-sr-speedbar/")
(add-to-list 'load-path "~/.emacs.d/readline-complete/")
(add-to-list 'load-path "~/.emacs.d/emacs-pry/")

(require 'package)
(require 'cask "/usr/local/Cellar/cask/0.7.1/cask.el")
(cask-initialize)
(add-to-list 'auto-mode-alist '("\\Cask\\'" . emacs-lisp-mode))

(require 'pallet)
(pallet-mode t)
(require 'graphene)

(eval-after-load 'persp-mode
  '(progn
     (defadvice persp-switch (before undedicate-speedbar activate)
       (when (boundp 'sr-speedbar-window)
         (ignore-errors
           (set-window-dedicated-p sr-speedbar-window nil))))))

(require 'midnight)
(setq clean-buffer-list-delay-general 7)
(midnight-delay-set 'midnight-delay "12:00am")

(setq warning-minimum-level :error)

(setq dropbox-directory "~/Dropbox")

;; (e)shell-mode
(let ((shell-name "bash"))
  (setq explicit-shell-file-name shell-name
        shell-file-name shell-name)
  (setenv "SHELL" shell-name)
  (setenv "PAGER" "/bin/cat"))

(defun rdg/remove-rogue-control-chars (op)
  (replace-regexp-in-string "\\(\\[0G\\)\\|\\(\\]2;\\)\\|\\(\\)" "" op))

(eval-after-load 'shell
  '(add-to-list 'comint-preoutput-filter-functions
                'rdg/remove-rogue-control-chars))

;; Set up readline-complete if not on Windows
(unless (eq system-type 'windows-nt)
  (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash")
        comint-process-echoes t)
  (require 'readline-complete)
  (push 'company-readline company-backends)
  (add-hook 'shell-mode-hook 'company-mode)
  (setq rlc-attempts 10))

;; Easily open/switch to a shell
(global-set-key (kbd "C-c `") 'shell-pop)

;; fiplr
(global-set-key (kbd "C-c f") 'fiplr-find-file)

;; er
(global-set-key (kbd "C-M-SPC") 'er/expand-region)

;; sp
(global-set-key (kbd "C-M-<left>") 'sp-backward-sexp)
(global-set-key (kbd "C-M-<right>") 'sp-forward-sexp)
(global-set-key (kbd "C-M-<up>") 'sp-backward-up-sexp)
(global-set-key (kbd "C-M-<down>") 'sp-down-sexp)
(global-set-key (kbd "C-M-<backspace>") 'sp-unwrap-sexp)

;; org
(defun load-org-and-capture ()
  (interactive)
  (require 'org)
  (org-capture))

(global-set-key (kbd "C-c c") 'load-org-and-capture)

(eval-after-load 'org
  '(progn
    (let ((maybe-org-directory (expand-file-name "org" user-emacs-directory)))
      (when (file-exists-p maybe-org-directory)
        (setq org-directory maybe-org-directory)
        (setq org-completion-use-ido t)
        (setq org-default-notes-file (expand-file-name "notes.org" org-directory))
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

;; Sane magit window creation
(setq magit-status-buffer-switch-function 'switch-to-buffer)

;; Use Alt-3 1o insert a #, unbind right alt
(fset 'insert-pound "#")
(define-key global-map "\M-3" 'insert-pound)
(setq ns-right-alternate-modifier nil)

;; Sensible window movement
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <left>") 'windmove-left)

;; Scroll up and down a line at a time
(global-set-key (kbd "C-S-v") 'scroll-up-line)
(global-set-key (kbd "M-V") 'scroll-down-line)

;; SCSS
(eval-after-load 'scss-mode
  '(setq scss-compile-at-save nil))

;; JS
(add-hook 'js-mode-hook
          (lambda () (setq js-indent-level 2)))

(eval-after-load 'ruby-mode
  '(progn
     (exec-path-from-shell-copy-env "GEM_HOME")))
;; RSense - TODO create company backend
;; (setq rsense-home nil)
;; (let ((rsense-home-val
;;        (cond ((eq system-type 'gnu/linux) "/usr/lib/rsense-0.3")
;;              ((eq system-type 'darwin) "/usr/local/Cellar/rsense/0.3/libexec"))))
;;   (when (and rsense-home-val (file-exists-p rsense-home-val))
;;     (setq rsense-home rsense-home-val)
;;     (add-to-list 'load-path (concat rsense-home "/etc"))))

;; (when rsense-home
;;   (require 'rsense)
;;   (add-hook 'ruby-mode-hook
;;             (lambda () (add-to-list 'ac-sources 'ac-source-rsense))))
;; ))

;; imenu
(add-hook 'graphene-prog-mode-hook
          (lambda ()
            (require 'imenu-anywhere)))

(eval-after-load 'imenu-anywhere
  '(progn
     (setq imenu-auto-rescan t)
     (global-set-key (kbd "C-c .") 'idomenu)
     (global-set-key (kbd "C-c C-.") 'imenu-anywhere)))

;; multi-occur
(defun multi-occur-in-open-buffers (regexp &optional allbufs)
  "Occur in all open buffers."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp))

(global-set-key (kbd "M-s O") 'multi-occur-in-open-buffers)

;; easier defun navigation
(defun beginning-of-next-defun ()
  (interactive)
  (end-of-defun)
  (end-of-defun)
  (beginning-of-defun))
(global-set-key (kbd "M-<down>") 'beginning-of-next-defun)
(global-set-key (kbd "M-<up>") 'beginning-of-defun)

;; goto-chg
(global-set-key (kbd "C-c /") 'goto-last-change)

;; No visible region on C-x C-x
(defun exchange-point-and-mark-no-region ()
  "Suppress region visibility when exchanging point and mark."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-region)

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; (setq graphene-default-font "Fira Mono OT-12"
;;       graphene-fixed-pitch-font "Fira Mono OT-12"
;;       graphene-variable-pitch-font "Fira Sans OT-12"
;;       graphene-line-spacing 1)

(when window-system
    (load-theme 'solarized t))
