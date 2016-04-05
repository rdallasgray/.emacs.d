(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(setq require-final-newline t)

(require 'server)
(if (server-running-p)
    (message "Server is running")
  (progn
    (message "Starting server")
    (server-start)))

;; git-wip
(let ((git-wip-path "/Users/robertdallasgray/Documents/Code/git-wip"))
  (add-to-list 'exec-path git-wip-path)
  (add-to-list 'load-path (format "%s/emacs/" git-wip-path)))

(require 'git-wip-mode)
(git-wip-mode t)

(add-to-list 'load-path "~/.emacs.d/graphene/")
(add-to-list 'load-path "~/.emacs.d/graphene-meta-theme/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/graphene-meta-theme/")
(add-to-list 'load-path "~/.emacs.d/pallet/")
(add-to-list 'load-path "~/.emacs.d/project-persist-git/")
(add-to-list 'load-path "~/.emacs.d/project-persist-drawer/")
(add-to-list 'load-path "~/.emacs.d/ppd-sr-speedbar/")
(add-to-list 'load-path "~/.emacs.d/readline-complete/")
(add-to-list 'load-path "~/.emacs.d/emacs-pry/")

(require 'package)
(require 'cask "/usr/local/Cellar/cask/0.7.2/cask.el")
(cask-initialize)
(add-to-list 'auto-mode-alist '("\\Cask\\'" . emacs-lisp-mode))

(when window-system
    (load-theme 'solarized t))

(require 'pallet)
(pallet-mode t)
(require 'graphene)

(require 'midnight)
(setq clean-buffer-list-delay-general 7)
(midnight-delay-set 'midnight-delay "12:00am")

(require 'expand-region)
(defhydra hydra-mark (global-map "C-c SPC")
  "mark"
  ("w" er/mark-word)
  ("s" er/mark-sexp)
  ("d" er/mark-defun)
  ("i" er/mark-inside-pairs)
  ("o" er/mark-outside-pairs)
  ("+" er/expand-region)
  ("-" er/contract-region)
  (">" mc/mark-next-like-this)
  ("<" mc/mark-previous-like-this)
  ("m" mc/mark-more-like-this-extended))

(setq warning-minimum-level :error)

(setq dropbox-directory "~/Dropbox")

;; (e)shell-mode
(let ((shell-name "bash"))
  (setq explicit-shell-file-name shell-name
        shell-file-name shell-name)
  (setenv "SHELL" shell-name)
  (setenv "PAGER" "/bin/cat"))

(defun rdg/remove-rogue-control-chars (op)
  (replace-regexp-in-string
   "\\(\\[0G\\)\\|\\(\\]2;\\)\\|\\(\\)|\\(\\[?25h\\)|\\(\\[1A\\)"
   ""
   op))


(with-eval-after-load 'shell
  (add-hook 'comint-output-filter-functions
            'comint-truncate-buffer)
  (add-to-list 'comint-output-filter-functions
               'ansi-color-process-output)
  (add-to-list 'comint-preoutput-filter-functions
               'rdg/remove-rogue-control-chars))

(add-hook 'coffee-mode-hook 'subword-mode)
(add-hook 'ruby-mode-hook 'subword-mode)

;; Set up readline-complete if not on Windows
(unless (eq system-type 'windows-nt)
  (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash")
        comint-process-echoes t)
  (require 'readline-complete)
  (push 'company-readline company-backends)
  (add-hook 'shell-mode-hook 'company-mode)
  (setq rlc-attempts 5))

;; Easily open/switch to a shell
(global-set-key (kbd "C-c `") 'shell-pop)

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

(with-eval-after-load 'org
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
          (add-hook 'kill-emacs-hook 'org-mobile-push))))))


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
(with-eval-after-load 'scss-mode
  (setq scss-compile-at-save nil))

;; JS/Coffee
(add-hook 'js-mode-hook
          (lambda () (setq js-indent-level 2)))
(push 'company-tern company-backends)
(with-eval-after-load 'flycheck
  (setq flycheck-coffee-executable "cjsx"))

;; Ruby
(with-eval-after-load 'ruby-mode
  (exec-path-from-shell-copy-env "GEM_HOME"))

(require 'idomenu)
(require 'imenu-anywhere)
(setq imenu-auto-rescan t)
(global-set-key (kbd "C-c .") 'idomenu)
(global-set-key (kbd "C-c C-.") 'imenu-anywhere)

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

;; No visible region on C-x C-x
(defun exchange-point-and-mark-no-region ()
  "Suppress region visibility when exchanging point and mark."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-region)

;; Remove trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'windmove)

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

(global-set-key (kbd "C-c S-<left>") 'buf-stack-left)
(global-set-key (kbd "C-c S-<right>") 'buf-stack-right)
(global-set-key (kbd "C-c S-<up>") 'buf-stack-up)
(global-set-key (kbd "C-c S-<down>") 'buf-stack-down)

(defun rdg/unwrap-and-mark-sexp (&optional arg)
  (interactive)
  (let ((sexp-info (sp-unwrap-sexp arg)))
    (message "%s" sexp-info)
    (goto-char (plist-get sexp-info :beg))
    (push-mark (- (plist-get sexp-info :end) 2) t t)
    (setq deactivate-mark nil)))

(global-set-key (kbd "C-M-<backspace>") 'rdg/unwrap-and-mark-sexp)

(defun kill-zombie-buffers ()
  "Kill buffers no longer associated with a file."
  (interactive)
  (let ((buffers (buffer-list)))
    (mapc (lambda (buf)
            (let ((filename (buffer-file-name buf)))
              (when (and filename (not (file-exists-p filename)))
                (kill-buffer buf))))
          buffers)))
