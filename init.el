
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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

(add-hook 'graphene-prog-mode-hook 'eldoc-mode)

;; git-wip
(let ((git-wip-path "/Users/robertdallasgray/Documents/Code/git-wip"))
  (add-to-list 'exec-path git-wip-path)
  (add-to-list 'load-path (format "%s/emacs/" git-wip-path)))

(require 'git-wip-mode)
(git-wip-mode t)

;; midnight
(require 'midnight)
(setq clean-buffer-list-delay-general 7)
(midnight-delay-set 'midnight-delay "12:00am")

;; anzu
(global-anzu-mode +1)

(set-face-attribute 'anzu-mode-line nil
                    :foreground 'unspecified
                    :inherit 'company-tooltip-search)

(custom-set-variables
 '(anzu-mode-lighter "")
 '(anzu-deactivate-region t)
 '(anzu-replace-to-string-separator " => "))

(define-key (current-global-map) [remap query-replace]
  #'anzu-query-replace)
(define-key (current-global-map) [remap query-replace-regexp]
  #'anzu-query-replace-regexg)
(define-key isearch-mode-map [remap isearch-query-replace]
  #'anzu-isearch-query-replace)
(define-key isearch-mode-map [remap isearch-query-replace-regexp]
  #'anzu-isearch-query-replace-regexp)


;; er
(global-set-key (kbd "C-M-SPC") 'er/expand-region)

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

(setq comint-buffer-maximum-size 10000)
(add-hook 'comint-output-filter-functions
          'comint-truncate-buffer)

(with-eval-after-load 'shell
  (add-hook 'comint-output-filter-functions
            'comint-truncate-buffer)
  (add-to-list 'comint-output-filter-functions
               'ansi-color-process-output)
  (add-to-list 'comint-preoutput-filter-functions
               'rdg/remove-rogue-control-chars))

;; Set up readline-complete if not on Windows
(unless (eq system-type 'windows-nt)
  (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash")
        comint-process-echoes t)
  (require 'readline-complete)
  (push 'company-readline company-backends)
  (add-hook 'shell-mode-hook 'company-mode)
  (setq rlc-attempts 5))

;; Easily open/switch to a shell
;; Please don't open my shells in new windows
(add-to-list 'display-buffer-alist '("*shell*" display-buffer-same-window))
(require 'shell-pop)
(global-set-key (kbd "C-c `") 'shell-pop)
(setq shell-pop-universal-key "C-c `"
      shell-pop-window-position "bottom")

;; google-this
;; (setq google-this-keybind (kbd "C-c g"))
(require 'google-this)
(google-this-mode t)

;; sp
(global-set-key (kbd "C-M-<left>") 'sp-backward-sexp)
(global-set-key (kbd "C-M-<right>") 'sp-forward-sexp)
(global-set-key (kbd "C-M-<up>") 'sp-backward-up-sexp)
(global-set-key (kbd "C-M-<down>") 'sp-down-sexp)
(global-set-key (kbd "C-M-k") 'sp-kill-sexp)
(global-set-key (kbd "C-M-w") 'sp-copy-sexp)
(global-set-key (kbd "C-]") 'sp-select-next-thing)
(global-set-key (kbd "C-)") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-(") 'sp-backward-slurp-sexp)
(global-set-key (kbd "C-}") 'sp-forward-barf-sexp)
(global-set-key (kbd "C-{") 'sp-backward-barf-sexp)
(global-set-key (kbd "M-F") 'sp-forward-symbol)
(global-set-key (kbd "M-B") 'sp-backward-symbol)

(sp-pair "{ " " }")

(defun rdg/unwrap-and-mark-sexp (&optional arg)
  (interactive)
  (let ((sexp-info (sp-unwrap-sexp arg)))
    (goto-char (plist-get sexp-info :beg))
    (push-mark (- (plist-get sexp-info :end) 2) t t)
    (setq deactivate-mark nil)))

(global-set-key (kbd "C-M-<backspace>") 'rdg/unwrap-and-mark-sexp)

(defun rdg/wrap-and-mark-region (orig &rest args)
  (apply orig args)
  (let ((sexp-info sp-last-wrapped-region))
    (goto-char (plist-get sexp-info :beg))
    (push-mark (plist-get sexp-info :end) t t)
    (setq deactivate-mark nil)))

(advice-add 'sp-wrap :around 'rdg/wrap-and-mark-region)

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
(add-hook 'coffee-mode-hook 'subword-mode)

;; Ruby
(with-eval-after-load 'ruby-mode
  (exec-path-from-shell-copy-env "GEM_HOME"))
(add-hook 'ruby-mode-hook 'subword-mode)
(add-hook 'ruby-mode-hook 'yard-mode)
(add-hook 'ruby-mode-hook 'ruby-tools-mode)

;; imenu
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

;; String inflection
;; Could do this with Hydra
(global-set-key (kbd "C-c i") 'string-inflection-all-cycle)

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

(defun kill-zombie-buffers ()
  "Kill buffers no longer associated with a file."
  (interactive)
  (let ((buffers (buffer-list)))
    (mapc (lambda (buf)
            (let ((filename (buffer-file-name buf)))
              (when (and filename (not (file-exists-p filename)))
                (kill-buffer buf))))
          buffers)))
