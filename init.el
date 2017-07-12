;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq max-lisp-eval-depth 5000
      max-specpdl-size 5000)

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(setq require-final-newline t
      warning-minimum-level :error)

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
(require 'cask "/usr/local/Cellar/cask/0.8.1/cask.el")
(cask-initialize)
(add-to-list 'auto-mode-alist '("\\Cask\\'" . emacs-lisp-mode))

(when window-system
    (load-theme 'solarized t))

(require 'pallet)
(pallet-mode t)
(require 'graphene)

(add-hook 'graphene-prog-mode-hook 'eldoc-mode)

(add-hook 'before-save-hook 'whitespace-cleanup)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;; git-wip
(let ((git-wip-path "/Users/robertdallasgray/Documents/Code/git-wip"))
  (add-to-list 'exec-path git-wip-path)
  (add-to-list 'load-path (format "%s/emacs/" git-wip-path)))

(require 'git-wip-mode)
(add-hook 'graphene-prog-mode-hook (lambda () (git-wip-mode t)))

;; midnight
(require 'midnight)
(setq clean-buffer-list-delay-general 7)
(midnight-delay-set 'midnight-delay "12:00am")

;; text scaling
(global-set-key (kbd "C-M-=") 'default-text-scale-increase)
(global-set-key (kbd "C-M--") 'default-text-scale-decrease)

;; anzu
(global-anzu-mode +1)

;; company
(setq company-idle-delay 0.15)

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

;; er/mc
(defun hydra-mark-begin (&optional arg)
  (interactive "p")
  (er/expand-region arg)
  (hydra-mark/body))

(defhydra hydra-mark (global-map "C-M-SPC")
  "Mark"
  ("w" er/mark-word "Word")
  ("s" er/mark-sexp "Sexp")
  ("d" er/mark-defun "Defun")
  ("i" er/mark-inside-pairs "Inside pairs")
  ("o" er/mark-outside-pairs "Outside pairs")
  ("+" er/expand-region "Expand")
  ("-" er/contract-region "Contract")
  (">" mc/mark-next-like-this "Next")
  ("<" mc/mark-previous-like-this "Previous")
  ("m" mc/mark-more-like-this-extended "More"))

(global-set-key (kbd "C-M-SPC") 'hydra-mark-begin)

;; (e)shell-mode
(unless (eq system-type 'windows-nt)
  (let ((shell-name "bash"))
    (setq explicit-shell-file-name shell-name
          shell-file-name shell-name)
    (setenv "SHELL" shell-name)
    (setenv "PAGER" "/bin/cat"))
  (setq explicit-bash-args '("-li" "-c" "export EMACS=; stty echo; bash"))
  (require 'readline-complete)
  (push 'company-readline company-backends)
  (add-hook 'shell-mode-hook 'company-mode)
  (setq rlc-attempts 10
        rlc-timeout 0.05
        rlc-idle-time 0.1))

;; Easily open/switch to a shell
;; Please don't open my shells in new windows
(add-to-list 'display-buffer-alist '("*shell*" display-buffer-same-window))
(require 'shell-pop)
(global-set-key (kbd "C-c `") 'shell-pop)
(setq shell-pop-universal-key "C-c `"
      shell-pop-window-position "bottom")

(defun rdg/remove-shell-control-chars (op)
  "Replace control characters in output OP with blank."
  (replace-regexp-in-string "\\[[0-9]+[JGK]" "" op))

(defun rdg/setup-shell ()
  "Hook to set up shell modes."
  (setq comint-prompt-read-only t
        comint-process-echoes t
        comint-buffer-maximum-size 1000)
  (ansi-color-for-comint-mode-on)
  (add-to-list 'comint-output-filter-functions
               'ansi-color-process-output)
  (add-hook 'comint-output-filter-functions
            'comint-truncate-buffer)
  (add-to-list 'comint-preoutput-filter-functions
               'rdg/remove-shell-control-chars))

(with-eval-after-load 'shell
  (add-hook 'shell-mode-hook 'rdg/setup-shell))

;; dired
(require 'dired+)
(diredp-make-find-file-keys-reuse-dirs)
(setq diredp-hide-details-initially-flag nil)

;; company
(with-eval-after-load 'company
  (require 'company-try-hard)
  (global-set-key (kbd "C-<tab>") #'company-try-hard)
  (define-key company-active-map (kbd "C-<tab>") #'company-try-hard))

;; swiper/ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key "\C-r" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h f") 'counsel-describe-function)
(global-set-key (kbd "C-h v") 'counsel-describe-variable)
(global-set-key (kbd "C-h i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "C-c u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git-grep)
(global-set-key (kbd "C-c .") 'counsel-imenu)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;; google-this
(require 'google-this)
(google-this-mode t)

;; sp
(setq sp-escape-quotes-after-insert nil)
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

;; easier defun navigation
(defun beginning-of-next-defun ()
  (interactive)
  (end-of-defun)
  (end-of-defun)
  (beginning-of-defun))
(global-set-key (kbd "M-<down>") 'beginning-of-next-defun)
(global-set-key (kbd "M-<up>") 'beginning-of-defun)

(defun rdg/unwrap-and-mark-sexp (&optional arg)
  (interactive)
  (let ((sexp-info (sp-unwrap-sexp arg)))
    (goto-char (plist-get sexp-info :beg))
    (push-mark (- (plist-get sexp-info :end) 2) t t)
    (setq deactivate-mark nil)))

(global-set-key (kbd "C-M-<backspace>") 'rdg/unwrap-and-mark-sexp)

(sp-pair " " " " :actions '(wrap))

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

;; Scroll up and down a line at a time
(global-set-key (kbd "C-S-v") 'scroll-up-line)
(global-set-key (kbd "M-V") 'scroll-down-line)

;; SCSS
(with-eval-after-load 'scss-mode
  (setq scss-compile-at-save nil))

;; JS/Coffee
(add-to-list 'auto-mode-alist '("\\.cjsx\\'" . coffee-mode))

(require 'prettier-js)
(let ((hook (lambda ()
              (setq js-indent-level 2
                    sgml-basic-offset 2)
              (subword-mode)
              (add-hook 'before-save-hook 'prettier-before-save))))
  (mapc (lambda (mode-hook) (add-hook mode-hook hook))
        '(js-mode-hook js2-mode-hook rjsx-mode-hook)))

(push 'company-tern company-backends)

(with-eval-after-load 'flycheck
  (setq flycheck-coffee-executable "cjsx"
        flycheck-idle-change-delay 5))
(with-eval-after-load 'coffee-mode
  (setq coffee-command "cjsx"))
(add-hook 'coffee-mode-hook 'subword-mode)

(with-eval-after-load 'flycheck
  ;; (setq-default flycheck-disabled-checkers
  ;;               (append flycheck-disabled-checkers
  ;;                       '(javascript-jshint)))
  ;; This screws up errors in e.g. .erb files. Look into it.
  (flycheck-add-mode 'javascript-eslint 'web-mode))

;; (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(add-hook 'web-mode-hook
          (lambda()
            (setq web-mode-markup-indent-offset 2
                  web-mode-code-indent-offset 2
                  web-mode-css-indent-offset 2
                  web-mode-attr-indent-offset 2
                  js-indent-level 2)))

(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

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

;; bm
(setq bm-highlight-style 'bm-highlight-only-line)
(global-set-key (kbd "C-c b") 'hydra-bm/body)
(defhydra hydra-bm ()
  "Bookmarks"
  ("t" bm-toggle "toggle")
  ("n" bm-next "next")
  ("p" bm-previous "previous")
  ("N" bm-lifo-next "next (lifo)")
  ("P" bm-lifo-previous "previous (lifo)")
  ("^" bm-first "first")
  ("$" bm-last "last")
  ("d" bm-remove-all-current-buffer "delete all (current buffer)")
  ("D" bm-remove-all-all-buffers "delete all (all buffers)"))

;; multi-occur
(defun multi-occur-in-open-buffers (regexp &optional allbufs)
  "Occur in all open buffers."
  (interactive (occur-read-primary-args))
  (multi-occur-in-matching-buffers ".*" regexp))

(global-set-key (kbd "M-s O") 'multi-occur-in-open-buffers)

;; No visible region on C-x C-x
(defun exchange-point-and-mark-no-region ()
  "Suppress region visibility when exchanging point and mark."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))

(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-region)

;; Remove trailing whitespace
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; String inflection
(global-set-key (kbd "C-c i") 'hydra-string-inflection/body)
(defhydra hydra-string-inflection ()
  "Inflect"
  ("u" string-inflection-underscore "underscore")
  ("U" string-inflection-upcase "UPPER_UNDERSCORE")
  ("c" string-inflection-lower-camelcase "camelCase")
  ("C" string-inflection-camelcase "UpperCamelCase")
  ("-" string-inflection-lisp "hyphenate"))

;; Sensible window movement
(require 'windmove)
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

;; Show all files in speedbar
;; TODO fix this to not show files ending in ~
(let ((re "^\\.\\.?\\(\\(DS_Store\\)\\|\\(#.+\\)\\)?$"))
  (setq speedbar-directory-unshown-regexp re
        speedbar-file-unshown-regexp re))

;; Don't create .# lockfiles
(setq create-lockfiles nil)

(undo-tree-mode)
(define-key undo-tree-map (kbd "C-/") 'undo-tree-undo)

(setq-default bidi-display-reordering nil)

(setq kill-do-not-save-duplicates t)
