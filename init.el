;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;; (package-initialize)

(let ((file-name-handler-alist nil))
  (setq history-length 100)
  (put 'minibuffer-history 'history-length 50)
  (put 'kill-ring 'history-length 25)

  (setq gc-cons-threshold 8000000)

  (setq max-lisp-eval-depth 5000
        max-specpdl-size 5000)

  (setq custom-file "~/.emacs.d/custom.el")
  (when (file-exists-p custom-file)
    (load custom-file))

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

  (require 'package)
  (require 'cask "/usr/local/Cellar/cask/0.8.4/cask.el")
  (cask-initialize)
  (add-to-list 'auto-mode-alist '("\\Cask\\'" . emacs-lisp-mode))

  (when window-system
    (load-theme 'solarized t))

  (require 'pallet)
  (pallet-mode t)
  (require 'graphene)

  (require 'dash)
  (require 's)

  ;; long lines
  (require 'so-long)
  (defvar so-long-target-modes)
  (defvar so-long-threshold)
  (defvar so-long-max-lines)
  (add-to-list 'so-long-target-modes 'shell-mode)
  (setq so-long-threshold 500
        so-long-max-lines nil)
  (global-so-long-mode t)

  (add-hook 'graphene-prog-mode-hook 'eldoc-mode)

  (add-hook 'before-save-hook 'whitespace-cleanup)

  (autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR.")
  (global-set-key (kbd "M-Z") 'zap-up-to-char)

  (defun rdg/remove-fringe-and-margin ()
    (fringe-mode nil)
    (set-window-margins nil 0))

  ;; git-wip
  (let ((git-wip-path "/Users/robertdallasgray/Documents/Code/git-wip"))
    (add-to-list 'exec-path git-wip-path)
    (add-to-list 'load-path (format "%s/emacs/" git-wip-path)))

  (add-hook 'graphene-prog-mode-hook
            (lambda ()
              (require 'git-wip-mode)
              (git-wip-mode t)))

  ;; drawer
  (global-set-key (kbd "C-c d d") 'project-persist-drawer-toggle)

  ;; midnight
  (setq clean-buffer-list-delay-general 7)
  (midnight-delay-set 'midnight-delay "12:00am")

  ;; text scaling
  (global-set-key (kbd "C-M-=") 'default-text-scale-increase)
  (global-set-key (kbd "C-M--") 'default-text-scale-decrease)

  ;; anzu
  (global-anzu-mode t)

  ;; company
  (defvar rdg/company-no-return-key-modes '(shell-mode))

  (defun rdg/company-complete-on-return-p ()
    (not (derived-mode-p 'comint-mode)))

  (defun rdg/company-maybe-try-hard (buf win tick pos)
    (when (and (not company-candidates) (looking-back "[A-Za-z0-9_\-\/\.]" 1))
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
    (advice-remove 'company-idle-begin #'rdg/company-maybe-try-hard)
    (advice-add 'company-idle-begin :after #'rdg/company-maybe-try-hard))

  (setq tags-revert-without-query 1)

  (defvar rdg/company-default-backends
    '(company-files company-dabbrev-code company-etags company-capf company-keywords company-dabbrev))

  (defvar rdg/company-shell-backends '(company-files company-native-complete company-capf company-dabbrev))

  (defun rdg/company-set-mode-backends (backends)
    "Set BACKENDS locally"
    (set (make-local-variable 'company-backends)
         (-uniq (append backends rdg/company-default-backends))))

  (with-eval-after-load 'company
    (setq company-backends rdg/company-default-backends
          company-idle-delay 0.15)
    (rdg/advise-company-try-hard)
    (company-prescient-mode t)
    (define-key company-active-map (kbd "<tab>") #'company-complete-selection)
    (define-key company-active-map (kbd "C-<tab>") #'company-try-hard)
    (define-key company-active-map [return] #'rdg/company-maybe-complete-on-return)
    (define-key company-active-map (kbd "RET") #'rdg/company-maybe-complete-on-return))

  ;; counsel/swiper/ivy
  (ivy-prescient-mode t)
  (global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
  (global-set-key (kbd "C-r") 'counsel-grep-or-swiper)
  (global-set-key (kbd "C-x f") 'counsel-recentf)
  (global-set-key (kbd "C-c C-s") 'isearch-forward)
  (global-set-key (kbd "C-c C-r") 'isearch-backward)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "C-c u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g c") 'counsel-git-checkout)
  (global-set-key (kbd "C-c g f") 'counsel-git)
  (global-set-key (kbd "C-c g g") 'counsel-git-grep)
  (global-set-key (kbd "C-c e f") 'counsel-etags-find-tag)
  (global-set-key (kbd "C-c ! !") 'counsel-flycheck)
  (global-set-key (kbd "C-c //") 'counsel-tramp)

  ;; anzu
  (set-face-attribute 'anzu-mode-line nil
                      :foreground 'unspecified
                      :inherit 'company-tooltip-search)

  (setq anzu-mode-lighter ""
        anzu-deactivate-region t
        anzu-replace-to-string-separator " => ")

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
    ("+" er/expand-region "Expand")
    ("-" er/contract-region "Contract")
    (">" mc/mark-next-like-this "Next")
    ("<" mc/mark-previous-like-this "Previous")
    ("m" mc/mark-more-like-this-extended "More"))

  (global-set-key (kbd "C-M-SPC") 'hydra-mark-begin)

  ;; (e)shell-mode

  ;; Easily open/switch to a shell
  ;; Please don't open my shells in new windows
  (setq native-complete-style-regex-alist '((".+*(pry|guard).*> " . tab)))
  (add-to-list 'display-buffer-alist '("*shell*" display-buffer-same-window))
  (global-set-key (kbd "C-c `") 'shell-pop)
  (setq shell-pop-universal-key "C-c `"
        shell-pop-window-position "bottom")

  (defun rdg/remove-shell-control-chars (op)
    "Replace control characters in output OP with blank."
    (replace-regexp-in-string "\\[[0-9]+[JGK]" "" op))

  (defun rdg/setup-shell ()
    "Hook to set up shell modes."
    (setq comint-prompt-read-only t
          ;; comint-process-echoes t
          comint-buffer-maximum-size 10000)
    (ansi-color-for-comint-mode-on)
    (add-to-list 'comint-output-filter-functions
                 'ansi-color-process-output)
    (add-hook 'comint-output-filter-functions
              'comint-truncate-buffer)
    ;; (add-to-list 'comint-preoutput-filter-functions
    ;;              'rdg/remove-shell-control-chars)
    (unless (eq system-type 'windows-nt)
      (let ((shell-name "bash"))
        (setq explicit-shell-file-name shell-name
              shell-file-name shell-name)
        (setenv "SHELL" shell-name)
        (setenv "PAGER" "/bin/cat"))
      ;; (setq explicit-bash-args '("-li" "-c" "export EMACS=; stty echo; bash"))
      (define-key shell-mode-map (kbd "<tab>") #'company-complete)))

  (with-eval-after-load 'shell
    (require 'native-complete)
    (rdg/setup-shell)
    (native-complete-setup-bash)
    (add-hook 'shell-mode-hook
              (lambda ()
                (set (make-local-variable 'completion-at-point-functions)
                     (append completion-at-point-functions '(pcomplete-completions-at-point)))
                (rdg/company-set-mode-backends rdg/company-shell-backends)
                (rdg/remove-fringe-and-margin))))

  ;; sqlformat
  (setq sqlformat-command 'pgformatter)

  ;; dired
  (with-eval-after-load 'dired+
    (diredp-make-find-file-keys-reuse-dirs)
    (setq diredp-hide-details-initially-flag nil))

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

  ;; Scroll up and down a line at a time
  (global-set-key (kbd "C-S-v") 'scroll-up-line)
  (global-set-key (kbd "M-V") 'scroll-down-line)

  ;; SCSS
  (with-eval-after-load 'scss-mode
    (setq scss-compile-at-save nil))

  ;; JS/Coffee
  (add-to-list 'auto-mode-alist '("\\.cjsx\\'" . coffee-mode))

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

  (add-hook 'flycheck-mode-hook #'rdg/set-flycheck-eslint-executable)

  (defun rdg/eslint-fix-buffer ()
    (rdg/use-eslint-from-node-modules
     (lambda (eslint)
       (let ((file (buffer-file-name (current-buffer))))
         (call-process eslint nil nil nil "--fix" file)
         (revert-buffer t t t)))))

  (defun rdg/add-eslint-fix-buffer-hook ()
    (remove-hook 'after-save-hook 'rdg/eslint-fix-buffer t)
    (add-hook 'after-save-hook 'rdg/eslint-fix-buffer nil t))

  (with-eval-after-load 'flycheck
    (setq flycheck-coffee-executable "cjsx"
          flycheck-idle-change-delay 5
          flycheck-disabled-checkers (append flycheck-disabled-checkers
                                             '(javascript-jshint json-jsonlist)))
    (mapc (lambda (mode) (flycheck-add-mode 'javascript-eslint mode))
          '(js-mode js2-mode rjsx-mode)))

  (with-eval-after-load 'coffee-mode
    (setq coffee-command "cjsx"))

  (add-hook 'coffee-mode-hook 'subword-mode)

  (defun rdg/enable-minor-mode (re-mode)
    "Enable minor mode if filename matches the regexp. RE-MODE is a cons cell (regexp . minor-mode)."
    (if (buffer-file-name)
        (if (string-match (car re-mode) buffer-file-name)
            (funcall (cdr re-mode)))))

  ;; Web Mode
  (add-hook 'web-mode-hook (lambda () (setq web-mode-markup-indent-offset 2)))

  ;; Ruby
  (setq rubocop-prefer-system-executable t)

  (defun rdg/ruby-update-tags ()
    (let ((root (locate-dominating-file default-directory ".git")))
      (when root
        (cd root)
        (call-process-shell-command "ripper-tags -R -e --exclude=vendor"))))

  (defun rdg/add-ruby-update-tags-hook ()
    (remove-hook 'after-save-hook 'rdg/ruby-update-tags t)
    (add-hook 'after-save-hook 'rdg/ruby-update-tags nil t))

  (defvar rdg/ignored-compilation-buffer-match '("*RuboCop"))

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

  (defun rdg/rubocop-autocorrect-and-revert()
    (rubocop-autocorrect-current-file)
    (revert-buffer t t t))

  (defun rdg/rubocop-fix-layout-and-revert()
    (require 'rubocop)
    (rubocop--file-command "rubocop --fix-layout --format emacs")
    (revert-buffer t t t))

  (defun rdg/add-rubocop-autocorrect-hook ()
    (remove-hook 'after-save-hook 'rdg/rubocop-autocorrect-and-revert t)
    (add-hook 'after-save-hook 'rdg/rubocop-autocorrect-and-revert nil t))

  (defun rdg/add-rubocop-fix-layout-hook ()
    (remove-hook 'after-save-hook 'rdg/rubocop-fix-layout-and-revert t)
    (add-hook 'after-save-hook 'rdg/rubocop-fix-layout-and-revert nil t))

  (with-eval-after-load 'ruby-mode
    (exec-path-from-shell-copy-env "GEM_HOME"))

  (defun rdg/ruby-mode-hook ()
    (subword-mode)
    (yard-mode)
    (ruby-tools-mode)
    (setq ruby-insert-encoding-magic-comment nil
          ruby-deep-arglist nil
          ruby-deep-indent-paren nil
          ruby-deep-indent-paren-style nil
          ruby-use-smie t)
    (rdg/add-rubocop-fix-layout-hook)
    ;; (rdg/add-rubocop-autocorrect-hook)
    (rdg/add-ruby-update-tags-hook))

  (add-hook 'ruby-mode-hook 'rdg/ruby-mode-hook)

  ;; imenu
  (setq imenu-auto-rescan t)
  (global-set-key (kbd "C-c .") 'ivy-imenu-anywhere)

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

  ;; No visible region on C-x C-x
  (defun exchange-point-and-mark-no-region ()
    "Suppress region visibility when exchanging point and mark."
    (interactive)
    (exchange-point-and-mark)
    (deactivate-mark nil))

  (define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-region)

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

  (add-hook 'speedbar-mode-hook 'rdg/remove-fringe-and-margin)

  ;; Don't create .# lockfiles
  (setq create-lockfiles nil)

  (global-undo-tree-mode)
  (which-key-mode)

  (setq-default bidi-display-reordering nil)

  (setq kill-do-not-save-duplicates t))
