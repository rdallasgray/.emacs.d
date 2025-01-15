(electric-indent-mode t)

(use-package smartparens
  :config
  (smartparens-global-mode t)
  (require 'smartparens-config)
  (let ((pairs '(("{" nil) ("[" nil))))
    (mapc
     (lambda (pair)
       (sp-pair (-first-item pair)
                (-last-item pair)
                :post-handlers
                '(:add ("||\n[i]" "RET"))))
     pairs))

  (defun rdg/sp-ruby-mode-hook ()
    (lambda ()
      (sp-local-pair 'ruby-mode
                     "{"
                     nil
                     :post-handlers
                     '(:add ("||\n[i]" "RET")))))

  (defun rdg/sp-ruby-ts-mode-hook ()
    (lambda ()
      (sp-local-pair 'ruby-ts-mode
                     "{"
                     nil
                     :post-handlers
                     '(:add ("||\n[i]" "RET")))))

  ;; Fix for ruby-mode, which appears to override handlers
  (add-hook 'ruby-mode-hook (rdg/sp-ruby-mode-hook))
  (add-hook 'ruby-ts-mode-hook (rdg/sp-ruby-ts-mode-hook))

  (sp-local-pair
   '(markdown-mode gfm-mode) "*" "*"
   :unless '(sp-in-string-p) :actions '(insert wrap))

  (setq sp-highlight-pair-overlay nil))

(use-package flycheck
  :config
  (flycheck-mode)
  (defun rdg/flycheck-display-errors-function (errors)
    (mapc (lambda (err)
            (message "FlyC: %s" (flycheck-error-message err)) (sit-for 1))
          errors))
  (setq flycheck-highlighting-mode nil
        flycheck-display-errors-function 'rdg/flycheck-display-errors-function))

(use-package web-mode
  :config
  (add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-disable-auto-pairing t))))


;; Delete marked text on typing
(delete-selection-mode t)

;; Soft-wrap lines
(global-visual-line-mode t)

;; Don't use tabs for indent; replace tabs with two spaces.
(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

;; Better scrolling with mouse wheel/trackpad.
(unless (and (boundp 'mac-mouse-wheel-smooth-scroll) mac-mouse-wheel-smooth-scroll)
  (global-set-key [wheel-down] (lambda () (interactive) (scroll-up-command 1)))
  (global-set-key [wheel-up] (lambda () (interactive) (scroll-down-command 1)))
  (global-set-key [double-wheel-down] (lambda () (interactive) (scroll-up-command 2)))
  (global-set-key [double-wheel-up] (lambda () (interactive) (scroll-down-command 2)))
  (global-set-key [triple-wheel-down] (lambda () (interactive) (scroll-up-command 4)))
  (global-set-key [triple-wheel-up] (lambda () (interactive) (scroll-down-command 4))))

;; Character encodings default to utf-8.
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; apply syntax highlighting to all buffers
(global-font-lock-mode t)

;; auto json-mode
(push '("\\.json\\'" . json-mode) auto-mode-alist)

;; 2-space indent for CSS
(setq css-indent-offset 2)

;; Default Ruby filetypes
(dolist (regex
         '("\\.watchr$" "\\.arb$" "\\.rake$" "\\.gemspec$" "\\.ru$" "Rakefile$"
           "Gemfile$" "Capfile$" "Guardfile$" "Rakefile$" "Cheffile$" "Vagrantfile$"
           "Berksfile$" "\\.builder$"))
  (add-to-list 'auto-mode-alist `(,regex . ruby-mode))
  (add-to-list 'auto-mode-alist `(,regex . ruby-ts-mode)))

(provide 'rdg-editing)
