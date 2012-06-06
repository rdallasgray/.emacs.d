(deftheme graphene "The Graphene color theme -- a muted version of the Emacs default light theme.")

(let ((class '((class color) (min-colors 89)))
      ;; Graphene palette
      ;; colors with +x are lighter, colors with -x are darker
      (graphene-min "#333")
      (graphene-grey-0 "#888")
      (graphene-grey-1 "#aaa")
      (graphene-grey-2 "#ddd")
      (graphene-grey-3 "#e9eaec")
      (graphene-max "#f6f6f6")
      (graphene-sidebar "#e0e4ea")
      (graphene-region "#bdd6ff")
      (graphene-prompt "#23d")
      (graphene-paren-match "#aaffff")
      (graphene-paren-mismatch "#bb0000")
      (graphene-type "#723")
      (graphene-function-name "#23d")
      (graphene-string "#366")
      (graphene-constant "#179")
      (graphene-builtin "#56d")
      (graphene-keyword "#509")
      (graphene-variable-name "#b50"))

  (make-face 'graphene-speedbar-face)
  (set-face-background 'graphene-speedbar-face graphene-sidebar)
  (add-hook 'speedbar-mode-hook '(lambda () (buffer-face-set 'graphene-speedbar-face)))
  
  (custom-theme-set-faces
   'graphene
   `(link ((,class (:foreground ,graphene-constant :underline t :weight bold))))
   `(link-visited ((,class (:foreground ,graphene-constant :underline t :weight normal))))

   ;;; basic coloring
   `(default ((,class (:foreground ,graphene-min :background ,graphene-max))))
   `(cursor ((,class (:background ,graphene-grey-1))))
   `(escape-glyph-face ((,class (:foreground ,graphene-function-name))))
   `(fringe ((,class (:foreground ,graphene-grey-3 :background nil))))
   `(highlight ((,class (:background ,graphene-grey-2))))

   `(mode-line
     ((,class (:foreground ,graphene-max
                           :background ,graphene-grey-1
                           :box nil))))
   `(mode-line-buffer-id ((,class (:foreground ,graphene-max :weight bold))))
   `(mode-line-inactive
     ((,class (:foreground ,graphene-grey-3
                           :background ,graphene-grey-2
                           :box nil))))
   `(region ((,class (:background ,graphene-region))))
   `(vertical-border ((,class (:foreground ,graphene-grey-1))))

   ;;; font lock
   `(font-lock-builtin-face ((,class (:foreground ,graphene-builtin))))
   `(font-lock-comment-face ((,class (:foreground ,graphene-grey-0))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,graphene-grey-1))))
   `(font-lock-constant-face ((,class (:foreground ,graphene-constant))))
   `(font-lock-doc-face ((,class (:foreground ,graphene-string))))
   `(font-lock-doc-string-face ((,class (:foreground ,graphene-grey-1))))
   `(font-lock-function-name-face ((,class (:foreground ,graphene-function-name))))
   `(font-lock-keyword-face ((,class (:foreground ,graphene-keyword))))
   `(font-lock-negation-char-face ((,class (:foreground ,graphene-min))))
   `(font-lock-preprocessor-face ((,class (:foreground ,graphene-builtin))))
   `(font-lock-string-face ((,class (:foreground ,graphene-string))))
   `(font-lock-type-face ((,class (:foreground ,graphene-type))))
   `(font-lock-variable-name-face ((,class (:foreground ,graphene-variable-name))))
   `(font-lock-warning-face ((,class (:foreground ,graphene-paren-mismatch))))

   `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

   ;; speedbar
   `(speedbar-directory-face ((,class (:height 110 :inherit 'variable-pitch :weight bold :foreground ,graphene-min :weight bold))))
   `(speedbar-file-face ((,class (:inherit 'speedbar-directory-face :foreground ,graphene-min :weight normal))))
   `(speedbar-tag-face ((,class (:inherit 'speedbar-directory-face :weight normal :foreground ,graphene-builtin))))
   `(speedbar-selected-face ((,class (:inherit 'speedbar-directory-face :weight normal :foreground ,graphene-function-name))))
   `(speedbar-highlight-face ((,class (:inherit 'speedbar-directory-face :background ,graphene-region))))
   `(speedbar-button-face ((,class (:inherit 'fixed-pitch :foreground ,graphene-builtin :weight normal))))
   `(speedbar-separator-face ((,class (:inherit 'speedbar-directory-face :foreground ,graphene-min :background nil :overline nil))))

   ;; linum-mode
   `(linum ((,class (:foreground ,graphene-grey-1 :background ,graphene-grey-3 :height 110 :box (:line-width 1 :color ,graphene-grey-3)))))

   `(paren-face-match ((,class (:foreground ,graphene-max :background ,graphene-paren-match))))
   `(paren-face-mismatch ((,class (:foreground ,graphene-max :background ,graphene-paren-mismatch))))
   `(paren-face-no-match ((,class (:foreground ,graphene-max :background ,graphene-paren-mismatch))))

   ;; show-paren
   `(show-paren-mismatch ((,class (:foreground ,graphene-min :background ,graphene-paren-mismatch))))
   `(show-paren-match ((,class (:foreground ,graphene-min :background ,graphene-paren-match))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'graphene)
