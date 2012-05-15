(deftheme blunted "The Blunted color theme -- a muted version of the Emacs default light theme.")

(let ((class '((class color) (min-colors 89)))
      ;; Blunted palette
      ;; colors with +x are lighter, colors with -x are darker
      (blunted-min "#333")
      (blunted-grey-0 "#888")
      (blunted-grey-1 "#aaa")
      (blunted-grey-2 "#ddd")
      (blunted-grey-3 "#e6e6e6")
      (blunted-max "#f3f3f0")
      (blunted-region "#bdd6ff")
      (blunted-prompt "#23d")
      (blunted-paren-match "#aaffff")
      (blunted-paren-mismatch "#ff7766")
      (blunted-type "#474")
      (blunted-function-name "#23d")
      (blunted-constant "#28a")
      (blunted-string "#723")
      (blunted-builtin "#56d")
      (blunted-keyword "#637")
      (blunted-variable-name "#950"))
  (custom-theme-set-faces
   'blunted
   '(button ((t (:underline t))))
   `(link ((,class (:foreground ,blunted-constant :underline t :weight bold))))
   `(link-visited ((,class (:foreground ,blunted-constant :underline t :weight normal))))

   ;;; basic coloring
   `(default ((,class (:foreground ,blunted-min :background ,blunted-max))))
   `(cursor ((,class (:background ,blunted-grey-1))))
   `(escape-glyph-face ((,class (:foreground ,blunted-function-name))))
   `(fringe ((,class (:foreground ,blunted-grey-3 :background ,blunted-max))))
   `(header-line ((,class (:foreground ,blunted-grey-1
                                       :background ,blunted-grey-2
                                       :box nil))))
   `(highlight ((,class (:background ,blunted-grey-2))))

   `(mode-line
     ((,class (:foreground ,blunted-max
                           :background ,blunted-grey-1
                           :box nil))))
   `(mode-line-buffer-id ((,class (:foreground ,blunted-max :weight bold))))
   `(mode-line-inactive
     ((,class (:foreground ,blunted-grey-3
                           :background ,blunted-grey-2
                           :box nil))))
   `(region ((,class (:background ,blunted-region))))
   `(vertical-border ((,class (:foreground ,blunted-grey-2))))

   ;;; font lock
   `(font-lock-builtin-face ((,class (:foreground ,blunted-builtin))))
   `(font-lock-comment-face ((,class (:foreground ,blunted-grey-0))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,blunted-grey-1))))
   `(font-lock-constant-face ((,class (:foreground ,blunted-constant))))
   `(font-lock-doc-face ((,class (:foreground ,blunted-string))))
   `(font-lock-doc-string-face ((,class (:foreground ,blunted-grey-1))))
   `(font-lock-function-name-face ((,class (:foreground ,blunted-function-name))))
   `(font-lock-keyword-face ((,class (:foreground ,blunted-keyword))))
   `(font-lock-negation-char-face ((,class (:foreground ,blunted-min))))
   `(font-lock-preprocessor-face ((,class (:foreground ,blunted-builtin))))
   `(font-lock-string-face ((,class (:foreground ,blunted-string))))
   `(font-lock-type-face ((,class (:foreground ,blunted-type))))
   `(font-lock-variable-name-face ((,class (:foreground ,blunted-variable-name))))
   `(font-lock-warning-face ((,class (:foreground ,blunted-paren-mismatch :underline t))))

   `(c-annotation-face ((,class (:inherit font-lock-constant-face))))

   ;;; external

   ;; speedbar
   `(speedbar-directory-face ((,class (:height 110 :inherit 'variable-pitch :weight bold :foreground ,blunted-constant))))
   `(speedbar-file-face ((,class (:inherit 'speedbar-directory-face :weight normal :foreground ,blunted-string))))
   `(speedbar-tag-face ((,class (:inherit 'speedbar-directory-face :weight normal :foreground ,blunted-builtin))))
   `(speedbar-selected-face ((,class (:inherit 'speedbar-directory-face :weight normal :foreground ,blunted-type))))
   `(speedbar-highlight-face ((,class (:inherit 'speedbar-directory-face :background ,blunted-region))))
   `(speedbar-button-face ((,class (:inherit 'fixed-pitch :foreground ,blunted-builtin :weight normal))))

   ;; linum-mode
   `(linum ((,class (:foreground ,blunted-grey-1 :background ,blunted-grey-3 :height 110 :box (:line-width 1 :color ,blunted-grey-3)))))

   `(paren-face-match ((,class (:foreground ,blunted-max :background ,blunted-paren-match))))
   `(paren-face-mismatch ((,class (:foreground ,blunted-max :background ,blunted-paren-mismatch))))
   `(paren-face-no-match ((,class (:foreground ,blunted-max :background ,blunted-paren-mismatch))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,blunted-builtin))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,blunted-constant))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,blunted-builtin))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,blunted-function-name))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,blunted-constant))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,blunted-builtin))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,blunted-string))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,blunted-function-name))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,blunted-constant))))
   `(rainbow-delimiters-depth-10-face ((,class (:foreground ,blunted-string))))
   `(rainbow-delimiters-depth-11-face ((,class (:foreground ,blunted-builtin))))
   `(rainbow-delimiters-depth-12-face ((,class (:foreground ,blunted-function-name))))

   ;; show-paren
   `(show-paren-mismatch ((,class (:foreground ,blunted-min :background ,blunted-paren-mismatch))))
   `(show-paren-match ((,class (:foreground ,blunted-min :background ,blunted-paren-match))))

  ;;; custom theme variables
  (custom-theme-set-variables
   'blunted
   `(ansi-color-names-vector [,blunted-max ,blunted-function-name ,blunted-string ,blunted-constant
                                          ,blunted-builtin ,blunted-variable-name ,blunted-builtin ,blunted-min])

   ;; fill-column-indicator
   `(fci-rule-color ,blunted-grey-2))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'blunted)
