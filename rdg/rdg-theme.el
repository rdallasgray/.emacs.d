;; Set relative font heights
(defvar rdg-font-height
  (face-attribute 'default :height)
  "Default font height.")
(defvar rdg-small-font-height
  (floor (* .917 rdg-font-height))
  "Relative size for 'small' fonts.")

(deftheme rdg "RDG meta-theme")

(custom-theme-set-faces
 'rdg

 `(bm-face
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit compilation-warning
                    :inverse-video t))))
 `(bm-fringe-face
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit bm-face
                    :inverse-video nil))))
 `(bm-persistent-face
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit font-lock-function-name-face
                    :inverse-video t))))
 `(bm-fringe-persistent-face
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit bm-persistent-face
                    :inverse-video nil))))

 `(persp-selected-face
   ((t (:weight bold))))

 `(treemacs-file-face
   ((t (:inherit variable-pitch :height ,rdg-small-font-height))))
 `(treemacs-directory-face
   ((t (:inherit treemacs-file-face :weight bold))))
 `(treemacs-tags-face
   ((t (:inherit treemacs-file-face))))
 `(treemacs-root-face
   ((t (:inherit treemacs-file-face :weight bold))))
 `(treemacs-term-node-face
   ((t (:inherit fixed-pitch))))
 `(treemacs-git-unmodified-face
   ((t (:inherit (treemacs-file-face)))))
 `(treemacs-git-modified-face
   ((t (:inherit (font-lock-variable-name-face treemacs-file-face)))))
 `(treemacs-git-renamed-face
   ((t (:inherit (font-lock-doc-face treemacs-file-face)))))
 `(treemacs-git-ignored-face
   ((t (:inherit (font-lock-comment-face treemacs-file-face)))))
 `(treemacs-git-untracked-face
   ((t (:inherit (font-lock-string-face treemacs-file-face)))))
 `(treemacs-git-added-face
   ((t (:inherit (font-lock-type-face treemacs-file-face)))))
 `(treemacs-git-conflict-face
   ((t (:inherit (error treemacs-file-face)))))


 `(tree-sitter-hl-face:property
   ((t (:inherit :inherit font-lock-constant-face))))


 `(linum
   ((t (:height ,rdg-small-font-height
                :foreground unspecified
                :inherit 'shadow
                :slant normal))))

 `(line-number
   ((t (:height ,rdg-small-font-height
                :foreground unspecified
                :background unspecified
                :inherit 'shadow))))

 `(visible-mark-face
   ((t (:foreground unspecified
                    :background unspecified
                    :inverse-video unspecified
                    :inherit 'hl-line))))

 `(hl-sexp-face
   ((t (:bold nil
              :background unspecified
              :inherit 'hl-line))))

 `(fringe
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit 'shadow))))

 `(vertical-border
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit file-name-shadow))))

 `(font-lock-comment-face
   ((t (:slant normal))))
 `(font-lock-comment-delimiter-face
   ((t (:slant normal))))

 `(font-lock-doc-face
   ((t (:slant normal))))

 `(popup-face
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit linum
                    :height ,rdg-font-height))))
 `(popup-scroll-bar-foreground-face
   ((t (:background unspecified
                    :inherit region))))
 `(popup-scroll-bar-background-face
   ((t (:background unspecified
                    :inherit popup-face))))

 `(corfu-default
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit highlight))))
 `(corfu-current
   ((t :background unspecified
       :foreground unspecified
       :inherit font-lock-variable-name-face
       :inverse-video t)))
 `(corfu-bar
   ((t :foreground unspecified
       :background unspecified
       :inherit default
       :inverse-video t)))

 `(company-preview
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit file-name-shadow))))
 `(company-preview-common
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit company-preview))))
 `(company-preview-search
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit company-preview
                    :weight bold))))
 `(company-tooltip
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit highlight
                    :height ,rdg-font-height))))
 `(company-tooltip-common
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit highlight
                    :weight bold
                    :height ,rdg-font-height))))
 `(company-tooltip-selection
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit font-lock-variable-name-face
                    :inverse-video t))))
 `(company-tooltip-common-selection
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit font-lock-variable-name-face
                    :weight bold
                    :inverse-video t))))
 `(company-tooltip-mouse
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit region))))
 `(company-tooltip-annotation
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit font-lock-function-name-face))))
 `(company-echo-common
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit company-echo
                    :weight bold))))
 `(company-scrollbar-fg
   ((t (:background unspecified
                    :inherit popup-scroll-bar-foreground-face))))
 `(company-scrollbar-bg
   ((t (:background unspecified
                    :inherit popup-scroll-bar-background-face))))

 `(flymake-warnline
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit font-lock-preprocessor-face))))

 `(web-mode-symbol-face
   ((t (:foreground unspecified
                    :inherit font-lock-constant-face))))
 `(web-mode-builtin-face
   ((t (:foreground unspecified
                    :inherit default))))
 `(web-mode-doctype-face
   ((t (:foreground unspecified
                    :inherit font-lock-comment-face))))
 `(web-mode-html-tag-face
   ((t (:foreground unspecified
                    :inherit font-lock-function-name-face))))
 `(web-mode-html-attr-name-face
   ((t (:foreground unspecified
                    :inherit font-lock-variable-name-face))))
 `(web-mode-html-param-name-face
   ((t (:foreground unspecified
                    :inherit font-lock-constant-face))))
 `(web-mode-whitespace-face
   ((t (:foreground unspecified
                    :inherit whitespace-space))))
 `(web-mode-block-face
   ((t (:foreground unspecified
                    :inherit highlight))))

 `(sp-show-pair-match-face
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit show-paren-match))))
 `(sp-show-pair-mismatch-face
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit show-paren-mismatch))))

 `(vr/match-0
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit font-lock-regexp-grouping-construct
                    :inverse-video t))))
 `(vr/match-1
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit font-lock-regexp-grouping-backslash
                    :inverse-video t))))
 `(vr/group-0
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit font-lock-keyword-face
                    :inverse-video t))))
 `(vr/group-1
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit font-lock-function-name-face
                    :inverse-video t))))
 `(vr/group-2
   ((t (:background unspecified
                    :foreground unspecified
                    :inherit font-lock-constant-face
                    :inverse-video t))))

 `(whitespace-space
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit highlight))))
 `(ivy-action
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit font-lock-builtin-face))))
 `(ivy-confirm-face
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit ivy-action
                    :weight bold))))
 `(ivy-current-match
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit highlight))))
 `(ivy-cursor
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit cursor))))
 `(ivy-match-required-face
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit font-lock-warning-face))))
 `(ivy-minibuffer-match-face-1
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit secondary-selection))))
 `(ivy-minibuffer-match-face-2
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit font-lock-constant-face
                    :inverse-video t))))
 `(ivy-minibuffer-match-face-3
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit font-lock-function-name-face
                    :inverse-video t))))
 `(ivy-minibuffer-match-face-4
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit font-lock-keyword-face
                    :inverse-video t))))
 `(ivy-remote
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit font-lock-keyword-face))))
 `(ivy-subdir
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit font-lock-function-name-face))))
 `(ivy-virtual
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit ivy-action))))
 `(swiper-line-face
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit highlight))))
 `(swiper-match-face-1
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit font-lock-regexp-grouping-backslash
                    :inverse-video t))))
 `(swiper-match-face-2
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit font-lock-regexp-grouping-construct
                    :inverse-video t))))
 `(swiper-match-face-3
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit font-lock-keyword-face
                    :inverse-video t))))
 `(swiper-match-face-4
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit font-lock-negation-char-face
                    :inverse-video t))))
 `(hydra-face-amaranth
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit link-visited))))
 `(hydra-face-blue
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit font-lock-function-name-face))))
 `(hydra-face-pink
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit font-lock-regexp-grouping-construct))))
 `(hydra-face-red
   ((t (:foreground unspecified
                    :background unspecified
                    :inherit font-lock-warning-face))))
 `(hydra-face-teal
   ((t (:foreground unspecified
                    :background unspecified
                    :Inherit font-lock-constant-face)))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'rdg)
