(deftheme graphene-speedbar "The Graphene colour theme for Speedbar.")

(require 'graphene-theme)

(custom-theme-set-faces
 'graphene-speedbar
 `(default ((t (:foreground unspecified :background "#E0e4ea" :inherit sidebar-face))))
 `(fringe ((t (:background "#E0e4ea"))))
 `(speedbar-directory-face ((t (:foreground unspecified :background unspecified
                                            :inherit sidebar-face :weight bold))))
 `(speedbar-file-face ((t (:foreground unspecified :inherit speedbar-directory-face :weight normal))))
 `(speedbar-tag-face ((t (:foreground unspecified :inherit sidebar-button-face))))
 `(speedbar-selected-face ((t (:background unspecified :height unspecified :inherit sidebar-selected-face))))
 `(speedbar-highlight-face ((t (:background unspecified :inherit sidebar-highlight-face))))
 `(speedbar-button-face ((t (:foreground unspecified :background unspecified :inherit sidebar-button-face))))
 `(speedbar-separator-face ((t (:foreground unspecified :background unspecified
                                            :inherit speedbar-directory-face :overline nil))))
 )

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'graphene-speedbar)
