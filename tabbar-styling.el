;; change faces for better-looking tabs (and more obvious selected tab!)
;; full face specification to avoid inheriting from the frame font
;; or from mode-line
(set-face-attribute 'tabbar-default nil
		    :inherit nil
		    :height 110
		    :weight 'normal
		    :width 'normal
		    :slant 'normal
		    :underline nil
		    :strike-through nil
;; inherit from frame		    :inverse-video
		    :stipple nil
		    :background "gray80"
		    :foreground "black"
;;		    :box '(:line-width 2 :color "white" :style nil)
		    :box nil
		    :family "Lucida Grande")

(set-face-attribute 'tabbar-selected nil
		    :background "gray95"
		    :foreground "gray20"
		    :inherit 'tabbar-default
		    :box '(:line-width 3 :color "grey95" :style nil))
;; 		    :box '(:line-width 2 :color "white" :style released-button))

(set-face-attribute 'tabbar-unselected nil
		    :inherit 'tabbar-default
		    :background "gray80"
		    :box '(:line-width 3 :color "grey80" :style nil))

(defface tabbar-selected-highlight '((t
		    :foreground "black"
		    :background "gray95"))
  "Face for selected, highlighted tabs."
  :group 'tabbar)

(defface tabbar-unselected-highlight '((t
                    :foreground "black"
		    :background "grey75"
		    :box (:line-width 3 :color "grey75" :style nil)))
  "Face for unselected, highlighted tabs."
  :group 'tabbar)

(set-face-attribute 'tabbar-button nil
		    :inherit 'tabbar-default
		    :box nil)

(set-face-attribute 'tabbar-separator nil
		    :background "grey50"
 		    :foreground "grey50"
		    :height 1.0)

(setq tabbar-separator '(1)) ;; set tabbar-separator size to 1 pixel

(defface tabbar-selected-modified
  '((t
     :inherit tabbar-selected
     :weight bold
     :height 110
     ))
  "Face used for unselected tabs."
  :group 'tabbar)

(defface tabbar-unselected-modified
  '((t
     :inherit tabbar-unselected
     :weight bold
     :height 110
     ))
  "Face used for unselected tabs."
  :group 'tabbar)

(defface tabbar-key-binding '((t
				 :foreground "white"))
    "Face for unselected, highlighted tabs."
    :group 'tabbar)
