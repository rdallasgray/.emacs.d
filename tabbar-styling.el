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
		    :stipple nil
		    :background "gray80"
		    :foreground "black"
		    :box nil
		    :family "Lucida Grande")

(set-face-attribute 'tabbar-selected nil
		    :inherit 'tabbar-default
		    :background "gray95"
		    :foreground "gray20"
		    :box '(:line-width 3 :color "grey95" :style nil))

(set-face-attribute 'tabbar-unselected nil
		    :inherit 'tabbar-default
		    :background "gray70"
		    :box '(:line-width 3 :color "grey70" :style nil))

(set-face-attribute 'tabbar-selected-highlight nil
		    :underline nil)

(set-face-attribute 'tabbar-unselected-highlight nil
		    :underline nil)

(set-face-attribute 'tabbar-button nil
		    :inherit 'tabbar-default
		    :box nil)

(set-face-attribute 'tabbar-separator nil
		    :background "grey60"
 		    :foreground "grey60"
		    :height 1.0)

(set-face-attribute 'tabbar-selected-modified nil
		    :background "grey50"
 		    :foreground "grey50"
		    :height 1.0)

(set-face-attribute 'tabbar-unselected-modified nil
		    :background "grey50"
 		    :foreground "grey50"
		    :height 1.0)

(set-face-attribute 'tabbar-key-binding nil
		    :background "grey50"
 		    :foreground "grey50"
		    :height 1.0)

