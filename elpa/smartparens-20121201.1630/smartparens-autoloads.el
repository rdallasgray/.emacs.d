;;; smartparens-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (turn-off-smartparens-mode turn-on-smartparens-mode
;;;;;;  smartparens-global-mode smartparens-mode) "smartparens" "smartparens.el"
;;;;;;  (20667 14386))
;;; Generated autoloads from smartparens.el

(defvar sp-keymap (make-sparse-keymap) "\
Keymap used for smartparens-mode.  Remaps all the trigger keys
to `self-insert-command'.  This means we lose some functionality
in some modes (like c-electric keys).")

(autoload 'smartparens-mode "smartparens" "\
Toggle smartparens mode

\(fn &optional ARG)" t nil)

(defvar smartparens-global-mode nil "\
Non-nil if Smartparens-Global mode is enabled.
See the command `smartparens-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `smartparens-global-mode'.")

(custom-autoload 'smartparens-global-mode "smartparens" nil)

(autoload 'smartparens-global-mode "smartparens" "\
Toggle Smartparens mode in all buffers.
With prefix ARG, enable Smartparens-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Smartparens mode is enabled in all buffers where
`turn-on-smartparens-mode' would do it.
See `smartparens-mode' for more information on Smartparens mode.

\(fn &optional ARG)" t nil)

(autoload 'turn-on-smartparens-mode "smartparens" "\
Turn on `smartparens-mode'.

\(fn)" t nil)

(autoload 'turn-off-smartparens-mode "smartparens" "\
Turn off `smartparens-mode'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("smartparens-pkg.el") (20667 14386 324385))

;;;***

(provide 'smartparens-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; smartparens-autoloads.el ends here
