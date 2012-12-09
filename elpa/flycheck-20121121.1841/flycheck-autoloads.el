;;; flycheck-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (flycheck-mode-off flycheck-mode-on flycheck-mode
;;;;;;  flycheck-cleanup flycheck-init) "flycheck" "flycheck.el"
;;;;;;  (20667 30182))
;;; Generated autoloads from flycheck.el

(autoload 'flycheck-init "flycheck" "\
Wrap checker PROPERTIES into an init function.

PROPERTIES is the properties list describing a checker.

Use this function `apply-partially' to construct a real init
function for flymake.

\(fn)" nil nil)

(defadvice flymake-get-init-function (around flycheck-get-init-function first activate compile) "\
Get the flymake checker.

Return `flycheck-init-function', if variable `flycheck-mode' is enabled." (setq ad-return-value (if flycheck-mode (quote flycheck-init) ad-do-it)))

(autoload 'flycheck-cleanup "flycheck" "\
Perform cleanup for flycheck.

\(fn)" nil nil)

(defadvice flymake-get-cleanup-function (around flycheck-get-cleanup-function activate compile) "\
Get the cleanup function for the current checker." (setq ad-return-value (if flycheck-mode (quote flycheck-cleanup) ad-do-it)))

(defadvice flymake-mode (around flycheck-flymake-mode activate compile) "\
Variable `flymake-mode' is incompatible with variable `flycheck-mode'.
Signal an error if the latter is active." (if flycheck-mode (error "Flymake-mode is incompatible with flycheck-mode.  Use either flymake-mode or flycheck-mode") (setq ad-return-value ad-do-it)))

(autoload 'flycheck-mode "flycheck" "\
Toggle extended on-the-fly syntax checking.

Extended on-the-fly syntax checking based on flymake, but with
easier configuration and improved checkers.

`flycheck-mode' is incompatible with `flymake-mode'.
Signal an error if the latter is active.  Note: Pure flymake is
INCOMPATIBLE with this mode.

\(fn &optional ARG)" t nil)

(autoload 'flycheck-mode-on "flycheck" "\
Unconditionally enable variable `flycheck-mode'.

\(fn)" nil nil)

(autoload 'flycheck-mode-off "flycheck" "\
Unconditionally disable variable `flycheck-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("flycheck-pkg.el") (20667 30182 277028))

;;;***

(provide 'flycheck-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flycheck-autoloads.el ends here
