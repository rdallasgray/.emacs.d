;;; flycheck-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (flycheck-mode-off flycheck-mode-on flycheck-mode)
;;;;;;  "flycheck" "flycheck.el" (20713 49311))
;;; Generated autoloads from flycheck.el

(defconst flycheck-mode-line-lighter " FlyC" "\
The standard lighter for flycheck mode.")

(autoload 'flycheck-mode "flycheck" "\
Minor mode for on-the-fly syntax checking.

When called interactively, toggle `flycheck-mode'.  With prefix
ARG, enable `flycheck-mode' if ARG is positive, otherwise disable
it.

When called from Lisp, enable `flycheck-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `flycheck-mode'.
Otherwise behave as if called interactively.

\(fn &optional ARG)" t nil)

(autoload 'flycheck-mode-on "flycheck" "\
Unconditionally enable variable `flycheck-mode'.

\(fn)" nil nil)

(autoload 'flycheck-mode-off "flycheck" "\
Unconditionally disable variable `flycheck-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("flycheck-pkg.el" "tests.el") (20713 49312
;;;;;;  2882))

;;;***

(provide 'flycheck-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; flycheck-autoloads.el ends here
