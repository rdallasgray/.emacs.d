;;; wgrep-ack-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (wgrep-ack-setup wgrep-ack-and-a-half-setup) "wgrep-ack"
;;;;;;  "wgrep-ack.el" (20676 29458))
;;; Generated autoloads from wgrep-ack.el

(autoload 'wgrep-ack-and-a-half-setup "wgrep-ack" "\


\(fn)" nil nil)

(autoload 'wgrep-ack-setup "wgrep-ack" "\


\(fn)" nil nil)
(add-hook 'ack-and-a-half-mode-hook 'wgrep-ack-and-a-half-setup)
(add-hook 'ack-mode-hook 'wgrep-ack-setup)

;;;***

;;;### (autoloads nil nil ("wgrep-ack-pkg.el") (20676 29458 401029))

;;;***

(provide 'wgrep-ack-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; wgrep-ack-autoloads.el ends here
