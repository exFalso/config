;;; tss-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (tss-config-default tss-setup-current-buffer tss-restart-current-buffer
;;;;;;  tss-reload-current-project tss-run-flymake tss-jump-to-definition
;;;;;;  tss-popup-help) "tss" "tss.el" (21236 56671 268269 155000))
;;; Generated autoloads from tss.el

(autoload 'tss-popup-help "tss" "\
Popup help about anything at point.

\(fn)" t nil)

(autoload 'tss-jump-to-definition "tss" "\
Jump to method definition at point.

\(fn)" t nil)

(autoload 'tss-run-flymake "tss" "\
Run check by flymake for current buffer.

\(fn)" t nil)

(autoload 'tss-reload-current-project "tss" "\
Reload project data for current buffer.

\(fn)" t nil)

(autoload 'tss-restart-current-buffer "tss" "\
Restart TSS for current buffer.

\(fn)" t nil)

(autoload 'tss-setup-current-buffer "tss" "\
Do setup for using TSS in current buffer.

\(fn)" t nil)

(autoload 'tss-config-default "tss" "\
Do setting recommemded configuration.

\(fn)" nil nil)

;;;***

;;;### (autoloads (typescript-mode) "typescript" "typescript.el"
;;;;;;  (21236 56671 278269 154000))
;;; Generated autoloads from typescript.el

(autoload 'typescript-mode "typescript" "\
Major mode for editing typescript.

Key bindings:

\\{typescript-mode-map}

\(fn)" t nil)

(eval-after-load 'folding '(when (fboundp 'folding-add-to-marks-list) (folding-add-to-marks-list 'typescript-mode "// {{{" "// }}}")))

;;;***

;;;### (autoloads nil nil ("tss-pkg.el") (21236 56671 299378 408000))

;;;***

(provide 'tss-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; tss-autoloads.el ends here