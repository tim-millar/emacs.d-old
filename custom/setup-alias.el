;; Aliases for Interactive Commands
;; ================================

(defalias 'yes-or-no-p 'y-or-n-p)

;; make frequently used commands short
(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)
(defalias 'dml 'delete-matching-lines)
(defalias 'dnml 'delete-non-matching-lines)
(defalias 'dtw 'delete-trailing-whitespace)
(defalias 'sl 'sort-lines)
(defalias 'rr 'reverse-region)
(defalias 'rs 'replace-string)

(defalias 'g 'grep)
(defalias 'gf 'grep-find)
(defalias 'fd 'find-dired)

(defalias 'rb 'revert-buffer)

(defalias 'sh 'shell)
(defalias 'fb 'flyspell-buffer)
(defalias 'sbc 'set-background-color)
(defalias 'rof 'recentf-open-files)
(defalias 'lcd 'list-colors-display)
(defalias 'cc 'calc)

;; elisp
(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'ed 'eval-defun)
(defalias 'eis 'elisp-index-search)
(defalias 'lf 'load-file)

;; ; major modes ; most of these are not needed
;; (defalias 'hm 'html-mode)
;; (defalias 'wm 'web-mode)
;; (defalias 'tm 'text-mode)
;; (defalias 'em 'emacs-lisp-mode)
;; (defalias 'om 'org-mode)
;; (defalias 'ssm 'shell-script-mode)

;; ; minor modes -- these are mostly not right
;; (defalias 'wsm 'whitespace-mode)
;; (defalias 'gwsm 'global-whitespace-mode)
;; (defalias 'vlm 'visual-line-mode)
;; (defalias 'glm 'global-linum-mode)

(provide 'setup-alias)
