; Set package repos
(require 'package)
(require 'json)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

; switch off splash
(setq inhibit-splash-screen t)

; Configure org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(setq org-log-done t)

;; Language support for org babel 
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (python . t)
   (R . t)
   (ruby . t)
   (ditaa . t)
   (dot . t)
   (octave . t)
   (scheme . t)
   (sqlite . t)
   (sql . t)
   (makefile . t)
   (perl . t)
   (C . t)
   (gnuplot . t)
   (latex . t)
   (lisp . t)
   (emacs-lisp . t)
   (java . t)
   (R . t)
   (scala . t)
   (haskell . t)
   ))

;; fontify code in code blocks
(setq org-src-fontify-natively t)

; Setup custom global shortcuts
(global-set-key "\C-x\C-g" 'goto-line)
(global-set-key [f1] 'compile)
(global-set-key [f2] 'next-error)

; switch off tool bar
(tool-bar-mode -1)
; switch on menu bar
(menu-bar-mode -1)
; turn on paren highlighting
(show-paren-mode t)
; switch on column-number mode
(setq column-number-mode t)
; turn off (non-working) scroll bars
(scroll-bar-mode -1)
;; No blinking cursor
(blink-cursor-mode 0)
;; Highlight current line
(global-hl-line-mode 1)
;; winner mode
(winner-mode 1)
;; windmove
(windmove-default-keybindings)
(setq windmove-wrap-around t)
;; resize windows
(global-set-key (kbd "s-C-<kp-left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-C-<kp-right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-<kp-down>") 'shrink-window)
(global-set-key (kbd "s-C-<kp-up>") 'enlarge-window)

;; no tabs!
(setq-default indent-tabs-mode nil)
;; whitespace mode
(require 'whitespace)
(global-set-key "\C-c_w" 'whitespace-mode)
(global-set-key "\C-c_t" 'whitespace-toggle-options)

;; set-up z-shell for shell-mode
(setq explicit-shell-file-name "/usr/bin/zsh")
(setq shell-file-name "zsh")
(setq explicit-zsh-args '("--login" "-i"))
(setenv "SHELL" shell-file-name)

;; better defaults
;; rename buffers w/ location
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
;; saves location of point when killng buffers
(require 'saveplace)
(setq-default save-place t)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

;; ibuffer-vc
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 18 18 :left :elide)
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              (vc-status 16 16 :left)
              " "
              filename-and-process)))

;; Stop backup changing file creation data
(setq backup-by-copying t)

;; make backup to a designated dir, mirroring the full path
(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
	 (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, ⁖ “C:”
	 (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setq make-backup-file-name-function 'my-backup-file-name)

;; Dired
;; allow dired to be able to delete or copy a whole dir.
(setq dired-recursive-copies (quote always)) ; “always” means no asking
(setq dired-recursive-deletes (quote top)) ; “top” means ask once
(setq dired-dwim-target t)
; dirtree
(require 'dirtree)
(global-set-key (kbd "s-d") 'dirtree)
; dired-details
(require 'dired-details)
(dired-details-install)

;; which-func-mode
(which-function-mode 1)
(eval-after-load "which-func"
      '(add-to-list 'which-func-modes '(java-mode cc-mode python-mode ruby-mode)))

;; Ido-mode
;; enable ido-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(require 'ido-hacks) ;ido-hacks
(require 'ido-vertical-mode) ;vertical-mode
(ido-vertical-mode 1) 
(require 'flx-ido) ;flx-ido-mode
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)

;; Enable smex
(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                  ; when Smex is auto-initialized on its first run.
;; smex key-bindings
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
 
;; rainbow-delimeter
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters)

 ;; pretty-lambda
(require 'pretty-lambdada)
(pretty-lambda-for-modes)

;; yasnippets
(add-to-list 'load-path "~/.emacs.d/elpa/")
(require 'yasnippet)
(yas-global-mode 1)

;; git
(global-set-key [?\C-c ?g ?c] 'mo-git-blame-current)
(global-set-key [?\C-c ?g ?f] 'mo-git-blame-file)
(global-set-key (kbd "C-x g") 'magit-status)

;; projectile / project management
(projectile-global-mode)
(setq projectile-mode-line '(:eval (format " Proj[%s]" (projectile-project-name))))

;; ctags
(setq exec-path (append exec-path '("/usr/bin/ctags")))
(require 'ggtags)
(add-hook 'projectile-mode (lambda ()
                             (setq ggtags-mode 1))) ; turn on ggtags in projectile mode
;; (defun create-tags (dir-name) ; is this not needed?
;;     "Create tags file."
;;     (interactive "DDirectory: ")
;;     (shell-command
;;      (format "ctags -a -f TAGS -e -R %s" (directory-file-name dir-name))))

;; smartparens-mode
(package-initialize) ; not picking up smart-parens in the load-path
(add-to-list 'load-path "~/.emacs.d/elpa/")
(smartparens-global-mode t)
(require 'smartparens-config)
(require 'smartparens-ruby)
(sp-with-modes '(web-mode)
  (sp-local-pair "<" ">")
  (sp-local-pair "<%" "%>"))

;; powerline-mode
(require 'powerline)
(powerline-default-theme)

;; start emacs server
(load "server")
(unless (server-running-p)
  (server-start))

;; turn off join/part messages in erc-mode
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; Set RE-builder to use 'string' syntax
(require 're-builder)
(setq reb-re-syntax 'string)

;; Add markdown mode to alist
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Shrink & enlarge windows
(global-set-key (kbd "M-<kp-up>") 'enlarge-window)
(global-set-key (kbd "M-<kp-down>") 'shrink-window)

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Source .bashrc for env variables
(let ((path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path 
        (append
         (split-string-and-unquote path ":")
         exec-path)))

;; add color to compilation buffers
(require 'ansi-color)
(defun my/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)

;; multiple cursors mode
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; autocomplete
(require 'auto-complete)
(require 'auto-complete-config) ; default config
(ac-config-default)
(setq ac-ignore-case nil)
;; (add-to-list 'ac-modes 'ruby-mode)

; tramp
(require 'tramp)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
; hooks
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

; ruby-mode hook
(defun ruby-mode-hook ()
  (autoload 'ruby-mode "ruby-mode" nil t)
  (add-to-list 'auto-mode-alist '("Capfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))
  (add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
  (add-hook 'ruby-mode-hook '(lambda ()
                               (setq ruby-deep-arglist t)
                               (setq ruby-deep-indent-paren nil)
                               (setq c-tab-always-indent nil)
                               (setq ggtags-mode 1)
                               (require 'inf-ruby)
                               (require 'ruby-compilation))))

; rvm-mode
(require 'rvm)
(rvm-use-default) ;; use rvm's default ruby for the current Emacs session

; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; ;; Robe setup
;; (add-hook 'ruby-mode-hook 'robe-mode)
;; (add-hook 'robe-mode-hook 'ac-robe-setup)
;; (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
;;   (rvm-activate-corresponding-ruby))

;; haskell config
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
; keybindings
(eval-after-load 'haskell-mode
          '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))
(eval-after-load 'haskell-mode 
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
     (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
     (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
     (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
     (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
; Unicode symbols
(defvar haskell-font-lock-symbols)
(setq haskell-font-lock-symbols t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (misterioso)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote ghci))
 '(projectile-enable-idle-timer t)
 '(projectile-idle-timer-hook nil)
 '(projectile-idle-timer-seconds 900)
 '(show-paren-mode t)
 '(tool-bar-mode nil))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
