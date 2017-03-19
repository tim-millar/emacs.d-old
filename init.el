;; start emacs as server
(load "server")
(unless (server-running-p)
  (server-start))

; Set package repos
(require 'package)
(require 'json)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

;; add custom config files to load path
(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'load-path "~/.emacs.d/elpa/")

; switch off splash
(setq inhibit-splash-screen t)

;; start initial emasc in full-screen mode
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; all-the-icone
(use-package all-the-icons
  :config
  (setq all-the-icons-color-icons t)
  (setq all-the-icons-for-buffer t)
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; pull in emojis
(use-package emojify
  :init
  (add-hook 'org-mode-hook 'emojify-mode)
  (add-hook 'erc-mode-hook 'emojify-mode)
  (add-hook 'elfeed-show-mode-hook 'emojify-mode))

;; Configure org-mode
;; ====================
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(require 'ox-md)                        ; support markdown export

(setq org-agenda-files (list "~/my-stuff/org/todo.org"
                             "~/my-stuff/org/logs.org" 
                             "~/my-stuff/org/notes.org"
			     "~/my-stuff/org/comments.org"))

;; Capture
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "Agenda Todo" entry
	 (file+headline "~/my-stuff/org/todo.org" "Agenda")
	 "\n\n** TODO %?\n%T\n\n%i\n%a\n\n\n"
	 :empty-lines 1)

	("n" "Agenda Notes" entry
	 (file+headline "~/my-stuff/org/todo.org" "Agenda")
	 "\n\n** %?\n%T\n%i\n%a\n\n\n"
	 :empty-lines 1)))

;; Insert immediate timestamp
(setq org-agenda-skip-additional-timestamps nil)
(defun tm/org-time-stamp ()
  (interactive)
  (when (eq major-mode 'org-mode)
    (org-insert-time-stamp nil t t)
    (insert "\n")))
(define-key global-map (kbd "<f9>") 'tm/org-time-stamp)

;; Language support for org babel 
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (sh . t)
   (http . t)
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
   (scala . t)
   (haskell . t)
   ))

; fontify code in code blocks
(setq org-src-fontify-natively t)
; hide inline markup characters
(setq org-hide-emphasis-markers t)

;; org-bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; make org lists use bullets
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

; Setup custom global shortcuts
(global-set-key "\C-x\C-g" 'goto-line)
(global-set-key [f1] 'compile)
(global-set-key [f2] 'next-error)
(global-set-key (kbd "C-x K") 'kill-this-buffer)

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
;; sentences end with a single space
(setq sentence-end-double-space nil)
;; resize windows
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-<down>") 'shrink-window)
(global-set-key (kbd "C-S-<up>") 'enlarge-window)

;; ask before exit
(add-hook 'kill-emacs-query-functions
          (lambda () (y-or-n-p "Do you really want to exit Emacs? "))
          'append)

;;keep cursor at same position when scrolling
(setq scroll-preserve-screen-position 1)
;;scroll window up/down by one line
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

;; ansi colour codes in compilation buffers
(defun compilation-ansi-color-process-output ()
  (ansi-color-process-output nil)
  (set (make-local-variable 'comint-last-output-start)
       (point-marker)))
(add-hook 'compilation-filter-hook #'compilation-ansi-color-process-output)

;; require aliases
(require 'setup-alias)

;; set up some shortcuts to dotfiles & orgfiles
(set-register ?e (cons 'file "~/.emacs.d/init.el"))
(set-register ?z (cons 'file "~/.zshrc"))
(set-register ?n (cons 'file "~/my-stuff/org/notes.org"))
(set-register ?c (cons 'file "~/my-stuff/org/comments.org"))


;; no tabs!
(setq-default indent-tabs-mode nil)
;; whitespace mode
(require 'whitespace)
(global-set-key "\C-c\w" 'whitespace-mode)
(global-set-key "\C-c_t" 'whitespace-toggle-options)

;; set-up z-shell for shell-mode
(setq explicit-shell-file-name "/usr/bin/zsh")
(setq shell-file-name "zsh")
(setq explicit-zsh-args '("--login" "-i"))
(setenv "SHELL" shell-file-name)

(use-package all-the-icons)

;; jump to last edit
(require 'goto-chg)
(global-set-key (kbd "C-.") 'goto-last-change)
(global-set-key (kbd "C-,") 'goto-last-change-reverse)

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

;; (defun tm/ibuffer-show-filename ()
;;   (interactive)
;;   (let ((buf (ibuffer-current-buffer))
;;         (lsoutput nil))
;;     (when (file-exists-p (buffer-file-name buf))
;;       (with-temp-buffer
;;         (let* ((filename (buffer-file-name buf))
;;                (default-directory (file-name-directory filename))
;;                (just-filename (file-name-nondirectory filename)))
;;           (call-process "/bin/ls" nil t nil "-l" just-filename)
;;           (setq lsoutput (buffer-substring-no-properties (point-min) (- (point-max) 1))))))
;;     (message lsoutput)))
 
;; (define-key ibuffer-mode-map (kbd "a") 'tm/ibuffer-show-filename) ; ibuffer-mode-map is void

;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; Handy key definition
(define-key global-map "\M-Q" 'unfill-paragraph)

;; ibuffer-vc
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (add-to-list 'ibuffer-never-show-predicates "^\\*helm")
            (add-to-list 'ibuffer-never-show-predicates "^\\*magit")
            (setq ibuffer-show-empty-filter-groups nil)
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

;; ====================
;; Dired
;; ====================
(use-package async
  :ensure t
  :defer t
  :config (dired-async-mode 1))

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

;; info+
(eval-after-load "info" '(require 'info+))

;; load eshell config
(require 'setup-eshell)

;; anzu
(global-anzu-mode +1)

;; which-func-mode
(which-function-mode 1)
(eval-after-load "which-func"
  '(add-to-list 'which-func-modes '(java-mode cc-mode python-mode ruby-mode)))

;; helm setup is in setup-helm.el
(use-package helm
  :ensure t)
(require 'setup-helm)
(helm-flx-mode 1)
(helm-fuzzier-mode 1)
(use-package helm-themes
  :ensure t
  :defer t)
;; helm-dash
(setq helm-dash-browser-func 'eww)
;; helm-git-grep
(global-set-key (kbd "C-c g g") 'helm-git-grep)
;; Invoke `helm-git-grep' from isearch.
(define-key isearch-mode-map (kbd "C-c g g") 'helm-git-grep-from-isearch)
;; Invoke `helm-git-grep' from other helm.
(eval-after-load 'helm
  '(define-key helm-map (kbd "C-c g g") 'helm-git-grep-from-helm))
;; hide boring buffers
;; (setq helm-boring-buffer-regexp-list (list (rx "*magit-") (rx "*helm")))
(add-to-list 'helm-boring-buffer-regexp-list "\\`\\*magit")
;; (defun tm/dash-hook ()
;;   (local-set-key "C-h D d" 'helm-dash)
;;   (local-set-key "C-h D p" 'helm-dash-at-point))
(use-package helm-dash
  :ensure t
  :defer t
  :init
  (setq helm-dash-docsets-path (format "%s/.docsets" (getenv "HOME")))
  (setq helm-dash-common-docsets '("Ruby on Rails" "Ruby" "Haskell"))
  ;;   (add-hook 'ruby-mode-hook 'tm/dash-hook)
  ;;   (add-hook 'haskell-mode-hook 'tm/dash-hook)
  )
;; helm-tramp
(setq tramp-default-method "ssh")
(defalias 'exit-tramp 'tramp-cleanup-all-buffers)
(define-key global-map (kbd "C-c t") 'helm-tramp)

;; avy / ace-jump stuff
(avy-setup-default)
(global-set-key (kbd "C-#") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-g e") 'avy-goto-word-0)

;; rainbow-delimeter
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; nlinum-relative
(require 'nlinum-relative)
(add-hook 'prog-mode-hook 'nlinum-relative-mode)
(setq nlinum-relative-current-symbol "->")

 ;; pretty-lambda
(require 'pretty-lambdada)
(pretty-lambda-for-modes)

;; nicer colours in terminal emacs
(color-theme-approximate-on)

;; yasnippets
(add-to-list 'load-path "~/.emacs.d/elpa/")
(require 'yasnippet)
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand)

;; ====================
;; Version Control
;; ====================
;; magit
(global-set-key [?\C-c ?g ?b] 'mo-git-blame-current)
(global-set-key [?\C-c ?g ?f] 'mo-git-blame-file)
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-refs-local-branch-format "%4c %-25n %h %U%m\n")

;; timemachine
(global-set-key (kbd "C-c g t") 'git-timemachine-toggle)

;; git-messenger
(global-set-key (kbd "C-c g m") 'git-messenger:popup-message)
;; (define-key git-messenger-map (kbd "m") 'git-messenger:copy-message)
;; Enable magit-commit-mode after typing 's', 'S', 'd'
(add-hook 'git-messenger:popup-buffer-hook 'magit-commit-mode)

;; smeargle
(global-set-key (kbd "C-c g s") 'smeargle)
(global-set-key (kbd "C-c g c") 'smeargle-commits)
(global-set-key (kbd "C-c g r") 'smeargle-clear)
;; ;; Highlight regions at opening file
;; (add-hook 'find-file-hook 'smeargle) ; too intrusive - back to git-gutter?
;; ;; Updating after save buffer
;; (add-hook 'after-save-hook 'smeargle)

;; github
(use-package magithub
  :ensure t
  :after magit
  :config (magithub-feature-autoinject t))
(global-set-key (kbd "C-c g o") 'github-browse-file)
(global-set-key (kbd "C-c g l") 'git-link)

;; projectile / project management
(projectile-mode)
(setq projectile-mode-line '(:eval (format " [%s]" (projectile-project-name))))
(setq projectile-completion-system 'helm)
(helm-projectile-on)
;; projectile-rails
(add-hook 'projectile-mode-hook 'projectile-rails-on)
(projectile-rails-global-mode)

;; neotree setup
(use-package neotree
  :ensure t
  :defer t
  :config
  (setq neo-theme (if window-system 'icons 'arrow))
  (setq neo-smart-open t)
  (setq neo-mode-line-type 'neotree)
  (setq neo-smart-open 1)
  (setq neo-cwd-line-style 'button)
  (setq neo-auto-indent-point 1)
  ;; (setq projectile-switch-project-action 'neotree-projectile-action)
  :init
  (global-set-key [f7] 'neotree-toggle))

;; (defun neotree-project-dir () ;; not working
;;   "Open NeoTree using the git root."
;;   (interactive)
;;   (let ((project-dir (projectile-project-root))
;;         (file-name (buffer-file-name)))
;;     (if project-dir
;;         (if (neotree-toggle)
;;             (progn
;;               (neotree-dir project-dir)
;;               (neotree-find file-name)))
;;       (message "Could not find git project root."))))
;; (global-set-key [f7] 'neotree-project-dir)

;; ctags
(setq exec-path (append exec-path '("/usr/bin/ctags")))
(require 'ggtags)
;; (add-hook 'projectile-mode (lambda () ; turn on ggtags in projectile mode
;;                              (setq ggtags-mode 1)))
(require 'setup-helm-gtags)

;; auto-revert to tail logs
(add-to-list 'auto-mode-alist '("\\.log\\'" . auto-revert-mode))

;; ========================================
;; Hydra
;; ========================================
(use-package hydra
  :ensure t)

(global-set-key                         ; simple tester for package
 (kbd "C-c u")
 (defhydra utils (:color red)
   "utils"
   ("+" text-scale-increase "in")
   ("-" text-scale-decrease "out")
   ("s" flyspell-mode "flyspell")
   ("f" flycheck-mode "flycheck")
   ("r" (lambda () (interactive) (rvm-activate-corresponding-ruby)) "rvm")
   ("b" bundle-install "bundle")
   ))

(global-set-key
 (kbd "C-c j")
 (defhydra navigation (:color red)
   "navigation"
   ("l" forward-char "forward")
   ("h" backward-char "backward")
   ("k" (lambda () (interactive) (forward-line -1)) "up")
   ("j" forward-line "down")
   ("w" forward-word "next-word")
   ("e" (lambda () (interactive) (forward-word -1)) "last-word")
   ("]" forward-paragraph "next-para")
   ("[" (lambda () (interactive) (forward-paragraph -1)) "last-para")
   ("," beginning-of-buffer "start")
   ("." end-of-buffer "end")
   ("f" forward-sexp "next-sexp")
   ("b" backward-sexp "last-sexp")
   ("g" goto-line "goto")
   ("s" isearch-forward "search")
   ("r" isearch-backward "search-back")
   ("SPC" set-mark-command "mark")
   ("'" avy-goto-char-2 "goto-char")))

;; smartparens-mode
(package-initialize) ; not picking up smart-parens in the load-path
(add-to-list 'load-path "~/.emacs.d/elpa/")
(smartparens-global-mode t)
(require 'smartparens-config)
(require 'smartparens-ruby)
(sp-with-modes '(web-mode)
  (sp-local-pair "<" ">")
  (sp-local-pair "<%" "%>"))
(require 'web-mode-edit-element)
(add-hook 'web-mode-hook 'web-mode-edit-element-minor-mode)

;; load source code pro font
(set-frame-font "Source Code Pro 13")

;; ====================
;; Themes
;; ====================
(use-package creamsody-theme :ensure t :defer t)
(use-package darkokai-theme :ensure t :defer t)
(use-package darktooth-theme :ensure t :defer t)
(use-package gruvbox-theme :ensure t :defer t)
(use-package monokai-theme :ensure t :defer t)
(use-package spacemacs-theme :ensure t :defer t)

;; powerline-mode
(require 'powerline)
(powerline-default-theme)
(defvar mode-line-height 30)
(setq darkokai-mode-line-padding 1) ;; Default mode-line box width
(load-theme 'darkokai t)
;; terminal emacs won't load with this config
;; (add-to-list 'after-make-frame-functions
;;              (lambda ()
;;                (if (tty-type)
;;                    (require 'spacemacs-dark-theme)
;;                  (load-theme 'darkokai t))))

;; spacemacs' powerline
(require 'spaceline-config)
(spaceline-emacs-theme)
;; (setq powerline-default-separator 'wave)
(spaceline-toggle-minor-modes-off)
(spaceline-helm-mode 1)
(spaceline-info-mode 1)

;; ====================
;; ERC mode
;; ====================
;; turn off join/part messages in erc-mode
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
;; erc image
(require 'erc-image)
(add-to-list 'erc-modules 'image)
(erc-update-modules)
;; erc yt image
(require 'erc-yt)
(add-to-list 'erc-modules 'youtube)
(erc-update-modules)

;; Set RE-builder to use 'string' syntax
(require 're-builder)
(setq reb-re-syntax 'string)

;; Add markdown mode to alist
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; easy-kill
(global-set-key [remap kill-ring-save] 'easy-kill)

;; Source .bashrc for env variables
(let ((path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path 
        (append
         (split-string-and-unquote path ":")
         exec-path)))

;; which key mode
;; (require 'which-key)
;; (which-key-mode)
;; (global-set-key [f5] 'which-key-show-top-level)
(use-package which-key
  :config
  (which-key-mode)
  (bind-key* [f5] 'which-key-show-top-level))

;; add color to compilation buffers
(require 'ansi-color)
(defun tm/ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'tm/ansi-colorize-buffer)

;; multiple cursors mode
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; ;; autocomplete
;; (require 'auto-complete)
;; (require 'auto-complete-config) ; default config
;; (ac-config-default)
;; (setq ac-ignore-case nil)
;; ;; (add-to-list 'ac-modes 'ruby-mode)

;; company mode
(use-package company
  :ensure t
  :config
  '(add-to-list 'company-backends 'company-inf-ruby))

;; tramp
(require 'tramp)

;; http-mode for restclient
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;; ;; use linum with git-gutter
;; (git-gutter:linum-setup)

;; flycheck
(global-set-key [f6] 'flycheck-mode)

;; configure elfeed
(use-package elfeed
  :ensure t
  :defer t
  :init
  (require 'elfeed-goodies)
  (elfeed-goodies/setup)
  (global-set-key (kbd "C-c e") 'elfeed)
  (setq elfeed-feeds
        '(("http://www.xenosystems.net/feed/" nrx xs pol)
          ("http://www.xenosystems.net/comments/feed/" nrx xs)
          ("http://scholars-stage.blogspot.com/feeds/posts/default" pol)
          ("http://planet.emacsen.org/atom.xml" emacs development)
          ("http://planet.haskell.org/rss20.xml" haskell development fp)
          ("https://kseo.github.io/rss.xml" haskell development fp)
          ("http://www.kovach.me/rss.xml" haskell fp development)
          ("https://lexi-lambda.github.io/feeds/all.rss.xml" development fp)
          ("https://meaningness.com/feeds" meaningness pol)
          ("http://www.ribbonfarm.com/feed/" ribbonfarm pol)
          ("http://thearchdruidreport.blogspot.com/feeds/posts/default" archdruid pol)
          ("https://8thlight.com/blog/feed/rss.xml" 8thlight fp development))))

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
; hooks
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
;; (add-hook 'my-web-mode-hook 'git-gutter-mode)
(add-hook 'my-web-mode-hook 'whitespace-cleanup-mode)
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; js config
(setq js-indent-level 2)

;; ========================================
;; ruby config
;; ========================================

;; (use-package chruby
;;   :ensure t)
(use-package rvm
  :ensure t
  :config (rvm-use-default))

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
                               (setq ruby-insert-encoding-magic-comment nil)
                               (setq rake-completion-system 'helm)
                               (rvm-activate-corresponding-ruby)
                               (require 'inf-ruby)
                               (require 'ruby-compilation))))

;; (add-hook 'ruby-mode-hook 'git-gutter-mode)
(add-hook 'ruby-mode-hook 'whitespace-cleanup-mode)
(add-hook 'ruby-mode-hook 'column-enforce-mode)
(add-hook 'ruby-mode-hook 'ruby-refactor-mode-launch)
;; (add-hook 'ruby-mode-hook 'robe-mode)

;; rspec snippets
(require 'rspec-mode)
(add-hook 'dired-mode-hook 'rspec-dired-mode)
(eval-after-load 'rspec-mode
  '(rspec-install-snippets))
(setq rspec-use-spring-when-possible nil)

; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml.example$" . yaml-mode))

;; ========================================
;; Haskell config
;; ========================================
(setq exec-path (append exec-path '("/home/timothymillar/.cabal/bin")))
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(use-package intero
  :ensure t
  :defer t)
(add-hook 'haskell-mode-hook 'intero-mode)
(add-hook 'haskell-mode-hook '(lambda ()
                                (setq haskell-indentation-mode t)))

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
     (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-jump-to-def-or-tag)))

; Unicode symbols
(defvar haskell-font-lock-symbols)
(setq haskell-font-lock-symbols t)

;; racket config
(add-hook 'racket-mode-hook
          (lambda ()
            (local-unset-key "<f5>")))

;; lisp-modes
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'racket-mode-hook (lambda () (lispy-mode 1)))

;; ========================================
;; Clojure setup
;; ========================================
(use-package clojure-mode
  :ensure t
  :init
  (defconst clojure--prettify-symbols-alist
    '(("fn"   . ?λ)
      ("__"   . ?⁈)))
  :config
  (add-hook 'clojure-mode-hook 'global-prettify-symbols-mode)
  (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1))))

(use-package cider
  :ensure t
  :defer t
  :commands (cider cider-connect cider-jack-in))

(use-package 4clojure
  :defer t
  :init
  (bind-key "<f12> a" '4clojure-check-answers clojure-mode-map)
  (bind-key "<f12> n" '4clojure-next-question clojure-mode-map)
  (bind-key "<f12> p" '4clojure-previous-question clojure-mode-map)

  :config
  (defadvice 4clojure-open-question (around 4clojure-open-question-around)
     "Start a cider/nREPL connection if one hasn't already been started when
     opening 4clojure questions."
     ad-do-it
     (unless cider-current-clojure-buffer
       (cider-jack-in))))

(defun endless/4clojure-check-and-proceed ()
  "Check the answer and show the next question if it worked."
  (interactive)
  (unless
      (save-excursion
        ;; Find last sexp (the answer).
        (goto-char (point-max))
        (forward-sexp -1)
        ;; Check the answer.
        (cl-letf ((answer
                   (buffer-substring (point) (point-max)))
                  ;; Preserve buffer contents, in case you failed.
                  ((buffer-string)))
          (goto-char (point-min))
          (while (search-forward "__" nil t)
            (replace-match answer))
          (string-match "failed." (4clojure-check-answers))))
    (4clojure-next-question)))

(defadvice 4clojure/start-new-problem
    (after endless/4clojure/start-new-problem-advice () activate)
    ;; Prettify the 4clojure buffer.
  (goto-char (point-min))
  (forward-line 2)
  (forward-char 3)
  (fill-paragraph)
  ;; Position point for the answer
  (goto-char (point-max))
  (insert "\n\n\n")
  (forward-char -1)
  ;; Define our key.
  (local-set-key (kbd "M-j") #'endless/4clojure-check-and-proceed))

;; Mongo
(use-package inf-mongo
  :ensure t
  :defer t
  :init
  (setq inf-mongo-command "/usr/bin/mongo 127.0.0.1:27017")
  :bind (("C-c m" . inf-mongo)))

;; Java
;; (require 'meghanada)
;; (add-hook 'java-mode-hook
;;           (lambda ()
;;             (meghanada-mode t)
;;             (setq c-basic-offset 2)))

(use-package meghanada
  :ensure t
  :defer t
  :config
  (add-hook 'java-mode-hook
          (lambda ()
            (meghanada-mode t))))

;; Python
(use-package virtualenvwrapper
  :ensure t
  :defer t
  :init
  (setq venv-location '("/home/timothymillar/my-stuff/data-sci/fast-ai-support/week2/cs231n/classification"
                        "/home/timothymillar/my-stuff/data-sci/fast-ai-course"))
  )

;; ========================================
;; skewer-mode
;; ========================================
(use-package skewer-mode
  :ensure t
  :defer t
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

;; (spaceline-toggle-projectile-root-on)
;; (spaceline-toggle-version-control-on)

(require 'diminish)
(diminish 'yas-minor-mode)
(diminish 'whitespace-cleanup-mode)
(diminish 'auto-complete-mode)
(diminish 'undo-tree-mode)
(diminish 'smartparens-mode)
(diminish 'helm-mode)
(diminish 'anzu-mode)
(diminish 'helm-gtags-mode)
(diminish 'which-key-mode)
(diminish 'git-gutter-mode)
(diminish 'auto-revert-mode)
(diminish 'rspec-mode)
(diminish 'projectile-rails-mode)
(diminish 'column-enforce-mode)

(add-to-list 'exec-path "~/bin")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes (quote (darkokai)))
 '(custom-safe-themes
   (quote
    ("70403e220d6d7100bae7775b3334eddeb340ba9c37f4b39c189c2c29d458543b" default)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote auto))
 '(haskell-tags-on-save t)
 '(projectile-enable-idle-timer t)
 '(projectile-idle-timer-hook nil)
 '(projectile-idle-timer-seconds 900)
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4))))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
