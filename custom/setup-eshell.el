;; Eshell Setup
;; =============

;; set pager so programmes can't pause output
(setenv "PAGER" "cat")

;; make sure cabal and stack binaries are available
(setenv "PATH"
        (concat
         "/home/timothymillar/.local/bin:/home/timothymillar/.cabal/bin:/home/timothymillar/.rbenv/bin:/home/timothymillar/.rbenv/shims"
         (getenv "PATH")))

(require 'eshell)

;; Visual commands are commands which require a proper terminal.
;; eshell will run them in a term buffer when you invoke them.
(setq eshell-visual-commands
      '("less" "tmux" "htop" "top" "bash" "zsh" "fish" "ssh" "tail"
        "vi" "more" "lynx" "screen" "ncftp" "pine" "tin" "trn" "elm"))
(setq eshell-visual-subcommands
      '(("git" "log" "l" "ls" "diff" "show")))
;; (add-to-list 'eshell-command-completions-alist
;;              '("gunzip" "gz\\'"))
;; (add-to-list 'eshell-command-completions-alist
;;              '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))

;; Define a pretty prompt.
(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(setq eshell-cmpl-cycle-completions nil)

;; Use the Plan 9 commandline
(require 'em-smart)
(require 'em-cmpl)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

;; smartparens isn't working in eshell
(add-hook 'eshell-mode-hook #'smartparens-mode)

;; use port of z
(add-hook 'eshell-mode-hook
          (defun my-eshell-mode-hook ()
            (require 'eshell-z)))

;; eshell popups
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

(defun tm/eshell-quit-or-delete-char (arg)
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp))
      (progn
        (eshell-life-is-too-much) ; Why not? (eshell/exit)
        (delete-window))
    (delete-forward-char arg)))

(defun spacemacs/eshell-auto-end ()
  (when (and (eq major-mode 'eshell-mode)
             ;; Not on last line, we might want to edit within it.
             (not (eq (line-end-position) (point-max))))
    (end-of-buffer)))
(add-hook 'evil-insert-state-entry-hook 'spacemacs/eshell-auto-end)

(defun eshell/cdp ()
  "Change directory to the project's root."
  (eshell/cd (projectile-project-root)))

(defconst pcmpl-git-commands
  '("add" "bisect" "branch" "checkout" "clone"
    "commit" "diff" "fetch" "grep"
    "init" "log" "merge" "mv" "pull" "push" "rebase"
    "reset" "rm" "show" "status" "tag" )
  "List of `git' commands")

;; ;; open dired buffer - doesn't work properly
;; (defun eshell/d (&rest args)
;;   (dired (pop args) "."))

;; define aliases
(add-hook 'eshell-mode-hook (lambda ()

    (eshell/alias "ec" "find-file $1")
    (eshell/alias "ff" "find-file $1")
    (eshell/alias "emacs" "find-file $1")
    (eshell/alias "oo" "find-file-other-window $1")
    (eshell/alias "ll" "ls -AlohG")
    (eshell/alias "d" "dired $1")

    (eshell/alias "be" "bundle exec")
    (eshell/alias "befs" "bundle exec foreman start")

    (eshell/alias "gco" "git checkout $1")
    (eshell/alias "gbv" "git branch -vv")
    (eshell/alias "gfa" "git fetch --all")
    (eshell/alias "gfb" "gfa && gbv")

    (eshell/alias "gd" "magit-diff-unstaged")
    (eshell/alias "gds" "magit-diff-staged")
    (eshell/alias "gst" "magit-status")
    ;; (eshell/alias "gl" "magit-log-current") ; why does this not work?

    (define-key eshell-mode-map (kbd "C-d")
                               'tm/eshell-quit-or-delete-char)))

(provide 'setup-eshell)
