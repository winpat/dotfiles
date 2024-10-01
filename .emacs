;; Increase GC threshold to speed up startup
(setq gc-cons-threshold 100000000)

;; Always use `text-mode` as default major mode
(setq default-major-mode 'text-mode)

;; Disable message in scratch buffer
(setq initial-scratch-message nil)

;; Disable the Emacs startup screen and show *scratch* instead
(setq inhibit-startup-screen t)

;; Don't pollute .emacs with the result of customize invocations
(setq custom-file (make-temp-file ""))
(setq custom-safe-themes t)

;; Disable the ugly scoll, tool and menu bar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Enable line and column numbering in modeline
(line-number-mode 1)
(column-number-mode 1)

;; Disable the blinking cursor
(setq blink-cursor-mode nil)
(setq visible-cursor nil)

;; Show matching parens
(show-paren-mode 1)

;; Allow to answer even important questions with "y" or "n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Highlight selections and allow overwriting them
(transient-mark-mode 1)
(delete-selection-mode 1)

;; Emacs...  Don't break lines for me, thx
(setq-default truncate-lines t)

;; Automatically follow symlinks and don't ask about it
(setq vc-follow-symlinks t)

;; Font type and size
(set-face-attribute 'default nil :height 130)
(set-face-attribute 'default nil :family "Hack")

;; Allow to resize emacs to exactly 50% on openbox
(setq frame-resize-pixelwise t)

;; Copy paste
(setq x-select-enable-primary t)

;; Common paths that change depending on the operating system
(setq sync-directory  "~/shared"
      notes-directory (format "%s/notes" sync-directory)
      vcs-directory   (format "%s/vcs"   sync-directory))

;; Don't create backup files
(setq make-backup-files nil)

;; Always trim trailing whitespace.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Emacs keybinding improvements
(global-set-key (kbd "M-o") 'switch-to-buffer)
(global-set-key (kbd "C-o") 'other-window)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(use-package naysayer-theme
  :ensure t
  :init (load-theme 'naysayer t))

(use-package helpful
  :ensure t
  :commands (helpful-callable
	     helpful-variable
	     helpful-key
	     helpful-command
	     helpful-function
	     helpful-at-point)
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)
   ("C-h F" . 'helpful-function)
   ("C-c C-d" . helpful-at-point)))

(use-package diminish
  :init
  (diminish 'python-black-on-save-mode)
  :ensure t)

(use-package eldoc
  :diminish eldoc-mode)

(use-package dimmer
  :ensure t
  :config
  (setq dimmer-fraction 0.2)
  (setq dimmer-exclusion-regexp "^\*helm.*\\|^ \*Minibuf-.*\\|^ \*Echo.*")
  (dimmer-mode))

(use-package autorevert
  :diminish auto-revert-mode)

(use-package evil
  :ensure t
  :init (setq evil-want-keybinding nil)
  :config (evil-mode 1))

(use-package evil-leader
  :ensure t
  :init (global-evil-leader-mode)
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "," 'xref-pop-marker-stack
    "." 'xref-find-definitions
    "a" 'xref-find-references
    "<SPC>" 'execute-extended-command
    "b," '(lambda () (interactive) (global-text-scale-adjust 1))
    "b." '(lambda () (interactive) (global-text-scale-adjust -1))
    "b0" '(lambda () (interactive) (global-text-scale-adjust (- text-scale-mode-amount)) (global-text-scale-mode -1))
    "bR" 'rename-file-and-buffer
    "bk" 'kill-buffer
    "bl" 'ibuffer
    "br" (lambda () (interactive) (revert-buffer t t))
    "bn" (lambda () (interactive) (kill-new (buffer-name)))
    "bw" (lambda () (interactive) (kill-new (buffer-file-name)))
    "c" 'calc
    "d." (lambda () (interactive) (dired "."))
    "dd" (lambda () (interactive) (dired "~/downloads/"))
    "dh" (lambda () (interactive) (dired "~"))
    "ds" (lambda () (interactive) (dired "~/shared/"))
    "dv" (lambda () (interactive) (dired "~/vcs/"))
    "ff" 'find-file
    "fo" 'find-file-other-window
    "fc" (lambda () (interactive) (find-file "/etc/nixos/configuration.nix"))
    "fe" (lambda () (interactive) (find-file "~/.emacs"))
    "fh" (lambda () (interactive) (find-file "/etc/nixos/host-configuration.nix"))
    "fs" (lambda () (interactive) (find-file "~/shared/"))
    "ft" (lambda () (interactive) (find-file "~/shared/todo.org"))
    "o" 'switch-to-buffer
    "t" 'toggle-source-and-tests
    "w1" 'delete-other-windows
    "w2" (lambda () (interactive) (split-window-vertically) (other-window 1))
    "w3" (lambda () (interactive) (split-window-horizontally) (other-window 1))
    "w=" 'balance-windows
    "wd" 'delete-window
    "wo" 'occur
    "wo" 'other-window
    "ws" 'toggle-window-split
    "wt" 'swap-windows))

(use-package windmove
  :init
  (evil-leader/set-key
    "wh" 'windmove-left
    "wj" 'windmove-down
    "wk" 'windmove-up
    "wl" 'windmove-right))

(use-package elisp-mode
  :config
  (evil-leader/set-key-for-mode
    'emacs-lisp-mode
    "mt" 'ert
    "mr" 'eval-region
    "mb" 'eval-buffer
    "me" 'eval-last-sexp))

(use-package package
  :config
  (evil-define-key 'normal package-menu-mode-map
    "i" 'package-menu-mark-install
    "U" 'package-menu-mark-upgrades
    "d" 'package-menu-mark-delete
    "u" 'package-menu-mark-unmark
    "x" 'package-menu-execute
    "q" 'quit-window))

(use-package key-chord
  :ensure t
  :init (key-chord-mode 1)
  :after (evil)
  :config
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state))

;; Make a visual selection with v or V, and then hit * to search that
;; selection forward, or # to search that selection backward.
(use-package evil-visualstar
  :ensure t
  :after evil
  :config
  (global-evil-visualstar-mode 1))

;; Effectively comment out stuff
(use-package evil-commentary
  :ensure t
  :diminish evil-commentary-mode
  :after evil
  :config (evil-commentary-mode 1))

;; TODO make use of this
(use-package evil-numbers
  :ensure t
  :after evil)

(use-package evil-surround
  :ensure t
  :after evil
  :config (global-evil-surround-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config (evil-collection-init))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init (projectile-mode 1)
  :config
  (defun run-projectile-invalidate-cache (&rest _args)
    ;; We ignore the args to `magit-checkout'.
    (projectile-invalidate-cache nil))

  (advice-add 'magit-checkout :after #'run-projectile-invalidate-cache)
  (advice-add 'magit-rebase-branch :after #'run-projectile-invalidate-cache)
  (advice-add 'magit-branch-and-checkout :after #'run-projectile-invalidate-cache)

  (setq projectile-enable-caching t
	projectile-switch-project-action #'magit-status
	projectile-project-search-path '("~/vcs/" ("~/vcs/parashift" . 1)))

  (evil-leader/set-key
    "pp"  'projectile-switch-project
    "pd"  'projectile-dired-find-dir
    "pg"  'projectile-rg
    "pG"  'projectile-grep
    "po"  'projectile-find-other-file
    "pf"  'projectile-find-file
    "fp"  'projectile-find-file
    "pb"  'projectile-switch-to-buffer
    "bp"  'projectile-switch-to-buffer
    "p!"  'projectile-run-shell-command-in-root
    "pk"  'projectile-kill-buffers
    "pr"  'projectile-replace
    "pR"  'projectile-replace-regexp))

(use-package projectile-ripgrep
  :after projectile
  :ensure t)

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :diminish company-mode
  :config
  ;; Disable autocompletion delay
  (setq company-idle-delay 0)
  (setq company-backends
	'((company-files
	   company-keywords
	   company-capf
	   company-yasnippet))))

(use-package shell-pop
  :ensure t
  :bind (("M-z" . shell-pop))
  :config
  (custom-set-variables
   '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))))

;; TODO Fix
;; (use-package smerge-mode
;;   :config
;;   (evil-leader/set-key
;;    "sn" 'smerge-next
;;    "sp" 'smerge-prev
;;    "sl" 'smerge-keep-lower
;;    "su" 'smerge-keep-upper
;;    "se" 'smerge-ediff))

(use-package ediff
  :defer t
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; Put windows side by side
  (setq ediff-split-window-function (quote split-window-horizontally)))

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package persistent-scratch
  :ensure t
  :init (persistent-scratch-setup-default))

(use-package unfill
  :ensure t
  :bind (("C-c q" . unfill-region)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("\\README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "pandoc --standalone --mathjax -f markdown -t html")
  (setq markdown-enable-math t)
  (setq markdown-enable-html t)
  (setq markdown-enable-wiki-links t)
  (setq markdown-link-space-sub-char " ")

  (evil-leader/set-key-for-mode
    'markdown-mode
    "mi" 'markdown-toggle-inline-images))

(use-package browse-at-remote
  :ensure t
  :config
  (custom-set-variables
   '(browse-at-remote-remote-type-domains
     (quote
       ("github.com" . "github")
       ("gitlab.com" . "gitlab"))))
  :bind
  (("C-c b" . browse-at-remote)))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :init (global-git-gutter-mode 1))

(use-package git-link
  :ensure t
  :bind (("C-x l" . git-link)))

(use-package git-timemachine
  :ensure t
  :commands git-timemachine-toggle
  :init
  (evil-leader/set-key "v" 'git-timemachine-toggle)
  :config
  ;; http://blog.binchen.org/posts/use-git-timemachine-with-evil.html
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;; Force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

;; Magit <3
(use-package magit
  :ensure t
  :init
  (evil-leader/set-key
    "e"  'magit-dispatch
    "h"  'magit-log-buffer-file
    "l"  'magit-log-current
    "g"  'magit-status)
  :config
  (transient-append-suffix 'magit-commit "c"
    '("a" "Absorb" magit-commit-absorb))

  (setq magit-diff-refine-hunk t
	magit-repository-directories '(("~/vcs/" . 2))))

(use-package hl-todo
  :ensure t
  :hook (prog-mode))


;; Taken from http://emacsredux.com/blog/2013/05/04/rename-file-and-buffer/
(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
	(if (not (and filename (file-exists-p filename)))
		(message "Buffer is not visiting a file!")
	  (let ((new-name (read-file-name "New name: " filename)))
		(cond
		 ((vc-backend filename) (vc-rename-file filename new-name))
		 (t
		  (rename-file filename new-name t)
		  (set-visited-file-name new-name t t)))))))


;; TODO Refactor and study
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))


;; TODO Refactor and study
(defun swap-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
	    (next-win (window-buffer (funcall selector))))
	(set-window-buffer (selected-window) next-win)
	(set-window-buffer (funcall selector) this-win)
	(select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(use-package winner
  :init
  (winner-mode 1)
  :config
  (evil-leader/set-key
    "wu" 'winner-undo
    "wr" 'winner-redo))

(use-package abbrev
  :init (setq-default abbrev-mode 1)
  :diminish abbrev-mode
  :hook prog-mode
  :config
  (setq save-abbrevs 'silently)
  (define-abbrev-table 'ruby-mode-abbrev-table
    '(("bp" "byebug # FIXME")))
  (define-abbrev-table 'python-mode-abbrev-table
    '(("bp" "breakpoint()  # FIXME"))))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (evil-leader/set-key
    "s" 'yas-insert-snippet)
  :config
  (setq yas-snippet-dirs '("~/shared/snippets")))

(use-package display-line-numbers-mode
  :init (setq display-line-numbers-type 'relative)
  :hook (prog-mode git-timemachine-mode))

(use-package rainbow-mode
  :ensure t
  :config (setq rainbow-mode 1))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paredit
  :ensure t
  :diminish (paredit-mode)
  :hook ((clojure-mode . paredit-mode)
	 (emacs-lisp-mode . paredit-mode)))

(use-package dired
  :defer t
  :config
  (defun dired-start-process (cmd &optional file-list)
    (interactive
     (let ((files (dired-get-marked-files t current-prefix-arg)))
       (list
	(dired-read-shell-command "& on %s: " current-prefix-arg files)
	files)))
    (apply
     #'start-process
     (list cmd nil shell-file-name shell-command-switch
	   (format "nohup 1>/dev/null 2>/dev/null %s \"%s\""
		   (if (> (length file-list) 1)
		       (format "%s %s"
			       cmd
			       (cadr (assoc cmd dired-filelist-cmd)))
		     cmd)
		   (mapconcat #'expand-file-name file-list "\" \"")))))

  (setq dired-guess-shell-alist-user
	'(("\\.pdf\\'" "zathura")
	  ("\\.eps\\'" "zathura")
	  ("\\.jpe?g\\'" "feh")
	  ("\\.png\\'" "feh")
	  ("\\.gif\\'" "feh")
	  ("\\.xpm\\'" "feh")
	  ("\\.ods\\'" "libreoffice")
	  ("\\.csv\\'" "libreoffice")
	  ("\\.tex\\'" "pdflatex" "latex")
	  ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\'" "vlc")
	  ("\\.\\(?:mp3\\|flac\\)\\'" "vlc")
	  ("\\.html?\\'" "firefox")))

  ;; If there is a Dired buffer displayed in the next window, use its current directory, instead of
  ;; this Dired buffer's current directory as target (rename, copy, ...).
  (setq dired-dwim-target t)

  ;; Use human readable units
  (setq-default dired-listing-switches "-alh")

  ;; Reload dired buffer when content changes
  (setq dired-auto-revert-buffer t)

  ;; Hide hidden files
  (require 'dired-x)
  (setq dired-omit-files (rx (seq bol "." (not (any ".")))))
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

  (evil-define-key 'normal dired-mode-map
    "o" 'dired-start-process
    "s" 'dired-sort-toggle-or-edit))

(use-package org
  :mode (("\\.org\\'" . org-mode))
  :custom-face
  (org-block-begin-line ((t (:foreground "#8b8b8b" :extend t))))
  (org-block-end-line ((t (:foreground "#8b8b8b" :extend t))))
  :config
  ;; Allow to modify image size
  (setq org-image-actual-width nil)
  ;; Hide emphasis markers
  (setq org-hide-emphasis-markers t)
  ;; Render emphasied text
  (setq org-fontify-emphasized-text t)
  ;; Required so <s templates work
  (require 'org-tempo))

(use-package org-tree-slide
  :ensure t
  :after org
  :config
  (setq org-tree-slide-heading-emphasis t))

(use-package vertico
  :ensure t
  :config (vertico-mode 1))

(use-package vertico-prescient
  :ensure t
  :config
  (vertico-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package consult
  :ensure t
  :bind (("M-g" . goto-line-preview))
  :init
  (evil-leader/set-key
    "r" 'consult-ripgrep
    "n" (lambda () (interactive) (consult-fd notes-directory))))

(use-package eglot
  :ensure t
  :config
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
  (evil-leader/set-key "-" 'eglot-rename)
  :hook (python-mode . eglot-ensure))

(use-package python-black
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode))

(use-package zig-mode
  :ensure t
  :mode (("\\.zig\\'" . zig-mode)))

(use-package rust-mode
  :ensure t
  :mode (("\\.rs\\'" . rust-mode)))

;; TODO Research difference to flyspell
(use-package flycheck
  :ensure t
  :hook (clojure-mode . flycheck-mode))

(use-package ispell
  :ensure t
  :config
  (setenv "DICPATH" "/home/patrick/.nix-profile/share/hunspell/")
  (when (executable-find "hunspell")
    (setq-default ispell-program-name "hunspell")
    (setq ispell-really-hunspell t)))

(use-package flyspell
  :ensure t
  :after (ispell)
  :hook ((markdown-mode org-mode) . flyspell-mode)
  :config
  (setq flyspell-default-dictionary "en_US"
	markdown-fontify-code-blocks-natively t))

(use-package flycheck-clj-kondo
  :hook clojure-mode
  :ensure t)

(use-package clojure-mode
  :ensure t
  :bind (("C-x C-d" . cider-debug-defun-at-point)
	 ("C-x C-i" . cider-inspect-last-result))
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)))

(use-package cider
  :ensure t
  :after clojure-mode
  :hook (clojure-mode . cider-mode)
  :config
  (evil-leader/set-key-for-mode
    'clojure-mode
    "mt" 'cider-test-run-project-tests
    "mj" 'cider-jack-in
    "mb" 'cider-eval-buffer
    "md" 'cider-debug-defun-at-point
    "mr" 'cider-eval-region
    "me" 'cider-eval-last-sexp))

(use-package makefile-executor
  :ensure t
  :defer t
  :config
  (add-hook 'makefile-mode-hook 'makefile-executor-mode)
  (evil-leader/set-key
    "pm"  'makefile-executor-execute-project-target
    "pl"  'makefile-executor-execute-last))

(use-package justl
  :commands justl-exec-recipe-in-dir
  :ensure t
  :init
  (evil-leader/set-key
    "i"  'justl-exec-recipe-in-dir))

(use-package just-mode
  :mode (("\\Justfile\\'" . just-mode))
  :ensure t)

(use-package plantuml-mode
  :ensure t
  :mode (("\\.plantuml\\'" . plantuml-mode))
  :config
  (setq plantuml-default-exec-mode 'executable
	plantuml-output-type "png"))

(use-package graphviz-dot-mode
  :ensure t
  :mode (("\\.dot\\'" . graphviz-dot-mode)))

(use-package nix-mode
  :ensure t
  :mode (("\\.nix\\'" . nix-mode)))

(use-package dockerfile-mode
  :ensure t
  :mode (("\\Dockerfile\\'" . dockerfile-mode)))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
	 ("\\.yaml\\'" . yaml-mode)))

(use-package json-mode
  :mode (("\\.json\\'" . json-mode))
  :ensure t)

(use-package hcl-mode
  :ensure t
  :mode (("\\.tf\\'" . hcl-mode)
	 ("\\.hcl\\'" . hcl-mode)))

(use-package jinja2-mode
  :ensure t
  :mode (("\\.j2\\'" . jinja2-mode)))

(use-package protobuf-mode
  :ensure t
  :mode (("\\.proto\\'" . protobuf-mode)))

(use-package sqlformat
  :ensure t
  :hook (sql-mode-hook . sqlformat-on-save-mode)
  :config
  (setq sqlformat-command 'pgformatter
	sqlformat-args '("-f1")))

(use-package undo-tree
  :ensure t
  :config
  (undo-tree-mode 1)
  (evil-leader/set-key
    "u" 'undo-tree-visualize))

(use-package esup
  :ensure t
  :pin melpa
  :config
  ;; https://github.com/jschaf/esup/issues/85#issuecomment-1130110196
  (setq esup-depth 0))

(defun toggle-source-and-tests ()
  "Toggle between the source and test file"
  (interactive)
  (pcase major-mode
    ('python-mode (python/toggle-source-and-test))
    ('ruby-mode (ruby/toggle-source-and-test))))

(defun python/toggle-source-and-test ()
  (let* ((module-path (buffer-file-name))
	 (module-name (file-name-nondirectory module-path))
	 (package-path (file-name-directory module-path)))
    (if (string-suffix-p "tests/" package-path)
	(find-file (concat package-path "/../" (string-remove-prefix "test_" module-name)))
      (find-file (concat package-path "/tests/test_" module-name)))))

(defun ruby/toggle-source-and-test ()
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
	 (is-spec (string-match-p "_spec\\.rb\\'" file-name))
	 (project-root (projectile-project-root))
	 (project-files (projectile-project-files project-root))
	 (target (if is-spec
		     (replace-regexp-in-string "_spec\\.rb\\'" ".rb" file-name)
		   (replace-regexp-in-string "\\.rb\\'" "_spec.rb" file-name)))
	 (matches (seq-filter (lambda (path) (string-match-p target path)) project-files))
	 (open (lambda (path) (find-file (expand-file-name path project-root)))))
    (cond
     ((not matches) (message (format "No matches found for %s." target)))
     ((= (length matches) 1) (funcall open (car matches)))
     (t (funcall open (completing-read "Matches: " matches))))))

;; Reset GC threshold to back to default
(setq gc-cons-threshold 800000)
