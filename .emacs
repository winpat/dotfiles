;; Increase GC threshold to speed up startup
(setq gc-cons-threshold 100000000)

;; Increase the default line width
(setq-default fill-column 80)

;; Paths to frequently used directories
(setq sync-directory "~/shared"
      notes-directory (format "%s/notes" sync-directory)
      todo-list-directory (format "%s/todo" sync-directory)
      vcs-directory "~/vcs")

;; Font type and size
(set-face-attribute 'default nil :height 120)
(set-face-attribute 'default nil :family "Hack")

;; Disable message in scratch buffer
(setq initial-scratch-message nil)

;; Disable the Emacs startup screen and show *scratch* buffer instead
(setq inhibit-startup-screen t)

;; Disable the scoll, tool and menu bar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Enable line and column numbering in modeline
(line-number-mode 1)
(column-number-mode 1)

;; Don't emit sounds
(setq visible-bell 1)

;; Disable the blinking cursor in GUI
(setq blink-cursor-mode nil)

;; Disable the blinking cursor in TUI
(setq visible-cursor nil)

;; Allow to answer even important questions with "y" or "n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Emacs...  Don't break lines for me, thx
(setq-default truncate-lines t)

;; Automatically follow symlinks and don't ask ab2out it
(setq vc-follow-symlinks t)

;; Allow to resize emacs to exactly 50% on openbox
(setq frame-resize-pixelwise t)

;; Don't create backup and auto-save files
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Always trim trailing whitespace.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; TODO Emacs keybinding improvements
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'delete-other-windows)
(global-set-key (kbd "C-o") 'switch-to-buffer)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line 1)))
(global-set-key (kbd "C-.") 'xref-find-definitions)
(global-set-key (kbd "C-,") 'xref-go-back)
;; (global-set-key (kbd "M-m") 'comment-dwim)
(global-set-key (kbd "C-c s") 'save-buffer)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "C-c <SPC>") 'set-mark-command)
(global-set-key (kbd "C-c f") 'find-file)
(global-set-key (kbd "C-c t") 'open-todo-list)
(global-set-key (kbd "C-c n") 'open-note)
(global-set-key (kbd "C-c b ,") (lambda () (interactive) (global-text-scale-adjust 1)))
(global-set-key (kbd "C-c b .") (lambda () (interactive) (global-text-scale-adjust -1)))
(global-set-key (kbd "C-c b 0") (lambda () (interactive) (global-text-scale-adjust (- text-scale-mode-amount)) (global-text-scale-mode -1)))
(global-set-key (kbd "C-c b R") 'rename-file-and-buffer)
(global-set-key (kbd "C-c b k") 'kill-buffer)
(global-set-key (kbd "C-c b l") 'ibuffer)
(global-set-key (kbd "C-c b r") (lambda () (interactive) (revert-buffer t t)))
(global-set-key (kbd "C-c b n") (lambda () (interactive) (kill-new (buffer-name))))
(global-set-key (kbd "C-c b w") (lambda () (interactive) (kill-new (buffer-file-name))))
(global-set-key (kbd "C-c b p") (lambda () (interactive) (kill-new (f-relative (buffer-file-name) (projectile-project-root)))))
(global-set-key (kbd "C-c i u") (lambda () (interactive) (string-chop-newline (shell-command "uuidgen" t))))
(global-set-key (kbd "C-c i i") 'create-init-py-file)
(global-set-key (kbd "C-c 1") 'delete-other-windows)
(global-set-key (kbd "C-c 2") (lambda () (interactive) (split-window-vertically) (other-window 1)))
(global-set-key (kbd "C-c 3") (lambda () (interactive) (split-window-horizontally) (other-window 1)))
(global-set-key (kbd "C-c =") 'balance-windows)
(global-set-key (kbd "C-c w d") 'delete-window)
(global-set-key (kbd "C-c w o") 'other-window)
(global-set-key (kbd "C-c w s") 'toggle-window-split)
(global-set-key (kbd "C-c w t") 'swap-windows)

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;;(use-package naysayer-theme
;;  :ensure t
;;  :init (load-theme 'naysayer t))

(use-package timu-caribbean-theme
  :ensure t
  :init (load-theme 'timu-caribbean t))

(use-package helpful
  :ensure t
  :commands (helpful-callable
	     helpful-variable
	     helpful-key
	     helpful-command
	     helpful-function
	     helpful-at-point)
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 ("C-h x" . helpful-command)
	 ("C-h F" . 'helpful-function)
	 ("C-c C-d" . helpful-at-point)))

(use-package which-key
  :diminish which-key-mode
  :init (which-key-mode))


(use-package diminish
  :ensure t)

(use-package eldoc
  :diminish eldoc-mode)

(use-package autorevert
  :diminish auto-revert-mode)

(use-package display-line-numbers-mode
  :init (setq display-line-numbers-type 'relative)
  :hook (prog-mode git-timemachine-mode))

(use-package rainbow-mode
  :ensure t
  :diminish (rainbow-mode)
  :init (rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package god-mode
  :ensure t
  :bind (("<escape>" . god-local-mode)))

(use-package paredit
  :ensure t
  :diminish (paredit-mode)
  :hook ((emacs-lisp-mode clojure-mode janet-mode)))

(use-package undo-tree
  :ensure t
  :diminish (undo-tree-mode)
  :init (undo-tree-mode)
  :bind (("C-c U" . undo-tree-undo)))

(use-package windmove
  :bind (("C-c l" . 'windmove-right)
	 ("C-c k" . 'windmove-up)
	 ("C-c j" . 'windmove-down)
	 ("C-c h" . 'windmove-left)))

(use-package winner
  :init (winner-mode 1)
  :bind (("C-c u" . winner-undo)
	 ("C-c r" . winner-redo)))

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

(use-package dired
  :defer t
  :config
  (setq dired-guess-shell-alist-user
	'(("\\.pdf\\'" "zathura")
	  ("\\.eps\\'" "zathura")
	  ("\\.jpe?g\\'" "feh")
	  ("\\.png\\'" "feh")
	  ("\\.ods\\'" "libreoffice")
	  ("\\.csv\\'" "libreoffice")
	  ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\'" "vlc")
	  ("\\.\\(?:mp3\\|flac\\)\\'" "vlc")
	  ("\\.html?\\'" "firefox")))

  ;; If the next window is a dired buffer, make it the target for dired actions.
  (setq dired-dwim-target t)

  ;; Use human readable units
  (setq-default dired-listing-switches "-alh")

  ;; Reload dired buffer when content changes
  (setq dired-auto-revert-buffer t)

  ;; Hide hidden files
  (require 'dired-x)
  (setq dired-omit-files (rx (seq bol "." (not (any ".")))))
  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))

  :bind (("C-c d ." . dired)
	 ("C-c d h" . (lambda () (interactive) (dired "~")))
	 ("C-c d d" . (lambda () (interactive) (dired "~/downloads/")))
	 ("C-c d s" . (lambda () (interactive) (dired sync-directory)))
	 ("C-c d v" . (lambda () (interactive) (dired vcs-directory)))))

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

(defun open-todo-list ()
  (interactive)
  (let* ((files (directory-files todo-list-directory))
	 (todo-lists (seq-filter (lambda (f) (not (string-prefix-p "." f))) files))
	 (target (completing-read "To Do List: " todo-lists)))
    (find-file (concat todo-list-directory "/" target))))

(defun open-note ()
  (interactive)
  (let* ((files (directory-files notes-directory))
	 (notes (seq-filter (lambda (f) (not (string-prefix-p "." f))) files))
	 (target (completing-read "Note: " notes)))
    (find-file (concat notes-directory "/" target))))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init (projectile-mode 1)
  (setq projectile-enable-caching t
	projectile-switch-project-action #'magit-status
	projectile-project-search-path '(vcs-directory))
  :bind (("C-c p p" . projectile-switch-project)
	 ("C-c p f" . projectile-find-file)
	 ("C-c p o" . projectile-find-other-file)
	 ("C-c p d" . projectile-dired-find-dir)
	 ("C-c p b" . projectile-switch-to-buffer)
	 ("C-c p !" . projectile-run-shell-command-in-root)
	 ("C-c p k" . projectile-kill-buffers)
	 ("C-c p s" . projectile-save-project-buffers)
	 ("C-c p r" . projectile-replace)))

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
  :bind (("M-g"   . consult-goto-line)
	 ("C-c r" . consult-ripgrep)))

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

(use-package abbrev
  :init (abbrev-mode)
  :diminish abbrev-mode
  :hook (python-ts-mode)
  :config (define-abbrev-table 'python-ts-mode-abbrev-table
	    '(("bp" "breakpoint()  # FIXME"))))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode)
  :config (setq yas-snippet-dirs '("~/shared/snippets")))


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
  :config
  ;; Allow to open wiki links with " " in their file name.
  (setq markdown-enable-wiki-links t)
  (setq markdown-link-space-sub-char " "))

(use-package org
  :mode "\\.org\\'"
  :config
  ;; Allow to modify image size
  (setq org-image-actual-width nil)
  ;; Hide emphasis markers
  (setq org-hide-emphasis-markers t)
  ;; Render emphasised text
  (setq org-fontify-emphasized-text t)
  ;; Required so <s templates work
  (require 'org-tempo))

(use-package browse-at-remote
  :ensure t
  :bind (("C-c B" . browse-at-remote)))

(use-package git-link
  :ensure t
  :bind (("C-c L" . git-link)))

(use-package git-timemachine
  :ensure t
  :commands git-timemachine-toggle)

;; Magit <3
(use-package magit
  :ensure t
  :config
  (transient-append-suffix 'magit-commit "c" '("a" "Absorb" magit-commit-absorb))
  (transient-append-suffix 'magit-commit "c" '("A" "Amend" magit-commit-amend))
  (setq magit-diff-refine-hunk t
	magit-repository-directories '(("~/vcs/" . 2)))
  :bind (("C-c g" . magit)))

(use-package hl-todo
  :ensure t
  :hook (prog-mode))

(use-package eglot
  :ensure t
  :config (setq eglot-ignored-server-capabilities
		'(:inlayHintProvider))
  :hook (python-mode . eglot-ensure))

(use-package ispell
  :ensure t
  :config
  (when (executable-find "hunspell")
    (setq-default ispell-program-name "hunspell")
    (setq ispell-really-hunspell t)))

(use-package flyspell
  :ensure t
  :after (ispell)
  :hook ((markdown-mode org-mode) . flyspell-mode)
  :config
  (setq flyspell-default-dictionary "en_US"))

(use-package zig-mode
  :ensure t
  :mode (("\\.zig\\'" . zig-mode))
  :hook ((zig-mode . eglot-ensure)
         (zig-mode . (lambda ()
                       (add-hook 'before-save-hook
                                 (lambda ()
                                   (when (eglot-managed-p)
                                     (eglot-code-action-organize-imports nil)))
                                 nil t)))))

(use-package python-mode
  :config (define-key python-mode-map (kbd "C-c C-p") nil))


(use-package cython-mode
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config (require 'flycheck-clj-kondo)
  :bind (("C-x C-d" . cider-debug-defun-at-point)
	 ("C-x C-i" . cider-inspect-last-result))
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)))

(use-package cider
  :ensure t
  :after clojure-mode
  :hook (clojure-mode . cider-mode))

(use-package janet-mode
  :ensure t)


(use-package flycheck
  :ensure t
  :hook clojure-mode)

(use-package flycheck-clj-kondo
  :ensure t)

(use-package justl
  :commands justl-exec-recipe-in-dir
  :ensure t)

(use-package calc
  :bind (("C-c c" . calc)))

(use-package just-mode
  :ensure t)

(use-package plantuml-mode
  :ensure t
  :config)

(use-package graphviz-dot-mode
  :ensure t)

(use-package nix-mode
  :mode "\\.nix\\'"
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :mode "\\.json\\'"
  :ensure t)

(use-package hcl-mode
  :ensure t)

(use-package jinja2-mode
  :ensure t)

(use-package protobuf-mode
  :mode "\\.proto\\'"
  :ensure t)

(use-package esup
  :ensure t
  :pin melpa
  :config
  ;; https://github.com/jschaf/esup/issues/85#issuecomment-1130110196
  (setq esup-depth 0))

(use-package ascii-table
  :ensure t)

(use-package f
  :ensure t)

(use-package gptel
  :ensure t
  :pin melpa
  :bind (("C-c m" . toggle-gptel)
	 ("C-c M" . gptel-menu))
  :config
  (add-hook 'gptel-mode-hook 'visual-line-mode)

  (setq
   gptel-model 'claude-sonnet-4-5-20250929
   gptel-backend (gptel-make-anthropic "Claude"
		   :stream t
		   :key (f-read-text "~/.anthropic"))))

(defun toggle-gptel ()
  (interactive)
  (let* ((current-buffer-name (buffer-name))
	 (gptel-buffer-name "*Claude*")
	 (gptel-buffer (get-buffer gptel-buffer-name)))
    (if (string= current-buffer-name gptel-buffer-name)
	(previous-buffer)
      (if (not gptel-buffer)
	  (switch-to-buffer (gptel gptel-buffer-name))
	(switch-to-buffer gptel-buffer)))))

(defun python/toggle-source-and-test ()
  (let* ((module-path (buffer-file-name))
	 (module-name (file-name-nondirectory module-path))
	 (package-path (file-name-directory module-path)))
    (if (string-suffix-p "tests/" package-path)
	(find-file (concat package-path "/../" (string-remove-prefix "test_" module-name)))
      (find-file (concat package-path "/tests/test_" module-name)))))

(defun create-init-py-file ()
  "Create an empty __init__.py file in the current directory if it doesn't exist."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (let ((init-file (expand-file-name "__init__.py" default-directory)))
	(if (file-exists-p init-file)
	    (message "__init__.py already exists.")
	  (progn
	    (write-region "" nil init-file)
	    (revert-buffer)
	    (message "__init__.py file created."))))))



(use-package inheritenv
  :vc (:url "https://github.com/purcell/inheritenv" :rev :newest))


(use-package eat
  :ensure t)

(use-package claude-code
  :ensure t
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :rev :newest)
  :config (claude-code-mode)
  :bind-keymap ("C-c c" . claude-code-command-map)
  :bind (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode)))


;; Reset GC threshold to back to default
(setq gc-cons-threshold 800000)
