;;; -*- lexical-binding: t; -*-

;; Increase GC threshold to speed up startup
(setq gc-cons-threshold 100000000)

;; Font type and size — use default-frame-alist so daemon-spawned frames
;; pick up the font (set-face-attribute doesn't survive daemon init).
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-13"))

;; Increase the default line width
(setq-default fill-column 80)

;; Disable the Emacs startup screen and show *scratch* buffer instead
(setq inhibit-startup-screen t)

;; Disable message in scratch buffer
(setq initial-scratch-message nil)

;; Enable text mode by default
(setq initial-major-mode 'text-mode)

;; Declutter the user interface
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Enable line and column numbering in modeline
(line-number-mode 1)
(column-number-mode 1)

;; Allow to resize emacs to exactly 50% on openbox
(setq frame-resize-pixelwise t)

;; Don't emit sounds
(setq visible-bell t)

;; Disable the blinking cursor in GUI
(blink-cursor-mode -1)

;; Disable the blinking cursor in TUI
(setq visible-cursor nil)

;; Emacs...  Don't break lines for me, thx
(setq-default truncate-lines t)

;; Allow to answer even important questions with "y" or "n"
(setq use-short-answers t)

;; Automatically follow symlinks and don't ask about it
(setq vc-follow-symlinks t)

;; Keep backup and auto-save files out of project directories.
(let ((backup-directory (expand-file-name "backups/" user-emacs-directory))
      (auto-save-directory (expand-file-name "auto-save/" user-emacs-directory)))
  (make-directory backup-directory t)
  (make-directory auto-save-directory t)
  (setq make-backup-files t
	auto-save-default t
	backup-directory-alist `(("." . ,backup-directory))
	auto-save-file-name-transforms `((".*" ,auto-save-directory t))
	auto-save-list-file-prefix (expand-file-name ".saves-" auto-save-directory)
	version-control t
	kept-new-versions 6
	kept-old-versions 2
	delete-old-versions t))

;; Save customizations to a different file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Trim trailing whitespace unless it is meaningful in the current mode.
(defun pat/delete-trailing-whitespace ()
  "Delete trailing whitespace, except in Markdown-derived modes."
  (unless (derived-mode-p 'markdown-mode)
    (delete-trailing-whitespace)))

(add-hook 'before-save-hook #'pat/delete-trailing-whitespace)

;; Paths to frequently used directories
(defgroup pat nil
  "Personal Emacs configuration."
  :group 'environment)

(defcustom pat/sync-directory "~/shared"
  "Directory containing synchronized files."
  :type 'directory
  :group 'pat)

(defcustom pat/notes-directory (file-name-concat pat/sync-directory "notes")
  "Directory containing notes."
  :type 'directory
  :group 'pat)

(defcustom pat/todo-list-directory (file-name-concat pat/sync-directory "todo")
  "Directory containing todo lists."
  :type 'directory
  :group 'pat)

(defcustom pat/vcs-directory "~/vcs"
  "Directory containing version-controlled projects."
  :type 'directory
  :group 'pat)

(defun pat/query-replace-buffer (command)
  "Run query-replacement COMMAND throughout the entire buffer."
  (save-restriction
    (widen)
    (goto-char (point-min))
    (let ((mark-active nil))
      (call-interactively command))))

(defun pat/search-and-replace ()
  "Query-replace throughout the entire buffer."
  (interactive)
  (pat/query-replace-buffer #'query-replace))

(defun pat/search-and-replace-regexp ()
  "Regexp query-replace throughout the entire buffer."
  (interactive)
  (pat/query-replace-buffer #'query-replace-regexp))

(defun pat/join-following-line ()
  "Join the following line onto the current line."
  (interactive) (join-line 1))

(defun pat/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (unless (and file-name (file-exists-p file-name))
      (user-error "Buffer is not visiting a file"))
    (let ((new-name (read-file-name "New name: " file-name)))
      (if (vc-backend file-name)
	  (vc-rename-file file-name new-name)
	(rename-file file-name new-name)
	(set-visited-file-name new-name t t)))))

(defun pat/filter-hidden-files (files)
  "Return FILES without entries whose names begin with a dot."
  (seq-filter (lambda (file) (not (string-prefix-p "." file))) files))

(defun pat/open-todo-list ()
  "Open todo list."
  (interactive)
  (let* ((files      (directory-files pat/todo-list-directory))
	 (todo-lists (pat/filter-hidden-files files))
	 (target     (completing-read "To Do List: " todo-lists)))
    (find-file (file-name-concat pat/todo-list-directory target))))

(defun pat/open-note ()
  "Open note."
  (interactive)
  (let* ((files  (directory-files pat/notes-directory))
	 (notes  (pat/filter-hidden-files files))
	 (target (completing-read "Note: " notes)))
    (find-file (file-name-concat pat/notes-directory target))))

(defun pat/revert-buffer ()
  "Revert current buffer from disk, discarding unsaved edits."
  (interactive)
  (revert-buffer t t))

(defun pat/toggle-other-buffer ()
  "Switch to the most recently visited other buffer.
Repeating the command toggles back, unlike `previous-buffer', which
keeps walking further back through the window history."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(defun pat/copy-buffer-name ()
  "Copy buffer name."
  (interactive)
  (kill-new (buffer-name)))

(defun pat/copy-absolute-buffer-path ()
  "Copy absolute path to file buffer."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (unless file-name
      (user-error "Current buffer is not visiting a file"))
    (kill-new file-name)))

(defun pat/copy-project-buffer-path ()
  "Copy path to file buffer relative to project root."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (unless file-name
      (user-error "Current buffer is not visiting a file"))
    (kill-new (file-relative-name file-name
				  (projectile-project-root)))))

(defun pat/insert-uuid ()
  "Insert UUID at point."
  (interactive)
  (insert (string-trim-right (shell-command-to-string "uuidgen"))))

(defun pat/split-window-vertically ()
  "Split window vertically and switch to window below."
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun pat/split-window-horizontally ()
  "Split window horizontally and switch to window below."
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun pat/toggle-window-split ()
  "Toggle a two-window frame between vertical and horizontal splits."
  (interactive)
  (unless (= (count-windows) 2)
    (user-error "Exactly two windows are required"))
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
	      #'split-window-horizontally
	    #'split-window-vertically)))
    (delete-other-windows)
    (let ((first-win (selected-window)))
      (funcall splitter)
      (if this-win-2nd (other-window 1))
      (set-window-buffer (selected-window) this-win-buffer)
      (set-window-buffer (next-window) next-win-buffer)
      (select-window first-win)
      (if this-win-2nd (other-window 1)))))


(defun pat/swap-windows (arg)
  "Swap buffers between windows ARG times.
Negative ARG moves through previous windows instead of next windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) #'next-window #'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
	    (next-win (window-buffer (funcall selector))))
	(set-window-buffer (selected-window) next-win)
	(set-window-buffer (funcall selector) this-win)
	(select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;; Emacs keybindings
;;; Window Navigation
(global-set-key (kbd "M-o")     #'other-window)
(global-set-key (kbd "M-i")     #'delete-other-windows)
(global-set-key (kbd "C-o")     #'switch-to-buffer)
(global-set-key (kbd "C-c 1")   #'delete-other-windows)
(global-set-key (kbd "C-c =")   #'balance-windows)
(global-set-key (kbd "C-c w d") #'delete-window)
(global-set-key (kbd "C-c w o") #'other-window)
(global-set-key (kbd "C-c w s") #'pat/toggle-window-split)
(global-set-key (kbd "C-c w t") #'pat/swap-windows)
(global-set-key (kbd "C-c 2")   #'pat/split-window-vertically)
(global-set-key (kbd "C-c 3")   #'pat/split-window-horizontally)
;;; Text Editing
(global-set-key (kbd "C-c s") #'save-buffer)
(global-set-key (kbd "M-j")   #'pat/join-following-line)

;;; Search
(global-set-key (kbd "M-s r")   #'pat/search-and-replace)
(global-set-key (kbd "M-s M-r") #'pat/search-and-replace-regexp)

;;; Ergonomic Remappings
;; Preserve the case commands on shifted variants, freeing the easier
;; lowercase chords for more frequently used operations.
(global-set-key (kbd "M-U") #'upcase-word)
(global-set-key (kbd "M-L") #'downcase-word)
(global-set-key (kbd "M-C") #'capitalize-word)
(global-set-key (kbd "M-u") #'undo-tree-undo)
(global-set-key (kbd "M-c") #'comment-dwim)
(global-unset-key (kbd "M-l"))

;; RET remains available for newlines.  A prefix argument makes `recompile'
;; prompt for an edited compilation command.
(global-set-key (kbd "C-j") #'recompile)
;;; Code Navigation
(global-set-key (kbd "C-.") #'xref-find-definitions)
(global-set-key (kbd "C-,") #'xref-go-back)
;;; Buffer Manipulation
(global-set-key (kbd "M-z") #'pat/toggle-other-buffer)
(global-set-key (kbd "C-c b l") #'ibuffer)
(global-set-key (kbd "C-c b k") #'kill-buffer)
(global-set-key (kbd "C-c b R") #'pat/rename-file-and-buffer)
(global-set-key (kbd "C-c b r") #'pat/revert-buffer)
(global-set-key (kbd "C-c b n") #'pat/copy-buffer-name)
(global-set-key (kbd "C-c b w") #'pat/copy-absolute-buffer-path)
(global-set-key (kbd "C-c b p") #'pat/copy-project-buffer-path)
;;; Insert data at point
(global-set-key (kbd "C-c i u") #'pat/insert-uuid)
;;; Open files
(global-set-key (kbd "C-c f") #'find-file)
(global-set-key (kbd "C-c t") #'pat/open-todo-list)
(global-set-key (kbd "C-c n") #'pat/open-note)
;;; Text Scale
(global-set-key (kbd "<C-wheel-up>")   (lambda () (interactive) (global-text-scale-adjust 1)))
(global-set-key (kbd "<C-wheel-down>") (lambda () (interactive) (global-text-scale-adjust -1)))
(global-set-key (kbd "C-c 0")        (lambda () (interactive) (global-text-scale-adjust 0)))

(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
	("nongnu" . "https://elpa.nongnu.org/nongnu/")
	("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(use-package naysayer-theme
  :ensure t
  :config
  (load-theme 'naysayer t))

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
	 ("C-h F" . helpful-function)
	 ("C-c C-d" . helpful-at-point)))

(use-package diminish
  :ensure t)

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package eldoc
  :diminish eldoc-mode)

(use-package autorevert
  :diminish auto-revert-mode)

(use-package display-line-numbers
  :hook ((prog-mode . display-line-numbers-mode)
	 (git-timemachine-mode . display-line-numbers-mode)))

(use-package rainbow-mode
  :ensure t
  :hook (prog-mode . rainbow-mode)
  :diminish rainbow-mode)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paredit
  :ensure t
  :hook ((emacs-lisp-mode . paredit-mode)
	 (clojure-mode . paredit-mode)
	 (janet-mode . paredit-mode))
  :diminish paredit-mode)

(use-package undo-tree
  :ensure t
  :demand t
  :bind (("C-c U" . undo-tree-visualize)
	 :map undo-tree-map
	 ("C-x u" . undo-tree-undo))
  :diminish undo-tree-mode
  :init
  (let ((history-directory
	 (expand-file-name "undo-tree-history/" user-emacs-directory)))
    (make-directory history-directory t)
    (setq undo-tree-history-directory-alist
	  `(("." . ,history-directory))))
  :config
  (global-undo-tree-mode 1))

(use-package windmove
  :bind (("C-c l" . windmove-right)
	 ("C-c k" . windmove-up)
	 ("C-c j" . windmove-down)
	 ("C-c h" . windmove-left)))

(use-package winner
  :demand t
  :bind (("C-c u" . winner-undo)
	 ("C-c w r" . winner-redo))
  :config
  (winner-mode 1))

(use-package dired
  :defer t
  :defines dired-omit-files
  :functions dired-omit-mode
  :bind (("C-c d ." . dired)
	 ("C-c d h" . (lambda () (interactive) (dired "~")))
	 ("C-c d d" . (lambda () (interactive) (dired "~/downloads/")))
	 ("C-c d s" . (lambda () (interactive) (dired pat/sync-directory)))
	 ("C-c d m" . (lambda () (interactive) (dired "/run/media/patrick/")))
	 ("C-c d v" . (lambda () (interactive) (dired pat/vcs-directory))))
  :hook (dired-mode . (lambda () (dired-omit-mode 1)))
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
  (setq dired-omit-files (rx (seq bol "." (not (any "."))))))

(use-package projectile
  :ensure t
  :demand t
  :functions projectile-project-root
  :bind (("C-c p p" . projectile-switch-project)
	 ("C-c p f" . projectile-find-file)
	 ("C-c p o" . projectile-find-other-file)
	 ("C-c p d" . projectile-dired-find-dir)
	 ("C-c p b" . projectile-switch-to-buffer)
	 ("C-c p !" . projectile-run-shell-command-in-root)
	 ("C-c p k" . projectile-kill-buffers)
	 ("C-c p s" . projectile-save-project-buffers)
	 ("C-c p r" . projectile-replace))
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t
	projectile-switch-project-action #'magit-status
	projectile-project-search-path (list pat/vcs-directory))
  :config
  (projectile-mode 1))

(use-package vertico
  :ensure t
  :config
  (vertico-mode 1))

(use-package vertico-prescient
  :ensure t
  :after vertico
  :functions prescient-persist-mode
  :config
  (vertico-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package consult
  :ensure t
  :bind (("M-s l" . consult-line)
	 ("M-s g" . consult-ripgrep)
	 ("M-g" . consult-goto-line)
	 ("C-c r" . consult-ripgrep)))

(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind (("C-c e" . embark-act)
	 ("M-." . embark-dwim)
	 ("C-h B" . embark-bindings))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  (context-menu-mode 1)
  (add-hook 'context-menu-functions #'embark-context-menu 100)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult))

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
  :demand t
  :hook ((python-mode . abbrev-mode)
	 (python-ts-mode . abbrev-mode))
  :diminish abbrev-mode
  :config
  (abbrev-mode 1)
  (dolist (table '(python-mode-abbrev-table
		   python-ts-mode-abbrev-table))
    (define-abbrev-table table
      '(("bp" "breakpoint()  # FIXME")))))

(use-package yasnippet
  :ensure t
  :functions yas-reload-all
  :diminish yas-minor-mode
  :init
  (setq yas-snippet-dirs '("~/shared/snippets"))
  :config
  (yas-global-mode 1)
  (yas-reload-all))

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package persistent-scratch
  :ensure t
  :demand t
  :config
  (persistent-scratch-setup-default))

(use-package unfill
  :ensure t
  :bind (("C-c q" . unfill-region)))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :config
  ;; Allow to open wiki links with " " in their file name.
  (setq markdown-enable-wiki-links t)
  (setq markdown-link-space-sub-char " "))

(use-package org
  :mode ("\\.org\\'" . org-mode)
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
  :commands (git-timemachine-toggle))

;; Magit <3
(use-package magit
  :ensure t
  :bind (("C-c g" . magit))
  :config
  (transient-append-suffix 'magit-commit "c" '("a" "Absorb" magit-commit-absorb))
  (transient-append-suffix 'magit-commit "c" '("A" "Amend" magit-commit-amend))
  (setq magit-diff-refine-hunk t
	magit-repository-directories '(("~/vcs/" . 2))))

(use-package smerge-mode
  :init
  (setq smerge-command-prefix (kbd "C-c v")))

(use-package hl-todo
  :ensure t
  :hook (prog-mode . hl-todo-mode))

(use-package calc
  :bind (("C-c c" . calc)))

(use-package eglot
  :ensure t
  :functions (eglot-code-action-organize-imports eglot-managed-p)
  :hook ((python-mode . eglot-ensure)
	 (python-ts-mode . eglot-ensure)
	 (zig-mode . eglot-ensure))
  :config
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider)))

(use-package ispell
  :ensure t
  :defer t
  :init
  (when (executable-find "hunspell")
    (setq-default ispell-program-name "hunspell")
    (setq ispell-really-hunspell t)))

(use-package flyspell
  :ensure t
  :hook ((markdown-mode . flyspell-mode)
	 (org-mode . flyspell-mode))
  :config
  (setq flyspell-default-dictionary "en_US"))

(use-package cc-mode
  :mode ("\\.c\\'" . c-mode)
  :hook (c-mode . (lambda ()
		    (setq comment-start "// "
			  comment-end   ""))))

(defun pat/zig-test-buffer-from-project-root (orig-fun &rest args)
  "Run ORIG-FUN from the current project root with ARGS."
  (let ((default-directory (project-root (project-current t))))
    (apply orig-fun args)))

(defun pat/zig--run-cmd-no-pty (orig-fun cmd &optional source &rest args)
  "Run Zig ORIG-FUN for CMD and SOURCE with a pipe and color enabled.
Pass any remaining arguments as ARGS."
  (let ((process-connection-type nil))
    (apply orig-fun cmd source (append args '("--color" "on")))))

(use-package zig-mode
  :ensure t
  :mode ("\\.zig\\'" . zig-mode)
  :hook (zig-mode . (lambda ()
		      (add-hook 'before-save-hook
				(lambda ()
				  (when (eglot-managed-p)
				    (eglot-code-action-organize-imports nil)))
				nil t)))
  :config
  (advice-add 'zig-test-buffer :around #'pat/zig-test-buffer-from-project-root)

  ;; The Zig compiler renders an in-place progress display (tree drawing,
  ;; cursor moves, OSC taskbar progress) whenever it writes to a TTY.  Emacs'
  ;; `compilation-start' gives the subprocess a pty by default, so the first
  ;; (non-cached) build fills the buffer with raw escape codes that
  ;; `ansi-color' can't strip -- they aren't SGR color sequences.  Force a
  ;; pipe so Zig sees no TTY and skips the progress UI, and pass `--color on'
  ;; so real diagnostic colors still come through for
  ;; `ansi-color-compilation-filter'.
  (advice-add 'zig--run-cmd :around #'pat/zig--run-cmd-no-pty))

(defun pat/compilation-focus-on-error (buffer status)
  "Focus BUFFER when compilation reports errors or abnormal STATUS."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (or (> compilation-num-errors-found 0)
		(string-prefix-p "exited abnormally" status))
	(pop-to-buffer buffer)))))

(use-package compile
  :defer t
  :defines compilation-num-errors-found
  :functions recompile
  :config
  (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
  (add-hook 'compilation-finish-functions
	    #'pat/compilation-focus-on-error))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :init
  ;; An installed ELPA `python-mode' package can replace this autoload during
  ;; package activation.  Point it explicitly at Emacs' built-in `python.el'.
  (autoload 'python-mode "python" nil t)
  :config
  (define-key python-mode-map (kbd "C-c C-p") nil))

(use-package cython-mode
  :ensure t
  :mode ("\\.pyx\\'" . cython-mode))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode))
  :bind (:map clojure-mode-map
	      ("C-x C-d" . cider-debug-defun-at-point)
	      ("C-x C-i" . cider-inspect-last-result)))

(use-package cider
  :ensure t
  :after clojure-mode
  :hook (clojure-mode . cider-mode))

(use-package flycheck
  :ensure t
  :hook (clojure-mode . flycheck-mode))

(use-package flycheck-clj-kondo
  :ensure t
  :after flycheck)

(use-package janet-mode
  :ensure t
  :mode ("\\.janet\\'" . janet-mode))

(use-package nix-mode
  :ensure t
  :mode ("\\.nix\\'" . nix-mode))

(use-package dockerfile-mode
  :ensure t
  :mode ("\\(?:Containerfile\\|Dockerfile\\)\\(?:\\.[^/]*\\)?\\'" . dockerfile-mode))

(use-package yaml-mode
  :ensure t
  :mode ("\\.ya?ml\\'" . yaml-mode))

(use-package json-mode
  :ensure t
  :mode ("\\.json\\'" . json-mode))

(use-package hcl-mode
  :ensure t
  :mode ("\\.\\(?:hcl\\|nomad\\|tf\\)\\'" . hcl-mode))

(use-package protobuf-mode
  :ensure t
  :mode ("\\.proto\\'" . protobuf-mode))

(use-package esup
  :ensure t
  :commands (esup)
  :config
  ;; https://github.com/jschaf/esup/issues/85#issuecomment-1130110196
  (setq esup-depth 0))

;; Reset GC threshold to back to default
(setq gc-cons-threshold 800000)
