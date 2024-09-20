;; Patrick Winter's Emacs Configuration

(require 'package)

;; Load emacs packages installed in the NixOS system and user profile
(add-to-list 'package-directory-list "/run/current-system/sw/share/emacs/site-lisp/elpa")
(add-to-list 'package-directory-list "~/.nix-profile/share/emacs/site-lisp/elpa")

(package-initialize)
(setq package-archives
	  '(("gnu"   . "https://elpa.gnu.org/packages/")
		("melpa" . "https://melpa.org/packages/")))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Font type and size
(set-face-attribute 'default nil :height 130)
(set-face-attribute 'default nil :family "Hack")

;; Allow to resize emacs to exactly 50% on openbox
(setq frame-resize-pixelwise t)

;; NOTE One of the following settings messes with prettified taskaper-mode symbols.
;; Use unicode
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(define-coding-system-alias 'UTF-8 'utf-8)

(setq user-full-name "Patrick Winter"
	  user-mail-address "patrickwinter@posteo.ch")

;; Common paths that change depending on the operating system
(setq sync-directory  "~/shared"
	  notes-directory (format "%s/notes" sync-directory)
	  vcs-directory   (format "%s/vcs"   sync-directory))

;; Always use `text-mode` as default major mode.
(setq default-major-mode 'text-mode)

;; https://emacs.stackexchange.com/questions/5553/async-shell-process-buffer-always-clobbers-window-arrangement
(add-to-list 'display-buffer-alist (cons "\\*Async Shell Command\\*.*" (cons #'display-buffer-no-window nil)))

;; Enable line and column numbering in modeline
(line-number-mode 1)
(column-number-mode 1)

;; Disable the blinking cursor
(blink-cursor-mode -1)

;; Show matching parens
(show-paren-mode t)

;; Flash the frame to represent a bell
;; (setq visible-bell 1)

;; Disable message in scratch buffer
(setq initial-scratch-message nil)

;; Disable the ugly scoll, tool and menu bar
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Disable the splash screen
(setq inhibit-splash-screen t)

;; Set standard indent
(setq standard-indent 4)
(setq-default tab-width 4)

;; Enable to move in cardinal directions
(windmove-default-keybindings)

;; Emacs...  Don't break lines for me, thx
(setq-default truncate-lines t)

;; Highlight selections and allow overwriting them
(transient-mark-mode 1)
(delete-selection-mode 1)

;; Auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t
	  global-auto-revert-mode t
	  auto-revert-verbose nil)

;; Configure backup settings
(setq backup-directory-alist `(("." . "~/.backups"))
	  delete-old-versions t
	  kept-new-versions 6
	  kept-old-versions 2
	  version-control t)

;; Disable auto save
(setq auto-save-default nil)

;; Use OS specific browser settings
(cond ((eq system-type 'darwin)
	   ;; On macOS use the browser specified in the system settings
	   (setq browse-url-browser-function 'browse-url-default-macosx-browser
			 browse-url-generic-program 'firefox))
	  ((eq system-type 'windows-nt)
	   ;; On Windows use the browser specified in the system settings
	   (setq browse-url-browser-function 'browse-url-default-windows-browser
			 browse-url-generic-program 'firefox))
	  ;; On other platforms (BSD, Linux, ...) use firefox...
	  ((setq browse-url-browser-function 'browse-url-firefox
			 browse-url-generic-program 'firefox)))

;; Line wrap at certain line length
(setq-default fill-column 80)

;; Don’t assume that sentences should have two spaces after periods.
(setq sentence-end-double-space nil)

;; Automatically follow symlinks and don't ask about it
(setq vc-follow-symlinks t)

;; Copy paste
(setq x-select-enable-primary t)

;; Use gtklp as printing dialog
(setq lpr-command "gtklp"
	  ps-lpr-command "gtklp")

;; Add additional paths to $PATH
(dolist (path '("~/bin" "~/share/go/bin" "~/.npm/node_modules/bin"))
  (setenv "PATH" (concat (getenv "PATH") ":" path))
  (setq exec-path (append exec-path `(,path))))

;; Set background of hex colour strings using font-lock-mode
;; Highlight hex colour strings when conf-mode is active
(add-hook
 'conf-mode-hook
 (lambda ()
   (font-lock-add-keywords
	nil
	'(("#[abcdef[:digit:]]\\{6\\}"
	   (0 (put-text-property (match-beginning 0)
							 (match-end 0)
							 'face (list :background
										 (match-string-no-properties 0)))))))))

;; Always trim trailing whitespace.
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Copy region selected by mouse automatically to clipboard (This is the
;; default behaviour on Linux but not on macOS)
(setq mouse-drag-copy-region t)

;; Use system clipboard
(setq x-select-enable-primary t)

;; Auto-select buffer when hovering with mouse
;; This is buggy in combination with 'magit-status`
;; https://github.com/magit/magit/issues/3330
;; (setq mouse-autoselect-window t)

;; Allow to answer even important questions with "y" or "n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable menu bar on macOS when using the cocoa interface
(if (and (eq system-type 'darwin) (eq (display-graphic-p) t))
	(menu-bar-mode 1))

;; Fix weird keyboard layout on macOS
(setq mac-command-modifier 'meta
	  mac-option-modifier 'none
	  default-input-method "MacOSX")

;; Don't pollute .emacs with the result of customize invocations
(setq custom-file (make-temp-file ""))
(setq custom-safe-themes t)


;; https://www.emacswiki.org/emacs/GlobalTextScaleMode
(define-globalized-minor-mode
  global-text-scale-mode
  text-scale-mode
  (lambda () (text-scale-mode 1)))

(defun global-text-scale-adjust (inc) (interactive)
	   (text-scale-set 1)
	   (kill-local-variable 'text-scale-mode-amount)
	   (setq-default text-scale-mode-amount (+ text-scale-mode-amount inc))
	   (global-text-scale-mode 1))

(use-package emacs-lisp
  :bind (("C-c C-d" . helpful-at-point)))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  :config
  (add-to-list 'evil-emacs-state-modes 'inferior-python-mode)
  (add-to-list 'evil-emacs-state-modes 'eshell-mode))

(use-package evil-leader
  :ensure t
  :init (global-evil-leader-mode)
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
	"s" 'yas-insert-snippet
	"t" 'toggle-source-and-tests
	;; "ap" 'list-packages
	"c" 'calc
	;; "aw" 'woman
	"bk" 'kill-buffer
	"bl" 'ibuffer
	"bn" 'switch-to-next-buffer
	"bw" 'copy-pwd
	"bp" 'switch-to-prev-buffer
	"bR" 'rename-file-and-buffer
	"br" 'revert-buffer-no-confirm
	"b0" '(lambda () (interactive) (global-text-scale-adjust (- text-scale-mode-amount)) (global-text-scale-mode -1))
	"b," '(lambda () (interactive) (global-text-scale-adjust 1))
	"b." '(lambda () (interactive) (global-text-scale-adjust -1))
	"o"  'switch-to-buffer
	"<SPC>" 'execute-extended-command
	"n" 'counsel-open-notes
	"," 'xref-pop-marker-stack
	"." 'xref-find-definitions
	"a" 'xref-find-references
	"-" 'eglot-rename
	"ff" 'find-file
	"fo" 'find-file-other-window
	"wh" 'windmove-left
	"wj" 'windmove-down
	"wk" 'windmove-up
	"wl" 'windmove-right
	"wt" 'transpose-windows
	"wo" 'occur
	"ws" 'toggle-window-split
	"w1" 'delete-other-windows
	"w2" (lambda () (interactive) (split-window-vertically) (other-window 1))
	"w3" (lambda () (interactive) (split-window-horizontally) (other-window 1))
	"wo" 'other-window
	"wd" 'delete-window
	"w=" 'balance-windows
	"d." (lambda () (interactive) (dired "."))
	"dh" (lambda () (interactive) (dired "~"))
	"dd" (lambda () (interactive) (dired "~/downloads/"))
	"dj" (lambda () (interactive) (dired "/ssh:jump:/"))
	"dv" (lambda () (interactive) (dired "~/vcs/"))
	"ds" (lambda () (interactive) (dired "~/shared/"))
	"df" (lambda () (interactive) (dired "~/shared/fhnw"))
	"fs" (lambda () (interactive) (find-file "~/shared/"))
	"ft" (lambda () (interactive) (find-file "~/shared/todo.org"))
	"fh" (lambda () (interactive) (find-file "/etc/nixos/host-configuration.nix"))
	"fc" (lambda () (interactive) (find-file "/etc/nixos/configuration.nix"))
	"fi" (lambda () (interactive) (find-file "~/.config/i3/config"))
	"fe" (lambda () (interactive) (find-file "~/.emacs"))))


(use-package xclip
  :ensure t
  :if (locate-file "xclip" exec-path)
  :init (xclip-mode 1))

(use-package helpful
  :ensure t
  :config
  (evil-leader/set-key
   "hf"   'helpful-callable
   "hF"   'helpful-function
   "hi"   'helpful-at-point
   "hv"   'helpful-variable
   "hk"   'helpful-key))

(use-package diminish
  :init
  (diminish 'python-black-on-save-mode)
  (diminish 'eldoc-mode)
  (diminish 'git-gutter-mode)
  :ensure t)

;; Emacs keybinding improvements
(global-set-key (kbd "M-o") 'switch-to-buffer)
(global-set-key (kbd "C-o") 'other-window)

;; There are may cool themes out there:
;;   - gruber-darker-theme
;;   - arc-dark-theme
;;   - color-theme-sanityinc-tomorrow
;;   - modus-operandi-theme
;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (load-theme 'solarized-dark t)
;;   :init
;;   ;; Make the fringe stand out from the background
;;   (setq solarized-distinct-fringe-background t)
;;   ;; Don't change the font for some headings and titles
;;   (setq solarized-use-variable-pitch nil)
;;   ;; Make the modeline high contrast
;;   (setq solarized-high-contrast-mode-line t)
;;   ;; Use less bolding
;;   (setq solarized-use-less-bold t)
;;   ;; Use more italics
;;   (setq solarized-use-more-italic t)
;;   ;; Use less colors for indicators such as git:gutter, flycheck and similar
;;   (setq solarized-emphasize-indicators nil)
;;   ;; Don't change size of org-mode headlines (but keep other size-changes)
;;   (setq solarized-scale-org-headlines nil)
;;   ;; Avoid all font-size changes
;;   (setq solarized-height-minus-1 1.0
;; 		solarized-height-plus-1  1.0
;; 		solarized-height-plus-2  1.0
;; 		solarized-height-plus-3  1.0
;; 		solarized-height-plus-4  1.0)
;;   (custom-set-faces
;;    '(magit-diff-hunk-heading ((t (:background "#073642" :foreground "#93a1a1"))))
;;    ;; Normally hunk headings are dark blue... which is barely readable
;;    '(ein:cell-input-area ((t (:background "#073642"))))
;;    ;; Who had the idea that the path of the directory should have a bright blue background?
;;    '(dired-header ((t (:background "#002b36" :foreground "#268bd2"))))))

(use-package naysayer-theme
  :ensure t
  :init (load-theme 'naysayer t))


(use-package key-chord
  :ensure t
  :init (key-chord-mode 1)
  :after (evil)
  :config
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
  (key-chord-define evil-insert-state-map "kj" 'evil-normal-state))

(defun counsel-open-notes ()
  (interactive)
  (counsel-find-file notes-directory))

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

;; Taken from https://emacs.stackexchange.com/questions/10348/revert-buffer-discard-unsaved-changes-without-y-n-prompt
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))

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

(defun transpose-windows (arg)
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

(defun copy-pwd ()
  "Copy PWD to clipboard."
  (interactive)
  (kill-new (buffer-file-name)))

;; (use-package lispy
;;   :ensure t
;;   :hook ((emacs-lisp-mode clojure-mode) . lispy-mode)
;;   :diminish lispy-mode)

;; (use-package lispyville
;;   :ensure t
;;   :hook (lispy-mode . lispyville-mode))

;; Allow to undo/redo window changes
(winner-mode 1)
(evil-leader/set-key
  "wu" 'winner-undo
  "wr" 'winner-redo)

;; Add evil bindings for emacs-lisp-mode
(evil-leader/set-key-for-mode
  'emacs-lisp-mode
  "mt" 'ert
  "mr" 'eval-region
  "mb" 'eval-buffer
  "me" 'eval-last-sexp)


;; Add evil-keybindings for list-packages menu
(evil-define-key 'normal package-menu-mode-map
  "i" 'package-menu-mark-install
  "U" 'package-menu-mark-upgrades
  "d" 'package-menu-mark-delete
  "u" 'package-menu-mark-unmark
  "x" 'package-menu-execute
  "q" 'quit-window)

;; Make a visual selection with v or V, and then hit * to search that
;; selection forward, or # to search that selection backward.
(use-package evil-visualstar
  :ensure t
  :after evil
  :config
  (global-evil-visualstar-mode 1))

;; Ergonomically navigate around text
(use-package evil-easymotion
  :ensure t
  :after evil
  :config (evilem-default-keybindings "gs"))


;; Effectively comment out stuff
(use-package evil-commentary
  :ensure t
  :diminish evil-commentary-mode
  :after evil
  :config (evil-commentary-mode 1))

(use-package evil-numbers
  :ensure t
  :after evil)

(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :ensure t
  :config (evil-collection-init))

(use-package compile
  :config
  ;; (evil-leader/set-key "c" 'compile)
  (setq compilation-scroll-output 'first-error))

(use-package deadgrep
  :ensure t
  :config
  (evil-leader/set-key
	"r"   'deadgrep))

(use-package projectile-ripgrep
  :ensure t)

(use-package unfill
  :ensure t
  :bind (("C-c q" . unfill-region)))

(use-package dimmer
  :ensure t
  :config
  (setq dimmer-fraction 0.2)
  (setq dimmer-exclusion-regexp "^\*helm.*\\|^ \*Minibuf-.*\\|^ \*Echo.*")
  (dimmer-mode))

(use-package abbrev
  :init (setq-default abbrev-mode t)
  :diminish abbrev-mode
  :config
  (setq save-abbrevs 'silently)

  (define-abbrev-table 'ruby-mode-abbrev-table
	'(("bp" "binding.pry  # FIXME")))
  (define-abbrev-table 'python-mode-abbrev-table
	'(("bp" "breakpoint()  # FIXME"))))

;; Emacs has this builtin now, it's fast
(use-package display-line-numbers-mode
  :init
  (setq display-line-numbers-type 'relative)
  :hook prog-mode)


(use-package dired
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
		  ("\\.html?\\'" "firefox")
		  ("\\.cue?\\'" "audacious")))

  ;; If there is a Dired buffer displayed in the next window, use its current directory, instead of
  ;; this Dired buffer's current directory as target (rename, copy, ...).
  (setq dired-dwim-target t)

  ;; Use human readable units
  (setq-default dired-listing-switches "-alh")
  (setq dired-auto-revert-buffer t)

  ;; Hide hidden files
  (require 'dired-x)
  (setq dired-omit-files
		(rx (seq bol "." (not (any ".")))))

  (add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
  (evil-define-key 'normal dired-mode-map
	"o" 'dired-start-process
	"s" 'dired-sort-toggle-or-edit))

(use-package company
  :ensure t
  :diminish company-mode
  :init
  (global-company-mode)
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

(use-package eshell
  :config
  (setq eshell-prompt-regexp "^.*λ "
		eshell-save-history-on-exit t
		;; Turn off the default prompt.
		eshell-highlight-prompt nil
		eshell-cmpl-cycle-completions nil
		eshell-banner-message ""
		eshell-prefer-lisp-variables t
		eshell-prefer-lisp-functions t)

  ;; Aliases (useful in eshell)
  (defalias 'e 'find-file)
  (defalias 'ec 'find-file)

  (defun eshell-clear-buffer ()
	"Clear terminal"
	(interactive)
	(let ((inhibit-read-only t))
	  (erase-buffer)
	  (eshell-send-input)
	  (previous-line)
	  (kill-line)
	  (next-line)
	  (move-end-of-line)))

  (evil-leader/set-key-for-mode
   'eshell-mode
   "C-l" 'eshell-clear-buffer))

(use-package smerge-mode
  :config
  (evil-leader/set-key
   "sn" 'smerge-next
   "sp" 'smerge-prev
   "sl" 'smerge-keep-lower
   "su" 'smerge-keep-upper
   "se" 'smerge-ediff))

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; Put windows side by side
  (setq ediff-split-window-function (quote split-window-horizontally)))

(use-package direnv
  :ensure t
  :config
  (direnv-mode))

(use-package tramp
  :config
  (setq tramp-remote-path
		(append tramp-remote-path
				'("~/.guix-profile/bin" "~/.guix-profile/sbin"
				  "/run/current-system/profile/bin"
				  "/run/current-system/profile/sbin")))

  (setq tramp-default-method "ssh"
		;; Enable caching of passwords
		password-cache t
		password-cache-expiry 3600
		;; Use control persist if conifgured so in ~/.ssh/config
		tramp-use-ssh-controlmaster-options nil)

  (setq vc-ignore-dir-regexp
		(format "\\(%s\\)\\|\\(%s\\)"
				vc-ignore-dir-regexp
				tramp-file-name-regexp)))

(use-package docker-tramp
  :ensure t
  :after (tramp))

(use-package vagrant-tramp
  :ensure t
  :after (tramp))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
		 ("\\.md\\'" . markdown-mode)
		 ("\\.markdown\\'" . markdown-mode))
  :config

  (setq markdown-command "pandoc --standalone --mathjax -f markdown -t html")
  (setq markdown-enable-math t)
  (setq markdown-enable-html t)
  (setq markdown-enable-wiki-links t)
  (setq markdown-link-space-sub-char " ")

  (defun markdown-insert-screenshot ()
	"Call external screenshot program and insert the link to the image as
markdown reference."
	(interactive)
	(let* ((out-path (string-remove-prefix "Directory " (format "./images/%s.png" (format-time-string "%Y-%m-%d_%T"))))
		   (screenshot-command (format "flameshot gui -r > %s" out-path)))
	  (progn
		(when (not (f-directory-p "images"))
		  (make-directory "images/"))
		(shell-command-to-string screenshot-command)
		(insert (format "![](%s \"%s\")"
						out-path
						(read-from-minibuffer "Description: "))))))


  (evil-leader/set-key-for-mode
   'markdown-mode
   "mi" 'markdown-toggle-inline-images
   "ms" 'markdown-insert-screenshot))

(use-package git-timemachine
  :ensure t
  :init
  ;; http://blog.binchen.org/posts/use-git-timemachine-with-evil.html
  '(progn
	 (evil-make-overriding-map git-timemachine-mode-map 'normal)
	 ;; Force update evil keymaps after git-timemachine-mode loaded
	 (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

;; Magit <3
(use-package magit
  :ensure t
  :config
  (setq magit-diff-refine-hunk t
		magit-repository-directories '(("~/vcs/" . 2)))
  (setenv "GIT_ASKPASS" "git-gui--askpass")

  (evil-leader/set-key
   "e"  'magit-dispatch
   "h"  'magit-log-buffer-file
   "l"  'magit-log-current
   "g"  'magit-status))

(use-package hl-todo
  :ensure t
  :init (global-hl-todo-mode))

(use-package forge
  :ensure t
  :after  magit
  :config
  (add-to-list 'forge-alist '("git.adfinis-sygroup.ch" "git.adfinis-sygroup.ch/api/v4" "git.adfinis-sygroup.ch" forge-gitlab-repository))
  (add-to-list 'forge-alist '("gitlab.fhnw.ch" "gitlab.fhnw.ch/api/v4" "gitlab.fhnw.ch" forge-gitlab-repository)))

(use-package browse-at-remote
  :ensure t
  :config
  (custom-set-variables
   '(browse-at-remote-remote-type-domains
	 (quote
	  (("bitbucket.org" . "bitbucket")
	   ("github.com" . "github")
	   ("gitlab.com" . "gitlab")
	   ("git.savannah.gnu.org" . "gnu")
	   ("gist.github.com" . "gist")
	   ("git.sr.ht" . "sourcehut")))))
  :bind
  (("C-c b" . browse-at-remote)))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :init (global-git-gutter-mode +1))

(use-package git-link
  :ensure t
  :bind (("C-x l" . git-link)))

(use-package rainbow-delimiters
  :ensure
  :hook (prog-mode . rainbow-delimiters-mode))



(use-package python
  :config
  (setq python-shell-interpreter "ipython"
		python-shell-interpreter-args "--simple-prompt -i"))

(use-package python-black
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode))

(use-package eglot
  :ensure t
  :config
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
  :hook (python-mode . eglot-ensure))

(use-package web-mode
  :ensure t
  :hook (ember-mode)
  :mode (("\\.phtml\\'" . web-mode)
		 ("\\.tpl\\.php\\'" . web-mode)
		 ("\\.[agj]sp\\'" . web-mode)
		 ("\\.as[cp]x\\'" . web-mode)
		 ("\\.erb\\'" . web-mode)
		 ("\\.mustache\\'" . web-mode)
		 ("\\.hbs\\'" . web-mode)
		 ("\\.djhtml\\'" . web-mode)
		 ("\\.jsx?$" . web-mode))
  :config
  (setq web-mode-engines-alist '(("php" . "\\.phtml\\'"))
		;; Don't auto-indent entire buffer when yanking... thx
		web-mode-enable-auto-indentation nil
		web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(use-package nix-mode
  :ensure t
  :mode (("\\.nix\\'" . nix-mode)))

(use-package dockerfile-mode
  :ensure t)

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

(use-package goto-line-preview
  :ensure t
  :bind (("M-g" . goto-line-preview)))


(use-package recentf
  :init (recentf-mode t)
  :config (setq recentf-max-saved-items 50)
  (evil-leader/set-key
	"j"   'consult-recent-file))

(use-package selectrum
  :ensure t
  :config
  (selectrum-mode +1))

(use-package selectrum-prescient
  :ensure t
  :config
  (selectrum-prescient-mode +1)
  (prescient-persist-mode +1))

(use-package consult
  :ensure t
  :config
  (evil-leader/set-key
 	"/"  'consult-isearch-forward
 	"?"  'consult-isearch-backward))

(use-package projectile
  :ensure t
  :after (magit)
  :diminish projectile-mode
  :init
  (projectile-global-mode)
  (projectile-mode +1)
  :config
  (defun discover-projects ()
  	(let ((git-repositories (butlast (split-string-shell-command (shell-command-to-string "fd -I -td  -H .git ~/vcs | grep '.*\.git/$'")))))
  	  (dolist (dir git-repositories)
  		(projectile-add-known-project (file-name-directory dir)))))

  (discover-projects)

  (defun run-projectile-invalidate-cache (&rest _args)
	;; We ignore the args to `magit-checkout'.
	(projectile-invalidate-cache nil))

  (setq projectile-project-search-path '("~/vcs/" ("~/vcs/parashift" . 1)))

  (advice-add 'magit-checkout :after #'run-projectile-invalidate-cache)
  (advice-add 'magit-rebase-branch :after #'run-projectile-invalidate-cache)
  (advice-add 'magit-branch-and-checkout :after #'run-projectile-invalidate-cache)

  (setq projectile-enable-caching t)
  (setq projectile-switch-project-action #'magit-status)
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

(use-package makefile-executor
  :ensure t
  :config
  (add-hook 'makefile-mode-hook 'makefile-executor-mode)
  (evil-leader/set-key
   "pm"  'makefile-executor-execute-project-target
   "pl"  'makefile-executor-execute-last))

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

(use-package org-mode
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
  :config
  (setq org-tree-slide-heading-emphasis t))

(use-package flycheck
  :ensure t
  :hook (clojure-mode . flycheck-mode))

(use-package rust-mode
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

(use-package counsel
  :ensure t)

(use-package sqlformat
  :ensure t
  :hook (sql-mode-hook . sqlformat-on-save-mode)
  :config
  (setq sqlformat-command 'pgformatter
		sqlformat-args '("-f1")))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo)
  (evil-leader/set-key-for-mode
	'clojure-mode
	"mt" 'cider-test-run-project-tests
	"mj" 'cider-jack-in
	"mb" 'cider-eval-buffer
	"md" 'cider-debug-defun-at-point
	"mr" 'cider-eval-region
	"me" 'cider-eval-last-sexp)
  :bind (("C-x C-d" . cider-debug-defun-at-point)
		 ("C-x C-i" . cider-inspect-last-result))
  :mode (("\\.clj\\'" . clojure-mode)
         ("\\.edn\\'" . clojure-mode)))

(use-package cider
  :ensure t
  :hook (clojure-mode . cider-mode))

(use-package just-mode
  :ensure t)

(use-package justl
  :ensure t
  :config
  (evil-leader/set-key
 	"i"  'justl-exec-recipe-in-dir))

(use-package ledger-mode
  :ensure t)

(use-package protobuf-mode
  :ensure t)

(use-package persistent-scratch
  :ensure t
  :init (persistent-scratch-setup-default))

(use-package paredit
  :ensure t
  :hook ((clojure-mode . paredit-mode)
		 (emacs-lisp-mode . paredit-mode)))

(use-package zig-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/shared/snippets"))
  (yas-reload-all)
  (yas-global-mode))

(use-package python-pytest
  :ensure t)


(use-package rspec-mode
  :ensure t
  :config
  (setq rspec-use-spring-when-possible nil)
  (setq rspec-use-bundler-when-possible nil)
  (setq rspec-spec-command "bin/rspec"))

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
	 (t (funcall open (selectrum-completing-read "Matches: " matches))))))
