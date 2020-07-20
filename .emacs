;; Patrick Winter's Emacs Configuration

(setq lexical-binding t)

(require 'package)
;; Also load emacs packages installed in the NixOS system profile
(add-to-list 'package-directory-list "/run/current-system/sw/share/emacs/site-lisp/elpa")
;; Also load emacs packages installed with nix-env
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


;; Include required libraries
(load-library "find-lisp")

;; Font type and size
(set-face-attribute 'default nil :height 100)
(set-face-attribute 'default nil :family "Hack")

;; Allow to resize emacs to exactly 50% on openbox
(setq frame-resize-pixelwise t)

;; TODO One of the following settings messes with prettified taskaper-mode symbols.
;; Use unicode
;; (set-language-environment "UTF-8")
;; (set-default-coding-systems 'utf-8)
;; (define-coding-system-alias 'UTF-8 'utf-8)

(setq user-full-name "Patrick Winter"
	  user-mail-address "patrickwinter@posteo.ch")

;; Common paths that change depending on the operating system
(setq sync-directory  "~/shared"
	  org-directory   (format "%s/gtd"   sync-directory)
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
			 browse-url-generic-program 'chrome))
	  ((eq system-type 'windows-nt)
	   ;; On Windows use the browser specified in the system settings
	   (setq browse-url-browser-function 'browse-url-default-windows-browser
			 browse-url-generic-program 'chrome))
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

(use-package xclip
  :ensure t
  :init (xclip-mode 1))

(use-package helpful
  :ensure t
  :config
  (evil-leader/set-key
   "hf"   'helpful-callable
   "hv"   'helpful-variable
   "hk"   'helpful-key))

(use-package diminish
  :init
  (diminish 'eldoc-mode)
  (diminish 'git-gutter-mode)
  :ensure t)

;; There are may cool themes out there:
;;   - gruber-darker-theme
;;   - arc-dark-theme
;;   - color-theme-sanityinc-tomorrow
(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-dark t)
  :init
  ;; Make the fringe stand out from the background
  (setq solarized-distinct-fringe-background t)
  ;; Don't change the font for some headings and titles
  (setq solarized-use-variable-pitch nil)
  ;; Make the modeline high contrast
  (setq solarized-high-contrast-mode-line t)
  ;; Use less bolding
  (setq solarized-use-less-bold t)
  ;; Use more italics
  (setq solarized-use-more-italic t)
  ;; Use less colors for indicators such as git:gutter, flycheck and similar
  (setq solarized-emphasize-indicators nil)
  ;; Don't change size of org-mode headlines (but keep other size-changes)
  (setq solarized-scale-org-headlines nil)
  ;; Avoid all font-size changes
  (setq solarized-height-minus-1 1.0
		solarized-height-plus-1  1.0
		solarized-height-plus-2  1.0
		solarized-height-plus-3  1.0
		solarized-height-plus-4  1.0)
  (custom-set-faces
   '(magit-diff-hunk-heading ((t (:background "#073642" :foreground "#93a1a1"))))
   ;; Normally hunk headings are dark blue... which is barely readable
   '(ein:cell-input-area ((t (:background "#073642"))))
   ;; Who had the idea that the path of the directory should have a bright blue background?
   '(dired-header ((t (:background "#002b36" :foreground "#268bd2"))))))

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

(defun counsel-open-org-files ()
  (interactive)
  (counsel-find-file org-directory))

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

(use-package eyebrowse
  :ensure t
  :after (evil)
  :diminish eyebrowse-mode
  :init
  (eyebrowse-setup-evil-keys)
  :config
  (progn
	(define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
	(define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
	(define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
	(define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
	(eyebrowse-mode t)
	(setq eyebrowse-new-workspace t)))

(use-package lispy
  :ensure t
  :diminish lispy-mode)

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  :config
  (add-to-list 'evil-emacs-state-modes 'inferior-octave-mode)
  (add-to-list 'evil-emacs-state-modes 'taskwarrior-mode)
  (add-to-list 'evil-emacs-state-modes 'haskell-interactive-mode)
  (add-to-list 'evil-emacs-state-modes 'inferior-python-mode)
  (add-to-list 'evil-emacs-state-modes 'eshell-mode))

;; Allow to undo/redo window changes
(winner-mode 1)
(evil-leader/set-key
  "wu" 'winner-undo
  "wr" 'winner-redo)

;; Add evil bindings for emacs-lisp-mode
(evil-leader/set-key-for-mode
  'emacs-lisp-mode-map
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

(use-package evil-leader
  :ensure t
  :init (global-evil-leader-mode)
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
	"ap" 'list-packages
	"ac" 'calc
	"am" 'mu4e
	"aw" 'woman
	"bk" 'kill-buffer
	"bl" 'ibuffer
	"bn" 'switch-to-next-buffer
	"bp" 'switch-to-prev-buffer
	"bR" 'rename-file-and-buffer
	"br" 'revert-buffer-no-confirm
	"b," 'text-scale-increase
	"b." 'text-scale-decrease
	"on" 'counsel-open-notes
	"oo" 'counsel-open-org-files
	"," 'xref-pop-marker-stack
	"." 'xref-find-definitions
	"ff" 'find-file
	"wh" 'windmove-left
	"wj" 'windmove-down
	"wk" 'windmove-up
	"wl" 'windmove-right
	"wt" 'transpose-windows
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
	"fbh" (lambda () (interactive) (find-file "/etc/nixos/host-configuration.nix"))
	"fbc" (lambda () (interactive) (find-file "/etc/nixos/configuration.nix"))
	"fbi" (lambda () (interactive) (find-file "~/.config/i3/config"))
	"fbe" (lambda () (interactive) (find-file "~/.emacs"))))

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

(use-package evil-magit
  :ensure t)

(use-package evil-collection
  :ensure t
  :config (evil-collection-init))

(use-package compile
  :config
  (setq compilation-scroll-output 'first-error)
  (evil-leader/set-key "c" 'compile))

(use-package deadgrep
  :ensure t
  :config
  (evil-leader/set-key
	"r"   'deadgrep))

(use-package projectile-ripgrep
  :ensure t)

(use-package counsel-projectile
  :after (projectile)
  :ensure t
  :init (counsel-projectile-mode)
  :config
  (counsel-projectile-modify-action
   'counsel-projectile-switch-project-action
   '((move counsel-projectile-switch-project-action-vc 1)
	 (setkey counsel-projectile-switch-project-action-vc "o")
	 (setkey counsel-projectile-switch-project-action " ")))

  (evil-leader/set-key
	"pp"  'counsel-projectile-switch-project
	"pd"  'counsel-projectile-dired-find-dir
	"pg"  'counsel-projectile-grep
	"po"  'counsel-projectile-find-other-file
	"pf"  'counsel-projectile-find-file
	"fp"  'counsel-projectile-find-file
	"pb"  'counsel-projectile-switch-to-buffer
	"bp"  'counsel-projectile-switch-to-buffer))

(use-package swiper
  :ensure t
  :config
  (define-key evil-normal-state-map "/" 'swiper)
  (define-key evil-normal-state-map "?" 'swiper))

(use-package unfill
  :ensure t
  :bind (("C-c q" . unfill-region)))

(use-package suggest
  :ensure t
  :config
  (evil-leader/set-key "as" 'suggest))

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
  (define-abbrev-table 'global-abbrev-table
	'(("pd" "__import__('ipdb').set_trace()  # FIXME"))))

;; Emacs has this builtin now, it's fast
(use-package display-line-numbers-mode
  :hook prog-mode)

(use-package package-lint
  :ensure t)

(use-package flycheck-package
  :ensure t)

(use-package font-lock-studio
  :ensure t)

(use-package esup
  :ensure t
  :commands esup)

(use-package vscode-icon
  :ensure t
  :commands (vscode-icon-for-file))

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
			(lambda ()
			  (unless (file-remote-p default-directory)
				(auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)

  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

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

  (defun winpat/dired-ediff-files ()
	(interactive)
	(let ((files (dired-get-marked-files))
		  (wnd (current-window-configuration)))
	  (if (<= (length files) 2)
		  (let ((file1 (car files))
				(file2 (if (cdr files)
						   (cadr files)
						 (read-file-name
						  "file: "
						  (dired-dwim-target-directory)))))
			(if (file-newer-than-file-p file1 file2)
				(ediff-files file2 file1)
			  (ediff-files file1 file2))
			(add-hook 'ediff-after-quit-hook-internal
					  (lambda ()
						(setq ediff-after-quit-hook-internal nil)
						(set-window-configuration wnd))))
		(error "no more than 2 files should be marked"))))

  (evil-define-key 'normal dired-mode-map "e" 'winpat/dired-ediff-files)
  (evil-define-key 'normal dired-mode-map "o" 'dired-start-process))


(use-package org
  :config
  (defun org-remove-link ()
	"Replace an org link by its description or if empty its address"
	(interactive)
	(if (org-in-regexp org-bracket-link-regexp 1)
		(let ((remove (list (match-beginning 0) (match-end 0)))
			  (description (if (match-end 3)
							   (org-match-string-no-properties 3)
							 (org-match-string-no-properties 1))))
		  (apply 'delete-region remove)
		  (insert description))))

  ;; Inspired by VSCode's Todo+ plugin
  (defface org-level-1 '((t (:inherit default :foreground "#66d9ef"))) "")
  (defface org-level-2 '((t (:inherit default :foreground "#dddddd"))) "")
  (defface org-level-3 '((t (:inherit default :foreground "#000000"))) "")
  (defface org-level-4 '((t (:inherit default :foreground "#000000"))) "")
  (defface org-level-5 '((t (:inherit default :foreground "#000000"))) "")
  (defface org-level-6 '((t (:inherit default :foreground "#000000"))) "")
  (defface org-level-7 '((t (:inherit default :foreground "#000000"))) "")
  (defface org-level-8 '((t (:inherit default :foreground "#000000"))) "")

  ;; Make org-block look nice (inspired by markdown-mode)
  (defface org-block-begin-line
	'((t (:background "#EEE8D5")))
	"Face used for the line delimiting the begin of source blocks.")
  (defface org-block
	'((t (:background "#EEE8D5")))
	"Face used for the source block background.")
  (defface org-block-background
	'((t (:background "#EEE8D5")))
	"Face used for the source block background.")
  (defface org-block-end-line
	'((t (:background "#EEE8D5")))
	"Face used for the line delimiting the end of source blocks.")

  ;; Highlight syntax in code blocks
  (setq org-src-fontify-natively t)

  ;; Also open `.org_archive` files using org-mode
  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

  ;; Allow to adjust inline image size
  (setq org-image-actual-width nil)

  ;; Allow refiling to other files instead of only headings in the current
  (setq org-refile-targets '((org-agenda-files :maxlevel . 1)))

  ;; Custom CSS file for html exports
  ;; (setq org-html-head (concat "<link rel=\"stylesheet\" type=\"text/css\" href=\""
  ;; 			      (expand-file-name "~/.emacs.d/org-mode/default.css")
  ;; 			      "\"/>"))

  ;;; Org Keywords
  ;; The "|"  separates the TODO from DONE keywords
  (setq org-todo-keywords '((sequence "PROJECT(p)" "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "REVIEW(r)"
									  "|" "DONE(d)" "CANCELED(c)"))

		org-todo-keyword-faces '(("TODO" . (:foreground "#dc322f" :weight bold))
								 ("IN-PROGRESS" . (:foreground "#cb4b16" :weight bold))
								 ("REVIEW" . (:foreground "#b58900" :weight bold))
								 ("WAITING" . (:foreground "#267bd2" :weight bold))
								 ("CANCELED" . (:forground "#6c71c4" :weight bold))
								 ("PROJECT" . (:forground "#657b83" :weight bold))
								 ("DONE" . (:foreground "#859900" :weight bold))))

  ;; Forces you mark all child tasks as “DONE” before you can mark the parent as “DONE.”
  (setq org-enforce-todo-dependencies t)

  ;; Set maximum indentation for description lists
  (setq org-list-description-max-indent 5)

  ;; Turn "***" into "  *"
  (setq org-startup-indented t)

  ;; Prevent that demoting a heading also shifts text inside the section
  (setq org-adapt-indentation nil)

  ;; Use a selection of keywords to choose from states, instead of spamming C-c C-t
  (setq org-use-fast-todo-selection t)

  ;; Only the first empty line belongs to the last headline
  (setq org-cycle-separator-lines 2)

  ;; Allow setting single tags without the menu
  (setq org-fast-tag-selection-single-key (quote expert))

  ;; Show clock sums as hours and minutes, not "n days" etc.
  (setq org-time-clocksum-format
		'(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

  ;; I prefer lowercase block names
  (setcar (nthcdr 0 org-structure-template-alist) '("s" "#+begin_src ?\n#+end_src" ""))
  (setcar (nthcdr 1 org-structure-template-alist) '("e" "#+begin_example ?\n#+end_example" ""))

  (setq org-capture-templates
		`(("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
		   "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
		  ("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
		   "* %? [[%:link][%:description]] \nCaptured On: %U")
		  ("l" "log" entry (file+datetree "~/shared/gtd/journal.org")
		   "* %T %?\n" :unnarrowed 1)
		  ("c" "capture" entry (file "~/shared/gtd/inbox.org")
		   "* TODO %?")
		  ("a" "capture with annotation" entry (file "~/shared/gtd/inbox.org")
		   "* TODO %A\n%?")))

  ;; Find all org files in a directory and set them as org-agenda-files
  (setq org-agenda-files
		(find-lisp-find-files org-directory "\.org$"))

  ;; Don't show scheduled tasks if they're marked as done
  (setq org-agenda-skip-scheduled-if-done t)

  ;; https://emacs.stackexchange.com/questions/12517/how-do-i-make-the-timespan-shown-by-org-agenda-start-yesterday
  (setq org-agenda-start-day "-1d"
		org-agenda-span 10
		org-agenda-start-on-weekday nil)

  ;; Keep archived org files in a subdirectory
  (setq org-archive-location "archive/%s_archive::")

  ;; Custom agenda views for each group of org files
  (setq org-agenda-custom-commands
		'(("F" "FHNW Agenda" agenda ""
		   ((org-agenda-files (list (concat org-directory "/school.org")))
			;; Show if the deadline for tasks is less then 3 days away
			(org-deadline-warning-days 3)
			;; Don't show the "FHNW" tag in the view
			(org-agenda-hide-tags-regexp "FHNW")
			(org-scheduled-past-days 0)
			(org-agenda-start-with-log-mode t)
			(org-agenda-span 22)
			;; Show last weeks tasks
			(org-agenda-start-day "-7d")))
		  ("A" "Adfinis SyGroup Agenda" agenda ""
		   ((org-agenda-files (list (concat org-directory "/work.org")))
			;; Show if the deadline for tasks is less then 3 days away
			(org-deadline-warning-days 3)
			;; Don't show the "FHNW" tag in the view
			(org-agenda-hide-tags-regexp "WORK")
			(org-scheduled-past-days 0)
			(org-agenda-start-with-log-mode t)
			(org-agenda-span 22)
			;; Show last weeks tasks
			(org-agenda-start-day "-7d")))
		  ("P" "Personal Agenda" agenda ""
		   ((org-agenda-files (list (concat org-directory "/personal.org")))
			;; Show if the deadline for tasks is less then 3 days away
			(org-deadline-warning-days 3)
			;; Don't show the "FHNW" tag in the view
			(org-agenda-hide-tags-regexp "PERSONAL")
			(org-scheduled-past-days 0)
			(org-agenda-start-with-log-mode t)
			(org-agenda-span 22)
			;; Show last weeks tasks
			(org-agenda-start-day "-7d")))
		  ("a" "Adfinis ToDo's" todo ""
		   ((org-agenda-files (list (concat org-directory "/work.org")))
			(org-agenda-prefix-format '((todo . " %i")))
			;; Don't show the "FHNW" tag in the view
			(org-agenda-hide-tags-regexp "ADFINIS")
			;; Hide the ugly header of org-todo-list
			(org-agenda-block-seperator nil)
			(org-agenda-overriding-header "ToDo's: ")))
		  ("f" "FHNW ToDo's" todo ""
		   ((org-agenda-files (list (concat org-directory "/school.org")))
			(org-agenda-prefix-format '((todo . " %i")))
			;; Don't show the "FHNW" tag in the view
			(org-agenda-hide-tags-regexp "FHNW")
			;; Hide the ugly header of org-todo-list
			(org-agenda-block-seperator nil)
			(org-agenda-overriding-header "ToDo's: ")))
		  ("p" "Personal ToDo's" todo ""
		   ((org-agenda-files (list (concat org-directory "/personal.org")))
			(org-agenda-prefix-format '((todo . " %i")))
			;; Don't show the "FHNW" tag in the view
			(org-agenda-hide-tags-regexp "PERSONAL")
			;; Hide the ugly header of org-todo-list
			(org-agenda-block-seperator nil)
			(org-agenda-overriding-header "ToDo's: ")))))

  (defun org-sort-buffer-by-priority ()
	"Sorts an org-mode buffer by it's priority"
	(interactive)
	(push-mark)
	(goto-char (point-min))
	(call-interactively 'org-sort)
	(org-sort 'o)
	(pop-mark))

  (defun winpat/org-agenda-show (key &optional args)
	"Show an org agenda in a vertical window that takes 1/3 of
 the current window"
	(let* ((buf (get-buffer-create "*Org Agenda*"))
		   (win (get-buffer-window buf 't)))
	  (if (equal win nil)
		  (let* ((height (nth 3 (window-edges)))
				 (nheight (- height (/ height 3)))
				 (win (split-window (selected-window) nheight)))
			(select-window win)
			(switch-to-buffer buf)))
	  (set-buffer-modified-p nil)
	  (org-agenda args key)))

  (setq org-priority-faces '((?A . (:foreground "red" :weight 'bold))
							 (?B . (:foreground "orange"))
							 (?C . (:foreground "yellow"))))

  (evil-leader/set-key
   "ol"  'org-store-link
   "oa"  'org-agenda
   "oc"  'org-capture)

  (evil-leader/set-key-for-mode 'org-mode-map
	"mo" 'org-open-at-point
	"mp" 'org-priority
	"md" 'org-deadline
	"ms" 'org-schedule
	"mr" 'org-refile
	"mi" 'org-insert-heading-after-current
	"mt" 'org-todo))

(use-package org-ref
  :ensure t
  :config
  (setq reftex-default-bibliography '("~/references.bib"))

  (setq org-ref-bibliography-notes "~/notes.org"
		org-ref-default-bibliography '("~/references.bib")
		org-ref-pdf-directory "~/bibtex-pdfs/")

  ;; Make sure that org-latex-pdf-process is set to process the bibliography (using bibtex or
  ;; biblatex)
  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f")))

(use-package org-tree-slide
  :ensure t
  :config
  (org-tree-slide-simple-profile)
  (evil-leader/set-key-for-mode
   'org-tree-slide-mode-map
   "mp" 'org-tree-slide-move-previous-tree
   "mn" 'org-tree-slide-move-next-tree))

(use-package org-journal
  :ensure t
  :config
  (setq org-journal-dir "~/shared/journal/"
		org-journal-file-type "=day="
		org-journal-file-format "%Y-%m-%d.org")
  (evil-leader/set-key
	"l" 'org-journal-new-entry))

(use-package org-download
  :ensure t
  :after (org)
  ;; Drag-and-drop to dired
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package mu4e
  :if (locate-file "mu" exec-path)
  :init
  (let ((mu4epath
		 (concat
		  (file-name-directory
		   (file-truename
			(executable-find "mu")))
		  "/../share/emacs/site-lisp/mu4e")))
	(when (and
		   (string-prefix-p "/nix/store/" mu4epath)
		   (file-directory-p mu4epath))
	  (add-to-list 'load-path mu4epath)))
  :config
  (require 'org-mu4e)

  ;; Disable annoying index messages in minibuffer, except when mbsync or mu4e returns not 0
  (setq mu4e-hide-index-messages t)

  (setq mu4e-get-mail-command "mbsync -a"
		mu4e-update-interval 300)

  ;; NOTE: This appears to break certain messages
  ;; (add-hook 'mu4e-view-mode-hook visual-line-mode)
  (add-hook 'mu4e-compose-mode-hook
			(defun my-do-compose-stuff ()
			  "My settings for message composition."
			  (set-fill-column 100)))
			  ;; Sign message by default
			  ;; (mml-secure-message-sign)))

  (setq mu4e-maildir (expand-file-name "~/mail")
		mu4e-headers-date-format "%Y/%m/%d"
		;; Show full addresses in view message (instead of just names)
		mu4e-view-show-addresses t
		;; Use emacs's built in smtp client to send mail
		message-send-mail-function 'smtpmail-send-it
		smtpmail-debug-info t
		;; Kill message buffer after the message is sent
		message-kill-buffer-on-exit t)

  (setq mu4e-contexts
		`(,(make-mu4e-context
			:name "Posteo"
			:match-func (lambda (msg)
						  (when msg
							(mu4e-message-contact-field-matches
							 msg
							 :to "patrickwinter@posteo.ch")))
			:vars '((user-mail-address . "patrickwinter@posteo.ch")
					(mu4e-drafts-folder . "/posteo/Drafts")
					(mu4e-sent-folder . "/posteo/Sent")
					(mu4e-trash-folder . "/posteo/Trash")
					(smtpmail-smtp-server . "posteo.de")
					(smtpmail-stream-type . ssl)
					(smtpmail-smtp-service . 465)))

		  ,(make-mu4e-context
			:name "Adfinis"
			:match-func (lambda (msg)
						  (when msg
							(mu4e-message-contact-field-matches
							 msg
							 :to "patrick.winter@adfinis.com")))
			:vars '((user-mail-address         . "patrick.winter@adfinis.com")
					(mu4e-sent-folder          . "/adfinis/Gesendete Objekte")
					(mu4e-drafts-folder        . "/adfinis/Entw\&APw-rfe")
					(mu4e-trash-folder         . "/adfinis/Gel&APY-schte Objekte")
					(smtpmail-smtp-server      . "smtp.adfinis.com")
					(smtpmail-stream-type      . starttls)
					(smtpmail-smtp-service     . 587)
					(mu4e-compose-signature    . (concat
												  "Adfinis AG\n"
												  "Patrick Winter, Software Engineer\n"
												  "\n"
												  "Güterstrasse 86 | CH-4053 Basel\n"
												  "Tel. 061 500 31 31\n"))))

		  ,(make-mu4e-context
			:name "FHNW"
			:match-func (lambda (msg)
						  (when msg
							(mu4e-message-contact-field-matches
							 msg
							 :to "patrick.winter@students.fhnw.ch")))
			:vars '((user-mail-address . "patrick.winter@students.fhnw.ch")
					(mu4e-sent-folder . "/fhnw/Sent Items")
					(mu4e-drafts-folder . "/fhnw/Drafts")
					(mu4e-trash-folder .  "/fhnw/Deleted Items")
					(smtpmail-smtp-server . "smtp.fhnw.ch")
					(smtpmail-stream-type . ssl)
					(smtpmail-smtp-service . 465)))

		  ,(make-mu4e-context
			:name "Hotmail"
			:match-func (lambda (msg)
						  (when msg
							(mu4e-message-contact-field-matches
							 msg
							 :to "patrick.winter@hotmail.ch")))
			:vars '((user-mail-address . "patrick.winter@hotmail.ch")
					(mu4e-sent-folder . "/hotmail/Sent")
					(mu4e-drafts-folder . "/hotmail/Drafts")
					(mu4e-trash-folder .  "/hotmail/Deleted")
					(smtpmail-smtp-server . "smtp-mail.outlook.com")
					(smtpmail-stream-type . starttls)
					(smtpmail-smtp-service . 587)))))


  ;; This sets `mu4e-user-mail-address-list' to the concatenation of all
  ;; `user-mail-address' values for all contexts. If you have other mail
  ;; addresses as well, you'll need to add those manually.
  (setq mu4e-user-mail-address-list
		(delq nil
			  (mapcar (lambda (context)
						(when (mu4e-context-vars context)
						  (cdr (assq 'user-mail-address (mu4e-context-vars context)))))
					  mu4e-contexts)))

  (setq message-citation-line-format "On %e %B %Y at %R %Z, %f wrote:"
		message-citation-line-function 'message-insert-formatted-citation-line)

  :bind (("C-x m" . mu4e)))


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

  (defun curr-dir-git-branch-string (pwd)
	"Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
	(interactive)
	(when (and (eshell-search-path "git")
			   (locate-dominating-file pwd ".git"))
	  (let ((git-output (shell-command-to-string (concat "cd " pwd " && git branch | grep '\\*' | sed -e 's/^\\* //'"))))
		(if (> (length git-output) 0)
			(concat " :" (substring git-output 0 -1))
		  "(no branch)"))))

  (defun pwd-replace-home (pwd)
	"Replace home in PWD with tilde (~) character."
	(interactive)
	(let* ((home (expand-file-name (getenv "HOME")))
		   (home-len (length home)))
	  (if (and
		   (>= (length pwd) home-len)
		   (equal home (substring pwd 0 home-len)))
		  (concat "~" (substring pwd home-len))
		pwd)))

  (defun pwd-shorten-dirs (pwd)
	"Shorten all directory names in PWD except the last two."
	(let ((p-lst (split-string pwd "/")))
	  (if (> (length p-lst) 2)
		  (concat
		   (mapconcat (lambda (elm) (if (zerop (length elm)) ""
									  (substring elm 0 1)))
					  (butlast p-lst 2)
					  "/")
		   "/"
		   (mapconcat (lambda (elm) elm)
					  (last p-lst 2)
					  "/"))
		pwd)))  ;; Otherwise, we just return the PWD

  ;; Create a custom prompt using the functions above
  (setq eshell-prompt-function
		(lambda ()
		  (let* ((directory (pwd-shorten-dirs (pwd-replace-home (eshell/pwd))))
				 (branch (or (curr-dir-git-branch-string (eshell/pwd)) "")))
			(concat   ;; Prompt for Dark Themes
			 (propertize "➜ " 'face `(:weight ultra-bold))
			 (propertize directory 'face `(:foreground normal))
			 (propertize branch 'face `(:foreground yellow :weight ultra-bold))
			 " λ "))))

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
   'eshell-mode-map
   "C-l" 'eshell-clear-buffer))

(use-package smerge-mode
  :config
  (evil-leader/set-key-for-mode
   'smerge-mode-map
   "mn" 'smerge-next
   "mp" 'smerge-prev
   "ml" 'smerge-keep-lower
   "mu" 'smerge-keep-upper
   "me" 'smerge-ediff))

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

(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; Put windows side by side
  (setq ediff-split-window-function (quote split-window-horizontally)))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
		 ("\\.md\\'" . markdown-mode)
		 ("\\.markdown\\'" . markdown-mode))
  :config
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

  (setq markdown-command "pandoc -c file:///home/patrick/vcs/dotfiles/stylesheets/github-pandoc.css --from gfm -t html5 --mathjax --highlight-style pygments --standalone")

  (evil-leader/set-key-for-mode
   'markdown-mode-map
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
   "g"   'magit-status))

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
	   ("git.sr.ht" . "sourcehut")
	   ("git.adfinis-sygroup.ch" . "gitlab")
	   ("gitlab.fhnw.ch" . "gitlab")))))
  :bind
  (("C-c b" . browse-at-remote)))

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :init (global-git-gutter-mode +1))

(use-package racket-mode
  :ensure t
  :mode (("\\.rkt\\'" . racket-mode)))

(use-package scribble-mode
  :ensure t)

(use-package rainbow-delimiters
  :ensure
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package paredit
  :ensure t
  :init
  (enable-paredit-mode))

(use-package clojure-mode
  :ensure t
  :mode (("\\.clj\\'" . clojure-mode))
  :config
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :ensure t
  :commands (cider cider-connect cider-jack-in)
  :config
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)
  (pyvenv-tracking-mode t)
  (evil-leader/set-key-for-mode
   'python-mode-map
   "ma" 'pvyenv-activate
   "md" 'pvyenv-deactivate))

(use-package python-pytest
  :ensure t
  :config
  (evil-leader/set-key-for-mode
   'python-mode-map
   "mt" 'python-pytest-popup))

(use-package python
  :config
  (setq python-shell-interpreter "ipython"
		python-shell-interpreter-args "--simple-prompt -i"))

(use-package python-black
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode))

(use-package prettier-js
  :ensure t
  :hook ((php-mode) . prettier-js-mode))

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

(use-package geben
  :ensure t
  :config
  (setq geben-path-mappings
		'(("/var/www/camac" "/home/patrick/vcs/camac-ng/php"))))

(use-package auth-source-pass
  :ensure t
  :init (auth-source-pass-enable)
  :config
  (setenv "PASSWORD_STORE_DIR" "/home/patrick/vcs/passwords"))

(use-package php-mode
  :ensure t
  :init
  (add-hook 'php-mode-hook 'php-enable-wordpress-coding-style)
  :mode (("\\.php\\'" . php-mode))
  :config
  (evil-leader/set-key-for-mode
   'php-mode-map
   "mdg" 'geben
   "mdq" 'geben-end
   "mds" 'geben-stop
   "mdi" 'geben-step-into
   "mdo" 'geben-step-over
   "mdr" 'geben-step-out
   "mdc" 'geben-run-to-cursor
   "mde" 'geben-eval-expression
   "mdd" 'geben-display-context))

(use-package haskell-mode
  :ensure t
  :init
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

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

(use-package fasd
  :ensure t
  :config
  ;; Let actions emacs also update the fasd database
  (global-fasd-mode 1)
  (evil-leader/set-key
   "j"   'fasd-find-file))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :hook (after-init . ivy-mode)
  :config (setq ivy-use-virtual-buffers t
				ivy-count-format "(%d/%d) "
				ivy-initial-inputs-alist nil
				;; Make the prompt line selectable as candidate (C-p) instead of
				;; using C-M-j to use the prompt line instead of the best
				;; matching candidate
				ivy-use-selectable-prompt t
				ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (evil-leader/set-key "bb"  'ivy-switch-buffer)
  :commands (ivy-switch-buffer))

(use-package ivy-rich
  :ensure t
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (projectile-global-mode)
  (projectile-mode +1)
  :config
  ;; Use `projectile-discover-projects-in-directory` to scan for projects
  (setq projectile-enable-caching t)

  (setq projectile-switch-project-action #'magit-status))

(use-package counsel
  :after (ivy)
  :config
  (setq counsel-find-file-ignore-regexp
		(concat
		 ;; File names beginning with # or .
		 "\\(?:\\`[#.]\\)"
		 ;; File names ending with # or ~
		 "\\|\\(?:\\`.+?[#~]\\'\\)"))
  (evil-leader/set-key
	"SPC" 'counsel-M-x
	"ff"  'counsel-find-file
	"fr"  'counsel-recentf
	"fL"  'counsel-locate))

(use-package smex
  :ensure t
  :after (counsel))

;; Python:
;;   pip install 'python-language-server[all]'
;; PHP:
;;   npm i intelephense -g
(use-package lsp-mode
  :ensure t
  :hook ((csharp-mode haskell-mode php-mode python-mode) . lsp))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :ensure t)

(use-package makefile-executor
  :ensure t
  :config
  (add-hook 'makefile-mode-hook 'makefile-executor-mode)
  (evil-leader/set-key
   "pm"  'makefile-executor-execute-project-target
   "pl"  'makefile-executor-execute-last))

(use-package unison-mode
  :ensure t
  :mode (("\\.prf\\'" . unison-mode)))

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
  (setq flyspell-default-dictionary "en_US"))

(use-package flycheck
  :ensure t)
