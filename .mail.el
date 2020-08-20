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


(use-package goto-line-preview
  :ensure t
  :bind (("M-g" . goto-line-preview)))


(use-package auth-source-pass
  :ensure t
  :init (auth-source-pass-enable)
  :config
  (setenv "PASSWORD_STORE_DIR" "/home/patrick/vcs/passwords"))


(use-package mu4e-alert
  :ensure t
  :config
  (mu4e-alert-set-default-style 'libnotify)
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications))

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

  ;; Don't ask for context on first start.
  (setq mu4e-context-policy 'pick-first
	mu4e-compose-context-policy 'ask-if-none)

  ;; Disable index messages in minibuffer, except when mbsync or mu4e
  ;; an error status code.
  (setq mu4e-hide-index-messages t)

  (setq mu4e-get-mail-command "mbsync -a"
	mu4e-update-interval 300)

  (add-hook 'mu4e-compose-mode-hook (lambda () (set-fill-column 100)))

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

(mu4e)

