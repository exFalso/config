;; Mu

(let ((default-directory "/usr/local/share/emacs/site-lisp"))
  (normal-top-level-add-subdirs-to-load-path))
(require 'mu4e)
(setq mu4e-maildir "~/Documents/MailDir")
(setq mu4e-sent-messages-behavior 'delete)
(setq mu4e-maildir-shortcuts
      '( ("/gmail/INBOX"           . ?g)
         ("/imperial/INBOX"        . ?i)))
(setq mu4e-get-mail-command "offlineimap")
(setq
 user-mail-address "0slemi0@gmail.com"
 user-full-name  "John ExFalso")
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)
(setq message-kill-buffer-on-exit t)
(setq mu4e-headers-date-format "%Y-%m-%d %a %R")
(setq mu4e-view-show-addresses t)
(setq mu4e-update-interval 180)
(setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-user-mail-address-regexp "0slemi0@gmail\.com\\|as13609@doc\.ic\.ac\.uk")
(setq mu4e-compose-keep-self-cc t)
(add-hook 'mu4e-compose-mode-hook
          (defun add-bcc ()
            (message-add-header "Cc: 0slemi0@gmail.com\n")))
(add-hook 'mu4e-view-mode-hook
          (lambda ()
            (local-set-key (kbd ",") 'mu4e~view-quit-buffer)
            (setq show-trailing-whitespace nil)))
(add-hook 'mu4e-headers-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))
(add-hook 'mu4e-compose-mode-hook 'flyspell-mode)

(provide 'setup-mu)
