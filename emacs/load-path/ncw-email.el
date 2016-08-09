
(defun ncw/configure-email ()
  (setq mu4e-maildir "~/Mail")
  (require 'ncw-email-config nil t)

  ;; Syncing messages
  (setq mu4e-get-mail-command "offlineimap")
  (setq mu4e-update-interval 600)

  ;; Viewing messages
  (setq mu4e-show-images t)
  (setq mu4e-view-show-images t)
  (setq mu4e-view-image-max-width 800)
  (setq mu4e-view-show-addresses t)
  (setq mu4e-use-fancy-chars t)
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; BEGIN UNDER CONSTRUCTION
  (require 'mu4e-contrib)
  (setq mu4e-html2text-command 'mu4e-shr2text)
  (setq shr-color-visible-luminance-min 60)
  ;; (setq shr-color-visible-distance-min 10)
  ;; (setq mu4e-html2text-command
  ;;       ;; Other options:
  ;;       ;;   "html2text -utf8 -nobs -width 72"
  ;;       ;;   "textutil -stdin -stdout -format html -convert txt"
  ;;       (if (executable-find "w3m")
  ;;           "w3m -T text/html"
  ;;         "html2text -utf8 -nobs -width 72"))
  ;; END UNDER CONSTRUCTION

  ;; Sending messages
  (setq mail-user-agent 'mu4e-user-agent)
  (setq smtpmail-stream-type 'starttls)
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq smtpmail-default-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-service 587)
  (setq message-kill-buffer-on-exit t)
  (setq mu4e-sent-messages-behavior 'delete)

  ;; Include sent messages in message threads from search results
  (setq mu4e-headers-include-related t)

  (setq mu4e-headers-leave-behavior 'apply)
  (setq mu4e-headers-skip-duplicates t)

  ;; Same as defaults except use :thread-subject instead of :subject to only
  ;; show subject lines once, on the original message line. Replies will have
  ;; blank subject lines.
  (setq mu4e-headers-fields
        '((:human-date . 12)
          (:flags . 6)
          (:mailing-list . 10)
          (:from . 22)
          (:thread-subject)))

  ;; Enable experimental bookmark support
  (setq mu4e-maildirs-extension-use-bookmarks t)

  ;; Don't expand the maildir tree; seeing the folders isn't useful
  (setq mu4e-maildirs-extension-default-collapse-level 0)

  ;; Refresh the unread/total counts when switching to the main mu4e buffer
  (add-hook 'mu4e-main-mode-hook
            (lambda ()
              (mu4e-maildirs-extension-force-update '(4))))

  ;; Define custom Mail layout - access with `SPC l o m'
  (spacemacs|define-custom-layout "Mail"
    :binding "m"
    :body (mu4e))
  )

(provide 'ncw-email)
