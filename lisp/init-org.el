;;; init-org.el --- Initialise org mode -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package org
  :ensure nil
  :bind (("C-c b" . org-switchb)
         :map org-mode-map
         ("M-j" . org-metadown)
         ("M-k" . org-metaup)
         ("M-h" . org-metaleft)
         ("M-l" . org-metaright)
         ("S-J" . org-shiftdown)
         ("S-K" . org-shiftup)
         ("S-H" . org-shiftleft)
         ("S-L" . org-shiftright)
         ("M-S-J" . org-shiftmetadown)
         ("M-S-K" . org-shiftmetaup)
         ("M-S-H" . org-shiftmetaleft)
         ("M-S-L" . org-shiftmetaright)
         ("C-S-J" . org-shiftcontroldown)
         ("C-S-K" . org-shiftcontrolup)
         ("C-S-H" . org-shiftcontrolleft)
         ("C-S-L" . org-shiftcontrolright)
         :map org-read-date-minibuffer-local-map
         ("M-j" . (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week 1))))
         ("M-k" . (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week 1))))
         ("M-h" . (lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day 1))))
         ("M-l" . (lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day 1)))))
  :init
  (setq org-directory (expand-file-name "documents/org" (getenv "HOME"))
        org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)")
                            (sequence "EMAIL(e)" "|" "SENT(s)")
                            (sequence "|" "CANCELLED(c)")))

  (defvar my/org-agenda-calendar-dir (expand-file-name "calendars" org-directory))
  (defvar my/org-agenda-refile-targets (list org-directory))
  (setq org-agenda-files (append my/org-agenda-refile-targets (list my/org-agenda-calendar-dir)))
  :config
  (defun my/verify-refile-target()
    "Exclude done todo states from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (setq org-catch-invisible-edits 'smart
        org-startup-indented t
        org-enforce-todo-dependencies t
        org-log-done 'time
        org-log-into-drawer t
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets `((nil :maxlevel . 9)
                             (,my/org-agenda-refile-targets :maxlevel . 9))
        org-refile-target-verify-function 'my/verify-refile-target
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        org-tags-column -80
        org-treat-S-cursor-todo-selection-as-state-change nil)

  (add-hook 'org-mode-hook 'visual-line-mode)

  ;; Setup latex equation preview
  (setq org-preview-latex-default-process 'dvisvgm
        org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  ;; Unbind org add file and remove file
  (unbind-key "C-c [" org-mode-map)
  (unbind-key "C-c ]" org-mode-map))

(use-package org-agenda
  :ensure nil
  :bind (("C-c a" . org-agenda)
         :map org-agenda-mode-map
         ("j" . org-agenda-next-line)
         ("k" . org-agenda-previous-line)
         ("J" . org-agenda-next-item)
         ("K" . org-agenda-previousitem))
  :config
  (setq org-agenda-dim-blocked-tasks t
        org-agenda-follow-indirect t
        org-agenda-span 'week)

  (setq org-agenda-custom-commands
        '((" " "Agenda"
           ((agenda "" nil)
            (tags "REFILE"
                  ((org-agenda-overriding-header "Tasks to Refile")
                   (orgs-tag-match-list-sublevels nil)))
            (tags-todo "-EMACS"
                       ((org-agenda-overriding-header "Unscheduled Tasks")
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))))
            (tags-todo "EMACS"
                       ((org-agenda-overriding-header "Emacs configuration")))
            )))))

(use-package org-capture
  :ensure nil
  :bind ("C-c c" . org-capture)
  :config
  (setq org-default-notes-file (expand-file-name "refile.org" org-directory)
        org-capture-bookmark nil)

  (setq org-capture-templates
        '(("t" "todo" entry (file org-default-notes-file)
           "* TODO %?\n%U\n" :clock-in t :clock-resume t)
          ("n" "note" entry (file org-default-notes-file)
           "* %?\n%U\n" :clock-in t :clock-resume t)
          ("e" "email" entry (file org-default-notes-file)
           "* EMAIL %?\n%U\n" :clock-in t :clock-resume t)
          ("r" "reply" entry (file org-default-notes-file)
           "* EMAIL Reply to %?\n%U\n" :clock-in t :clock-resume t))))

(use-package org-clock
  :ensure nil
  :init
  (org-clock-persistence-insinuate)
  :config
  (setq org-clock-in-resume t
        org-clock-into-drawer t
        org-clock-out-remove-zero-time-clocks t
        org-clock-out-when-done t
        org-clock-persist t))

(use-package oauth2)

(use-package org-caldav
  :if (require 'config-secrets nil t)
  :defines (gcal-oauth-client-id gcal-oauth-client-secret)
  :init
  (defun my/org-caldav-sync-on-close ()
    "This function will be called on close to sync calendar and save"
    (org-caldav-sync)
    (org-save-all-org-buffers))

  (defun my/expand-full-name (calendar-name)
    "Expands a calendar name into the inbox file"
    (expand-file-name (concat calendar-name ".org") my/org-agenda-calendar-dir))

  (defvar my/org-caldav-sync-timer nil "Timer that org-caldav-push-timer uses to rechedule")

  (defun my/org-caldav-sync-with-delay (secs)
    "Sync after emacs is idle for SECS seconds"
    (when my/org-caldav-sync-timer
      (cancel-timer my/org-caldav-sync-timer))
    (setq my/org-caldav-sync-timer (run-with-idle-timer (* 1 secs) nil 'org-caldav-sync)))

  (setq org-caldav-oauth2-client-id gcal-oauth-client-id
        org-caldav-oauth2-client-secret gcal-oauth-client-secret
        plstore-cache-passphrase-for-symmetric-encryption t)

  (setq org-caldav-url 'google)

  (setq org-caldav-calendars
        `((:calendar-id ,gcal-qutwork-id
                        :inbox ,(my/expand-full-name "qutwork")
                        :files ,(list (my/expand-full-name "qutwork")))
          (:calendar-id ,gcal-default-id
                        :inbox ,(my/expand-full-name "default")
                        :files ,(list (my/expand-full-name "default")))
          (:calendar-id ,gcal-us-id
                        :inbox ,(my/expand-full-name "us")
                        :files ,(list (my/expand-full-name "us")))
          (:calendar-id ,gcal-org-id
                        :inbox "cal-inbox.org"
                        :files ("todo.org"))))
  (setq org-caldav-save-directory my/org-agenda-calendar-dir)

  :config
  (setq org-icalendar-alarm-time 1
        org-icalendar-include-todo t
        org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due)
        org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))

  (add-hook 'after-save-hook
            (lambda ()
              (when (eq major-mode 'org-mode)
                (my/org-caldav-sync-with-delay 300))))
  (add-hook 'kill-emacs-hook 'org-caldav-sync-at-close)
  )

(provide 'init-org)
;;; init-org.el ends here
