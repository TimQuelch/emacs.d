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
        org-todo-keywords '((sequence "TODO(t)" "WAITING(w@/!)" "|" "DONE(d)")
                            (sequence "EMAIL(e)" "|" "SENT(s)")
                            (sequence "|" "CANCELLED(c@/!)")))
  :config
  (defun my/verify-refile-target()
    "Exclude done todo states from refile targets"
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))
  (setq org-catch-invisible-edits 'smart
        org-enforce-todo-dependencies t
        org-log-done 'time
        org-log-into-drawer t
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9))
        org-refile-target-verify-function 'my/verify-refile-target
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        org-tags-column -80
        org-treat-S-cursor-todo-selection-as-state-change nil)

  (add-hook 'org-mode-hook 'visual-line-mode)

  ;; Setup autosaves so that org files are always saved when changed
  (add-hook 'org-capture-after-finalize-hook 'org-save-all-org-buffers)
  (add-hook 'org-after-refile-insert-hook 'org-save-all-org-buffers)
  (advice-add 'org-agenda-quit :before 'org-save-all-org-buffers)

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
         ("K" . org-agenda-previousitem)
         ("C-d" . scroll-up-command)
         ("C-S-D" . scroll-down-command))
  :init
  (setq org-agenda-files (list org-directory))
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

(use-package org-indent
  :init (setq org-startup-indented t)
  :ensure nil
  :config
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch)))

(use-package org-habit
  :ensure nil
  :init
  (add-to-list 'org-modules 'org-habit))

(provide 'init-org)
;;; init-org.el ends here
