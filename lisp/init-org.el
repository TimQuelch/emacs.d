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
         ("C-S-L" . org-shiftcontrolright))
  :init
  (setq org-directory (expand-file-name "documents/org" (getenv "HOME"))
        org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)")
                            (sequence "EMAIL(e)" "|" "SENT(s)")
                            (sequence "| CANCELLED(c)")))
  :config
  (setq org-catch-invisible-edits 'smart
        org-startup-indented t
        org-enforce-todo-dependencies t
        org-log-done 'time
        org-log-into-drawer t
        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)))

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
  :init
  (setq org-agenda-files (list org-directory))
  :config
  (setq org-agenda-dim-blocked-tasks t
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil))

(use-package org-capture
  :ensure nil
  :bind ("C-c c" . org-capture)
  :config
  (setq org-default-notes-file (expand-file-name "refile.org" org-directory)
        org-capture-bookmark nil))

(provide 'init-org)
;;; init-org.el ends here
