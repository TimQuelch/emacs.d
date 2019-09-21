;;; init-org.el --- Initialise org mode -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package org
  :defines org-capture-bookmark
  :ensure nil
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c c" . org-capture)
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
         )
  :config
  (setq org-directory (expand-file-name "documents/org" (getenv "HOME"))
        org-catch-invisible-edits 'smart
        org-startup-indented t)

  ;; Todos and agenda
  (setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)" "CANCEL(c)")
                            (sequence "EMAIL(e)" "|" "SENT(s)")))

  (setq org-agenda-files (list org-directory)
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-log-done 'time
        org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9))
        org-refile-use-outline-path t
        org-refile-allow-creating-parent-nodes 'confirm
        org-capture-bookmark nil)

  ;; Unbind org add file and remove file
  (unbind-key "C-c [" org-mode-map)
  (unbind-key "C-c ]" org-mode-map))

(provide 'init-org)
;;; init-org.el ends here
