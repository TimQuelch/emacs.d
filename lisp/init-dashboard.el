;;; init-dashboard.el --- Helper functions for emacs init and general use -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;;
;;
;;; Code:

(use-package dashboard
  :demand
  :bind ("<f2>" . open-dashboard)
  :init
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-startup-banner nil
        dashboard-set-file-icons t
        dashboard-set-heading-icons t
        dashboard-set-footer nil
        dashboard-items '((recents  . 10)
                          (bookmarks . 5)
                          (projects . 5))
        dashboard-heading-icons '((recents   . "file-text")
                                  (bookmarks . "bookmark")
                                  (agenda    . "calendar")
                                  (projects  . "file-directory")
                                  (registers . "database"))
        )
  )

(provide 'init-dashboard)
;;; init-dashboard.el ends here
