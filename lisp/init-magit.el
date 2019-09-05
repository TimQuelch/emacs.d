;;; init-magit.el --- Helper functions for emacs init and general use -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;; 
;;
;;; Code:

(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (set-face-attribute 'magit-branch-current nil :box t))

(use-package evil-magit
  :after magit
  :demand)

(provide 'init-magit)
;;; init-magit.el ends here
