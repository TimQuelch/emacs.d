;;; init-magit.el --- Helper functions for emacs init and general use -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;; 
;;
;;; Code:

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package evil-magit
  :after magit
  :demand)

(provide 'init-magit)
;;; init-magit.el ends here
