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

(use-package forge
  :after magit
  :demand)

(use-package evil-magit
  :after magit
  :demand)

(use-package diff-hl
  :hook (after-init . global-diff-hl-mode)
  :config
  (diff-hl-flydiff-mode 1)
  (setq-default fringes-outside-margins t)

  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(use-package gitignore-mode)
(use-package gitattributes-mode)
(use-package gitconfig-mode)

(provide 'init-magit)
;;; init-magit.el ends here
