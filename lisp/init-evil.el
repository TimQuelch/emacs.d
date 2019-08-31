;;; init-evil.el --- Helper functions for emacs init and general use -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;; 
;;
;;; Code:

(use-package evil
  :hook (after-init . evil-mode)
  :bind (
         :map evil-normal-state-map
         ([remap evil-next-line] . evil-next-visual-line)
         ([remap evil-previous-line] . evil-previous-visual-line)
         :map evil-motion-state-map
         ([remap evil-next-line] . evil-next-visual-line)
         ([remap evil-previous-line] . evil-previous-visual-line))
  :init
  (setq evil-cross-lines t))

(provide 'init-evil)
;;; init-evil.el ends here
