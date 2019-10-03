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
         ([remap evil-previous-line] . evil-previous-visual-line)
         ("C-S-D" . evil-scroll-up))
  :init
  (setq evil-cross-lines t)
  :config
  ;; Unbind record and use macro keys (I don't use them)
  (unbind-key "q" evil-normal-state-map)
  (unbind-key "@" evil-normal-state-map)

  ;; Unbind keys which don't really do anything useful
  ;; This lets them be used by other keymaps in
  (unbind-key "SPC" evil-motion-state-map)
  (unbind-key "TAB" evil-motion-state-map)
  (unbind-key "RET" evil-motion-state-map))

(provide 'init-evil)
;;; init-evil.el ends here
