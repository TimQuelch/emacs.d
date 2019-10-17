;;; init-edit.el --- Helper functions for emacs init and general use -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;; 
;;
;;; Code:

;; Set backup file location
(setq backup-directory-alist `(("." . "~/.saves")))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

;; Disable autosaves
(setq auto-save-default nil)

;; Set default tab width
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(defvaralias 'c-basic-offset 'tab-width)

;; Prefer using UTF-8
(prefer-coding-system 'utf-8)

;; Set default fill column width
(setq-default fill-column 100)

;; Auto reload files modified by external program
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

;; Better comment dwim
(use-package comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2))

;; Autocomplete parens
(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode))

;; Adding in parenthesis and other brackets from visual mode
(use-package evil-surround
  :hook (after-init . global-evil-surround-mode))

;; Subword splitting
(use-package subword
  :ensure nil
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

;; Flycheck
(use-package flycheck
  :hook (prog-mode . global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))

(use-package flycheck-posframe
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (setq flycheck-posframe-border-width 3)
  (add-to-list 'flycheck-posframe-inhibit-functions
               #'(lambda () (bound-and-true-p company-backend))))

;; Flyspell
(use-package flyspell
  :ensure nil
  :hook ((text-mode outline-mode) . flyspell-mode))

;; Compilation
(use-package compile
  :ensure nil
  :commands compile
  :config
  (setq compilation-scroll-output 'first-error))

(provide 'init-edit)
;;; init-edit.el ends here
