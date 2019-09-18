;;; init-package.el --- Initialise package and use-package -*- lexical-binding: t; no-byte-compile: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;; 
;;
;;; Code:

;; Set up use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-expand-minimally t)
(setq use-package-compute-statistics t)

;; Auto compile elisp files when saved and loaded by emacs
(use-package auto-compile
  :hook (emacs-lisp-mode . auto-compile-on-save-mode)
  :demand
  :config
  (auto-compile-on-load-mode))

(provide 'init-package)
;;; init-package.el ends here
