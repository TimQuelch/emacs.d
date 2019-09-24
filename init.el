;;; init.el --- Initialisation for emacs -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;; 
;;
;;; Code:

;; Load early init if this is not done automatically
(when (version< emacs-version "27")
  (require 'early-init)
  (package-initialize))

;; Set customize file location
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Add subdirectories to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Use newer .el file if it is newer than compiled .elc
(setq load-prefer-newer t)

(require 'init-package)
(require 'init-utils)

(require 'init-ui)
(require 'init-hydra)

;; (require 'init-dashboard)
(require 'init-window)

(require 'init-evil)
(require 'init-edit)
(require 'init-magit)
(require 'init-navigation)
(require 'init-org)
(require 'init-lsp)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-latex)
(require 'init-languages)
(require 'init-shell)

(provide 'init)
;;; init.el ends here
