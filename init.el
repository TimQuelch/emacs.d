;;; init.el --- Initialisation for emacs -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;; 
;;
;;; Code:

;; Set garbage collecter thresholds
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold) ; Set high threashold during init
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold)))) ; Reset back down after init

;; Set customize file location
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Add subdirectories to load path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-package)
(require 'init-utils)

(require 'init-ui)

(require 'init-evil)
(require 'init-edit)
(require 'init-magit)
(require 'init-navigation)
(require 'init-lsp)
(require 'init-company)
(require 'init-yasnippet)
(require 'init-latex)
(require 'init-languages)

(provide 'init)
;;; init.el ends here
