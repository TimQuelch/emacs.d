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

;; Set up use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Load org mode config file
(use-package org
  :ensure org-plus-contrib)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

(provide 'init)
;;; init.el ends here
