;;; init.el --- Initialisation for emacs -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;; 
;;
;;; Code:

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Originally I had this set to 1 to speed up installs, but this was causing issues with tags
;; missing from repos
(setq straight-vc-git-default-clone-depth 'full)

;; Set up use-package
(straight-use-package 'use-package)

(eval-when-compile
  (require 'use-package))

;; Load org mode config file
(use-package org
  :straight org-plus-contrib)
(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))

(provide 'init)
;;; init.el ends here
