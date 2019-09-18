;;; early-init.el --- Early initialisation file -*- lexical-binding: t -*-
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

;; Setup package manager
(require 'package)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(setq package-enable-at-startup t)      ; Emacs will initialise packages between early-init and init
(setq package-quickstart t)             ; Compile package autoloads into one file


;; Initialise some ui stuff here
(push '(tool-bar-lines . 0) default-frame-alist)   ; Disable tool bar
(push '(menu-bar-lines . 0) default-frame-alist)   ; Disable menu bar
(push '(vertical-scroll-bars) default-frame-alist) ; Disable scroll bar
(push '(alpha . 90) default-frame-alist)           ; Turn on transparency
(push '(left-fringe . 11) default-frame-alist)     ; Configure fringes
(push '(right-fringe . 11) default-frame-alist)    ; Configure fringes

;; Set fonts
(push '(font . "DejaVu Sans Mono") default-frame-alist)
(set-face-font 'default "DejaVu Sans Mono")
(set-face-font 'variable-pitch "DejaVu Sans")
(copy-face 'default 'fixed-pitch)

(provide 'early-init)
;;; early-init.el ends here
