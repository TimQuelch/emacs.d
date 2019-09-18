;;; init-ui.el --- Helper functions for emacs init and general use -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;;
;;
;;; Code:

(setq use-file-dialog nil                          ; Disable file dialog
      use-dialog-box nil                           ; Disable dialog box
      inhibit-startup-screen t                     ; Diable startup screen
      inhibit-splash-screen t                      ; Disable splash screen
      inhibit-startup-echo-area-message t)         ; Disable startup message

;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 4
      window-divider-default-right-width 4)
(add-hook 'window-setup-hook #'window-divider-mode)

(use-package doom-themes
  :defines (doom-one-brighter-comments doom-one-comment-bg)
  :init
  (setq doom-one-brighter-comments t
        doom-one-comment-bg nil)
  (load-theme 'doom-one t))

(use-package all-the-icons)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  ;; prevent flash of unstyled modeline at startup
  (unless after-init-time
    (setq doom-modeline--old-format mode-line-format)
    (setq-default mode-line-format nil))

  (setq doom-modeline-major-mode-color-icon t
        doom-modeline-minor-modes nil
        doom-modeline-mu4e nil))

;; Turn on line numbers
(use-package linum
  :hook (after-init . global-linum-mode))

(use-package hlinum
  :hook (global-linum-mode . hlinum-activate)
  :config
  (setq linum-highlight-in-all-buffersp t))

;; Highlight current line
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

;; Highlight matching parens
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config (setq show-paren-delay 0
                show-paren-when-point-inside-paren t
                show-paren-when-point-in-periphery t))

;; Highlight brackets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight todos
(use-package hl-todo
  :hook (after-init . global-hl-todo-mode))

;; Use variable pitch in text mode (except for codey things)
(use-package mixed-pitch
  :hook (text-mode . mixed-pitch-mode))

(provide 'init-ui)
;;; init-ui.el ends here
