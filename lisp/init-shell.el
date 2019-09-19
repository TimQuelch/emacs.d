;;; init-shell.el --- Initialise shell -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;;
;;
;;; Code:

(defvar default-shell (getenv "SHELL"))
(setq-default shell-file-name (executable-find default-shell))
(setq-default explicit-shell-file-name (executable-find default-shell))

(use-package vterm
  :if (and (executable-find "cmake")
           (executable-find "libtool")
           (executable-find "make")))

(use-package xterm-color
  :defines (compilation-environment
            eshell-preoutput-filter-functions
            eshell-output-filter-functions)
  :functions (compilation-filter)
  :init
  ;; Setup color in shell
  (setenv "TERM" "xterm-256color")
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (add-hook 'shell-mode-hook
            (lambda ()
              ;; Disable font-locking in this buffer to improve performance
              (font-lock-mode -1)
              ;; Prevent font-locking from being re-enabled in this buffer
              (make-local-variable 'font-lock-function)
              (setq font-lock-function (lambda (_) nil))
              (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

  ;; Setup color in eshell
  (with-eval-after-load 'esh-mode
    (add-hook 'eshell-before-prompt-hook
              (lambda ()
                (setq xterm-color-preserve-properties t)))
    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions
          (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))

  ;; Setup color in compilation buffers
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my-advice-compilation-filter (f proc string)
    (funcall f proc (xterm-color-filter string)))
  (advice-add 'compilation-filter :around #'my-advice-compilation-filter))

(defun open-shell ()
  "Call the best shell for the current system."
  (interactive)
  (cond ((eq system-type 'windows-nt) (eshell))
        ((fboundp 'vterm) (vterm))
        (t (ansi-term default-shell))))
(bind-key "C-c C-t" 'open-shell)

(provide 'init-shell)
;;; init-shell.el ends here
