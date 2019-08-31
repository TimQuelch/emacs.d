;;; init-company.el --- Initialise company -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;; 
;;
;;; Code:

(use-package company
  :hook (after-init . global-company-mode)
  :bind (("C-<tab>" . company-complete)
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-j" . company-select-next-or-abort)
         ("C-k" . company-select-previous-or-abort)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . abort-and-company-yasnippet))
  :init
  (defun abort-and-company-yasnippet ()
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet))
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-selection-wrap-around t))

(use-package company-lsp
  :after (company lsp-mode)
  :demand
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-async t
        company-lsp-cache-candidates nil
        company-lsp-enable-recompletion t))

(use-package company-c-headers
  :after (company)
  :demand
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package company-auctex
  :after (company latex)
  :demand
  :config
  (company-auctex-init))

(provide 'init-company)
;;; init-company.el ends here
