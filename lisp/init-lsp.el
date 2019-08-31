;;; init-lsp.el --- Initialise lsp -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;; 
;;
;;; Code:

(use-package lsp-mode
  :hook ((c++-mode . lsp)
         (python-mode . lsp))
  :config
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error")
        lsp-prefer-flymake nil
        lsp-enable-snippet nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind (
         :map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         :map lsp-ui-peek-mode-map
         ("M-j" . lsp-ui-peek--select-next-file)
         ("M-k" . lsp-ui-peek--select-prev-file)
         ("C-j" . lsp-ui-peek--select-next)
         ("C-k" . lsp-ui-peek--select-prev)
         ("j" . lsp-ui-peek--select-next)
         ("k" . lsp-ui-peek--select-prev)
         )
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-always-show t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25))

(provide 'init-lsp)
;;; init-lsp.el ends here
