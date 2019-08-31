;;; init-latex.el --- Initialise latex -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;; 
;;
;;; Code:

(use-package latex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (setq TeX-PDF-mode t)
  (defvaralias 'LaTeX-indent-level 'tab-width)
  (defvaralias 'TeX-brace-indent-level 'tab-width)
  (setq LaTeX-item-indent -2))

(use-package reftex
  :after latex
  :demand
  :config
  (setq reftex-plug-into-AUCTeX t))

(provide 'init-latex)
;;; init-latex.el ends here
