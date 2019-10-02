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
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (setq-default TeX-master nil
                TeX-PDF-mode t
                LaTeX-indent-level tab-width
                TeX-brace-indent-level tab-width
                LaTeX-item-indent -2)
  (setq TeX-parse-self t))

(use-package reftex
  :after latex
  :demand
  :defines (default-bibliography)
  :config
  (require 'init-org)
  (setq reftex-plug-into-AUCTeX t
        reftex-default-bibliography (list default-bibliography)))

(use-package bibtex
  :ensure nil
  :config
  (setq bibtex-dialect 'biblatex))

(provide 'init-latex)
;;; init-latex.el ends here
