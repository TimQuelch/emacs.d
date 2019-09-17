;;; init-yasnippet.el --- Initialise yasnippet -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;; 
;;
;;; Code:

(use-package yasnippet
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
