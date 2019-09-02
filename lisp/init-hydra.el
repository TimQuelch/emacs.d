;;; init-hydra.el --- Initialise hydra -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;; 
;;
;;; Code:

(use-package hydra
  :commands (hydra-default-pre
             hydra-keyboard-quit
             hydra--call-interactively-remap-maybe
             hydra-show-hint
             hydra-set-transient-map))

(use-package pretty-hydra
  :demand)

(provide 'init-hydra)
;;; init-hydra.el ends here
