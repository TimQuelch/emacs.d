;;; init-languages.el --- Initialise programming language settings -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;; 
;;
;;; Code:


;; C++
(use-package cc-mode
  :ensure nil
  :mode ("\\.h\\'" . c++-mode))

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package clang-format
  :after cc-mode
  :commands (clang-format-buffer clang-format-region clang-format)
  :bind (:map c-mode-base-map ("C-c C-f" . clang-format-buffer)))

;; OpenCL
(use-package opencl-mode
  :mode "\\.cl\\'")

;; CMake
(use-package cmake-mode
  :commands cmake-mode
  :config
  (setq cmake-tab-width tab-width))

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

;; Python
(use-package python-mode
  :mode "\\.py\\'"
  :interpreter "python")

;; Matlab
(use-package matlab
  :ensure matlab-mode
  :defines (matlab-fill-code matlab-shell-command-switches matlab-indent-level matlab-cont-level)
  :commands (matlab-mode matlab-shell)
  :config
  (setq matlab-fill-code nil
        matlab-indent-level tab-width
        matlab-cont-level tab-width
        matlab-shell-command-switches '("-nodesktop" "-nosplash")))

;; Dockerfile
(use-package dockerfile-mode
  :config
  (setq dockerfile-use-sudo t))

(provide 'init-languages)
;;; init-languages.el ends here
