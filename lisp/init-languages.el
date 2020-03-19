;;; init-languages.el --- Initialise programming language settings -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;; 
;;
;;; Code:


;; C and C++
(use-package cc-mode
  :ensure nil
  :mode ("\\.h\\'" . c++-mode)
  :hook (c-mode . (lambda () (c-toggle-comment-style -1))))

(use-package company-c-headers
  :after (company cc-mode)
  :demand
  :hook ((c-mode c++-mode) . (my/add-company-backend-locally 'company-c-headers)))

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

(use-package company-cmake              ; Included in company
  :ensure nil
  :after (company cmake-mode)
  :hook (cmake-mode . (my/add-company-backend-locally 'company-cmake)))

;; Python
(use-package elpy
  :hook (elpy-mode . (my/add-company-backend-locally 'elpy-company-backend))
  :init                                 ;
  (remove-hook 'elpy-modules 'elpy-module-company)
  (advice-add 'python-mode :before 'elpy-enable))

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

(use-package company-matlab-shell
  :ensure nil
  :after (company matlab)
  :demand
  :hook (matlab-shell-mode . (my/add-company-backend-locally 'company-matlab-shell)))

;; Dockerfile
(use-package dockerfile-mode
  :config
  (setq dockerfile-use-sudo t))

;; Systemd
(use-package systemd)

(provide 'init-languages)
;;; init-languages.el ends here
