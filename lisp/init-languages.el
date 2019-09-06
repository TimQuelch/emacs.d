;;; init-languages.el --- Initialise programming language settings -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;; 
;;
;;; Code:


;; C++
(use-package c++-mode
  :ensure nil
  :mode "\\.h\\'")

(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package clang-format
  :defines c-mode-base-map
  :commands (clang-format-buffer clang-format-region clang-format)
  :init
  (require 'cc-mode)
  :bind (:map c-mode-base-map ("C-c C-f" . clang-format-buffer)))

;; OpenCL
(use-package opencl-mode
  :mode "\\.cl\\'")

;; CMake
(use-package cmake-mode
  :mode "CMakeLists.txt"
  :config
  (defvaralias 'cmake-tab-width 'tab-width))

(use-package cmake-font-lock
  :hook cmake-mode)

;; Python
(use-package python-mode
  :mode "\\.py\\'"
  :interpreter "python")

;; Matlab
(use-package matlab-mode
  :defines (matlab-fill-code matlab-shell-command-switches)
  :mode "\\.m\\'"
  :config
  (defvaralias 'matlab-indent-level 'tab-width)
  (defvaralias 'matlab-cont-level 'tab-width)
  (setq matlab-fill-code nil
        matlab-shell-command-switches '("-nodesktop" "-nosplash")))

(provide 'init-languages)
;;; init-languages.el ends here
