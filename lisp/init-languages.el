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
  :commands (clang-format-buffer clang-format-region clang-format)
  :init
  ;; Bind clang-format within all C modes. bind-key rather than :bind required for autoload
  (require 'cc-mode)
  (bind-key "C-c C-f" 'clang-format-buffer c-mode-base-map))

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

(use-package matlab-mode
  :mode "\\.m\\'"
  :config
  (defvaralias 'matlab-indent-level 'tab-width)
  (defvaralias 'matlab-cont-level 'tab-width)
  (setq matlab-fill-code nil))

(provide 'init-languages)
;;; init-languages.el ends here
