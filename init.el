;;; init.el --- Initialisation for emacs -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;;
;;
;;; Code:

(defvar tq/config-file (expand-file-name "config.org" user-emacs-directory)
  "Main configuration file")

(defun tq/tangle-and-compile-config ()
  (let* ((org-file tq/config-file)
         (el-file (concat (file-name-sans-extension org-file) ".el")))
    (when (or (not (file-exists-p el-file))
              (file-newer-than-file-p org-file el-file))
      (require 'ob-tangle)
      (org-babel-tangle-file org-file el-file)
      (byte-compile-file el-file))))

(tq/tangle-and-compile-config)

(load-file (concat (file-name-sans-extension tq/config-file) ".el"))

(provide 'init)
;;; init.el ends here
