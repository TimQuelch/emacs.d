;;; init-company.el --- Initialise company -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;; 
;;
;;; Code:

(use-package company
  :hook (after-init . global-company-mode)
  :bind (("C-<tab>" . company-complete)
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-j" . company-select-next-or-abort)
         ("C-k" . company-select-previous-or-abort)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . abort-and-company-yasnippet))
  :init
  (defun abort-and-company-yasnippet ()
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet))
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-selection-wrap-around t))

(use-package company-box
  :diminish
  :functions (my-company-box--make-line
              my-company-box-icons--elisp)
  :commands (company-box--get-color
             company-box--resolve-colors
             company-box--add-icon
             company-box--apply-color
             company-box--make-line
             company-box-icons--elisp)
  :hook (company-mode . company-box-mode)
  :init (setq company-box-backends-colors nil
              company-box-show-single-candidate t
              company-box-max-candidates 50
              company-box-doc-delay 0.5)
  :config
  ;; Support `company-common'
  (defun my-company-box--make-line (candidate)
    (-let* (((candidate annotation len-c len-a backend) candidate)
            (color (company-box--get-color backend))
            ((c-color a-color i-color s-color) (company-box--resolve-colors color))
            (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
            (candidate-string (concat (propertize (or company-common "") 'face 'company-tooltip-common)
                                      (substring (propertize candidate 'face 'company-box-candidate) (length company-common) nil)))
            (align-string (when annotation
                            (concat " " (and company-tooltip-align-annotations
                                             (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))))))
            (space company-box--space)
            (icon-p company-box-enable-icon)
            (annotation-string (and annotation (propertize annotation 'face 'company-box-annotation)))
            (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                            (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                          (company-box--apply-color icon-string i-color)
                          (company-box--apply-color candidate-string c-color)
                          align-string
                          (company-box--apply-color annotation-string a-color)))
            (len (length line)))
      (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
                                       'company-box--color s-color)
                           line)
      line))
  (advice-add #'company-box--make-line :override #'my-company-box--make-line)

  ;; Prettify icons
  (defun my-company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym) 'Function)
              ((featurep sym) 'Module)
              ((facep sym) 'Color)
              ((boundp sym) 'Variable)
              ((symbolp sym) 'Text)
              (t . nil)))))
  (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

  (declare-function all-the-icons-faicon 'all-the-icons)
  (declare-function all-the-icons-material 'all-the-icons)
  (declare-function all-the-icons-octicon 'all-the-icons)
  (setq company-box-icons-all-the-icons
        `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.2))
          (Text . ,(all-the-icons-faicon "text-width" :height 0.85 :v-adjust -0.05))
          (Method . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
          (Function . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
          (Constructor . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
          (Field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
          (Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
          (Class . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
          (Interface . ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Module . ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Property . ,(all-the-icons-faicon "wrench" :height 0.85 :v-adjust -0.05))
          (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.9 :v-adjust -0.2))
          (Value . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Enum . ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
          (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.9 :v-adjust -0.2))
          (Snippet . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))
          (Color . ,(all-the-icons-material "palette" :height 0.9 :v-adjust -0.2))
          (File . ,(all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.05))
          (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.9 :v-adjust -0.2))
          (Folder . ,(all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05))
          (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
          (Constant . ,(all-the-icons-faicon "square-o" :height 0.9 :v-adjust -0.05))
          (Struct . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
          (Event . ,(all-the-icons-faicon "bolt" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-orange))
          (Operator . ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.2))
          (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
          (Template . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2)))
        company-box-icons-alist 'company-box-icons-all-the-icons))

(use-package company-lsp
  :after (company lsp-mode)
  :demand
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-async t
        company-lsp-cache-candidates nil
        company-lsp-enable-recompletion t))

(use-package company-c-headers
  :after (company)
  :demand
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package company-auctex
  :after (company latex)
  :demand
  :config
  (company-auctex-init))

(provide 'init-company)
;;; init-company.el ends here
