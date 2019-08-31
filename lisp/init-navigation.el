;;; init-navigation.el --- Initialise helm, projectile, and other navigation -*- lexical-binding: t -*-
;;
;; Author: Tim Quelch
;;
;;; Commentary:
;;
;;
;;; Code:

(bind-key "M-n" 'xref-find-definitions)
(bind-key "M-m" 'xref-find-references)

;; Projectile
(use-package projectile
  :hook (after-init . projectile-mode))

;; Helm
(use-package helm
  :bind (
         ("C-c h" . helm-command-prefix)
         ("M-x" . helm-M-x)
         ("M-p" . helm-show-kill-ring)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         )
  :config
  (require 'helm-config)
  (require 'helm-files)

  (setq helm-split-window-inside-p            t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line        t
        helm-scroll-amount                    4 ; scroll 4 lines other window using M-<next>/M-<prior>
        ;; helm-quick-update                  t ; do not display invisible candidates
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.

        ;; helm-candidate-number-limit 500 ; limit the number of displayed canidates
        helm-buffer-skip-remote-checking      t
        helm-mode-fuzzy-match                 t
        helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non-nil useful in helm-mini that lists buffers
        helm-org-headings-fontify             t
        ;; helm-find-files-sort-directories   t
        ;; ido-use-virtual-buffers            t
        helm-semantic-fuzzy-match             t
        helm-M-x-fuzzy-match                  t
        helm-imenu-fuzzy-match                t
        helm-lisp-fuzzy-completion            t
        ;; helm-apropos-fuzzy-match           t
        helm-locate-fuzzy-match               t
        helm-display-header-line              nil

        ;; you can customize helm-do-grep to execute ack-grep
        ;; helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
        ;; helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f"
        )

  (defun helm-hide-minibuffer-maybe ()
    "Hide minibuffer in Helm session if we use the header line as input field."
    (when (with-helm-buffer helm-echo-input-in-header-line)
      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
        (overlay-put ov 'window (selected-window))
        (overlay-put ov 'face
                     (let ((bg-color (face-background 'default nil)))
                       `(:background ,bg-color :foreground ,bg-color)))
        (setq-local cursor-type nil))))

  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

  (helm-mode 1))

(use-package helm-xref
  :after helm
  :demand)

(provide 'init-navigation)
;;; init-navigation.el ends here
