;; Set up package manager
(require 'package)

;; Set garbage collecter thresholds
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold) ; Increase during startup
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold)))) ; Decrease after startup

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; Set up use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-expand-minimally t)
(setq use-package-compute-statistics t)

;; Configure GUI
(push '(tool-bar-lines . 0) default-frame-alist)   ; Disable tool bar
(push '(menu-bar-lines . 0) default-frame-alist)   ; Disable menu bar
(push '(vertical-scroll-bars) default-frame-alist) ; Disable scroll bar

(setq use-file-dialog nil                          ; Disable file dialog
      use-dialog-box nil                           ; Disable dialog box
      inhibit-startup-screen t                     ; Diable startup screen
      inhibit-splash-screen t                      ; Disable splash screen
      inhibit-startup-echo-area-message t)         ; Disable startup message

;; Display dividers between windows
(setq window-divider-default-places t
      window-divider-default-bottom-width 3
      window-divider-default-right-width 3)
(add-hook 'window-setup-hook #'window-divider-mode)

(use-package doom-themes
  :init
  (setq doom-one-brighter-comments t
        doom-one-comment-bg nil)
  (load-theme 'doom-one t))

(use-package all-the-icons)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  ;; prevent flash of unstyled modeline at startup
  (unless after-init-time
    (setq doom-modeline--old-format mode-line-format)
    (setq-default mode-line-format nil))

  (setq doom-modeline-major-mode-color-icon t
        doom-modeline-minor-modes nil
        doom-modeline-mu4e nil))

;; Turn on line numbers
(use-package linum
  :hook (after-init . global-linum-mode))

(use-package hlinum
  :hook (global-linum-mode . hlinum-activate)
  :config (setq linum-highlight-in-all-buffersp t))

;; Highlight current line
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

;; Highlight matching parens
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config (setq show-paren-delay 0
                show-paren-when-point-inside-paren t
                show-paren-when-point-in-periphery t))

;; Autocomplete parens
(use-package smartparens
  :hook (after-init . smartparens-global-mode))

(use-package evil-surround
  :hook (after-init . global-evil-surround-mode))

;; Highlight brackets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight todos
(use-package hl-todo
  :hook (after-init . global-hl-todo-mode))

;; Set customize file location
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Set backup file location
(setq backup-directory-alist `(("." . "~/.saves")))
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq version-control t)

;; Disable autosaves
(setq auto-save-default nil)

;; Turn on transparency
(set-frame-parameter (selected-frame) 'alpha 90)
(add-to-list 'default-frame-alist '(alpha . 90))

;; Set default tab width
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(defvaralias 'c-basic-offset 'tab-width)

;; Prefer using UTF-8
(prefer-coding-system 'utf-8)

;; Set default fill column width
(setq-default fill-column 100)

;; Auto reload files modified by external program
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode))

;; Evil
(use-package evil
  :hook (after-init . evil-mode)
  :bind (
         :map evil-normal-state-map
         ([remap evil-next-line] . evil-next-visual-line)
         ([remap evil-previous-line] . evil-previous-visual-line)
         :map evil-motion-state-map
         ([remap evil-next-line] . evil-next-visual-line)
         ([remap evil-previous-line] . evil-previous-visual-line))
  :init
  (setq evil-cross-lines t))

;; Magit
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package evil-magit
  :after magit
  :demand)

;; Better comment dwim
(use-package comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2))

;; Subword splitting
(use-package subword
  :ensure nil
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

;; Set show paren mode
;;(setq show-paren-delay 0)
;;(show-paren-mode t)

;; Set up GDB
;;(setq gdb-many-windows t)
;;(setq gdb-show-main t)

;; Navigation
(bind-key "M-n" 'xref-find-definitions)
(bind-key "M-m" 'xref-find-references)

;; LSP
(use-package lsp-mode
  :hook ((c++-mode . lsp)
         (python-mode . lsp))
  :config
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error")
        lsp-prefer-flymake nil
        lsp-enable-snippet nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind (
         :map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references)
         :map lsp-ui-peek-mode-map
         ("M-j" . lsp-ui-peek--select-next-file)
         ("M-k" . lsp-ui-peek--select-prev-file)
         ("C-j" . lsp-ui-peek--select-next)
         ("C-k" . lsp-ui-peek--select-prev)
         ("j" . lsp-ui-peek--select-next)
         ("k" . lsp-ui-peek--select-prev)
         )
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t
        lsp-ui-peek-enable t
        lsp-ui-peek-always-show t
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 25))

;; Flycheck
(use-package flycheck
  :hook (prog-mode . global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))

(use-package flycheck-pos-tip
  :hook (flycheck-mode . flycheck-pos-tip-mode))

;; Company
(use-package company
  :hook (after-init . global-company-mode)
  :bind (("C-<tab>" . company-complete)
         :map company-active-map
         ("C-j" . company-select-next-or-abort)
         ("C-k" . company-select-previous-or-abort))
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-selection-wrap-around t))

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

;; Compilation
(bind-key "<f5>" (lambda ()
                   (interactive)
                   (setq-local compilation-read-command nil)
                   (call-interactively 'compile)))

;; Semantic
;;(use-package semantic
;;  :config
;;  (global-semanticdb-minor-mode t)
;;  (global-semantic-idle-scheduler-mode t)
;;  (global-semantic-idle-summary-mode t)
;;  (global-semantic-stickyfunc-mode t)
;;  (use-package stickyfunc-enhance)
;;  (semantic-mode t)
;;  (semantic-add-system-include "/usr/include/boost" 'c++-mode))

;;(use-package ede
;;  :config
;;  (global-ede-mode t))

;; Flyspell
(use-package flyspell
  :ensure nil
  :hook ((text-mode outline-mode LaTeX-mode) . flyspell-mode))

;; LaTeX
(use-package latex
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :config
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (setq TeX-PDF-mode t)
  (defvaralias 'LaTeX-indent-level 'tab-width)
  (defvaralias 'TeX-brace-indent-level 'tab-width)
  (setq LaTeX-item-indent -2))

(use-package reftex
  :after latex
  :demand
  :config
  (setq reftex-plug-into-AUCTeX t))

;; Matlab
;;(use-package matlab-mode)

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

;; Ruby
(use-package ruby-mode
  :config
  (defvaralias 'ruby-tab-width 'tab-width))

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

(defun indent-buffer ()
  "Indent entire buffer."
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max) nil)))
(bind-key "C-c f" 'indent-buffer)

(defun align-trailing-comments (beginning end)
  "Align comments in region."
  (interactive "*r")
  (let (indent-tabs-mode align-to-tab-stop)
    (align-regexp beginning end (concat "\\(\\s-*\\)" (regexp-quote comment-start)))))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(provide 'init)
;;; init.el ends here
