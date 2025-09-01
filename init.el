;; -*- lexical-binding: t; -*-

(defvar tq/iosevka (font-spec :family "Iosevka" :size 18))
(defvar tq/dejavu-sans (font-spec :family "DejaVu Sans"))
(defvar tq/dejavu-serif (font-spec :family "DejaVu Serif"))

(defvar tq/config (cond
                   ;; Personal laptop
                   ((string-match-p "^\\(alpha\\|epsilon\\)$" (system-name))
                    `((enable-copilot . nil)))
                   ;; Versent laptop
                   ((string-match-p "-versent$" (system-name))
                    `((enable-copilot . nil)))
                   ((string-match-p ".*" (system-name))
                    `((enable-copilot . nil)))))

(defun tq/get-config (key &optional default)
  (alist-get key tq/config default))

;; Configure doom modules
(doom!
 :completion
 (corfu +icons)
 (vertico +icons)

 :ui
 doom
 modeline
 nav-flash
 ophints
 (popup +defaults)
 (treemacs +lsp)
 vc-gutter
 window-select

 :editor
 (evil +everywhere)
 format
 fold
 snippets

 :emacs
 (dired +icons)
 electric
 vc
 undo

 ;; vterm is a better terminal
 :term
 vterm

 :checkers
 syntax
 (spell +aspell)

 :tools
 docker
 lookup
 lsp
 (magit +forge)
 direnv
 (terraform +lsp)
 tree-sitter

 ;; Generally I want LSP and tree-sitter for all languages I use
 :lang
 (cc +lsp +tree-sitter)
 emacs-lisp
 data
 (json +lsp +tree-sitter)
 latex
 (markdown +tree-sitter)
 (web +lsp +tree-sitter)
 (org +hugo)
 (python +lsp +pyright +tree-sitter)
 (julia +lsp +tree-sitter)
 (sh +fish +powershell)
 (yaml +tree-sitter)
 (javascript +lsp +tree-sitter)
 (go +lsp +tree-sitteer)
 graphql
 (csharp +lsp +dotnet +tree-sitter)
 (nix +lsp +tree-sitter)
 (rust +lsp +tree-sitter)

 :config
 (default +bindings +smartparens))
