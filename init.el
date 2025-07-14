;; -*- lexical-binding: t; -*-

(defvar tq/iosevka (font-spec :family "Iosevka" :size 18))
(defvar tq/dejavu-sans (font-spec :family "DejaVu Sans"))

(defvar tq/config (cond
                   ;; Personal laptop
                   ((string-match-p "^\\(alpha\\|epsilon\\)$" (system-name))
                    `((enable-copilot . nil)
                      (font . ,tq/iosevka)
                      (variable-pitch-font . ,tq/dejavu-sans)))
                   ;; Versent laptop
                   ((string-match-p "-versent$" (system-name))
                    `((enable-copilot . nil)
                      (font . ,tq/iosevka)
                      (variable-pitch-font . ,tq/dejavu-sans)))
                   ;; Client mac
                   ((string-match-p "^WEL" (system-name))
                    `((enable-copilot . t)
                      (font . ,tq/iosevka)
                      (variable-pitch-font . ,tq/dejavu-sans)
                      (org-directory . "~/Documents/org")
                      (load-ssh-agent-from-shell-env . t)))))

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

 ;; Generally I want LSP and tree-sitter for all languages I use
 :lang
 (cc +lsp)
 emacs-lisp
 data
 (json +lsp)
 latex
 markdown
 (web +lsp)
 (org +hugo)
 (python +lsp +pyright)
 (julia +lsp)
 (sh +fish)
 (yaml +lsp)
 (javascript +lsp)
 (go +lsp)
 graphql
 (csharp +lsp +dotnet)
 (nix)
 (rust +lsp)

 :config
 (default +bindings +smartparens))
