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
                    `((enable-copilot . t)
                      (font . ,tq/iosevka)
                      (variable-pitch-font . ,tq/dejavu-sans)))
                   ;; Client mac
                   ((string-match-p "^WEL" (system-name))
                    `((enable-copilot . t)
                      (font . ,tq/iosevka)
                      (variable-pitch-font . ,tq/dejavu-sans)
                      (org-directory . "~/Documents/org")))))

(defun tq/get-config (key &optional default)
  (alist-get key tq/config default))

;; Configure doom modules
(doom!
 :completion
 (company +childframe)
 (ivy +prescient +icons)

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
 (spell +aspell +everywhere)

 :tools
 docker
 lookup
 lsp
 (magit +forge)
 tree-sitter

 ;; Generally I want LSP and tree-sitter for all languages I use
 :lang
 (cc +lsp +tree-sitter)
 emacs-lisp
 data
 (json +lsp +tree-sitter)
 latex
 markdown
 (web +lsp +tree-sitter)
 (org +hugo)
 (python +tree-sitter)
 (julia +lsp +tree-sitter)
 (sh +fish +tree-sitter)
 (yaml +lsp +tree-sitter)
 (javascript +lsp +tree-sitter)
 (go +lsp +tree-sitter)
 graphql
 (csharp +lsp +tree-sitter +dotnet)
 (nix +tree-sitter)

 :config
 (default +bindings +smartparens))
