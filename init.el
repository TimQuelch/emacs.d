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

 :config
 (default +bindings +smartparens))
