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
 vi-tilde-fringe
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
 :term
 vterm
 :checkers
 syntax
 (spell +aspell +everywhere)
 :tools
 docker                                  ; docker
 lookup                                  ; lookup of definitions/docs
 lsp                             ; enable language server
 (magit +forge)                          ; git wizardry
 tree-sitter
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
 (yaml +lsp)
 (javascript +lsp +tree-sitter)
 (go +lsp +tree-sitter)
 graphql
 (csharp +lsp +tree-sitter +dotnet)
 :config
 literate
 (default +bindings +smartparens)
 )
