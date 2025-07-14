;; -*- lexical-binding: t; -*-

;; Set up personalisation
(setq user-full-name "Tim Quelch"
      user-mail-address "tim@tquelch.com")

;; I have some secret values in here. This loads them and sets a var to tell us if they are loaded.
;; If the un-encrypted my-secrets is not present then this will be false
(defvar tq/secrets-loaded (load (concat doom-user-dir "my-secrets") t))

(after! org-crypt
  (setq org-crypt-key "07CFA8E6B5CA3E4243916E42CAE8E8818C4B8B84"))

;; The following ensure that the `PATH` variable is maintained on remote connections. This is
;; important for me because the unimelb HPC (and others) paths are define by `module load` in the
;; `.bash_profile`. This ensures that the explicitly loaded versions of git and other tools are
;; used.
(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Set up Fonts
(setq doom-font (tq/get-config 'font)
      doom-variable-pitch-font (tq/get-config 'variable-pitch-font))

;; Configure theme and UI
(setq doom-one-brighter-comments t
      doom-one-comment-bg nil
      doom-theme 'doom-one
      display-line-numbers-type t)

;; Increase number of context lines when scrolling by screens
(setq next-screen-context-lines 8)

;; Increase default fill column
(setq-default fill-column 100)

;; Ensure ~dired-omit-mode~ is not started with dired. It hides some files transparently.
(after! dired
  (remove-hook 'dired-mode-hook 'dired-omit-mode))

;; Dictionary to en_AU
(after! ispell
  (setq ispell-dictionary "en_AU"))

;; By default bookmarked lines are highlighted in an annoying orange background which often removes
;; other formatting. This disables that.
(after! bookmark
  (setq bookmark-fontify nil))

;;;; Completion
;; Reduce prefix length and delay. I want completion fast. This may cause performance issues
(after! corfu
  (setq corfu-auto-delay 0.2
        corfu-auto-prefix 1))

;; Don't use lsp-snippets. This is causing incorrect formatting on completion. I also don't really
;; use it. See https://github.com/doomemacs/doomemacs/issues/6949 for details.
;; Disable file watchers. Very poor performance for large projects
(after! lsp-mode
  (setq lsp-enable-snippet nil
        lsp-enable-file-watchers nil))

;; Configure copilot
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :init
  (map! :leader :desc "Copilot" "t p"  #'copilot-mode)
  :config
  (map! :map copilot-completion-map
        [C-tab] #'copilot-accept-completion
        [C-M-tab] #'copilot-accept-completion-by-word))

;;;; Editing and keybinds
;; Enable the use of =C-u= as the universal argument again
(after! evil
  (setq! evil-want-C-u-delete nil
         evil-want-C-u-scroll nil))

;; Instead map =C-s= to scroll up.
(global-unset-key (kbd "C-s"))
(map! :m "C-s" #'evil-scroll-up)

;; Unbind evil jumping keys. I don't use these and I've found that they interfere with other uses of
;; TAB (for example, in notmuch modes)
(map! :m [tab] nil
      :m [C-i] nil)

;; Avy config
(map! "C-'" #'avy-goto-char-timer
      "C-\"" #'avy-goto-line)
(after! avy
  (setq avy-timeout-seconds 0.3))

;; Use better ~comment-dwim~
(use-package! comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2)
  :config (setq cd2/region-command 'cd2/comment-or-uncomment-region))


;;;; Org config
(setq org-directory (tq/get-config 'org-directory "~/documents/org"))

;; Set the org-agenda files to be the org directory. This includes all the files in the base
;; directory, but no sub-directories.
(defvar org-agenda-files nil)
(add-to-list 'org-agenda-files org-directory)

(after! org
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "ONHOLD(h)" "|" "DONE(d)")
                            (sequence "EMAIL(e)" "|" "SENT(s)") ; deprecated
                            (sequence "|" "CANCELLED(c)")
                            (sequence "|" "MOVED(m)")))
  (setq org-enforce-todo-dependencies t) ; subtasks must be completed first
  (setq org-log-done 'time)
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-log-into-drawer t)
  (setq org-return-follows-link t)
  (setq org-highlight-latex-and-related '(native script entities))
  (setq org-startup-with-latex-preview nil
        org-startup-with-inline-images nil)
  ;; Turn on auto-revert mode in org mode files so that they automatically update when changed (e.g.
  ;; by syncthing, dropbox etc.). Doom does not do this automatically, instead only auto-reverting
  ;; the current buffers, which is fine for most cases except background buffers used for agendas
  ;; and capture.
  (add-hook 'org-mode-hook 'auto-revert-mode)
  (set-company-backend! 'org-mode 'company-capf)
  ;; Unmap keybind that I use for avy
  (map! :map org-mode-map "C-'" nil)
  (defun tq/org-exit-link-forward ()
    "Jump just outside a link forward"
    (interactive)
    (when (org-in-regexp org-link-any-re)
      (goto-char (match-end 0))
      (insert " ")))
  
  (defun tq/org-exit-link-backward ()
    "Jump just outside a link backward"
    (interactive)
    (when (org-in-regexp org-link-any-re)
      (goto-char (match-beginning 0))
      (save-excursion (insert " "))))
  
  (map! :map (evil-org-mode-map org-mode-map)
        :ni "C-k" #'tq/org-exit-link-forward
        :ni "C-j" #'tq/org-exit-link-backward)
  (map! :map evil-org-mode-map
        :n "zf" #'org-latex-preview)
  )

(after! org-archive
  (setq org-archive-location "archive/%s_archive::datetree/"))

(use-package! org-id
  :after org
  :config
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

(after! org-clock
  (setq org-clock-out-remove-zero-time-clocks t))

;;;; Org exporting

;; Don't export headlines tagged with :ignore:
(use-package ox-extra
  :after org
  :config
  (ox-extras-activate '(ignore-headlines)))

(use-package ox-latex
  :after org
  :config
  (add-to-list 'org-latex-classes '("a4article"
                                    "\\documentclass[11pt,a4paper]{article}"
                                    ("\\section{%s}" . "\\section*{%s}")
                                    ("\\subsection{%s}" . "\\subsection*{%s}")
                                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (setq org-latex-default-class "a4article")
  (setq org-latex-packages-alist '(("titletoc, title" "appendix" nil) ; Setup appendices
                                   ("margin=25mm" "geometry")         ; Setup margins
                                   ("" "tocbibind" nil)  ; Put bibliography in TOC
                                   ("" "pdflscape" nil)  ; Allow landscape pages
                                   ("" "pdfpages" nil)   ; Allow inclusion of pdfs
                                   ("" "svg" nil)        ; Allow SVG images (req. inkscape?)
                                   ("" "subcaption" nil) ; Allow subcaptions
                                   ("" "listings" nil)   ; Source code listings
                                   ("" "color" nil)      ; Color in source code listings
                                   ("binary-units" "siunitx" t)))     ; SI units

  (setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

  (setq org-latex-src-block-backend t)                                         ; Turn on source code inclusion
  (setq org-latex-listings-options '(("basicstyle" "\\linespread{0.85}\\ttfamily")
                                     ("numbers" "left")
                                     ("numberstyle" "\\tiny")
                                     ("frame" "tb")
                                     ("tabsize" "4")
                                     ("columns" "fixed")
                                     ("showstringspaces" "false")
                                     ("showtabs" "false")
                                     ("keepspaces" "true")
                                     ("commentstyle" "\\color{red}")
                                     ("keywordstyle" "\\color{blue}")
                                     ("breaklines" "true"))))

(after! org-attach
  (add-hook 'org-export-before-parsing-functions #'org-attach-expand-links))

;; Disable 'smart' quote export. This should remove apostrophes and quotes turning into things like ~rsquo;~
;; The reason this needs to be in a hook rather than just a normal ~after~ is that doom configures
;; this in a hook on org load. This additional hook will ensure that this option takes precedence
(defun tq/no-smart-quotes () (setq org-export-with-smart-quotes nil))
(add-hook! 'org-load-hook :append #'tq/no-smart-quotes)

(use-package! org-roam
  :commands (org-roam-node-find
             org-roam-node-insert
             org-roam-show-graph
             org-roam-buffer-toggle
             org-roam-dailies-goto-date
             org-roam-dailies-goto-today
             org-roam-dailies-goto-tomorrow
             org-roam-dailies-goto-yesterday)
  :init
  (map! :leader
        :prefix "n"
        "f" #'org-roam-node-find
        "t" #'org-roam-dailies-goto-today
        "d" nil
        (:prefix ("d" . "by date")
         :desc "Goto date"          "d" #'org-roam-dailies-goto-date
         :desc "Goto tomorrow"      "m" #'org-roam-dailies-goto-tomorrow
         :desc "Goto today"         "t" #'org-roam-dailies-goto-today
         :desc "Goto yesterday"     "y" #'org-roam-dailies-goto-yesterday
         :desc "Goto directory"     "." #'org-roam-dailies-find-directory)
        :after org
        "i" #'org-roam-node-insert
        "g" #'org-roam-show-graph
        "r" #'org-roam-buffer-toggle
        (:prefix ("d" . "by date")
         :desc "Goto previous note" "b" #'org-roam-dailies-goto-previous-note
         :desc "Goto next note"     "f" #'org-roam-dailies-goto-next-note
         "m" nil
         (:prefix ("m" . "metadata")
                  "t" #'org-roam-tag-add
                  "T" #'org-roam-tag-delete
                  "a" #'org-roam-alias-add
                  "A" #'org-roam-alias-delete)
         ))
  (setq org-roam-directory (concat (file-name-as-directory org-directory) "notes/"))
  (setq org-roam-db-location (concat doom-cache-dir "org-roam.db"))
  (add-to-list 'display-buffer-alist
               '(("\\*org-roam\\*"
                  (display-buffer-in-direction)
                  (direction . right)
                  (window-width . 0.33)
                  (window-height . fit-window-to-buffer))))
  :config
  (org-roam-db-autosync-enable)
  (setq org-roam-mode-sections (list #'org-roam-backlinks-insert-section
                                     #'org-roam-reflinks-insert-section
                                     #'org-roam-unlinked-references-insert-section))
  (setq org-roam-verbose nil)
  (setq org-roam-capture-templates
        `(("d" "default" plain "%?"
           :if-new
           (file+head
            "${slug}.org"
            "#+title: ${title}\n\n")
           :unnarrowed t
           :immediate-finish t)))
  ;; Ensure tags come from both the directory and the ~roam_tag~ file property. The default is just the property
  (setq org-roam-tag-sources '(prop all-directories))
  (setq org-roam-graph-exclude-matcher '("daily/")) ; Exclude daily notes from the graph
  ;; Update the database immediately on file changes. The alternative is to do it on an idle timer,
  ;; but I've found that to be buggy and I haven't noticed the immediate updates to be very
  ;; noticeable.
  (setq org-roam-db-update-method 'immediate)
  (add-hook! 'org-roam-file-setup-hook
    (setq-local completion-ignore-case t))
  (setq org-roam-completion-everywhere nil)
  (defun tq/org-rename-to-new-title ()
    (when-let*
        ((old-file (buffer-file-name))
         (is-roam-file (org-roam-file-p old-file))
         (in-roam-base-directory? (string-equal
                                   (expand-file-name org-roam-directory)
                                   (file-name-directory old-file)))
         (file-node (save-excursion
                      (goto-char 1)
                      (org-roam-node-at-point)))
         (slug (org-roam-node-slug file-node))
         (new-file (expand-file-name (concat slug ".org")))
         (different-name? (not (string-equal old-file new-file))))
      (rename-buffer new-file)
      (rename-file old-file new-file)
      (set-visited-file-name new-file)
      (set-buffer-modified-p nil)))
  (add-hook! 'after-save-hook #'tq/org-rename-to-new-title))

(setq org-roam-v2-ack t)

(defun tq/org-agenda-nearby-notes (&optional distance)
  (interactive "P")
  (let ((org-agenda-files (org-roam-db--links-with-max-distance
                           buffer-file-name (or distance 3)))
        (org-agenda-custom-commands '(("e" "" ((alltodo ""))))))
    (org-agenda nil "e")))

(map! :leader :prefix "n" :desc "Agenda nearby" "a" #'tq/org-agenda-nearby-notes)

(defun tq/org-roam-graph-without-cites (&optional arg)
  (interactive "P")
  (let ((org-roam-graph-exclude-matcher (cons "lit/" org-roam-graph-exclude-matcher)))
    (org-roam-graph-show arg)))

(map! :leader :prefix "n" "G" #'tq/org-roam-graph-without-cites)

(defun tq/refile-to-inbox ()
  (interactive)
  (let ((id (org-id-get-create)))
    (org-refile 3 nil (list org-default-notes-file org-default-notes-file nil nil))
    (org-edit-headline (concat "[[id:" id "][HERE]] " (nth 4 (org-heading-components))))
    (let ((new-id (org-id-get-create t)))
      (save-window-excursion
        (org-id-goto id)
        (org-set-property "ORIGIN" (concat "[[id:" new-id "]]")))))
  (let ((org-enforce-todo-dependencies nil))
    (org-map-entries (lambda () (org-todo "MOVED")) nil 'tree)))

(after! org
  (map! :map org-mode-map :localleader :prefix "r" "i" #'tq/refile-to-inbox))

(use-package org-roam-ui
  :after org-roam
  :hook (after-init . org-roam-ui-mode))


;;;; Additional language configuration
(use-package systemd
  :defer t)

(use-package docker-compose-mode
  :defer t)

;; Disable some warnings in python lsp
(after! python
  (setq! lsp-pylsp-plugins-pydocstyle-ignore t))

;; Use vterm as backend for Julia repl
(after! julia-repl
  (julia-repl-set-terminal-backend 'vterm))

;; Ensure that global environments are used. This still doesn't activate the local environment but
;; it should be good enough.
(setq lsp-julia-package-dir nil)

;; Also used for matlab
(use-package octave-mode
  :mode "\\.m\\'")

;; Set desired comment style
(after! web-mode
  (setq web-mode-comment-formats '(("java"       . "/*")
                                   ("javascript" . "//")
                                   ("typescript" . "//")
                                   ("jsx"        . "//")
                                   ("tsx"        . "//")
                                   ("php"        . "/*")
                                   ("css"        . "/*"))))

;; Don't use lsp to format ts code. I've find that this doesn't pick up correct prettier configs
(after! typescript-mode
  (setq-hook! 'typescript-mode-hook +format-with-lsp nil))
(after! web-mode
  (setq-hook! 'typescript-tsx-mode-hook +format-with-lsp nil))

(use-package sops
  :hook (after-init . global-sops-mode))

(when (tq/get-config 'load-ssh-agent-from-shell-env nil)
  (defun tq/load-ssh-agent-from-env ()
    (interactive)
    (exec-path-from-shell-copy-envs '("SSH_AUTH_SOCK" "SSH_AGENT_PID")))

  (use-package! exec-path-from-shell
    :config
    (setq exec-path-from-shell-arguments nil))

  (after! magit
    (tq/load-ssh-agent-from-env)
    (add-to-list 'magit-process-password-prompt-regexps "^Enter passphrase for .*: $")))

(setq code-review-auth-login-marker 'forge)

;; Allow cloudformation tags in yaml
(use-package lsp-yaml
  :config
  (setq lsp-yaml-custom-tags
        ["!And scalar"
         "!And mapping"
         "!And sequence"
         "!If scalar"
         "!If mapping"
         "!If sequence"
         "!Not scalar"
         "!Not mapping"
         "!Not sequence"
         "!Equals scalar"
         "!Equals mapping"
         "!Equals sequence"
         "!Or scalar"
         "!Or mapping"
         "!Or sequence"
         "!FindInMap scalar"
         "!FindInMap mappping"
         "!FindInMap sequence"
         "!Base64 scalar"
         "!Base64 mapping"
         "!Base64 sequence"
         "!Cidr scalar"
         "!Cidr mapping"
         "!Cidr sequence"
         "!Ref scalar"
         "!Ref mapping"
         "!Ref sequence"
         "!Sub scalar"
         "!Sub mapping"
         "!Sub sequence"
         "!GetAtt scalar"
         "!GetAtt mapping"
         "!GetAtt sequence"
         "!GetAZs scalar"
         "!GetAZs mapping"
         "!GetAZs sequence"
         "!ImportValue scalar"
         "!ImportValue mapping"
         "!ImportValue sequence"
         "!Select scalar"
         "!Select mapping"
         "!Select sequence"
         "!Split scalar"
         "!Split mapping"
         "!Split sequence"
         "!Join scalar"
         "!Join mapping"
         "!Join sequence"]))

(use-package aidermacs
  :commands (aidermacs-run aidermacs-run-in-current-dir)
  :config
  ;; loading the vterm backend is skipped if vterm is not currently loaded. In my config loading
  ;; vterm is deferred until it is acutally used, so we need to manually load the backend (which
  ;; will also actually trigger loading vterm)
  (require 'aidermacs-backend-vterm)
  (setq aidermacs-watch-files t
        aidermacs-backend 'vterm))

;; Disable spell fu mode for yaml mode. yaml-mode is derived from text-mode, which turns on spell
;; checking on hook. yaml-mode-hook should run after this hook to turn it off again
(add-hook! 'yaml-mode-hook (spell-fu-mode -1))

;; Add host configs for SSH hosts
(dolist (host '("primary_github" "client_github"))
  (let ((host host))
    (after! forge
      (add-to-list 'forge-alist (cons host (cdr (assoc "github.com" forge-alist)))))

    (after! browse-at-remote
      (add-to-list 'browse-at-remote-remote-type-regexps
                   `(:host ,(concat "^" host "$") :type "github" :actual-host "github.com")))
    (after! magit
      (add-to-list 'magit-clone-name-alist
                   `(,(concat "\\`" host "\\([^:]+\\)\\'") "github.com" "user (this is ignored)"))
      (add-to-list 'magit-clone-url-format (cons host "%h:%n")))))

;; Override +vc/browse-at-remote,{-kill} to first check if the thing at point is a forge topic

(defun tq/browse-at-remote ()
  (interactive)
  (require 'forge)
  (if (forge-topic-at-point)
      (forge-browse)
    (+vc/browse-at-remote)))

(defun tq/browse-at-remote-kill ()
  (interactive)
  (require 'forge)
  (if (forge-topic-at-point)
      (forge-copy-url-at-point-as-kill)
    (+vc/browse-at-remote-kill)
    (message "Copied to clipboard")))

(map! :leader (:prefix-map ("g" . "git") "y" #'tq/browse-at-remote-kill
                           (:prefix ("o" . "open in browser") "o" #'tq/browse-at-remote)))

;; Required so that 'merge pr' options are shown
(after! magit (setq transient-default-level 7))
