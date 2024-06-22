(setq user-full-name "Tim Quelch"
      user-mail-address "tim@tquelch.com")

(defvar tq/secrets-loaded (load (concat doom-private-dir "my-secrets") t))

(after! org-crypt
  (setq org-crypt-key "07CFA8E6B5CA3E4243916E42CAE8E8818C4B8B84"))

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(setq doom-font (font-spec :family "Iosevka" :size 18)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans"))

(setq doom-one-brighter-comments t
      doom-one-comment-bg nil
      doom-theme 'doom-one)

(setq display-line-numbers-type t)

(setq-default fill-column 100)

(setq next-screen-context-lines 8)

(after! dired
  (remove-hook 'dired-mode-hook 'dired-omit-mode))

(after! ispell
  (setq ispell-dictionary "en_AU"))

(after! bookmark
  (setq bookmark-fontify nil))

(after! company
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1))

(set-company-backend! '(text-mode prog-mode conf-mode) 'company-capf)

(after! lsp-mode
  (setq lsp-enable-snippet nil
        lsp-file-watch-threshold 1500))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :config
  (map! :leader :desc "Copilot" "t p"  #'copilot-mode)
  (map! :map copilot-completion-map
        [C-tab] #'copilot-accept-completion
        [C-M-tab] #'copilot-accept-completion-by-word))

(after! evil
  (setq! evil-want-C-u-delete nil
         evil-want-C-u-scroll nil))

(global-unset-key (kbd "C-s"))
(map! :m "C-s" #'evil-scroll-up)

(map! :m [tab] nil
      :m [C-i] nil)

(map! "C-'" #'avy-goto-char-timer
      "C-\"" #'avy-goto-line)

(after! avy
  (setq avy-timeout-seconds 0.3))

(use-package! comment-dwim-2
  :bind ([remap comment-dwim] . comment-dwim-2)
  :config (setq cd2/region-command 'cd2/comment-or-uncomment-region))

(setq org-directory "~/documents/org/")

(defvar org-agenda-files nil)
(add-to-list 'org-agenda-files org-directory)

(after! org
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITING(w)" "ONHOLD(h)" "|" "DONE(d)")
                            (sequence "EMAIL(e)" "|" "SENT(s)")
                            (sequence "|" "CANCELLED(c)")
                            (sequence "|" "MOVED(m)")))
  (setq org-enforce-todo-dependencies t)
  (setq org-log-done 'time)
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-log-into-drawer t)
  (setq org-return-follows-link t)
  (setq org-highlight-latex-and-related '(native script entities))
  (setq org-startup-with-latex-preview nil
        org-startup-with-inline-images nil)
  (add-hook 'org-mode-hook 'auto-revert-mode)
  (set-company-backend! 'org-mode 'company-capf)
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
        :n "zf" #'org-toggle-latex-fragment)
  )

(after! org-archive
  (setq org-archive-location "archive/%s_archive::datetree/"))

(use-package! org-id
  :after org
  :config
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

(after! org-clock
  (setq org-clock-out-remove-zero-time-clocks t))

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

  (setq org-latex-listings t)                                         ; Turn on source code inclusion
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
  (add-hook 'org-export-before-parsing-hook #'org-attach-expand-links))

(after! ox (setq org-export-with-smart-quotes nil))

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
  (map! :after org
        :leader
        :prefix "n"
        "f" #'org-roam-node-find
        "i" #'org-roam-node-insert
        "g" #'org-roam-show-graph
        "r" #'org-roam-buffer-toggle
        "n" #'org-roam-dailies-capture-today
        "t" #'org-roam-dailies-goto-today
        "d" nil
        (:prefix ("d" . "by date")
           :desc "Goto previous note" "b" #'org-roam-dailies-goto-previous-note
           :desc "Goto date"          "d" #'org-roam-dailies-goto-date
           :desc "Goto next note"     "f" #'org-roam-dailies-goto-next-note
           :desc "Goto tomorrow"      "m" #'org-roam-dailies-goto-tomorrow
           :desc "Capture today"      "n" #'org-roam-dailies-capture-today
           :desc "Goto today"         "t" #'org-roam-dailies-goto-today
           :desc "Capture Date"       "v" #'org-roam-dailies-capture-date
           :desc "Goto yesterday"     "y" #'org-roam-dailies-goto-yesterday
           :desc "Goto directory"     "." #'org-roam-dailies-find-directory)
        "m" nil
        (:prefix ("m" . "metadata")
         "t" #'org-roam-tag-add
         "T" #'org-roam-tag-delete
         "a" #'org-roam-alias-add
         "A" #'org-roam-alias-delete))
  (setq org-roam-directory (concat (file-name-as-directory org-directory) "notes/"))
  (setq org-roam-db-location (concat doom-cache-dir "org-roam.db"))
  (add-to-list 'display-buffer-alist
               '(("\\*org-roam\\*"
                  (display-buffer-in-direction)
                  (direction . right)
                  (window-width . 0.33)
                  (window-height . fit-window-to-buffer))))
  :config
  (org-roam-setup)
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
  (setq org-roam-tag-sources '(prop all-directories))
  (setq org-roam-graph-exclude-matcher '("daily/"))
  (setq org-roam-db-update-method 'immediate)
  (add-hook! 'org-roam-file-setup-hook
    (setq-local completion-ignore-case t))
  (setq org-roam-completion-everywhere nil)
  (add-hook! 'after-save-hook
             (defun org-rename-to-new-title ()
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
                 (set-buffer-modified-p nil))))
  )

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

(use-package systemd
  :defer t)

(use-package docker-compose-mode
  :defer t)

(after! python
  (setq! lsp-pylsp-plugins-pydocstyle-ignore t))

(after! julia-repl
  (julia-repl-set-terminal-backend 'vterm))

(setq lsp-julia-package-dir nil)

(after! org
  (defun tq/send-block-to-julia-repl ()
      (interactive)
      (save-mark-and-excursion
        (org-babel-mark-block)
        (julia-repl-send-region-or-line)))
  (map! :map org-mode-map "C-c C-v C-c" #'tq/send-block-to-julia-repl))

(define-minor-mode julia-repl-interaction-mode
  "Toggle keybinds to send lines to the julia-repl"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-s") #'julia-repl-send-region-or-line)
            map))

(use-package octave-mode
  :mode "\\.m\\'")

(after! web-mode
  (setq web-mode-comment-formats '(("java"       . "/*")
                                   ("javascript" . "//")
                                   ("typescript" . "//")
                                   ("jsx"        . "//")
                                   ("tsx"        . "//")
                                   ("php"        . "/*")
                                   ("css"        . "/*"))))

(after! typescript-mode
  (setq-hook! 'typescript-mode-hook +format-with-lsp nil))
(after! web-mode
  (setq-hook! 'typescript-tsx-mode-hook +format-with-lsp nil))
