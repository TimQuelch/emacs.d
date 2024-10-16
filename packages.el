;; Disable doom snippets (I use snippets for LSP completion snippets, not weird opinionated pregenerated ones)
(package! doom-snippets :ignore t)

(if (tq/get-config 'enable-copilot)
    (package! copilot
      :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
  (package! copilot :disable t))

(package! comment-dwim-2 :pin "7cdafd6d98234a7402865b8abdae54a2f2551c94")

;; Evil packages that I don't use
(disable-packages! evil-snipe evil-lion)

;; Weird formatting in org
(disable-packages! org-superstar)

(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam")
  :pin "5c06471c3a11348342719fd9011486455adeb701")
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex")
  :pin "efdac6fe4134c33f50b06a0a6d192003d0e5094c")
(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out"))
  :pin "5ac74960231db0bf7783c2ba7a19a60f582e91ab")

(package! systemd :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae")

(package! docker-compose-mode :pin "abaa4f3aeb5c62d7d16e186dd7d77f4e846e126a")

;; I keep getting errors with this
(package! go-eldoc :disable t)

;; This package seems broken. I don't use it so I can safely disable
(package! code-review :disable t)

(package! sops :pin "46548b854b35983b2e9e5eb4276634dfc41abfa0")

(if (tq/get-config 'load-ssh-agent-from-shell-env nil)
    (package! exec-path-from-shell :pin "72ede29a0e0467b3b433e8edbee3c79bab005884")
  (package! exec-path-from-shell :disable t))
