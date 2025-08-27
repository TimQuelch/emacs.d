;; -*- lexical-binding: t; -*-

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
  :pin "c17295134510c8759baad0e8a9f151029d5e5fe1")
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex")
  :pin "b065198f2c3bc2a47ae520acd2b1e00e7b0171e6")
(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out"))
  :pin "5ac74960231db0bf7783c2ba7a19a60f582e91ab")

(package! systemd :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae")

(package! docker-compose-mode :pin "abaa4f3aeb5c62d7d16e186dd7d77f4e846e126a")

(package! terraform-doc :pin "31f1c47453ad14181883f78258a72c02b95d9783")

(package! just-mode :pin "4c0df4cc4b8798f1a7e99fb78b79c4bf7eec12c1")

;; I keep getting errors with this
(package! go-eldoc :disable t)

(package! sops :pin "46548b854b35983b2e9e5eb4276634dfc41abfa0")

(package! aidermacs :recipe (:host github :repo "MatthewZMD/aidermacs" :files ("*.el")))

(if (tq/get-config 'load-ssh-agent-from-shell-env nil)
    (package! exec-path-from-shell :pin "72ede29a0e0467b3b433e8edbee3c79bab005884")
  (package! exec-path-from-shell :disable t))

;; Fix broken gitlab pacakges. Restrictive firewalls mean cloning from gitlab is inconsistent.
;; Replace with github mirrors
(package! shrink-path
  :recipe (:host github :repo "TimQuelch/shrink-path.el")
  :pin "c14882c8599aec79a6e8ef2d06454254bb3e1e41")
(package! gcmh
  :recipe (:host github :repo "emacsmirror/gcmh")
  :pin "0089f9c3a6d4e9a310d0791cf6fa8f35642ecfd9")
