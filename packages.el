(package! doom-snippets :ignore t)

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

(package! comment-dwim-2 :pin "7cdafd6d98234a7402865b8abdae54a2f2551c94")

(disable-packages! evil-snipe evil-lion)

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

(package! go-eldoc :disable t)

(package! code-review :disable t)
