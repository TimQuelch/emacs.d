(package! doom-snippets :ignore t)

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

(package! comment-dwim-2 :pin "7cdafd6d98234a7402865b8abdae54a2f2551c94")

(disable-packages! evil-snipe evil-lion)

(disable-packages! org-superstar)

(package! org-ref :pin "6a759a969d92dd1c69f540129ebaa8e47ef70cf3")

(package! citeproc-org :pin "20cd7e817420a3f6e7b82faea901a3c67c6d4d9f")

(package! helm-bibtex :pin "ce8c17690ddad73d01531084b282f221f8eb6669")

(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam")
  :pin "5c06471c3a11348342719fd9011486455adeb701")
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex")
  :pin "efdac6fe4134c33f50b06a0a6d192003d0e5094c")
(package! org-roam-ui
  :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out"))
  :pin "5ac74960231db0bf7783c2ba7a19a60f582e91ab")

(package! notmuch :pin "a9b5f8959a20bbce774dec8a65a8b207555e52bd")

(package! helm-notmuch :pin "97a01497e079a7b6505987e9feba6b603bbec288")

(package! systemd :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae")

(package! docker-compose-mode :pin "abaa4f3aeb5c62d7d16e186dd7d77f4e846e126a")

(package! go-eldoc :disable t)

(package! code-review :disable t)
