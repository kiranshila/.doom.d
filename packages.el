;; -*- no-byte-compile: t; -*-

(package! vlf :recipe (:host github :repo "m00natic/vlfi" :files ("*.el"))
  :disable t)

(package! evil-cleverparens)
(package! evil-smartparens)

(package! ox-ipynb :recipe (:type git :host github :repo "jkitchin/ox-ipynb"))

(package! org-super-agenda)
(package! doct :recipe (:host github :repo "progfolio/doct"))

(package! org-pretty-table
  :recipe (:host github :repo "Fuco1/org-pretty-table"))

(package! org-fragtog)

(package! org-appear :recipe (:host github :repo "awth13/org-appear"))

(package! org-pretty-tags)

(package! org-ref)

(package! org-roam-bibtex)

(package! systemd)

(unpin! clojure-mode)

(package! impatient-mode)
