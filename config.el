;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Kiran Shila"
      user-mail-address "me@kiranshila.com")

(setq! eglot-connect-timeout 60)

;; Julia setup
(after! julia-repl
  (julia-repl-set-terminal-backend 'vterm))
  ;(setq vterm-kill-buffer-on-exit nil)
  (setq eglot-jl-language-server-project "~/.julia/environments/v1.6")

(setenv "SSH_AUTH_SOCK" (string-trim (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket")))

(setq doom-font (font-spec :family "Fira Code Retina")
      doom-unicode-font (font-spec :family "JuliaMono Medium"))

(setq doom-theme 'doom-dracula)
(setq doom-treemacs-use-generic-icons nil)

                                        ; Python tools
(setenv "PATH" (concat (getenv "PATH") "~/.pyenv/libexec"))
(setq exec-path (append exec-path '("~/.pyenv/libexec")))
(setenv "PATH" (concat (getenv "PATH") "~/.poetry/bin"))
(setq exec-path (append exec-path '("~/.poetry/bin")))

(setq org-directory "~/org/")
(setq org-roam-directory "~/org/roam")

(remove-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'message-mode-hook #'word-wrap-mode)

(eval-after-load "preview"
  '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

(setq-default
 reftex-default-bibliography '("~/Dropbox/Bibliographies/main.bib")
 org-ref-default-bibliography '("~/Dropbox/Bibliographies/main.bib")
 bibtex-completion-bibliography "~/Dropbox/Bibliographies/main.bib"
 bibtex-completion-library-path "~/Dropbox/Bibliographies/"
 org-ref-pdf-directory "~/Dropbox/Bibliographies/")

(after! org
  (setq ob-async-no-async-languages-alist '("jupyter-julia" "jupyter-clojure" "jupyter-python"))
  (setq org-latex-compiler "xelatex")
  (setq org-latex-pdf-process '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-listings 'minted)
  (require 'org-roam-protocol)
  (require 'org-ref)
  (require 'org-ref-ivy-cite)
  (require 'org-roam-bibtex)
  (require 'ox-ipynb)
  (org-roam-bibtex-mode 1))

(after! org-capture
  (add-to-list 'org-capture-templates
               (list "b" "Start a new blog post" 'plain '(file blog-post-name)
                     (concat "#+TITLE: %(symbol-value 'blog-title)\n"
                             "#+AUTHOR: %n\n"
                             "#+DATE: %<%Y-%m-%d>\n"
                             "#+HUGO_TAGS: %^{Tags}\n"
                             "#+HUGO_BASE_DIR: ~/Projects/Clojure/blarg/resources\n"
                             "#+HUGO_FRONT_MATTER_FORMAT: yaml\n"
                             "#+HUGO_AUTO_SET_LASTMOD: t\n"
                             "#+HUGO_DRAFT: t\n"
                             "#+HUGO_CUSTOM_FRONT_MATTER: :featured_image \"\"\n"
                             "#+STARTUP: inlineimages\n"
                             "#+PROPERTY: header-args:latex :exports code\n\n%?"))))

(defun blog-post-name ()
  (setq blog-title (read-string "Title: "))
  (expand-file-name (format "%s.org" (replace-regexp-in-string "\s+" "-" (string-trim (downcase blog-title)))) "~/org/blog"))

(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(setq display-line-numbers-type t)
(setq projectile-project-search-path `("~/Projects" "~/src"))

(setq-default evil-escape-key-sequence "jk")

(use-package! impatient-mode
  :defer t)

(setq orb-preformat-keywords
      '(("citekey" . "=key=") "title" "url" "file" "author-or-editor" "keywords"))

(setq orb-templates
      '(("r" "ref" plain (function org-roam-capture--get-point)
         ""
         :file-name "${citekey}"
         :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}

- tags ::

* ${title}
:PROPERTIES:
:Custom_ID: ${citekey}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
:NOTER_PAGE:
:END:")))

(use-package! evil-cleverparens)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(with-eval-after-load 'cider
  (setq cider-repl-pop-to-buffer-on-connect t))

;; Add some hooks to the clojure mode
(add-hook! clojure-mode #'(smartparens-strict-mode
                           evil-cleverparens-mode
                           evil-smartparens-mode))

;; Define my "Hyper" key
(setq ns-right-option-modifier 'hyper)

(map!
 :ni "H-s" #'sp-forward-slurp-sexp
 :ni "H-S" #'sp-backward-slurp-sexp
 :ni "H-b" #'sp-forward-barf-sexp
 :ni "H-B" #'sp-backward-barf-sexp
 :ni "H-c" #'sp-convolute-sexp
 :ni "H-t" #'sp-transpose-sexp
 :ni "H-r" #'sp-raise-sexp
 :ni "H-w" #'sp-wrap-round
 :ni "H-u" #'sp-unwrap-sexp
 :ni "H-[" #'sp-wrap-square
 :ni "H-{" #'sp-wrap-curly
 :ni "H-j" #'sp-join-sexp
 :ni "H-|" #'sp-split-sexp)

(map!
 ;; Comma for shortcut to local-leader
 :n "," (cmd! (push (cons t ?m) unread-command-events)
              (push (cons t 32) unread-command-events))
 (:leader
  (:prefix "b"
   :desc "Previous buffer (Spacemacs)" :n "p" #'previous-buffer
   :desc "Next buffer (Spacemacs)" :n "n" #'next-buffer
   :desc "Switch buffer (Spacemacs)" :n "b" #'switch-to-buffer)
  (:prefix "w"
   :desc "Vertical split (Spacemacs)" :n "/" #'evil-window-vsplit
   :desc "Horizontal split (Spacemacs)" :n "-" #'evil-window-split
   :desc "New frame (Spacemacs)" :n "F" #'make-frame
   :desc "Next frame (Spacemacs)" :n "o" #'other-frame
   :desc "Delete window (Spacemacs)" :n "d" #'evil-quit)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(citeproc-org))
 '(safe-local-variable-values
   '((cider-figwheel-main-default-options . ":dev")
     (cider-default-cljs-repl . figwheel-main)
     (cider-clojure-cli-global-options . "-A:fig"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
