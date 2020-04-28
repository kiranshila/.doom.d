;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Kiran Shila"
      user-mail-address "me@kiranshila.com")

(setq doom-font (font-spec :family "Fira Code Retina" :size 14))
(setq doom-theme 'doom-dracula)

(setq org-directory "~/org/")
(setq org-roam-directory "~/org/roam")

(after! org
  (setq ob-async-no-async-languages-alist '("jupyter-julia" "jupyter-clojure" "jupyter-python")))
(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(setq display-line-numbers-type t)
(setq projectile-project-search-path `("~/Projects" "~/src"))

(setq-default evil-escape-key-sequence "fd")

(use-package! impatient-mode
  :defer t)

(use-package! evil-cleverparens)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(with-eval-after-load 'cider
  (setq cider-repl-pop-to-buffer-on-connect t))

;; Fix the smartparen backspace strict mode stuff
(map! :mode clojure-mode (:i "<backspace>" #'backward-delete-char-untabify))
;; Add some hooks to the clojure mode
(add-hook! clojure-mode #'(smartparens-strict-mode
                           evil-cleverparens-mode))

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
 :ni "H-j" #'sp-join-sexp
 :ni "H-|" #'sp-split-sexp)

 (map!
  ;; Comma for shortcut to local-leader
  :n "," (lambda! (push (cons t ?m) unread-command-events)
                  (push (cons t 32) unread-command-events))
  (:leader
    (:prefix "b"
      :desc "Previous buffer (Spacemacs)" :n "p" #'previous-buffer
      :desc "Next buffer (Spacemacs)" :n "n" #'next-buffer
      :desc "Buffer delete" :n "e" #'next-buffer
      :desc "Switch buffer (Spacemacs)" :n "b" #'switch-to-buffer)
    (:prefix "w"
      :desc "Vertical split (Spacemacs)" :n "/" #'evil-window-vsplit
      :desc "Horizontal split (Spacemacs)" :n "-" #'evil-window-split
      :desc "New frame (Spacemacs)" :n "F" #'make-frame
      :desc "Next frame (Spacemacs)" :n "o" #'other-frame
      :desc "Delete window (Spacemacs)" :n "d" #'evil-quit)))
