;;; config.el -*- lexical-binding: t; -*-

;; [[file:config.org::*Personal Information][Personal Information:1]]
(setq user-full-name "Kiran Shila"
      user-mail-address "me@kiranshila.com")
;; Personal Information:1 ends here

;; [[file:config.org::*SSH Agent][SSH Agent:1]]
(setenv "SSH_AUTH_SOCK" (string-trim (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket")))
;; SSH Agent:1 ends here

;; [[file:config.org::*Simple Settings][Simple Settings:1]]
(setq-default window-combination-resize t  ; New window space comes from all window
              x-stretch-cursor t)          ; Cursor size to match glyph width
(setq undo-limit 80000000                  ; Undo limit to 80Mb
      evil-want-fine-undo t                ; More granular evil-undo
      auto-save-default t)                 ; Always auto-save
;; Simple Settings:1 ends here

;; [[file:config.org::*Windows][Windows:1]]
(setq evil-vsplit-windows-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))
(setq +ivy-buffer-preview t)
;; Windows:1 ends here

;; [[file:config.org::*Windows][Windows:2]]
(map!
 :leader
 (:prefix "b"
  :desc "Previous buffer" :n "p" #'previous-buffer
  :desc "Next buffer" :n "n" #'next-buffer
  :desc "Switch buffer" :n "b" #'switch-to-buffer)
 (:prefix "w"
  :desc "Vertical split" :n "/" #'evil-window-vsplit
  :desc "Horizontal split" :n "-" #'evil-window-split
  :desc "New frame" :n "F" #'make-frame
  :desc "Next frame" :n "o" #'other-frame
  :desc "Delete window" :n "d" #'evil-quit))
;; Windows:2 ends here

;; [[file:config.org::*Windows][Windows:3]]
(setq frame-title-format
      '(""
        (:eval
         (if (s-contains-p org-roam-directory (or buffer-file-name ""))
             (replace-regexp-in-string
              ".*/[0-9]*-?" "☰ "
              (subst-char-in-string ?_ ?  buffer-file-name))
           "%b"))
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format (if (buffer-modified-p)  " ◉ %s" "  ●  %s") project-name))))))
;; Windows:3 ends here

;; [[file:config.org::*Local Leader][Local Leader:1]]
(map! :n "," (cmd! (push (cons t ?m) unread-command-events)
                   (push (cons t 32) unread-command-events)))
;; Local Leader:1 ends here

;; [[file:config.org::*EVIL][EVIL:1]]
(setq-default evil-escape-key-sequence "jk")
;; EVIL:1 ends here

;; [[file:config.org::*Smartparen Madness][Smartparen Madness:1]]
(setq ns-right-option-modifier 'hyper) ; This is for my mac, I xmodmap caps lock on my desktop
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
;; Smartparen Madness:1 ends here

;; [[file:config.org::*Font][Font:1]]
(setq doom-font (font-spec :family "Fira Code Retina" :size 14)
      doom-unicode-font (font-spec :family "JuliaMono"))
;; Font:1 ends here

;; [[file:config.org::*Theme][Theme:1]]
(setq doom-theme 'doom-dracula)
;; Theme:1 ends here

;; [[file:config.org::*Misc][Misc:1]]
(setq display-line-numbers-type 'relative)
;; Misc:1 ends here

;; [[file:config.org::*Misc][Misc:2]]
(setq doom-fallback-buffer-name "► Doom"
      +doom-dashboard-name "► Doom")
;; Misc:2 ends here

(use-package! vlf-setup
  :defer-incrementally vlf-tune vlf-base vlf-write vlf-search vlf-occur vlf-follow vlf-ediff vlf)

(use-package! org-pretty-table
  :commands (org-pretty-table-mode global-org-pretty-table-mode))

(use-package org-roam-server
  :after (org-roam server)
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8078
        org-roam-server-export-inline-images t
        org-roam-server-authenticate nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20)
  (defun org-roam-server-open ()
    "Ensure the server is active, then open the roam graph."
    (interactive)
    (org-roam-server-mode 1)
    (browse-url-xdg-open (format "http://localhost:%d" org-roam-server-port))))

(use-package! impatient-mode
  :defer t)

;; [[file:config.org::*Calc][Calc:1]]
(setq calc-angle-mode 'rad  ; radians are rad
      calc-symbolic-mode t) ; keeps expressions like \sqrt{2} irrational for as long as possible
;; Calc:1 ends here

;; [[file:config.org::*Emojis][Emojis:1]]
(setq emojify-emoji-set "twemoji-v2")
;; Emojis:1 ends here

;; [[file:config.org::*Emojis][Emojis:2]]
(defvar emojify-disabled-emojis
  '(;; Org
    "◼" "☑" "☸" "⚙" "⏩" "⏪" "⬆" "⬇" "❓"
    ;; Terminal powerline
    "✔"
    ;; Box drawing
    "▶" "◀")
  "Charachters that should never be affected by `emojify-mode'.")

(defadvice! emojify-delete-from-data ()
  "Ensure `emojify-disabled-emojis' don't appear in `emojify-emojis'."
  :after #'emojify-set-emoji-data
  (dolist (emoji emojify-disabled-emojis)
    (remhash emoji emojify-emojis)))
;; Emojis:2 ends here

;; [[file:config.org::*Treemacs][Treemacs:1]]
(after! treemacs
  (defvar treemacs-file-ignore-extensions '()
    "File extension which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-globs '()
    "Globs which will are transformed to `treemacs-file-ignore-regexps' which `treemacs-ignore-filter' will ensure are ignored")
  (defvar treemacs-file-ignore-regexps '()
    "RegExps to be tested to ignore files, generated from `treeemacs-file-ignore-globs'")
  (defun treemacs-file-ignore-generate-regexps ()
    "Generate `treemacs-file-ignore-regexps' from `treemacs-file-ignore-globs'"
    (setq treemacs-file-ignore-regexps (mapcar 'dired-glob-regexp treemacs-file-ignore-globs)))
  (if (equal treemacs-file-ignore-globs '()) nil (treemacs-file-ignore-generate-regexps))
  (defun treemacs-ignore-filter (file full-path)
    "Ignore files specified by `treemacs-file-ignore-extensions', and `treemacs-file-ignore-regexps'"
    (or (member (file-name-extension file) treemacs-file-ignore-extensions)
        (let ((ignore-file nil))
          (dolist (regexp treemacs-file-ignore-regexps ignore-file)
            (setq ignore-file (or ignore-file (if (string-match-p regexp full-path) t nil)))))))
  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-filter))
;; Treemacs:1 ends here

;; [[file:config.org::*Treemacs][Treemacs:2]]
(setq treemacs-file-ignore-extensions
      '(;; LaTeX
        "aux"
        "ptc"
        "fdb_latexmk"
        "fls"
        "synctex.gz"
        "toc"
        ;; LaTeX - glossary
        "glg"
        "glo"
        "gls"
        "glsdefs"
        "ist"
        "acn"
        "acr"
        "alg"
        ;; LaTeX - pgfplots
        "mw"
        ;; LaTeX - pdfx
        "pdfa.xmpi"
        ))
(setq treemacs-file-ignore-globs
      '(;; LaTeX
        "*/_minted-*"
        ;; AucTeX
        "*/.auctex-auto"
        "*/_region_.log"
        "*/_region_.tex"))
;; Treemacs:2 ends here

;; [[file:config.org::*Projectile][Projectile:1]]
(setq projectile-project-search-path `("~/Projects" "~/src"))
;; Projectile:1 ends here

;; [[file:config.org::*Reference Management][Reference Management:1]]
(setq-default
 reftex-default-bibliography '("~/Dropbox/Bibliographies/main.bib")
 org-ref-default-bibliography '("~/Dropbox/Bibliographies/main.bib")
 bibtex-completion-bibliography "~/Dropbox/Bibliographies/main.bib"
 bibtex-completion-library-path "~/Dropbox/Bibliographies/"
 org-ref-pdf-directory "~/Dropbox/Bibliographies/")
;; Reference Management:1 ends here

;; [[file:config.org::*Plaintext][Plaintext:1]]
(after! text-mode
  (add-hook! 'text-mode-hook
             ;; Apply ANSI color codes
             (with-silent-modifications
               (ansi-color-apply-on-region (point-min) (point-max)))))
;; Plaintext:1 ends here

(after! org
  (setq org-directory "~/.org"                      ; let's put files here
        org-use-property-inheritance t              ; it's convenient to have properties inherited
        org-log-done 'time                          ; having the time a item is done sounds convininet
        org-list-allow-alphabetical t               ; have a. A. a) A) list bullets
        org-export-in-background t                  ; run export processes in external emacs process
        org-catch-invisible-edits 'smart            ; try not to accidently do weird stuff in invisible regions
        org-export-with-sub-superscripts '{}        ; don't treat lone _ / ^ as sub/superscripts, require _{} / ^{}
        org-re-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (remove-hook 'text-mode-hook #'visual-line-mode)
  (add-hook 'text-mode-hook #'auto-fill-mode)
  (use-package! org-ref
    :after org
    :config
    (setq org-ref-completion-library 'org-ref-ivy-cite))
  (add-hook 'org-mode-hook 'turn-on-org-cdlatex)
  (defadvice! org-edit-latex-emv-after-insert ()
    :after #'org-cdlatex-environment-indent
    (org-edit-latex-environment))
  (add-hook 'org-mode-hook 'turn-on-flyspell)
  (use-package! org-super-agenda
    :commands (org-super-agenda-mode))
  (after! org-agenda
    (org-super-agenda-mode))
  
  (setq org-agenda-skip-scheduled-if-done t
        org-agenda-skip-deadline-if-done t
        org-agenda-include-deadlines t
        org-agenda-block-separator nil
        org-agenda-tags-column 100 ;; from testing this seems to be a good value
        org-agenda-compact-blocks t)
  
  (setq org-agenda-custom-commands
        '(("o" "Overview"
           ((agenda "" ((org-agenda-span 'day)
                        (org-super-agenda-groups
                         '((:name "Today"
                            :time-grid t
                            :date today
                            :todo "TODAY"
                            :scheduled today
                            :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-super-agenda-groups
                          '((:name "Next to do"
                             :todo "NEXT"
                             :order 1)
                            (:name "Important"
                             :tag "Important"
                             :priority "A"
                             :order 6)
                            (:name "Due Today"
                             :deadline today
                             :order 2)
                            (:name "Due Soon"
                             :deadline future
                             :order 8)
                            (:name "Overdue"
                             :deadline past
                             :face error
                             :order 7)
                            (:name "Assignments"
                             :tag "Assignment"
                             :order 10)
                            (:name "Issues"
                             :tag "Issue"
                             :order 12)
                            (:name "Emacs"
                             :tag "Emacs"
                             :order 13)
                            (:name "Projects"
                             :tag "Project"
                             :order 14)
                            (:name "Research"
                             :tag "Research"
                             :order 15)
                            (:name "To read"
                             :tag "Read"
                             :order 30)
                            (:name "Waiting"
                             :todo "WAITING"
                             :order 20)
                            (:name "University"
                             :tag "uni"
                             :order 32)
                            (:name "Trivial"
                             :priority<= "E"
                             :tag ("Trivial" "Unimportant")
                             :todo ("SOMEDAY" )
                             :order 90)
                            (:discard (:tag ("Chore" "Routine" "Daily")))))))))))
  (use-package! doct
    :commands (doct))
  
  (after! org-capture
    (defun +doct-icon-declaration-to-icon (declaration)
      "Convert :icon declaration to icon"
      (let ((name (pop declaration))
            (set  (intern (concat "all-the-icons-" (plist-get declaration :set))))
            (face (intern (concat "all-the-icons-" (plist-get declaration :color))))
            (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
        (apply set `(,name :face ,face :v-adjust ,v-adjust))))
  
    (defun +doct-iconify-capture-templates (groups)
      "Add declaration's :icon to each template group in GROUPS."
      (let ((templates (doct-flatten-lists-in groups)))
        (setq doct-templates (mapcar (lambda (template)
                                       (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                   (spec (plist-get (plist-get props :doct) :icon)))
                                         (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                        "\t"
                                                                        (nth 1 template))))
                                       template)
                                     templates))))
  
    (setq doct-after-conversion-functions '(+doct-iconify-capture-templates))
  
    (defun set-org-capture-templates ()
      (setq org-capture-templates
            (doct `(("Personal todo" :keys "t"
                     :icon ("checklist" :set "octicon" :color "green")
                     :file +org-capture-todo-file
                     :prepend t
                     :headline "Inbox"
                     :type entry
                     :template ("* TODO %?"
                                "%i %a")
                     )
                    ("Personal note" :keys "n"
                     :icon ("sticky-note-o" :set "faicon" :color "green")
                     :file +org-capture-todo-file
                     :prepend t
                     :headline "Inbox"
                     :type entry
                     :template ("* %?"
                                "%i %a")
                     )
                    ("Tasks" :keys "k"
                     :icon ("inbox" :set "octicon" :color "yellow")
                     :file +org-capture-todo-file
                     :prepend t
                     :headline "Tasks"
                     :type entry
                     :template ("* TODO %? %^G%{extra}"
                                "%i %a")
                     :children (("General Task" :keys "k"
                                 :icon ("inbox" :set "octicon" :color "yellow")
                                 :extra ""
                                 )
                                ("Task with deadline" :keys "d"
                                 :icon ("timer" :set "material" :color "orange" :v-adjust -0.1)
                                 :extra "\nDEADLINE: %^{Deadline:}t"
                                 )
                                ("Scheduled Task" :keys "s"
                                 :icon ("calendar" :set "octicon" :color "orange")
                                 :extra "\nSCHEDULED: %^{Start time:}t"
                                 )))))))
  
    (set-org-capture-templates)
    (unless (display-graphic-p)
      (add-hook 'server-after-make-frame-hook
                (defun org-capture-reinitialise-hook ()
                  (when (display-graphic-p)
                    (set-org-capture-templates)
                    (remove-hook 'server-after-make-frame-hook
                                 #'org-capture-reinitialise-hook))))))
  (setq org-roam-directory "~/org/roam")
  (after! org-superstar
    (setq org-superstar-headline-bullets-list '("◉" "○" "✸" "✿" "✤" "✜" "◆" "▶")
          org-superstar-prettify-item-bullets t ))
  
  (setq org-ellipsis " ▾ "
        org-hide-leading-stars t
        org-priority-highest ?A
        org-priority-lowest ?E
        org-priority-faces
        '((?A . 'all-the-icons-red)
          (?B . 'all-the-icons-orange)
          (?C . 'all-the-icons-yellow)
          (?D . 'all-the-icons-green)
          (?E . 'all-the-icons-blue)))
  (appendq! +ligatures-extra-symbols
            `(:checkbox      "☐"
              :pending       "◼"
              :checkedbox    "☑"
              :list_property "∷"
              :em_dash       "—"
              :ellipses      "…"
              :arrow_right   "→"
              :arrow_left    "←"
              :title         "𝙏"
              :subtitle      "𝙩"
              :author        "𝘼"
              :date          "𝘿"
              :property      "☸"
              :options       "⌥"
              :latex_class   "🄲"
              :latex_header  "⇥"
              :beamer_header "↠"
              :attr_latex    "🄛"
              :attr_html     "🄗"
              :begin_quote   "❝"
              :end_quote     "❞"
              :caption       "☰"
              :header        "›"
              :results       "🠶"
              :begin_export  "⏩"
              :end_export    "⏪"
              :properties    "⚙"
              :end           "∎"
              :priority_a   ,(propertize "⚑" 'face 'all-the-icons-red)
              :priority_b   ,(propertize "⬆" 'face 'all-the-icons-orange)
              :priority_c   ,(propertize "■" 'face 'all-the-icons-yellow)
              :priority_d   ,(propertize "⬇" 'face 'all-the-icons-green)
              :priority_e   ,(propertize "❓" 'face 'all-the-icons-blue)))
  (set-ligatures! 'org-mode
    :merge t
    :checkbox      "[ ]"
    :pending       "[-]"
    :checkedbox    "[X]"
    :list_property "::"
    :em_dash       "---"
    :ellipsis      "..."
    :arrow_right   "->"
    :arrow_left    "<-"
    :title         "#+title:"
    :subtitle      "#+subtitle:"
    :author        "#+author:"
    :date          "#+date:"
    :property      "#+property:"
    :options       "#+options:"
    :latex_class   "#+latex_class:"
    :latex_header  "#+latex_header:"
    :beamer_header "#+beamer_header:"
    :attr_latex    "#+attr_latex:"
    :attr_html     "#+attr_latex:"
    :begin_quote   "#+begin_quote"
    :end_quote     "#+end_quote"
    :caption       "#+caption:"
    :header        "#+header:"
    :begin_export  "#+begin_export"
    :end_export    "#+end_export"
    :results       "#+RESULTS:"
    :property      ":PROPERTIES:"
    :end           ":END:"
    :priority_a    "[#A]"
    :priority_b    "[#B]"
    :priority_c    "[#C]"
    :priority_d    "[#D]"
    :priority_e    "[#E]")
  (plist-put +ligatures-extra-symbols :name "⁍")
  (setq org-highlight-latex-and-related '(native script entities))
  (add-to-list 'org-src-block-faces '("latex" (:inherit default :extend t)))
  (use-package! org-fragtog
    :hook (org-mode . org-fragtog-mode))
  (setq org-format-latex-header "\\documentclass{article}
  \\usepackage[usenames]{color}
  
  \\usepackage[T1]{fontenc}
  
  \\usepackage{booktabs}
  
  \\pagestyle{empty}             % do not remove
  % The settings below are copied from fullpage.sty
  \\setlength{\\textwidth}{\\paperwidth}
  \\addtolength{\\textwidth}{-3cm}
  \\setlength{\\oddsidemargin}{1.5cm}
  \\addtolength{\\oddsidemargin}{-2.54cm}
  \\setlength{\\evensidemargin}{\\oddsidemargin}
  \\setlength{\\textheight}{\\paperheight}
  \\addtolength{\\textheight}{-\\headheight}
  \\addtolength{\\textheight}{-\\headsep}
  \\addtolength{\\textheight}{-\\footskip}
  \\addtolength{\\textheight}{-3cm}
  \\setlength{\\topmargin}{1.5cm}
  \\addtolength{\\topmargin}{-2.54cm}
  % my custom stuff
  \\usepackage[nofont,plaindd]{bmc-maths}
  \\usepackage{arev}
  ")
  (setq org-format-latex-options
        (plist-put org-format-latex-options :background "Transparent"))
  (setq org-latex-pdf-process '("latexmk -%latex -shell-escape -interaction=nonstopmode -f -pdf -output-directory=%o %f"))
  (setq org-latex-listings 'minted)
  (setq ob-async-no-async-languages-alist '("jupyter-julia" "jupyter-clojure" "jupyter-python"))
)

;; [[file:config.org::*LSP Languages][LSP Languages:1]]
(setq! eglot-connect-timeout 60)
;; LSP Languages:1 ends here

;; [[file:config.org::*Julia][Julia:1]]
(after! julia-repl
  (julia-repl-set-terminal-backend 'vterm))
(after! julia-mode
  (setq eglot-jl-language-server-project "~/.julia/environments/v1.6"))
;; Julia:1 ends here

;; [[file:config.org::*Clojure][Clojure:1]]
(with-eval-after-load 'cider
  (setq cider-repl-pop-to-buffer-on-connect t))
;; Clojure:1 ends here

;; [[file:config.org::*Clojure][Clojure:2]]
(add-hook! clojure-mode #'(smartparens-strict-mode
                           evil-cleverparens-mode
                           evil-smartparens-mode))
;; Clojure:2 ends here
