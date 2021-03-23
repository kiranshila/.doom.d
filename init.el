;;; init.el -*- lexical-binding: t; -*-
(doom! :completion
       (company                     ; the ultimate code completion backen
        +tng)                       ; For not-insane tab completion
       (ivy                         ; a search engine for love and life
        +childframe                 ; Because a popup is better
        +icons                      ; ... icons are nice
        +prescient)                 ; ... I know what I want(ed)
       :ui
       doom                         ; what makes DOOM look the way it does
       doom-dashboard               ; a nifty splash screen for Emacs
       doom-quit                    ; DOOM quit-message prompts when you quit Emacs
       (emoji +unicode)             ; 🙂
       hl-todo                      ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       modeline                     ; snazzy, Atom-inspired modeline, plus API
       nav-flash                    ; blink the current line after jumping
       ophints                      ; highlight the region an operation acts on
       (popup                       ; tame sudden yet inevitable temporary windows
        +all                        ; catch all popups that start with an asterix
        +defaults)                  ; default popup rules
       treemacs                     ; a project drawer, like neotree but cooler
       vc-gutter                    ; vcs diff in the fringe
       vi-tilde-fringe              ; fringe tildes to mark beyond EOB
       (window-select +numbers)     ; visually switch windows
       workspaces                   ; tab emulation, persistence & separate workspaces
       (ligatures +extra)           ; ligatures and symbols to make your code pretty again
       :editor
       (evil +everywhere)           ; come to the dark side, we have cookies
       file-templates               ; auto-snippets for empty files
       fold                         ; (nigh) universal code folding
       (format +onsave)             ; automated prettiness
       multiple-cursors             ; editing in many places at once
       rotate-text                  ; cycle region at point between text candidates
       snippets                     ; my elves. They type so I don't have to
       word-wrap                    ; soft wrapping with language-aware indent
       :emacs
       (dired +icons)               ; making dired pretty [functional]
       electric                     ; smarter, keyword-based electric-indent
       (ibuffer +icons)             ; interactive buffer management
       (undo +tree)                 ; persistent, smarter undo for your inevitable mistakes
       vc                           ; version-control and Emacs, sitting in a tree
       :term
       vterm                        ; the best terminal emulation in Emacs
       :checkers
       syntax                       ; tasing you for every semicolon you forget
       (:if
        (executable-find "aspell")
        spell)                      ; tasing you for misspelling mispelling
       grammar                      ; tasing grammar mistake every you make
       :tools
       debugger                     ; FIXME stepping through code, to help you add bugs
       (eval +overlay)              ; run code, run (also, repls)
       (lookup                      ; helps you navigate your code and documentation
        +dictionary                 ; dictionary/thesaurus is nice
        +docsets)                   ; ...or in Dash docsets locally
       (lsp                         ; Language Server Protocol
        +elgot)                     ; Because LSP mode is lame
       docker                       ; Contain yourself
       magit                        ; a git porcelain for Emacs
       make                         ; run make tasks from Emacs
       pass                         ; password manager for nerds
       pdf                          ; pdf enhancements
       rgb                          ; creating color strings
       upload                       ; map local to remote projects via ssh/ftp
       :os
       tty                          ; improve the terminal Emacs experience
       :lang
       cc                           ; C/C++/Obj-C madness
       clojure                      ; java with a lisp
       common-lisp                  ; if you've seen one lisp, you've seen them all
       data                         ; config/data formats
       emacs-lisp                   ; drown in parentheses
       (haskell +lsp)               ; a language that's lazier than I am
       json                         ; At least it ain't XML
       (javascript +lsp)            ; all(hope(abandon(ye(who(enter(here))))))
       (julia +lsp)                 ; a better, faster MATLAB
       (latex                       ; writing papers in Emacs has never been so fun
        +latexmk                    ; what else would you use?
        +cdlatex                    ; quick maths symbols
        +lsp                        ; Is serves a purpose
        +fold)                      ; fold the clutter away nicities
       markdown                     ; writing docs for people to ignore
       (org                         ; organize your plain life in plain text
        +pretty                     ; yessss my pretties! (nice unicode symbols)
        +dragndrop                  ; drag & drop files/images into org buffers
        +hugo                       ; use Emacs for hugo blogging
        +noter                      ; enhanced PDF notetaking
        +jupyter                    ; ipython/jupyter support for babel
        +pandoc                     ; export-with-pandoc support
        +gnuplot                    ; who doesn't like pretty pictures
        +present                    ; using org-mode for presentations
        +roam)                      ; wander around notes
       (python                      ; beautiful is better than ugly
        +lsp                        ; Useless otherwise
        +pyright                    ; Because M$ makes good stuff sometimes
        +pyenv                      ; Unfucks python versioning
        +poetry)                    ; Unfucks python projects
       rest                         ; Emacs as a REST client
       (rust +lsp)                  ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       sh                           ; she sells {ba,z,fi}sh shells on the C xor
       web                          ; the tubes
       yaml                         ; JSON, but readable
       :app
       irc                          ; how neckbeards socialize
       (rss +org)                   ; emacs as an RSS reader
       :config
       literate
       (default +bindings +smartparens)
       )
(define-advice define-obsolete-function-alias (:filter-args (ll) fix-obsolete)
  (let ((obsolete-name (pop ll))
        (current-name (pop ll))
        (when (if ll (pop ll) "1"))
        (docstring (if ll (pop ll) nil)))
    (list obsolete-name current-name when docstring)))
