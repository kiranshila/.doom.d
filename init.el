(doom!
       :completion
       (company
        ;+childframe
        +tng)           ; the ultimate code completion backend
       (ivy
        +childframe
        +icons)               ; a search engine for love and life

       :app
       irc

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       ophints           ; highlight the region an operation acts on
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       treemacs          ; a project drawer, like neotree but cooler
       ;unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       ligatures

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       multiple-cursors                                 ; editing in many places at once
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       undo
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer           ; interactive buffer management
       vc                ; version-control and Emacs, sitting in a tree

       :term
       (vterm
        +toggle)             ; another terminals in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       (spell +flyspell)             ; tasing you for misspelling mispelling
       grammar           ; tasing grammar mistake every you make

       :tools
       (debugger)          ; FIXME stepping through code, to help you add bugs
       (eval
        +overlay)     ; run code, run (also, repls)
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       (lsp
        +eglot)
       magit             ; a git porcelain for Emacs
       make              ; run make tasks from Emacs
       pdf               ; portable?
       pass
       (docker
        +lsp)            ; Contain yourself

       :lang
       common-lisp
       clojure           ; java with a lisp
       data              ; config/data formats
       emacs-lisp         ; drown in parentheses
       (javascript
        +lsp)        ; all(hope(abandon(ye(who(enter(here))))))
       (julia
        +lsp)             ; a better, faster MATLAB
       (latex
        +latexmk
        +cdlatex
        +lsp
        +fold)             ; writing papers in Emacs has never been so fun
       markdown          ; writing docs for people to ignore
       (org              ; organize your plain life in plain text
        +dragndrop       ; drag & drop files/images into org buffers
        +jupyter        ; ipython/jupyter support for babel
        +pandoc          ; export-with-pandoc support
        +present
        +roam
        +hugo)        ; using org-mode for presentations
       (python
        +lsp
        +pyright
        +pyenv
        +poetry)            ; beautiful is better than ugly
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       web               ; the tubes
       rest
       (rust
        +lsp)

       :config
       (default +bindings))

;; Fix broken emacs
(define-advice define-obsolete-function-alias (:filter-args (ll) fix-obsolete)
  (let ((obsolete-name (pop ll))
        (current-name (pop ll))
        (when (if ll (pop ll) "1"))
        (docstring (if ll (pop ll) nil)))
    (list obsolete-name current-name when docstring)))
