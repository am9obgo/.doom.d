;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       (layout)            ; auie,ctsrnm is the superior home row

       :completion
       (company)           ; the ultimate code completion backend
       (helm)              ; the *other* search engine for love and life
       (ivy)               ; a search engine for love and life
       ;;ido             ; the other *other* search engine...

       :ui
       (doom)              ; what makes DOOM look the way it does
       (doom-dashboard)    ; a nifty splash screen for Emacs
       ;;(fill-column)       ; a `fill-column' indicator
       (hl-todo)           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;;(ligatures)         ; ligatures and symbols to make your code pretty again
       (minimap)           ; show a map of the code on the side
       (modeline)          ; snazzy, Atom-inspired modeline, plus API
       ;;(neotree)           ; a project drawer, like NERDTree for vim
       (ophints)           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       (vc-gutter)         ; vcs diff in the fringe
       (vi-tilde-fringe)   ; fringe tildes to mark beyond EOB
       (window-select)     ; visually switch windows
       ;;(workspaces)        ; tab emulation, persistence & separate workspaces
       ;;doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;;indent-guides     ; highlighted indent columns
       ;;(emoji +unicode)  ; 🙂
       ;;deft              ; notational velocity for Emacs
       ;;hydra
       ;;nav-flash         ; blink cursor line after big motions
       ;;tabs              ; a tab bar for Emacs
       (treemacs +lsp +treemacs-git-mode)     ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       ;;zen               ; distraction-free coding or writing

       :editor
       (file-templates)    ; auto-snippets for empty files
       (fold)              ; (nigh) universal code folding
       (format +onsave)    ; automated prettiness
       (snippets)          ; my elves. They type so I don't have to
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       ;;multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       ;;rotate-text       ; cycle region at point between text candidates
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired +icons)      ; making dired pretty [functional]
       (electric)          ; smarter, keyword-based electric-indent
       (ibuffer +icons)    ; interactive buffer management
       (undo +tree)        ; persistent, smarter undo for your inevitable mistakes
       ;;vc                ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       vterm             ; the best terminal emulation in Emacs

       :checkers
       (syntax)            ; tasing you for every semicolon you forget
       ;;(spell +aspell)     ; tasing you for misspelling mispelling
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       (eval +overlay)     ; run code, run (also, repls)
       (debugger +lsp)     ; stepping through code, to help you add bugs
       (docker +lsp)       ; manipulate Docker images, containers & more
       (lookup)            ; navigate your code and its documentation
       (lsp)               ; Client for Language Server Protocol
       (magit)             ; a git porcelain for Emacs
       ;;ansible
       ;;direnv
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       ;;gist              ; interacting with github gists
       ;;make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       ;;pdf               ; pdf enhancements
       ;;prodigy           ; managing external services & code builders
       ;;rgb               ; creating color strings
       ;;taskrunner        ; taskrunner for all your projects
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       (tty)               ; improve the terminal Emacs experience

       :lang
       (cc +lsp)           ; C/C++/Obj-C madness
       (data)              ; config/data formats
       (emacs-lisp +lsp)   ; extended support for Emacs Lisp
       (javascript +lsp)   ; all(hope(abandon(ye(who(enter(here))))))
       (json +lsp)         ; At least it ain't XML
       (markdown)          ; writing docs for people to ignore
       ;;(org)               ; organize your plain life in plain text
       (python +lsp +pyenv)  ; beautiful is better than ugly
       (sh)                ; she sells {ba,z,fi}sh shells on the C xor
       (web +lsp)          ; the tubes
       (yaml +lsp)         ; JSON, but readable
       ;;(dart +flutter +lsp); paint ui and not much else
       ;;(go +lsp)         ; the hipster dialect
       ;;(haskell +dante)  ; a language that's lazier than I am
       ;;(hy)              ; readability of scheme w/ speed of python
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;agda              ; types of types of types of types...
       ;;clojure           ; java with a lisp
       (common-lisp)       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       ;;idris             ; a language you can depend on
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;lean
       ;;ledger            ; an accounting system in Emacs
       ;;lua               ; one-based indices? one-based indices
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       ;;rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       (rust +lsp)         ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;scheme            ; a fully conniving family of lisps
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       ;;literate
       (default +bindings +smartparens))
