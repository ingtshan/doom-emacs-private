;;; ~/.doom.d/init.el -*- lexical-binding: t; -*-

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
(doom! :completion
       ;; (company +childframe)
       ;;ivy
       ;;helm
       ;;ido
       (vertico +icons +posframe)
       ;; corfu

       :ui
       ;;deft
       doom
       doom-dashboard
       doom-quit
       (emoji +unicode)
       gitmoji
       ;;fill-column
       hl-todo
       ;;hydra
       ;;indent-guides     ; highlighted indent columns
       ;;ligatures
       ;;minimap
       modeline
       nav-flash
       ;;neotree
       ophints
       (popup +defaults)
       ;;treemacs
       ;;tree-sitter
       ;;unicode
       ;;tabs
       vc-gutter
       window-select
       workspaces
       zen
       vi-tilde-fringe
       workspaces

       :input
       ;;chinese
       ;;japanese

       :editor
       (evil +everywhere)
       file-templates
       fold              ; (nigh) universal code folding
       ;;objed
       format            ; automated prettiness
       ;;lispy             ; vim for lisp, for people who dont like vim
       multiple-cursors  ; editing in many places at once
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets
       word-wrap

       :emacs
       (dired +icons)             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ;;ibuffer           ; interactive buffer management
       undo
       vc

       :term
       eshell            ; a consistent, cross-platform shell (WIP)
       ;;shell
       ;;term              ; terminals in Emacs
       vterm

       :checkers
       syntax
       (spell +flyspell +aspell)
       grammar

       :tools
       ;;ansible
       ;; (debugger +lsp)
       debugger
       direnv
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)
       ;;gist
       (lookup +docsets +dictionary)
       ;; (lsp +eglot )
       ;; (lsp +peek )
       lsp-bridge
       ;;macos             ; MacOS-specific commands
       (magit +forge)             ;
       ;;make              ; run make tasks from Emacs
       ;;pass                ; password manager for nerds
       pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       rgb               ; creating color strings
       taskrunner
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp
       i18n
       ;; pim                 ; personal information management

       :os
       ;;arch
       (:if IS-MAC macos)
       (:if IS-MAC mackey)
       ;;nixos
       ;;tty               ; enable terminal integration

       :lang
       ;;agda
       ;;assembly
       beancount
       ;; (cc +lsp)
       cc
       ;;crystal
       ;;clojure
       ;; (csharp +lsp +dotnet)            ; unity, .NET, and mono shenanigans
       common-lisp
       ;;coq
       ;;data
       ;;dart
       ;;erlang
       ;;elixir
       ;;elm
       emacs-lisp
       ;;ess
       ;;faust
       ;;fortran
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;go
       ;;haskell
       ;;hy
       ;; (json +lsp)              ; At least it ain't XML
       json
       (java +meghanada)
       ;; (javascript +lsp)
       (javascript)
       ;;julia
       ;;latex
       ;;ledger
       (lua +fennel)
       markdown
       ;;nim
       ;;nix
       ;;ocaml
       ;; (org +dragndrop +roam2 +present)
       (org +dragndrop +present +journal +hugo)
       ;;perl
       ;;php
       ;;plantuml
       ;;purescript
       ;; (python +lsp)
       (python)
       ;;qt
       ;;racket
       rest
       ;; (ruby +rails +rbenv +lsp)
       (ruby +rails +rbenv)
       ;; (rust +lsp)
       (rust)
       ;;scala
       ;;(scheme +guile)
       sh
       ;;sml
       solidity
       ;;swift
       ;; (web +lsp)
       web
       ;; (yaml +lsp)
       yaml

       :email
       ;;(mu4e +gmail)       ; WIP
       ;;notmuch             ; WIP
       ;;(wanderlust +gmail) ; WIP

       :app
       ;;calendar
       everywhere
       ;;irc
       (rss +org)
       ereader

       :config
       ;;literate
       (default +bindings +smartparens)

       :private
       ;; journal
       playground
       denote
       ;; (lsp-bridge +acm)

       ) ;; end of doom!
