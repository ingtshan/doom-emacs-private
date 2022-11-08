;;; ~/.config/doom/packages.el

;; (package! doom-snippets
;;   :recipe (:local-repo "~/projects/conf/doom-snippets"
;;            :files ("*.el" "snippets")
;;            :build (:not compile)))

;; (package! doom-themer
;;   :recipe (:local-repo "~/projects/plugins/emacs-doom-themer/"))
;; (package! doom-themes
;;   :recipe (:local-repo "~/projects/plugins/emacs-doom-themes/"
;;            :files ("*.el" "themes/*.el" "extensions/*.el")))

(package! string-inflection)
(package! kubernetes)
(package! kubernetes-evil)
(package! multi-line)
(package! ruby-refactor)
(package! command-log-mode)
(package! google-translate)
(package! ruby-hash-syntax)
(package! f)
(package! js-import)
(package! lsp-tailwindcss)
(package! crdt)
(package! exotica-theme)
(package! org-reveal)
(package! noccur)
(package! orgit)
(package! sis)
(package! super-save)
(package! ebuku
  :recipe (:host github
           :repo "flexibeast/ebuku"))
;;(package! dirvish)
