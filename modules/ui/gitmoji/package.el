;; -*- no-byte-compile: t; -*-
;;; ui/emoji/packages.el

(when (featurep! +gitmoji)
  (package! insert-gitmoji
    :recipe `(:host github
              :repo "ingtshan/insert-gitmoji.el"))
  (package! insert-angular
  :recipe `(:host github
            :repo "ingtshan/insert-angular.el")))
