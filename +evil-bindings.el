;;; +evil-bingdings.el -*- lexical-binding: t; -*-

;;; my key bindings

;;; <leader> i --- insert
(when (featurep! :ui gitmoji)
  (map! :leader :desc "CZ angular with gitmoji" :g "ic" #'+gitmoji-angularmoji-insert)
  (map! :leader :desc "Gitmoji" :g "ig" #'gitmoji-insert-emoji))
