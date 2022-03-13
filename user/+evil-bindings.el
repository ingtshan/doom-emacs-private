;;; +evil-bingdings.el -*- lexical-binding: t; -*-

;;; my key bindings

;;; <leader> i --- insert
(when (featurep! :ui gitmoji)
  (map! :leader :desc "CZ angular with gitmoji" :g "ic" #'+gitmoji-angularmoji-insert)
  (map! :leader :desc "Gitmoji" :g "ig" #'gitmoji-insert-emoji))

;;; ?
(map! (:after evil-org
       :map evil-org-mode-map
       :n "gk" (cmd! (if (org-on-heading-p)
                         (org-backward-element)
                       (evil-previous-visual-line)))
       :n "gj" (cmd! (if (org-on-heading-p)
                         (org-forward-element)
                       (evil-next-visual-line))))

      :o "o" #'evil-inner-symbol

      :leader
      "h L" #'global-keycast-mode
      (:prefix "f"
       "t" #'find-in-dotfiles
       "T" #'browse-dotfiles)
      (:prefix "n"
       "b" #'org-roam-buffer-toggle
       "d" #'org-roam-dailies-goto-today
       "D" #'org-roam-dailies-goto-date
       "e" (cmd! (find-file (doom-dir org-directory "ledger.gpg")))
       "i" #'org-roam-node-insert
       "r" #'org-roam-node-find
       "R" #'org-roam-capture))
;;; <leader> a --- action
(map! :leader "c-" #'indent-whole-buffer)

;;; feature
(map! :v "K" #'drag-stuff-up)
(map! :v "J" #'drag-stuff-down)
(map! :nv "]g" #'git-gutter:next-hunk)
(map! :nv "[g" #'git-gutter:previous-hunk)

;;; <leader> a --- action
(map! :leader
      (:prefix-map ("a" . "action"))
      :n "aa" #'embark-act
      :n "a;" #'embark-dwim)

;;; for ruby
;;; <leader> m
(map!
      :localleader
      :map ruby-mode-map
      "-" 'rubocop-on-current-file)
