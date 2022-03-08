;;; ui/emoji/+gitmoji.el -*- lexical-binding: t; -*-

(setq gitmoji-json-file (concat doom-emacs-dir ".local/straight/repos/insert-gitmoji.el" "/data/gitmojis.json" ))

(defun +gitmoji-angularmoji-insert ()
  (interactive)
  (let* ((type (iangular-type-completing-read "(C-RET to skip) Insert Angular Type : "))
         (scope (completing-read (concat "(C-RET to skip) " type "(<scope 1>,<scope 2>) ") nil))
         (input (concat type (unless (equal scope "") (format "(%s)" scope)) ": "))
         (emoji (gitmoji-completing-read (concat "(C-RET to skip) " input))))
    (insert input (unless (equal emoji "") (concat emoji " ")))))

(map! :leader :desc "CZ angular with gitmoji" :g "ic" #'+gitmoji-angularmoji-insert)
(map! :leader :desc "Gitmoji" :g "ig" #'gitmoji-insert-emoji)
