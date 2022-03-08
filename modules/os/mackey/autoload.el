;;; os/mackey/autoload.el -*- lexical-binding: t; -*-

(require 'undo-fu)

;;;###autoload
(defun +mackey-do-kill-9 (&optional pfx)
  "quit emacs with confirm"
  ;; (when (or pfx (y-or-n-p "Quit emacs now?"))
  (save-buffers-kill-terminal))

;;;###autoload
(defun +mackey-quite-emacs (&optional pfx)
  "quit emacs with need-test check"
  (interactive "P")
  (save-some-buffers)
  (cond
   (t (+mackey-do-kill-9))))

;;;###autoload
(defun +mackey-close-frame (&optional pfx)
  "close emacs frame"
  (interactive "P")
  (let ((q nil))
    (condition-case ex
	(delete-window) ('error (setq q t)))
    (if q (progn (setq q nil)
		 (condition-case ex
		     (delete-frame) ('error (setq q t)))
		 (if q (+mackey-quite-emacs pfx))))))
