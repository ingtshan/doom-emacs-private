;;; config.el --- i18n                               -*- lexical-binding: t; -*-

;; Copyright (C) 2022  ingtshan

;; Author: ingtshan <rongcanyo@gmail.com>
;; Keywords: i18n

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; my tool

;;; Code:

;; (add-to-list 'load-path "~/.emacs.d/.local/straight/repos/lsp-bridge")
;; (require 'lsp-bridge)             ;; load lsp-bridge
;; (require 'lsp-bridge-orderless)   ;; make lsp-bridge support fuzzy match, optional
;; (require 'lsp-bridge-icon)        ;; show icon for completion items, optional

;; (global-corfu-mode)               ;; use corfu as completion ui
;; (global-lsp-bridge-mode)

;;; Require
(after! company
  (add-to-list 'load-path "~/.emacs.d/.local/straight/repos/lsp-bridge")
  ;; (add-to-list 'load-path "~/.emacs.d/.local/straight/repos/corfu/extensions")
  ;; (require 'corfu)
  ;; (require 'corfu-info)
  ;; (require 'cape)
  (setq lsp-bridge-completion-provider 'company)

  (require 'lsp-bridge)
  (require 'lsp-bridge-orderless) ;; make lsp-bridge support fuzzy match, optional
  (require 'lsp-bridge-icon) ;; show icon for completion items, optional

  ;; 融合 `lsp-bridge' `find-function' 以及 `dumb-jump' 的智能跳转
  (defun lsp-bridge-jump ()
    (interactive)
    (cond
     ((eq major-mode 'emacs-lisp-mode)
      (let ((symb (function-called-at-point)))
        (when symb
          (find-function symb))))
     (lsp-bridge-mode
      (lsp-bridge-find-def))
     (t
      (require 'dumb-jump)
      (dumb-jump-go))))

  (defun lsp-bridge-jump-back ()
    (interactive)
    (cond
     (lsp-bridge-mode
      (lsp-bridge-return-from-def))
     (t
      (require 'dumb-jump)
      (dumb-jump-back))))

  (global-lsp-bridge-mode)

  ;; For Xref support
  (add-hook 'lsp-bridge-mode-hook (lambda ()
                                    (add-hook 'xref-backend-functions #'lsp-bridge-xref-backend nil t)))
  )
;; (provide 'init-lsp-bridge)
;;
;;; config.el ends here
