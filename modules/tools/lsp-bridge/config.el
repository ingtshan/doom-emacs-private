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
(after! corfu

  (add-to-list 'load-path "~/.emacs.d/.local/straight/repos/lsp-bridge")
  (add-to-list 'load-path "~/.emacs.d/.local/straight/repos/corfu/extensions")
  (require 'corfu)
  (require 'corfu-info)
  (require 'cape)
  (require 'lsp-bridge)
  (require 'lsp-bridge-orderless) ;; make lsp-bridge support fuzzy match, optional
  (require 'lsp-bridge-icon) ;; show icon for completion items, optional
  ;; (require 'tabnine-capf)

;;; Code:

  ;; 修改Corfu行高，默认太小了
  ;; (custom-set-faces
  ;;  '(corfu-default ((t (:height 1.3)))))

  ;; 打开日志，开发者才需要
  ;; (setq lsp-bridge-enable-log t)

  ;; 默认用这三个补全后端
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)

  (dolist (hook (list
                 'emacs-lisp-mode-hook
                 ))
    (add-hook hook (lambda ()
                     (setq-local corfu-auto t) ; Elisp文件自动弹出补全
                     )))

  ;; 通过Cape融合不同的补全后端，比如lsp-bridge、 tabnine、 file、 dabbrev.
  (defun lsp-bridge-mix-multi-backends ()
    (setq-local completion-category-defaults nil)
    (setq-local completion-at-point-functions
                (list
                 (cape-capf-buster
                  (cape-super-capf
                   #'lsp-bridge-capf
                   #'cape-file
                   #'cape-dabbrev
                   )
                  'equal)
                 ))
     (append completion-at-point-functions
        (mapcar #'cape-company-to-capf
                (list #'company-dabbrev #'company-keywords #'company-etags))))

  (dolist (hook lsp-bridge-default-mode-hooks)
    (add-hook hook (lambda ()
                     (setq-local corfu-auto nil) ; 编程文件关闭Corfu自动补全， 由lsp-bridge来手动触发补全
                     (lsp-bridge-mode 1)         ; 开启lsp-bridge
                     (lsp-bridge-mix-multi-backends) ; 通过Cape融合多个补全后端
                     )))

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

  ;; 全局开启补全
  (global-corfu-mode)

  )

;; (provide 'init-lsp-bridge)

;;; config.el ends here
