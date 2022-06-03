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

;;; Code:

;; corfu
(add-to-list 'load-path "~/.emacs.d/.local/straight/repos/corfu/extensions")
(require 'corfu)
(require 'corfu-history)
(require 'cape)

;; lsp-bridge
(add-to-list 'load-path "~/.emacs.d/.local/straight/repos/lsp-bridge")

(require 'lsp-bridge)
(require 'lsp-bridge-icon)
(require 'lsp-bridge-orderless)
(require 'lsp-bridge-jdtls)

;; 默认用这三个补全后端
(add-to-list 'completion-at-point-functions #'cape-symbol)
(add-to-list 'completion-at-point-functions #'cape-file)
(add-to-list 'completion-at-point-functions #'cape-dabbrev)

;; 让Corfu适应高分屏
(when (> (frame-pixel-width) 3000) (custom-set-faces '(corfu-default ((t (:height 1.3))))))

;; 开启 history mode
(corfu-history-mode t)

(global-lsp-bridge-mode)

(global-corfu-mode)

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

;; (require 'tabnine-capf)

;; 通过Cape融合不同的补全后端，比如lsp-bridge、 tabnine、 file、 dabbrev.
(defun lsp-bridge-mix-multi-backends ()
  (setq-local completion-category-defaults nil)
  (setq-local completion-at-point-functions
              (list
               (cape-capf-buster
                (cape-super-capf
                 #'lsp-bridge-capf

                 ;; 我嫌弃TabNine太占用我的CPU了， 需要的同学注释下面这一行就好了
                 ;; #'tabnine-completion-at-point

                 #'cape-file
                 #'cape-dabbrev
                 )
                'equal)
               )))

(dolist (hook lsp-bridge-default-mode-hooks)
  (add-hook hook (lambda ()
                   (lsp-bridge-mix-multi-backends) ; 通过Cape融合多个补全后端
                   )))

;;; config.el ends here
