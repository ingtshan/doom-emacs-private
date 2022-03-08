;;; os/mackey/config.el -*- lexical-binding: t; -*-

;; os hot key
;; set right command key of macOS
(setq mac-command-modifier 'hyper mac-option-modifier 'meta)
;; what different between (kbd "H-v") and [(hyper v)] ?
;; os shortcut
(global-set-key (kbd "H-a") #'mark-page)          ; 全选
(global-set-key (kbd "H-v") #'yank)               ; 粘贴
(global-set-key (kbd "H-x") #'kill-region)        ; 剪切
(global-set-key (kbd "H-c") #'kill-ring-save)     ; 复制
(global-set-key (kbd "H-s") #'save-buffer)        ; 保存
(global-set-key (kbd "H-z") #'undo-fu-only-undo)  ; 撤销编辑修改
(global-set-key (kbd "H-Z") #'undo-fu-only-redo)  ; 撤销编辑修改
(global-set-key [(hyper n)] #'make-frame-command) ; 新建窗口
(global-set-key [(hyper q)] #'+mackey-quite-emacs); 退出
(global-set-key [(hyper w)] #'+mackey-close-frame); 退出frame
;; make select more like other editro
;; (delete-selection-mode 1)
;; use shift to extend select
;; (global-set-key (kbd "<S-down-mouse-1>") #'mouse-save-then-kill)
