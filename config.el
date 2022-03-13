;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-
;;;

;;
;;; private setting and key binding
(load! "user/setting")
(load! "+evil-bindings")

;; When I bring up Doom's scratch buffer with SPC x, it's often to play with
;; elisp or note something down (that isn't worth an entry in my notes). I can
;; do both in `lisp-interaction-mode'.
(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)


;;
;;; UI

(setq doom-theme 'doom-dracula
      doom-font (font-spec :family "JetBrainsMono" :size 12 :weight 'light)
      doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 13))

;; Line numbers are pretty slow all around. The performance boost of disabling
;; them outweighs the utility of always keeping them on.
(setq display-line-numbers-type nil)

;; Prevents some cases of Emacs flickering.
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))


;;
;;; Keybinds

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


;;
;;; Modules

;;; :lang ruby
(load! "user/ruby/rubocop")

;;; :lang org
(load! "user/org")

;;; :completion company
;; IMO, modern editors have trained a bad habit into us all: a burning need for
;; completion all the time -- as we type, as we breathe, as we pray to the
;; ancient ones -- but how often do you *really* need that information? I say
;; rarely. So opt for manual completion:
(after! company
  (setq company-idle-delay nil))


;;; :ui modeline
;; An evil mode indicator is redundant with cursor shape
(advice-add #'doom-modeline-segment--modals :override #'ignore)


;;; :editor evil
;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Implicit /g flag on evil ex substitution, because I use the default behavior
;; less often.
(setq evil-ex-substitute-global t)


;;; :tools lsp
;; Disable invasive lsp-mode features
(after! lsp-mode
  (setq lsp-enable-symbol-highlighting nil
        ;; If an LSP server isn't present when I start a prog-mode buffer, you
        ;; don't need to tell me. I know. On some machines I don't care to have
        ;; a whole development environment for some ecosystems.
        lsp-enable-suggest-server-download nil))
(after! lsp-ui
  (setq lsp-ui-sideline-enable nil  ; no more useful than flycheck
        lsp-ui-doc-enable nil))     ; redundant with K


;;; :tools magit
(setq magit-repository-directories '(("~/projects" . 2))
      magit-save-repository-buffers nil
      ;; Don't restore the wconf after quitting magit, it's jarring
      magit-inhibit-save-previous-winconf t
      transient-values '((magit-rebase "--autosquash" "--autostash")
                         (magit-pull "--rebase" "--autostash")
                         (magit-revert "--autostash")))

;;; :ui doom-dashboard
(setq fancy-splash-image (concat doom-private-dir "splash.png"))
;; Hide the menu for as minimalistic a startup screen as possible.
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)

;;; :app everywhere
(after! emacs-everywhere
  ;; Easier to match with a bspwm rule:
  ;;   bspc rule -a 'Emacs:emacs-everywhere' state=floating sticky=on
  (setq emacs-everywhere-frame-name-format "emacs-anywhere")

  ;; The modeline is not useful to me in the popup window. It looks much nicer
  ;; to hide it.
  (remove-hook 'emacs-everywhere-init-hooks #'hide-mode-line-mode)

  ;; Semi-center it over the target window, rather than at the cursor position
  ;; (which could be anywhere).
  (defadvice! center-emacs-everywhere-in-origin-window (frame window-info)
    :override #'emacs-everywhere-set-frame-position
    (cl-destructuring-bind (x y width height)
        (emacs-everywhere-window-geometry window-info)
      (set-frame-position frame
                          (+ x (/ width 2) (- (/ width 2)))
                          (+ y (/ height 2))))))

;;
;;; Language customizations

(define-generic-mode sxhkd-mode
  '(?#)
  '("alt" "Escape" "super" "bspc" "ctrl" "space" "shift") nil
  '("sxhkdrc") nil
  "Simple mode for sxhkdrc files.")
