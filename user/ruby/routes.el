;;; user/ruby/routes.el -*- lexical-binding: t; -*-

;;; Commentary:

;; This is a package to help you find and insert rails routes into your code.
;; Instead of going to the terminal or loading the path in the browser, you just call 'rails-routes-insert',
;; It will fetch and save on cache all routes used by your application, so you have a reliable and easy way to search
;; and insert your routes.
;;
;; New on 0.3
;; - Remove projectile dependency
;; - Add command to insert routes ignoring cache
;; - Improve cache upgrade
;;
;; New on 0.2
;; Add rails-routes-jump to jump to the route controller.  Works with activeadmin.

;;; Code:
(require 'savehist)
(require 'subr-x)
(require 'inflections)
(require 'cl-lib)
(require 'projectile)

(defgroup rails-routes nil
  "Search for and insert rails routes."
  :group 'tools
  :group 'languages)

(defcustom rails-routes-project-root-function #'projectile-project-root
  "Function used to get project root."
  :type 'symbol)

(defcustom rails-routes-project-name-function #'projectile-project-name
  "Function used to get project name."
  :type 'symbol)

(defcustom rails-routes-search-command "RUBYOPT=-W0 rails routes"
  "Command executed to search the routes."
  :type 'string)

(defcustom rails-routes-insert-after-path "_path"
  "What will be inserted after calling `rails-routes-insert'."
  :type 'string)

(defcustom rails-routes-class-name "Rails.application.routes.url_helpers."
  "Prefix used to access rails routes outside the views."
  :type 'string)

(defvar rails-routes-cache '())
(defvar rails-routes-cache-validations '())

(defun rails-routes--set-cache (val)
  "Set routes cache to VAL."
  (when (assoc (funcall rails-routes-project-name-function) rails-routes-cache)
    (setq rails-routes-cache (remove (assoc (funcall rails-routes-project-name-function) rails-routes-cache) rails-routes-cache)))
  (setq rails-routes-cache (cons `(,(funcall rails-routes-project-name-function) . ,val) rails-routes-cache)))

(defun rails-routes--set-cache-validations (val)
  "Set validations cache to VAL."
  (when (assoc (funcall rails-routes-project-name-function) rails-routes-cache-validations)
    (setq rails-routes-cache-validations
          (remove (assoc (funcall rails-routes-project-name-function) rails-routes-cache-validations) rails-routes-cache-validations)))
  (setq rails-routes-cache-validations (cons `(,(funcall rails-routes-project-name-function) . ,val) rails-routes-cache-validations)))

(defun rails-routes-clear-cache ()
  "Clear rails routes cache."
  (interactive)
  (setq rails-routes-cache '())
  (setq rails-routes-cache-validations '()))

(defun rails-routes--run-command ()
  "Run rails-routes-search-command and return it."
  (message "Fetching routes.  Please wait.")
  (let ((command-result (cl-remove-if-not
                         (lambda (element)
                           (let ((len (length (split-string element " +"))))
                             (or (eq len 5) (eq len 4))))
                         (split-string (shell-command-to-string rails-routes-search-command) "\n"))))

      (rails-routes--set-cache command-result)
      (rails-routes--set-cache-validations t)
    command-result))

(defun rails-routes--get-routes-cached ()
  "Get the routes, using the cache if possible."
  (let ((routes-result (if (cdr (assoc (funcall rails-routes-project-name-function) rails-routes-cache-validations))
                           (cdr (assoc (funcall rails-routes-project-name-function) rails-routes-cache))
                         (rails-routes--run-command))))
    (if (not routes-result)
        (rails-routes--run-command)
      routes-result)))

(defun rails-routes--guess-route (controller-full-path)
  "Guess the route name from CONTROLLER-FULL-PATH.
CONTROLLER-FULL-PATH is the controller name plus action."
  (let ((controller-path (nth 0 (split-string controller-full-path "#"))))
    (replace-regexp-in-string "\/" "_" controller-path)))

;;;###autoload
(defun rails-routes-insert-no-cache ()
  "Clean cache, then, call rails-routes-insert."
  (interactive)
  (rails-routes-clear-cache)
  (rails-routes-insert))

(defun rails-routes--guess-ignore-class ()
  "Return t if class need to be inserted."
  (string-match-p "app/views\\|app/controllers\\|app/helpers" (buffer-file-name)))

;;;###autoload
(defun rails-routes-insert ()
  "Ask for the route you want and insert on code.
With prefix argument INSERT-CLASS, fully-qualify the route with
the `rails-routes-class-name' prefix."
  (interactive)
  (let* ((selected-value (split-string (completing-read "Route: " (rails-routes--get-routes-cached)) " +"))
         (selected-route (nth (if (eq (length selected-value) 5) 3 2) selected-value)))
    (when (not (rails-routes--guess-ignore-class)) (insert rails-routes-class-name))
    (rails-routes--insert-value selected-value)
    (when (or (string-match-p ":id" selected-route)
              (string-match-p ":[a-zA-Z0-9]+_id" selected-route))
      (progn (insert "()") (backward-char)))))

(defun rails-routes--insert-value (selected-value)
  "Insert the selected_value.  SELECTED-VALUE: Item im list."
  (insert (if (eq (length selected-value) 5) (nth 1 selected-value)
            (rails-routes--guess-route (nth 3 selected-value)))
          rails-routes-insert-after-path))

;;;###autoload
(defun rails-routes-invalidate-cache ()
  "Invalidate cache when the file that will be saved is routes.rb."
  (when (string-match-p "routes.rb" (buffer-file-name))
    (rails-routes--set-cache-validations nil)))

(defun rails-routes--add-alist ()
  "Add the rails-routes-cache and rails-routes-cache-validations to alist."
    (add-to-list 'savehist-additional-variables 'rails-routes-cache
    (add-to-list 'savehist-additional-variables 'rails-routes-cache-validations)))

(defun rails-routes--remove-path-or-url (path)
  "Remove any \"_path\" or \"_url\" suffix from PATH."
  (replace-regexp-in-string "\\(_path\\|_url\\)\\'" "" path))

(defun rails-routes--find-controller (path)
  "Find controller for path in routes list.
PATH: a rails routes path or url."
  (let ((routes (rails-routes--get-routes-cached))
        (response nil))
    (dolist (item routes)
      (let ((parsed_item (split-string item " +")))
        (when (string-equal (nth 1 parsed_item) path)
          (setq response (nth 4 parsed_item)))))
    response))

(defun rails-routes--singularize-string (word)
  "Singularize all words in a route.  WORD: any_word."
  (setq word (substring word 5))
  (let ((words (split-string word "_")))
    (string-join (mapcar #'inflection-singularize-string words) "_")))

(defun rails-routes--goto-activeadmin-controller (controller-name action)
  "Try to go to activeadmin first, if not exists, go to app/controllers.
CONTROLLER-NAME: Path of controller.  ACTION:  Action of the path."
  (let* ((project-root (funcall rails-routes-project-root-function))
         (moved nil)
         (normal-path (expand-file-name (concat "app/admin" (rails-routes--singularize-string controller-name) ".rb") project-root))
         (expanded-path
          (expand-file-name (concat "app/admin"
                                    (replace-regexp-in-string "_" "/" (rails-routes--singularize-string controller-name)) ".rb")
                            project-root)))

    (when (file-exists-p normal-path)
      (find-file normal-path)
      (setq moved t))

    (when (and (not moved) (file-exists-p expanded-path))
      (find-file expanded-path)
      (setq moved t))

    (if moved
        (progn
          (goto-char (point-min))
          (search-forward (concat "member_action :" action) (point-max) t)
          (search-forward (concat "collection_action :" action) (point-max) t))
      (rails-routes--go-to-controller controller-name action))))

(defun rails-routes--go-to-controller-and-action (full-action)
  "Go to controller and then, go to def action_name.  FULL-ACTION: action showed on rails routes."
  (let ((controller-name (nth 0 (split-string full-action "#"))) (action (nth 1 (split-string full-action "#"))))
    (if (string-match-p "admin" controller-name)
        (rails-routes--goto-activeadmin-controller controller-name action)
      (rails-routes--go-to-controller controller-name action))))

(defun rails-routes--go-to-controller (controller action)
  "Go to controller using action.  CONTROLLER: controller showed on rails routes. ACTION: action showed on rails routes."
  (find-file (rails-routes--controller-full-path controller))
  (search-forward (concat "def " action) (point-max) t))

(defun rails-routes--controller-full-path (controller-name)
  "Return the path of a rails controller using only the name.  CONTROLLER-NAME: Name of the controller."
  (concat (funcall rails-routes-project-root-function) "app/controllers/" controller-name "_controller.rb"))

;;;###autoload
(defun rails-routes-jump ()
  "Go to the route at point."
  (interactive)
  (let* ((path (symbol-name (symbol-at-point)))
         (controller (rails-routes--find-controller (rails-routes--remove-path-or-url path))))
    (if controller
        (rails-routes--go-to-controller-and-action controller) (message "Route not found."))
    (recenter)))

(defun rails-routes--set-routes-hook ()
  "Set the hook for 'after-save-hook' only for routes.rb."
  (when (and (buffer-file-name)
             (string-equal "routes.rb" (file-name-nondirectory (buffer-file-name)))
             (assoc (funcall rails-routes-project-name-function) rails-routes-cache)
             (assoc (funcall rails-routes-project-name-function) rails-routes-cache-validations))
    (add-hook 'after-save-hook #'rails-routes-invalidate-cache nil t)))

;;;###autoload
(define-minor-mode rails-routes-global-mode
  "Initialize cache and routes watch."
  :global t
  :lighter " routes"
  (if rails-routes-global-mode
      (progn
        (add-hook 'ruby-mode-hook #'rails-routes--set-routes-hook)
        (add-hook 'savehist-mode-hook #'rails-routes--add-alist))
    (progn
      (remove-hook 'ruby-mode-hook #'rails-routes--set-routes-hook)
      (remove-hook 'savehist-mode-hook #'rails-routes--add-alist))))

(provide 'rails-routes)
;;; rails-routes.el ends here
