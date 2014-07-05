;; personal configuration

(require 'bind-key)

;; Windows and frames

(defun new-floating-frame ()
  "Creates a new floating frame.
This is special to my xmonad configuration which floats windows named floating"
  (interactive)
  (make-frame '((name . "floating")
                (title . "emacs"))))

(defun new-floating-center-frame ()
  "Creates a new floating frame.
This is special to my xmonad configuration which floats windows
named floating-center"
  (interactive)
  (make-frame '((name . "floating-center")
                (title . "emacs"))))

(defun new-floating-center-large-frame ()
  "Creates a new floating frame.
This is special to my xmonad configuration which floats windows
named floating-center"
  (interactive)
  (make-frame '((name . "floating-center-large")
                (title . "emacs"))))

(defun find-file-in-large-floating-frame (file)
  "Find file in large center floating frame."
  (interactive)
  (when (file-exists-p file)
    (let ((frame (new-floating-center-large-frame) ))
      (select-frame frame)
      (find-file file))))


(defun make-floating-minibuffer-frame ()
  "Creates a new floating frame.
This is special to my xmonad configuration which floats windows
named floating-center"
  (interactive)
  (make-frame '((name . "floating-center-large")
                (title . "emacs popup minibuffer"))))

(defvar popup-minibuffer-frame nil)
(add-hook 'minibuffer-exit-hook
          '(lambda ()
             (when (and
                    popup-minibuffer-frame
                      (frame-live-p popup-minibuffer-frame))
               (delete-frame popup-minibuffer-frame t)
               (setq popup-minibuffer-frame nil))))

(defun popup-minibuffer-switch ()
  "popup minibuffer swtich WIP
TODO detect if running under xmonad, otherwise dont popup
TODO generalise function to work with other ido prompts."
  (interactive)
  (setq popup-minibuffer-frame (make-floating-minibuffer-frame))
  (with-selected-frame
      popup-minibuffer-frame
        (call-interactively 'helm-for-files)))

(bind-key "C-<" 'popup-minibuffer-switch)

(load-theme 'solarized-dark t)

(load "~/.emacs-private.d/init.el" 'noerror 'nomessage)

(require 'goto-chg)
;; line numbers on the side
(global-linum-mode t)

;; keybindings using bind-key.el

(defun curly-brackets ()
  (interactive)
  (insert "{}")
  (backward-char 1))

(defun curly-brackets-newline ()
  (interactive)
  (insert "{")
  (newline)
  (indent-relative)
  (newline)
  (insert "}")
  (backward-char 1)
  (indent-relative))

(defun brackets ()
  (interactive)
  (insert "[]")
  (backward-char 1))


(bind-key "M-s-8" 'brackets)
(bind-key "C-8" 'brackets)
(bind-key "M-s-(" 'curly-brackets)
(bind-key "C-(" 'curly-brackets-newline)

(defun django-template-tag-bracket()
  (interactive)
  (insert "{%  %}")
  (backward-char 3))


(defun django-template-var-bracket()
  (interactive)
  (insert "{{   }}")
  (backward-char 4))

;;(bind-key "C-M-s-(", 'django-template-tag-bracket)
;;(bind-key "C-M-s-/", 'django-template-var-bracket)

(bind-key "C-+" 'goto-last-change)
(bind-key "C-´" 'goto-last-change-reverse)
(bind-key "s-2" "@")
(bind-key "s-4" "$")

;; duplicate line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
(bind-key "C-x C-o" 'duplicate-line)

;; ipdb
(defun insert-ipdb ()
  (interactive)
  (insert "import ipdb; ipdb.set_trace()"))
(bind-key "C-x C-'" 'insert-ipdb)

(require 'python-django)
(require 'python-flake8)
;; EPC (used for jedi)
(require 'epc)

;;(defvar my-epc (epc:start-epc "python" '("/home/vojd/.emacs.d/lib/epc/server.py")))
(defvar my-epc (epc:start-epc "python" '("/Users/mathias/.emacs.d/lib/epc/server.py")))

(deferred:$
  (epc:call-deferred my-epc 'echo '(10))
  (deferred:nextc it
    (lambda (x) (message "Return : %S" x))))

(message "Return : %S" (epc:call-sync my-epc 'echo '(10 40)))

;; Jedi
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)

;; org mode loaders
(setq org-log-done 'time)
(setq org-agenda-files (list "~/org/general.org")) ;; add more to list

;; kill ring browse-url
(require 'browse-kill-ring)
(bind-key "C-c C-y" 'browse-kill-ring)

;; loccur
(require 'loccur)
;; defines shortcut for loccur of the current word
(define-key global-map [(control o)] 'loccur-current)
;; defines shortcut for the interactive loccur command
(define-key global-map [(control meta o)] 'loccur)
;; defines shortcut for the loccur of the previously found word
(define-key global-map [(control shift o)] 'loccur-previous-match)

(defun loccur/list-Python-functions()
  "Displays only the lines corresponding to a function
declaration in a Python file."
  (loccur-no-highlight "^ *def "))

;; helm
;;(add-to-list 'load-path "~/.emacs.d/site-lisp/helm")
;;(require 'helm-config)

;; projectile
(require 'projectile)
