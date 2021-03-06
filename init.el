(load (expand-file-name "load-path" (file-name-directory load-file-name)) nil t)
(set-language-environment "UTF-8")
(add-to-list 'command-switch-alist
             (cons "--live-safe-mode"
                   (lambda (switch)
                     nil)))

(setq live-safe-modep
      (if (member "--live-safe-mode" command-line-args)
          "debug-mode-on"
        nil))

(setq initial-scratch-message "
;; Emacs failed to start correctly.
;; First up, could you try running Emacs Live in safe mode:
;;    emacs --live-safe-mode
;; This will only load the default packs.
;; In either case, you should try starting Emacs in debug mode to get
;; more information regarding the error:
;;    emacs --debug-init
;;
;; Please feel free to raise an issue on the Gihub tracker:
;;    https://github.com/overtone/emacs-live/issues
;;
;; Alternatively, let us know in the mailing list:
;;    http://groups.google.com/group/emacs-live
")

(setq live-supported-emacsp t)

(when (< emacs-major-version 24)
  (setq live-supported-emacsp nil)
  (setq initial-scratch-message (concat "
;; Emacs Live is only supported on Emacs 24+.
;;
;; You are running: " emacs-version "
;; OS X GUI     - http://emacsformacosx.com/
;; OS X Console - via homebrew (http://mxcl.github.com/homebrew/)
;;                brew install emacs
;; Windows      - http://alpha.gnu.org/gnu/emacs/windows/
;; Linux        - Consult your package manager or compile from source

"))
  (let* ((old-file (concat (file-name-as-directory "~") ".emacs-old.el")))
    (if (file-exists-p old-file)
      (load-file old-file)
      (error (concat "Oops - your emacs isn't supported. Emacs Live only works on Emacs 24+ and you're running version: " emacs-version ". Please upgrade your Emacs and try again, or define ~/.emacs-old.el for a fallback")))))

(let ((emacs-live-directory (getenv "EMACS_LIVE_DIR")))
  (when emacs-live-directory
    (setq user-emacs-directory emacs-live-directory)))

(when live-supported-emacsp
;; Store live base dirs, but respect user's choice of `live-root-dir'
;; when provided.
(setq live-root-dir (if (boundp 'live-root-dir)
                        (file-name-as-directory live-root-dir)
                      user-emacs-directory))

(setq
 live-tmp-dir      (file-name-as-directory (concat live-root-dir "tmp"))
 live-etc-dir      (file-name-as-directory (concat live-root-dir "etc"))
 live-pscratch-dir (file-name-as-directory (concat live-tmp-dir  "pscratch"))
 live-lib-dir      (file-name-as-directory (concat live-root-dir "lib"))
 live-packs-dir    (file-name-as-directory (concat live-root-dir "packs"))
 live-autosaves-dir(file-name-as-directory (concat live-tmp-dir  "autosaves"))
 live-backups-dir  (file-name-as-directory (concat live-tmp-dir  "backups"))
 live-custom-dir   (file-name-as-directory (concat live-etc-dir  "custom"))
 live-load-pack-dir nil
 live-disable-zone nil)

;; create tmp dirs if necessary
(make-directory live-etc-dir t)
(make-directory live-tmp-dir t)
(make-directory live-autosaves-dir t)
(make-directory live-backups-dir t)
(make-directory live-custom-dir t)
(make-directory live-pscratch-dir t)

;; Load manifest
(load-file (concat live-root-dir "manifest.el"))

;; load live-lib
(load-file (concat live-lib-dir "live-core.el"))

;;default packs
(let* ((pack-names '("foundation-pack"
                     "clojure-pack"
                     "lang-pack"
                     "power-pack"
                     "git-pack"
                     "bindings-pack"))
       (live-dir (file-name-as-directory "live"))
       (dev-dir  (file-name-as-directory "dev")))
  (setq live-packs (mapcar (lambda (p) (concat live-dir p)) pack-names) )
  (setq live-dev-pack-list (mapcar (lambda (p) (concat dev-dir p)) pack-names) ))

;; Helper fn for loading live packs

(defun live-version ()
  (interactive)
  (if (called-interactively-p 'interactive)
      (message "%s" (concat "This is Emacs Live " live-version))
    live-version))

;; Load `~/.emacs-live.el`. This allows you to override variables such
;; as live-packs (allowing you to specify pack loading order)
;; Does not load if running in safe mode
(let* ((pack-file (concat (file-name-as-directory "~") ".emacs-live.el")))
  (if (and (file-exists-p pack-file) (not live-safe-modep))
      (load-file pack-file)))

;; Load all packs - Power Extreme!
(mapcar (lambda (pack-dir)
          (live-load-pack pack-dir))
        (live-pack-dirs))

(setq live-welcome-messages
      (if (live-user-first-name-p)
          (list (concat "Hello " (live-user-first-name) ", somewhere in the world the sun is shining for you right now.")
                (concat "Hello " (live-user-first-name) ", it's lovely to see you again. I do hope that you're well.")
                (concat (live-user-first-name) ", turn your head towards the sun and the shadows will fall behind you.")
                )
        (list  "Hello, somewhere in the world the sun is shining for you right now."
               "Hello, it's lovely to see you again. I do hope that you're well."
               "Turn your head towards the sun and the shadows will fall behind you.")))


(defun live-welcome-message ()
  (nth (random (length live-welcome-messages)) live-welcome-messages))

(when live-supported-emacsp
  (setq initial-scratch-message (concat ";;
;;     MM  MMMMMMMM 88  88  88 88.  .88 88.  ...       88
;;     MMMMMMMMMMM MMMM MMMMMMMMMMM MMMMMMMMMMMM  Version " live-version
                                                                (if live-safe-modep
                                                                    "
;;                                                     --*SAFE MODE*--"
                                                                  "
;;"
                                                                  ) "
;;           http://github.com/overtone/emacs-live
;;
;; "                                                      (live-welcome-message) "

")))
)

;(if (not live-disable-zone)
;   (add-hook 'term-setup-hook 'zone))

(if (not custom-file)
    (setq custom-file (concat live-custom-dir "custom-configuration.el")))
(when (file-exists-p custom-file)
  (load custom-file))

;; ########################
;; # Personal config      #
;; # this shall be broken #
;; # out and made dynamic #
;; ########################

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

(defvar my-epc (epc:start-epc "python" '("/home/vojd/.emacs.d/lib/epc/server.py")))

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
(add-to-list 'load-path "~/.emacs.d/site-lisp/helm")
(require 'helm-config)

;; projectile
(require 'projectile)
