;;; load-path.el

(defconst user-emacs-directory
  (if (eq system-type 'ms-dos)
      ;; MS-DOS cannot have initial dot.
      "~/_emacs.d/"
    "~/.emacs.d/")
  "Directory beneath which additional per-user Emacs-specific files are placed.
Various programs in Emacs store information in this directory.
Note that this should end with a directory separator.
See also `locate-user-emacs-file'.")

(defconst user-data-directory
  (file-truename "~/.config/emacs-user-data"))
(defconst user-el-get-directory
  (expand-file-name "el-get" user-data-directory))
(defconst user-cache-directory
  (file-truename "~/.cache/emacs-user-cache"))
(defconst user-lisp-directory
  (expand-file-name "lisp" user-emacs-directory))
(defconst user-lib-directory
  (expand-file-name "lib" user-emacs-directory))
(defconst user-site-lisp-directory
  (expand-file-name "site-lisp" user-emacs-directory))
(defconst user-site-lisp-version-dependent-directory
  (expand-file-name  (concat "site-lisp-emacs"
                             (if (or (not (boundp 'emacs-version))
                                    (string< emacs-version "24.1"))
                                 "23"
                               "24"))
                     user-emacs-directory))
(defconst user-local-site-lisp-directory
  (expand-file-name "emacs-site-lisp" "~/.opt"))
(defconst user-override-directory
  (expand-file-name "override" user-emacs-directory))
(defconst user-local-override-directory
  (expand-file-name "emacs-override" "~/.opt"))
(defconst user-themes-directory
  (expand-file-name "themes" user-emacs-directory))
(defconst user-notes-directory
  (file-truename "~/notes"))

;; These should always exist
(make-directory user-data-directory t)
(make-directory user-cache-directory t)
(make-directory user-el-get-directory t)
(make-directory user-local-override-directory t)
(make-directory user-local-site-lisp-directory t)

;; emacs23 compat
(if (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path user-themes-directory)
  (add-to-list 'load-path user-themes-directory))

(defun add-to-load-path (path &optional dir)
  (setq load-path
        (cons (expand-file-name path (or dir user-emacs-directory)) load-path)))

;; Add top-level lisp directories, in case they were not setup by the
;; environment.
(dolist (dir (nreverse
              (list user-override-directory
                    user-lisp-directory

                    user-el-get-directory
                    user-lib-directory
                    user-local-site-lisp-directory
                    user-site-lisp-directory
                    user-site-lisp-version-dependent-directory)))
  (dolist (entry (nreverse (directory-files-and-attributes dir)))
    (if (cadr entry)
        (add-to-load-path (car entry) dir))))

(mapc #'add-to-load-path
      (nreverse
       (list
        user-emacs-directory

        (concat user-local-override-directory "/org-mode/contrib/lisp/")
        (concat user-local-override-directory "/org-mode/lisp/")
        (concat user-local-override-directory "/bbdb/lisp/")
        (concat user-local-override-directory "/cedet/")
        (concat user-local-override-directory "/cedet/lisp/cedet/")
        (concat user-local-override-directory "/cedet/lisp/eieio/")
        (concat user-local-override-directory "/cedet/lisp/speedbar/")
        (concat user-local-override-directory "/cedet/lisp/common/")
        (concat user-local-override-directory "/tramp/lisp/")
        (concat user-site-lisp-directory "/emms/lisp")
        "/usr/local/share/emacs/site-lisp/"
        "/usr/local/share/emacs/site-lisp/mu4e/"
        "/opt/local/share/emacs/site-lisp/"
        "/usr/share/emacs/site-lisp/SuperCollider/"
        "/usr/share/emacs/site-lisp/supercollider/")))

(let ((cl-p load-path))
  (while cl-p
    (setcar cl-p (file-name-as-directory
                  (expand-file-name (car cl-p))))
    (setq cl-p (cdr cl-p))))

(setq load-path (delete-dups load-path))

(eval-after-load "info"
  #'(progn
      (when (fboundp 'info-initialize)
        (info-initialize)
        (defun add-to-info-path (path &optional dir)
          (setq Info-directory-list
                (cons (expand-file-name path (or dir user-emacs-directory)) Info-directory-list)))
        (mapc #'add-to-info-path
              (nreverse
               (list
                (expand-file-name "~/.refdoc/info")
                (concat user-local-override-directory "/org-mode/doc/")
                (concat user-local-override-directory "/cedet/doc/info/")
                (concat user-local-override-directory "/tramp/info/")
                (concat user-local-site-lisp-directory "/slime/doc/")
                (concat user-local-site-lisp-directory "/w3m/doc/")
                (concat user-local-site-lisp-directory "/bbdb/doc/")
                (concat user-local-site-lisp-directory "/evil/doc/")))))))

(require 'cus-load nil t)

(provide 'load-path)

;;; load-path.el ends here
