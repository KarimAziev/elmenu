;;; elmenu.el --- Jump to elisp symbols in buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/elmenu
;; Version: 0.1.0
;; Keywords: lisp
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Completing read for elisp symbols in buffer
 
;;; Code:


(defcustom elmenu-multi-source-restore-last-input t
  "Whether to insert last typed source input."
  :group 'multi-source
  :type 'boolean)

(defun elmenu-intern-cars (alist)
  "Intern cars in ALIST."
  (mapcar
   (lambda (it)
     (setcar it (intern-soft
                 (car it)))
     it)
   alist))

(defvar elmenu-var-types (elmenu-intern-cars
                              '(("defvar" . 3)
                                ("defconst" . 3)
                                ("defvar-local" . 3))))

(defvar elmenu-custom-types (elmenu-intern-cars
                                 '(("defface" . 3)
                                   ("defcustom" . 3)
                                   ("defgroup" . 3)
                                   ("deftheme" . 3))))

(defvar elmenu-func-types
  (elmenu-intern-cars
   '(("defun" . 3)
     ("defmacro" . 3)
     ("defsubst" . 3)
     ("defalias" . 4)
     ("defhydra" . 3)
     ("transient-define-prefix" . 3)
     ("transient-define-suffix" . 3)
     ("transient-define-argument" . 3)
     ("transient-define-infix" . 3)
     ("cl-defun" . 3)
     ("cl-defsubst" . 3)
     ("cl-defmacro" . 3)
     ("cl-defgeneric" . 3)
     ("cl-defmethod" . 3))))

(defvar elmenu-modes-types
  (elmenu-intern-cars '(("define-minor-mode" . 2)
                        ("define-derived-mode" . 4)
                        ("define-generic-mode" . 8)
                        ("define-compilation-mode" .
                         3)
                        ("easy-mmode-define-minor-mode"
                         . 2))) )

(defvar elmenu-def-type-poses
  (append
   (elmenu-intern-cars
    '(("define-skeleton" . 2)
      ("ert-deftest" . 3)
      ("define-widget" . 3)
      ("easy-mmode-define-minor-mode"
       . 2)
      ("defclass" . 4)
      ("cl-defstruct" . 3)))
   elmenu-var-types
   elmenu-custom-types
   elmenu-func-types
   elmenu-modes-types))

(defvar elmenu-interactive-types
  (elmenu-intern-cars
   '(("defun" . 3)
     ("defsubst" . 3)
     ("cl-defun" . 3)
     ("cl-defsubst" . 3))))

(defvar elmenu-non-defun-command-types
  (elmenu-intern-cars
   '(("defhydra" . 3)
     ("transient-define-prefix" . 3))))

(defvar elmenu-custom-types (elmenu-intern-cars
                             '(("defface" . 3)
                               ("defcustom" . 3)
                               ("defgroup" . 3)
                               ("deftheme" . 3))))

(defvar elmenu-var-types (elmenu-intern-cars
                           '(("defvar" . 3)
                             ("defconst" . 3)
                             ("defvar-local" . 3))))

(defvar elmenu-def-type-poses
  (append
   (elmenu-intern-cars
    '(("define-skeleton" . 2)
      ("ert-deftest" . 3)
      ("define-widget" . 3)
      ("easy-mmode-define-minor-mode"
       . 2)
      ("defclass" . 4)
      ("cl-defstruct" . 3)))
   elmenu-var-types
   elmenu-custom-types
   elmenu-func-types
   elmenu-modes-types))

(defvar elmenu-func-types
  (elmenu-intern-cars
   '(("defun" . 3)
     ("defmacro" . 3)
     ("defsubst" . 3)
     ("defalias" . 4)
     ("defhydra" . 3)
     ("transient-define-prefix" . 3)
     ("transient-define-suffix" . 3)
     ("transient-define-argument" . 3)
     ("transient-define-infix" . 3)
     ("cl-defun" . 3)
     ("cl-defsubst" . 3)
     ("cl-defmacro" . 3)
     ("cl-defgeneric" . 3)
     ("cl-defmethod" . 3))))

(defvar elmenu-visited-syms nil)

(defun elmenu-re-search-forward-inner (regexp &optional bound count)
  "This function is helper for `elmenu-re-search-forward'.
Search forward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-forward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (nth 3 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-sexp))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-line))
              (t
               (setq count (1- count)))))))
  (point))

(defun elmenu-re-search-backward-inner (regexp &optional bound count)
  "This function is helper for `elmenu-re-search-backward'.
Search backward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-backward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (or (nth 3 parse))
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              (t
               (setq count (1- count)))))))
  (point))

(defun elmenu-re-search-forward (regexp &optional bound noerror count)
  "Search forward from point for REGEXP ignoring elisp comments and strings.
Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (unless count (setq count 1))
  (let ((init-point (point))
        (search-fun
         (cond ((< count 0)
                (setq count (- count))
                #'elmenu-re-search-backward-inner)
               ((> count 0) #'elmenu-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char init-point)
       (unless noerror
         (signal (car err)
                 (cdr err)))))))

(defun elmenu-parse-backward ()
  "Recursively scan backward forms."
  (let ((items))
    (while (and (elmenu-backward-list)
                (looking-at "[(]"))
      (setq items (nconc items (elmenu-parse-sexp-at-point))))
    items))

(defun elmenu-backward-list (&optional n)
  "Move backward across N balanced group of parentheses.
Return new position if changed, nil otherwise."
  (let ((pos (point))
        (end))
    (setq end (ignore-errors
                (backward-list (or n 1))
                (point)))
    (unless (equal pos end)
      end)))

(defun elmenu-unquote (exp)
  "Return EXP unquoted."
  (declare (pure t)
           (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun elmenu-parse-sexp-from-backward ()
  "Scan sexp at point from backward."
  (forward-sexp 1)
  (backward-char 1)
  (elmenu-parse-backward))

(defun elmenu-sexp-declare-p (sexp)
  "Return non-nil if SEXP is declared form."
  (pcase sexp
    (`(defvar ,name)
     (list 'defvar name))
    (`(declare-function ,name)
     (list 'declare-function name))
    (`(declare-function ,name
                        ,_file)
     (list 'declare-function name))
    (`(declare-function ,name
                        ,_file
                        ,_args)
     (list 'declare-function name))
    (`(declare-function ,name ,_file ,_args ,_fileonly)
     (list 'declare-function name))))

(defun elmenu-list-at-point ()
  "Return list at point."
  (when-let ((sexp (sexp-at-point)))
    (when (proper-list-p sexp)
      sexp)))

(defun elmenu-re-search-backward (regexp &optional bound noerror count)
  "Search backward from point for REGEXP ignoring elisp comments and strings.

Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (elmenu-re-search-forward regexp bound noerror
                                (if count (- count) -1)))

(defun elmenu-make-re (name)
  "Convert NAME to regexp and surround the result with `\\\\_<' and `\\\\_>'."
  (concat "\\_<" "\\(" (regexp-opt (list name
                                         (concat "@" name))
                                   t)
          "\\)"
          "\\_>"))

(defun elmenu-get-doc-from-sexp (sexp)
  "Return documentation from SEXP."
  (when (proper-list-p sexp)
    (let* ((type (car-safe sexp))
           (pos (and type
                     (cdr
                      (assq type elmenu-def-type-poses))))
           (doc-str
            (pcase type
              ('defvar-keymap (plist-get sexp :doc))
              ((guard (and pos
                           (eq type 'cl-defmethod)
                           (memq (nth 2 sexp) '(:around :after
                                                        :before))))
               (nth (1+ pos) sexp))
              ((guard (and pos))
               (nth pos sexp)))))
      doc-str)))

(defun elmenu-format-args (arglist)
  "Return ARGLIST as a string enclosed by ()."
  (if (stringp arglist)
      (substring-no-properties arglist)
    (if (not arglist)
        "()"
      (format "%S" (mapcar (lambda (arg)
                             (cond ;; Parameter name.
                              ((symbolp arg)
                               (let ((name (symbol-name arg)))
                                 (cond ((string-match "\\`&" name)
                                        (bare-symbol arg))
                                       ((string-match "\\`_." name)
                                        (intern (upcase (substring name 1))))
                                       (t (intern (upcase name))))))
                              ;; Parameter with a default value (from
                              ;; cl-defgeneric etc).
                              ((and (consp arg)
                                    (symbolp (car arg)))
                               (cons (intern (upcase (symbol-name (car arg))))
                                     (cdr arg)))
                              ;; Something else.
                              (t arg)))
                           arglist)))))

(defun elmenu-buffer-jump-to-form (cell)
  "Search for definition with CELL."
  (when-let* ((pl (cdr cell))
              (beg
               (when pl (plist-get pl :start)))
              (buff
               (let ((b (or
                         (if (markerp beg)
                             (marker-buffer beg)
                           (find-file-noselect (plist-get pl :file))))))
                 (when (buffer-live-p b)
                   b))))
    (if (not (eq (current-buffer)
                 buff))
        (with-current-buffer buff
          (goto-char beg)
          (pop-to-buffer-same-window buff)
          (elmenu-highlight-sexp-at-point)))
    (goto-char beg)
    (elmenu-highlight-sexp-at-point)))
(declare-function find-library-name "find-func")

(defun elmenu-find-lib-in-dir (sym dir)
  "Lookup for library SYM in current directory DIR."
  (require 'find-func)
  (when-let ((file (ignore-errors
                     (find-library-name (symbol-name sym)))))
    (when (file-in-directory-p file dir)
      file)))
(defun elmenu-parse-require (sym)
  "Lookup for library SYM in current directory and parse it."
  (require 'find-func)
  (when-let ((dir default-directory)
             (file (ignore-errors
                     (find-library-name (symbol-name sym)))))
    (when (file-in-directory-p file dir)
      (with-current-buffer (find-file-noselect file)
        (elmenu--buffer)))))
(defvar elmenu-readed-files)

(defun elmenu-read-file (&optional file)
  "Find items in FILE."
  (if (not (boundp 'elmenu-readed-files))
      (let ((elmenu-readed-files))
        (let ((file (or file
                        buffer-file-name)))
          (unless (member file elmenu-readed-files)
            (push file elmenu-readed-files)
            (with-temp-buffer
              (insert-file-contents file)
              (let ((sexps)
                    (sexp))
                (goto-char (point-min))
                (while (setq sexp (ignore-errors (read (current-buffer))))
                  (let* ((parsed (elmenu-parse-sexp sexp))
                         (autoload-item (save-excursion
                                          (forward-sexp -1)
                                          (forward-line -1)
                                          (when (looking-at ";;;###")
                                            (buffer-substring-no-properties
                                             (line-beginning-position)
                                             (line-end-position))))))
                    (setq sexps (append sexps (if (and autoload-item)
                                                  (mapcar (lambda (it)
                                                            (let
                                                                ((val (cdr it)))
                                                              (setq val
                                                                    (plist-put
                                                                     val
                                                                     :autoload
                                                                     autoload-item))
                                                              (cons it val)))
                                                          parsed)
                                                parsed)))))
                sexps)))))
    (unless (member file elmenu-readed-files)
      (push file elmenu-readed-files)
      (with-temp-buffer
        (insert-file-contents file)
        (let ((sexps)
              (sexp))
          (goto-char (point-min))
          (while (setq sexp (ignore-errors (read (current-buffer))))
            (let* ((parsed (elmenu-parse-sexp sexp))
                   (autoload-item (save-excursion
                                    (forward-sexp -1)
                                    (forward-line -1)
                                    (when (looking-at ";;;###")
                                      (buffer-substring-no-properties
                                       (line-beginning-position)
                                       (line-end-position))))))
              (setq sexps (append sexps (if (and autoload-item)
                                            (mapcar (lambda (it)
                                                      (let ((val (cdr it)))
                                                        (setq val
                                                              (plist-put
                                                               val
                                                               :autoload
                                                               autoload-item))
                                                        (setcdr it val)
                                                        it))
                                                    parsed)
                                          parsed)))))
          sexps)))))

(defun elmenu-parse-sexp (item)
  "Parse ITEM."
  (when (proper-list-p item)
    (let* ((type (car-safe item)))
      (when (and type
                 (symbolp type))
        (let ((doc (elmenu-get-doc-from-sexp item))
              (sym
               (cond ((not (cadr item))
                      nil)
                     ((and (symbolp (cadr item)))
                      (cadr item))
                     (t (elmenu-unquote (cadr item)))))
              (declaration (elmenu-sexp-declare-p item))
              (cell))
          (setq cell
                (pcase type
                  ((guard declaration)
                   (cons sym
                         (list
                          :type type
                          :declared t)))
                  ('cl-defstruct
                      (cons (if (symbolp sym)
                                sym
                              (car-safe sym))
                            (list
                             :type type)))
                  ((or 'use-package 'use-package!)
                   (let* ((data (mapcar 'elmenu-parse-sexp item))
                          (v (cons sym (list
                                        :type type))))
                     (if data
                         (setq data (nconc data (list v)))
                       v)))
                  ((or 'with-eval-after-load 'eval-when-compile
                       'eval-after-load
                       'straight-use-package 'if 'progn
                       'and
                       'let 'if-let 'when-let 'with-no-warnings
                       'when 'unless 'eval-and-compile)
                   (mapcar 'elmenu-parse-sexp (cdr item)))
                  ('require
                   (let* ((file (elmenu-find-lib-in-dir
                                 sym
                                 default-directory)))
                     (if file
                         (elmenu-read-file file)
                       (cons sym
                             (list :type type)))))
                  ('provide
                   (cons sym
                         (list :type 'provide)))
                  ((guard
                    (assq type elmenu-func-types))
                   (cons sym
                         (list
                          :type type
                          :args (seq-find 'proper-list-p item)
                          :doc doc
                          :interactive
                          (when (assq type elmenu-interactive-types)
                            (eq 'interactive
                                (if doc
                                    (car-safe
                                     (car-safe
                                      (cdr-safe
                                       (member doc
                                               item))))
                                  (car-safe (nth
                                             (cdr
                                              (assq type
                                                    elmenu-interactive-types))
                                             item))))))))
                  ((guard (assq type elmenu-custom-types))
                   (cons sym
                         (list
                          :type 'custom-variable
                          :value (nth 2 item)
                          :doc doc)))
                  ((guard (assq type (append
                                      elmenu-custom-types
                                      elmenu-var-types)))
                   (cons sym
                         (list
                          :type type
                          :doc doc)))
                  ((guard (assq type elmenu-modes-types))
                   (cons sym (list
                              :type type
                              :doc doc)))
                  ((guard (assq type elmenu-def-type-poses))
                   (cons sym (list
                              :doc doc
                              :type type)))
                  ((or 'add-hook 'remove-hook)
                   (cons sym (list
                              :type type)))))
          (if (and (car-safe cell)
                   (symbolp (car cell)))
              (list cell)
            cell))))))

(defun elmenu-parse-sexp-at-point ()
  "Parse `sexp-at-point' at point."
  (with-syntax-table emacs-lisp-mode-syntax-table
    (let* ((item (elmenu-list-at-point))
           (type (car-safe item)))
      (when (and type
                 (symbolp type)
                 (not (nth 4 (syntax-ppss (point)))))
        (let ((beg (point-marker))
              (doc (elmenu-get-doc-from-sexp item))
              (sym
               (cond ((not (cadr item))
                      nil)
                     ((and (symbolp (cadr item)))
                      (cadr item))
                     (t (elmenu-unquote (cadr item)))))
              (declaration (elmenu-sexp-declare-p item))
              (cell))
          (setq cell
                (pcase type
                  ((guard declaration)
                   (cons sym
                         (list
                          :type type
                          :declared t
                          :start beg)))
                  ('cl-defstruct
                      (cons (if (symbolp sym)
                                sym
                              (car-safe sym))
                            (list
                             :type type
                             :start beg)))
                  ((or 'use-package 'use-package!)
                   (let* ((data (save-excursion
                                  (elmenu-parse-sexp-from-backward)))
                          (v (cons sym (list
                                        :type type
                                        :start beg))))
                     (if data
                         (setq data (nconc data (list v)))
                       (list v))))
                  ((or 'with-eval-after-load 'eval-when-compile
                       'eval-after-load
                       'straight-use-package 'if 'progn
                       'and
                       'let 'if-let 'when-let 'with-no-warnings
                       'when 'unless 'eval-and-compile)
                   (save-excursion
                     (elmenu-parse-sexp-from-backward)))
                  ('require
                   (let* ((file (elmenu-find-lib-in-dir
                                 sym
                                 default-directory))
                          (visited (memq sym elmenu-visited-syms)))
                     (unless visited
                       (push sym elmenu-visited-syms))
                     (if (and (not visited) file)
                         (ignore-errors (with-current-buffer
                                            (find-file-noselect file)
                                          (elmenu--buffer)))
                       (cons sym
                             (list
                              :start beg
                              :type type)))))
                  ('provide
                   (cons sym
                         (list
                          :start beg
                          :type type)))
                  ((guard
                    (assq type elmenu-func-types))
                   (cons sym
                         (list
                          :type type
                          :start beg
                          :args (seq-find 'proper-list-p item)
                          :doc doc
                          :interactive
                          (when (assq type elmenu-interactive-types)
                            (eq 'interactive
                                (if doc
                                    (car-safe
                                     (car-safe
                                      (cdr-safe
                                       (member doc
                                               item))))
                                  (car-safe (nth
                                             (cdr
                                              (assq type
                                                    elmenu-interactive-types))
                                             item)))))
                          :autoload
                          (save-excursion
                            (forward-line -1)
                            (when (looking-at ";;;###")
                              (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)))))))
                  ((guard (assq type (append
                                      elmenu-custom-types
                                      elmenu-var-types)))
                   (cons sym
                         (list
                          :type type
                          :doc doc
                          :start beg
                          :autoload
                          (save-excursion
                            (forward-line -1)
                            (when (looking-at
                                   ";;;###")
                              (buffer-substring-no-properties
                               (line-beginning-position)
                               (line-end-position)))))))
                  ((guard (assq type elmenu-modes-types))
                   (cons sym (list
                              :type type
                              :start beg
                              :doc doc
                              :autoload
                              (save-excursion
                                (forward-line -1)
                                (when (looking-at
                                       ";;;###")
                                  (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position)))))))
                  ((guard (assq type elmenu-def-type-poses))
                   (cons sym (list
                              :doc doc
                              :type type
                              :start beg)))
                  ((or 'add-hook 'remove-hook)
                   (cons sym (list
                              :type type
                              :start beg)))))
          (if (and (car-safe cell)
                   (symbolp (car cell)))
              (list cell)
            cell))))))

(defmacro elmenu-with-every-top-form (&rest body)
  "Bind VARS and eval BODY in current buffer on every top level form."
  (declare (indent 1)
           (debug t))
  `(save-excursion
     (save-restriction
       (widen)
       (goto-char (point-max))
       (while (and (elmenu-backward-list)
                   (looking-at "[(]"))
         (save-excursion
           ,@body)))))

(defun elmenu--buffer ()
  "Scan current buffer."
  (let ((items))
    (elmenu-with-every-top-form
        (setq items (nconc items (elmenu-parse-sexp-at-point))))
    (nreverse items)))

(defvar-local elmenu-cached-items nil)
(defvar elmenu-cached-libs nil)
(defvar-local elmenu-cached-items-buffer-tick nil
  "Buffer modified tick.")

(defun elmenu-local-vars ()
  "Return local variables."
  (when-let ((vars (elisp--local-variables)))
    (mapcar (lambda (it)
              `(,it :type local))
            (elisp--local-variables))))

(defun elmenu-cached-or-rescan ()
  "Find items in buffer."
  (let ((tick (buffer-modified-tick)))
    (if (eq elmenu-cached-items-buffer-tick tick)
        elmenu-cached-items
      (setq elmenu-cached-items-buffer-tick tick)
      (setq elmenu-cached-items
            (elmenu--buffer)))))

(defun elmenu-buffer ()
  "Find items in buffer."
  (setq elmenu-visited-syms nil)
  (when-let ((name (save-excursion
                     (goto-char (point-max))
                     (cond ((re-search-backward
                             (rx "(provide '"
                                 (group (1+ (or (syntax
                                                 word)
                                                (syntax
                                                 symbol)))))
                             nil t)
                            (match-string-no-properties 1))
                           ((re-search-backward
                             "(provide-me)" nil t)
                            (file-name-sans-extension
                             (file-name-nondirectory
                              buffer-file-name)))))))
    (push (intern name) elmenu-visited-syms)
    (elmenu--buffer)))

(defun elmenu-plist-remove-nils (plist)
  "Return the keys in PLIST."
  (let* ((result (list 'head))
         (last result))
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (new (and val (list key val))))
        (when new
          (setcdr last new)
          (setq last (cdr new)))))
    (cdr result)))
(defun elmenu-minibuffer-get-metadata ()
  "Return current minibuffer completion metadata."
  (completion-metadata
   (buffer-substring-no-properties
    (minibuffer-prompt-end)
    (max (minibuffer-prompt-end)
         (point)))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun elmenu-minibuffer-ivy-selected-cand ()
  "Return the currently selected item in Ivy."
  (when (and (memq 'ivy--queue-exhibit post-command-hook)
             (boundp 'ivy-text)
             (boundp 'ivy--length)
             (boundp 'ivy-last)
             (fboundp 'ivy--expand-file-name)
             (fboundp 'ivy-state-current))
    (cons
     (completion-metadata-get (ignore-errors
                                (elmenu-minibuffer-get-metadata))
                              'category)
     (ivy--expand-file-name
      (if (and (> ivy--length 0)
               (stringp (ivy-state-current ivy-last)))
          (ivy-state-current ivy-last)
        ivy-text)))))

(defun elmenu-minibuffer-minibuffer-auto-default-candidates ()
  "Return all current completion candidates from the minibuffer."
  (when (minibufferp)
    (let* ((all (completion-all-completions
                 (minibuffer-contents)
                 minibuffer-completion-table
                 minibuffer-completion-predicate
                 (max 0 (- (point)
                           (minibuffer-prompt-end)))))
           (last (last all)))
      (when last (setcdr last nil))
      (cons
       (completion-metadata-get (elmenu-minibuffer-get-metadata) 'category)
       all))))

(defun elmenu-minibuffer-default-top-minibuffer-completion ()
  "Target the top completion candidate in the minibuffer.
Return the category metadatum as the type of the target.

This target finder is meant for the default completion UI and
completion UI highly compatible with it, like Icomplete."
  (when (and (minibufferp) minibuffer-completion-table)
    (pcase-let* ((`(,category . ,candidates)
                  (elmenu-minibuffer-minibuffer-auto-default-candidates))
                 (contents (minibuffer-contents))
                 (top (if (test-completion contents
                                           minibuffer-completion-table
                                           minibuffer-completion-predicate)
                          contents
                        (let ((completions (completion-all-sorted-completions)))
                          (if (null completions)
                              contents
                            (concat
                             (substring contents
                                        0 (or (cdr (last completions)) 0))
                             (car completions)))))))
      (cons category (or (car (member top candidates)) top)))))

(defvar elmenu-minibuffer-targets-finders
  '(elmenu-minibuffer-ivy-selected-cand
    elmenu-minibuffer-default-top-minibuffer-completion))


;;;###autoload
(defun elmenu-insert-and-exit ()
  "Exit minibuffer and insert selection."
  (interactive)
  (pcase-let ((`(,_category . ,current)
               (elmenu-minibuffer-get-current-candidate)))
    (with-minibuffer-selected-window
      (apply #'insert
             (if-let ((current-word
                       (symbol-at-point)))
                 (progn
                   (if
                       (string-prefix-p
                        (symbol-name
                         current-word)
                        current)
                       (list
                        (substring-no-properties
                         current
                         (length
                          (symbol-name
                           current-word))))
                     (list "\s" current)))
               (list current))))
    (abort-minibuffers)))

;;;###autoload
(defun elmenu-insert-no-exit ()
  "Exit minibuffer and insert selection."
  (interactive)
  (pcase-let ((`(,_category . ,current)
               (elmenu-minibuffer-get-current-candidate)))
    (with-minibuffer-selected-window
      (when (symbol-at-point)
        (newline-and-indent))
      (insert current))))

;;;###autoload
(defun elmenu-minibuffer-jump-to-item ()
  "Call ACTION with minibuffer candidate in its original window."
  (interactive)
  (pcase-let ((`(,_category . ,current)
               (elmenu-minibuffer-get-current-candidate)))
    (with-minibuffer-selected-window
      (when-let ((cell (or (assq (intern-soft current)
                                 (elmenu-buffer))
                           (assq (intern-soft current)
                                 (elmenu-all-buffers)))))
        (elmenu-buffer-jump-to-form cell)))))

(defun elmenu-minibuffer-get-current-candidate ()
  "Return cons filename for current completion candidate."
  (let (target)
    (run-hook-wrapped
     'elmenu-minibuffer-targets-finders
     (lambda (fun)
       (when-let ((result (funcall fun)))
         (when (and (cdr-safe result)
                    (stringp (cdr-safe result))
                    (not (string-empty-p (cdr-safe result))))
           (setq target result)))
       (and target (minibufferp))))
    target))



(defvar elmenu-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j")
                #'elmenu-minibuffer-jump-to-item)
    (define-key map (kbd "C-c TAB")
                #'elmenu-insert-and-exit)
    (define-key map (kbd "C-i")
                #'elmenu-insert-no-exit)
    map))

(defun elmenu-overlay-unset-and-remove (var-symbol)
  "Remove overlay from VAR-SYMBOL value."
  (when (overlayp (symbol-value var-symbol))
    (delete-overlay (symbol-value var-symbol)))
  (set var-symbol nil))

(defvar elmenu--overlay nil)
(defun elmenu-overlay-make (start end &optional buffer front-advance
                                      rear-advance &rest props)
  "Create a new overlay with range BEG to END in BUFFER and return it.
If omitted, BUFFER defaults to the current buffer.
START and END may be integers or markers.
The fourth arg FRONT-ADVANCE, if non-nil, makes the marker
for the front of the overlay advance when text is inserted there
\(which means the text *is not* included in the overlay).
The fifth arg REAR-ADVANCE, if non-nil, makes the marker
for the rear of the overlay advance when text is inserted there
\(which means the text *is* included in the overlay).
PROPS is a plist to put on overlay."
  (let ((overlay (make-overlay start end buffer front-advance
                               rear-advance)))
    (dotimes (idx (length props))
      (when (eq (logand idx 1) 0)
        (let* ((prop-name (nth idx props))
               (val (plist-get props prop-name)))
          (overlay-put overlay prop-name val))))
    overlay))

(defun elmenu-overlay-set (var-symbol start end &optional buffer
                                          front-advance rear-advance &rest
                                          props)
  "Create a new overlay and set value of VAR-SYMBOL to it.
If omitted, BUFFER defaults to the current buffer.
START and END may be integers or markers.
The fourth arg FRONT-ADVANCE, if non-nil, makes the marker
for the front of the overlay advance when text is inserted there
\(which means the text *is not* included in the overlay).
The fifth arg REAR-ADVANCE, if non-nil, makes the marker
for the rear of the overlay advance when text is inserted there
\(which means the text *is* included in the overlay).
PROPS is a plist to put on overlay."
  (elmenu-overlay-unset-and-remove var-symbol)
  (set var-symbol (apply #'elmenu-overlay-make
                         (append (list start end
                                       buffer front-advance
                                       rear-advance)
                                 props))))
(defun elmenu-highlight-sexp-at-point ()
  "Highlight region between START and END with FACE."
  (pcase-let ((`(,start . ,end)
               (bounds-of-thing-at-point 'sexp)))
    (when (and start end)
      (elmenu-overlay-highlight-region start end))))

(defun elmenu-overlay-highlight-region (start end &optional face)
  "Highlight region between START and END with FACE."
  (elmenu-overlay-set 'elmenu--overlay start end nil nil nil 'face
                          (or face
                              'success))
  (unwind-protect (read-key-sequence "")
    (setq unread-command-events
          (append (this-single-command-raw-keys)
                  unread-command-events))
    (elmenu-overlay-unset-and-remove 'elmenu--overlay)))

(defun elmenu-annotate-fn (alist)
  "Make annotation function from ALIST."
  (let* ((names (remove nil
                        (mapcar (lambda (it)
                                  (if (symbolp (car-safe it))
                                      (length (symbol-name (car it)))
                                    (if (stringp (car-safe it))
                                        (car-safe it)
                                      nil)))
                                alist)))
         (max-len (if names (1+ (apply #'max names))
                    1)))
    (lambda (str)
      (let* ((sym (if (stringp str)
                      (intern-soft str)
                    str))
             (pl (cdr (assq
                       sym
                       alist)))
             (type (propertize
                    (capitalize
                     (format "%s"
                             (plist-get pl
                                        :type)))
                    'face
                    'font-lock-keyword-face))
             (args (propertize (elmenu-format-args (plist-get pl :args))
                               'face
                               'font-lock-operator-face))
             (lib (if (plist-get pl :lib)
                      (format "%s"  (plist-get pl :lib))
                    ""))
             (interactivep
              (and (plist-get pl :interactive)
                   (propertize "Command" 'face
                               'diff-refine-added)))
             (autloaded (and (plist-get pl :autoload)
                             (propertize
                              "AUTOLOAD"
                              'face 'font-lock-warning-face)))
             (final
              (remove nil
                      (seq-filter 'stringp (list
                                            (if interactivep
                                                (concat interactivep " " type)
                                              type)
                                            args
                                            autloaded
                                            lib
                                            (and (if (stringp (plist-get pl :doc))
                                                     (car (split-string (plist-get
                                                                         pl :doc)
                                                                        "\n" t))
                                                   nil)))))))
        (concat
         (propertize " " 'display `(space :align-to ,max-len))
         (string-join
          final
          " "))))))


(defun elmenu-all-buffers ()
  "Scan all elisp buffers."
  (let ((items))
    (dolist (buff (buffer-list))
      (when (and (memq (buffer-local-value 'major-mode buff)
                       '(emacs-lisp-mode))
                 (buffer-file-name buff))
        (with-current-buffer buff
          (setq items (append items (ignore-errors (elmenu-buffer)))))))
    items))

(defun elmenu-jump-completing-read (prompt alist)
  "Read ALIST of elmenu items in minibuffer with PROMPT."
  (let* ((alist (copy-tree alist))
         (annotf (elmenu-annotate-fn alist))
         (category 'symbol))
    (minibuffer-with-setup-hook
        (lambda ()
          (when (minibufferp)
            (let ((map (make-composed-keymap elmenu-minibuffer-map
                                             (current-local-map))))
              (use-local-map map))))
      (let ((item (completing-read prompt
                                   (lambda (str pred action)
                                     (if (eq action 'metadata)
                                         `(metadata
                                           (annotation-function . ,annotf)
                                           (category . ,category))
                                       (complete-with-action action alist
                                                             str pred))))))
        (or (assq
             (intern-soft item)
             alist)
            item)))))

(defun elmenu-jump-completing-read-all (&optional prompt)
  "Read ALIST of elmenu items from all buffers with PROMPT."
  (elmenu-jump-completing-read (or prompt "Symbol: ")
                               (append
                                (elmenu-local-vars)
                                (elmenu-all-buffers))))

(defun elmenu-jump-completing-read-from-current-buffer (&optional prompt)
  "Read ALIST of elmenu items from current buffer in minibuffer with PROMPT."
  (elmenu-jump-completing-read (or prompt "Symbol: ")
                               (append
                                (elmenu-local-vars)
                                (elmenu-buffer))))

(defvar elmenu-multi-source--sources-list nil
  "Normalized sources.")

(defvar elmenu-multi-source-last-input nil
  "Last typed input in minibuffer.")

(defvar elmenu-multi-source--current-index nil
  "Index of active source.")

(defun elmenu-multi-source-switcher (step current-index switch-list)
  "Increase or decrease CURRENT-INDEX depending on STEP value and SWITCH-LIST."
  (cond ((> step 0)
         (if (>= (+ step current-index)
                 (length switch-list))
             0
           (+ step current-index)))
        ((< step 0)
         (if (or (<= 0 (+ step current-index)))
             (+ step current-index)
           (1- (length switch-list))))
        (t current-index)))

(defun elmenu-multi-source-set-last-input ()
  "Save last typed input in mininubbfer."
  (when (minibufferp)
    (setq elmenu-multi-source-last-input
          (buffer-substring (minibuffer-prompt-end)
                            (point)))))

(defvar elmenu-multi-source-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.") 'multi-source-select-next)
    (define-key map (kbd "C-.") 'multi-source-read-source)
    map)
  "Keymap to use in minibuffer.")

(defun elmenu-multi-source-map-sources (sources)
  "Normalize SOURCES to list of functions, labels and arguments."
  (let ((curr)
        (labels)
        (args)
        (fns))
    (while (setq curr (pop sources))
      (pcase curr
        ((pred stringp)
         (let ((fn (pop sources)))
           (push curr labels)
           (push fn fns)
           (push nil args)))
        ((pred functionp)
         (let ((label
                (if (symbolp curr)
                    (symbol-name curr)
                  "")))
           (push label labels)
           (push curr fns)
           (push nil args)))
        ((pred listp)
         (let* ((label (car curr))
                (rest (cdr curr))
                (fn (if (listp rest)
                        (car rest)
                      rest))
                (extra-args (when (listp rest)
                              (cdr rest))))
           (push label labels)
           (push (if (or (functionp fn)
                         (symbolp fn))
                     fn
                   `(lambda () ,fn))
                 fns)
           (push extra-args args)))))
    (list (reverse fns)
          (reverse labels)
          (reverse args))))

(defun elmenu-multi-source-read (sources)
  "Combine minibuffer SOURCES into a command with several alternatives.

Every alternative should be a function that reads data from minibuffer.

By default the first source is called and user can switch between
alternatives dynamically with commands:

 `multi-source-select-next' (bound to \\<multi-source-minibuffer-map>\
`\\[multi-source-select-next]') - select next alternative.
 `multi-source-select-prev' (bound to \\<multi-source-minibuffer-map>\
`\\[multi-source-select-prev]') - select previus alternative.
 `multi-source-read-source' (bound to \\<multi-source-minibuffer-map>\
`\\[multi-source-read-source]') - select from completions list.

Allowed forms for SOURCES are
 - a list of functions
 - a plist of backend's name and corresponding function,
-  an alist of backend's name, corresponding function and optionally extra
 arguments to pass."
  (setq elmenu-multi-source--sources-list (elmenu-multi-source-map-sources
                                           sources))
  (setq elmenu-multi-source--current-index 0)
  (setq elmenu-multi-source-last-input nil)
  (let ((curr)
        (fns (nth 0 elmenu-multi-source--sources-list))
        (args (nth 2 elmenu-multi-source--sources-list)))
    (while (numberp
            (setq curr
                  (catch 'next
                    (minibuffer-with-setup-hook
                        (lambda ()
                          (use-local-map
                           (make-composed-keymap
                            elmenu-multi-source-minibuffer-map
                            (current-local-map)))
                          (when (minibuffer-window-active-p
                                 (selected-window))
                            (when (and elmenu-multi-source-restore-last-input
                                       elmenu-multi-source-last-input)
                              (insert
                               elmenu-multi-source-last-input))
                            (add-hook
                             'post-command-hook
                             #'elmenu-multi-source-set-last-input
                             nil t)
                            (add-hook
                             'minibuffer-exit-hook
                             #'elmenu-multi-source-set-last-input
                             nil t)
                            (add-hook
                             'post-self-insert-hook
                             #'elmenu-multi-source-set-last-input
                             nil t)))
                      (apply (nth elmenu-multi-source--current-index fns)
                             (nth elmenu-multi-source--current-index args))))))
      (setq elmenu-multi-source--current-index
            (elmenu-multi-source-switcher curr
                                          elmenu-multi-source--current-index
                                          fns)))
    (setq elmenu-multi-source-last-input nil)
    (setq elmenu-multi-source--sources-list nil)
    curr))


;;;###autoload
(defun elmenu-jump (&optional arg)
  "Scan buffer and jump to item.
With optional argument ARG extract items from all buffers."
  (interactive "P")
  (let* ((sources `((,(buffer-name)
                     elmenu-jump-completing-read-from-current-buffer)
                    ("All" elmenu-jump-completing-read-all
                     "Symbol (all buffers): ")))
         (cell (elmenu-multi-source-read (if (not arg)
                                             sources
                                           (reverse sources)))))
    (elmenu-buffer-jump-to-form cell)))

(defun elmenu-read-buffer-items ()
  "Read items in buffer."
  (elmenu-jump-completing-read "Symbol: "
                               (append
                                (elmenu-local-vars)
                                (elmenu-buffer))))


;;;###autoload
(defun elmenu-insert (&optional arg)
  "Complete item at point.
With optional argument ARG extract items from all buffers."
  (interactive "P")
  (let* ((sources `(("All" elmenu-jump-completing-read-all
                     "Symbol (all buffers): ")
                    (,(buffer-name)
                     elmenu-jump-completing-read
                     "Symbol: "
                     ,(append
                       (elmenu-local-vars)
                       (elmenu-buffer)))))
         (cell (elmenu-multi-source-read (if (not arg)
                                             sources
                                           (reverse sources))))
         (current (symbol-name (car cell))))
    (apply #'insert
           (if-let ((current-word
                     (symbol-at-point)))
               (progn
                 (if
                     (string-prefix-p
                      (symbol-name
                       current-word)
                      cell)
                     (list
                      (substring-no-properties
                       current
                       (length
                        (symbol-name
                         current-word))))
                   (list "\s" current)))
             (list current)))))

(provide 'elmenu)
;;; elmenu.el ends here
