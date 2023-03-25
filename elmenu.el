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
      (setq items (nconc items (elmenu-parse-at-point))))
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
                      (assq type elmenu-def-type-poses)))))
      (pcase type
        ('defvar-keymap (plist-get sexp :doc))
        ((guard (and pos
                     (eq type 'cl-defmethod)
                     (memq (nth 2 sexp) '(:around :after
                                                  :before))))
         (let ((value (nth (1+ pos) sexp)))
           (when (stringp value)
             value)))
        ((guard (and pos))
         (let ((value (nth pos sexp)))
           (when (stringp value)
             value)))))))

(defun elmenu-buffer-jump-to-form (type name)
  "Search for definition with TYPE and NAME."
  (let ((re (elmenu-make-re (symbol-name name)))
        (found))
    (save-excursion
      (goto-char (point-max))
      (while (and (not found)
                  (elmenu-re-search-backward re nil t))
        (let ((parse-sexp-ignore-comments t))
          (setq found (ignore-errors (forward-sexp -1)
                                     (when (eq type (symbol-at-point))
                                       (backward-up-list 1)
                                       (when-let ((sexp
                                                   (sexp-at-point)))
                                         (when (and (eq (car-safe sexp)
                                                        type)
                                                    (eq (nth 1 sexp)
                                                        name))
                                           (point)))))))))
    (when found
      (goto-char found)
      found)))

(defun elmenu-parse-at-point ()
  "Parse `sexp-at-point' at point."
  (let* ((item (elmenu-list-at-point))
         (type (car-safe item)))
    (when (and type
               (symbolp type)
               (not (nth 4 (syntax-ppss (point)))))
      (let ((doc (elmenu-get-doc-from-sexp item))
            (declaration (elmenu-sexp-declare-p item))
            (autoload-cookies
             (save-excursion
               (forward-line -1)
               (when (looking-at ";;;###")
                 (buffer-substring-no-properties (line-beginning-position)
                                                 (line-end-position))))))
        (pcase type
          ((guard declaration)
           (list declaration))
          ((or 'use-package 'use-package!)
           (let ((data (save-excursion
                         (elmenu-parse-sexp-from-backward)))
                 (v `(use-package ,(nth 1 item))))
             (if data
                 (push v data)
               (list v))))
          ((or 'with-eval-after-load 'eval-when-compile
               'eval-after-load
               'straight-use-package 'if 'progn
               'and
               'if-let 'when-let 'with-no-warnings
               'when 'unless 'eval-and-compile)
           (save-excursion
             (elmenu-parse-sexp-from-backward)))
          ((or 'setq 'setq-default)
           (remove nil (seq-map-indexed (lambda (v i)
                                          (if (eq (logand i 1) 0)
                                              (list type v)
                                            nil))
                                        (seq-drop item 1))))
          ((or 'require 'provide)
           (list (append (list (car-safe item)
                               (elmenu-unquote (nth 1 item)))
                         (seq-drop item 2))))
          ((guard
            (and (assq type elmenu-interactive-types)
                 (eq 'interactive (if doc
                                      (car-safe
                                       (car-safe
                                        (cdr-safe
                                         (member doc
                                                 item))))
                                    (nth
                                     (cdr
                                      (assq type
                                            elmenu-interactive-types))
                                     item)))))
           (list (list (car item)
                       (cadr item)
                       (caddr item)
                       doc
                       :interactive
                       :autoload autoload-cookies)))
          ((guard
            (assq type elmenu-non-defun-command-types))
           (list (append
                  (seq-take item
                            (cdr
                             (assq type
                                   elmenu-non-defun-command-types)))
                  (list doc :interactive
                        :autoload autoload-cookies))))
          ((guard (assq type (append
                              elmenu-custom-types
                              elmenu-var-types)))
           (let* ((i (cdr (assq type (append elmenu-custom-types
                                             elmenu-var-types))))
                  (value (seq-take item (1- i))))
             (list (nconc value (list doc :autoload autoload-cookies)))))
          ((guard (assq type elmenu-def-type-poses))
           (let ((value
                  (seq-take item (cdr (assq type elmenu-def-type-poses)))))
             (list (nconc value (list doc :autoload autoload-cookies)))))
          ((or 'add-hook 'remove-hook)
           (list item))
          ((guard (and (or (special-form-p type)
                           (macrop type))
                       (listp (cdr item))))
           (list (seq-take item 2))))))))

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
        (setq items (nconc items (elmenu-parse-at-point))))
    (nreverse items)))

(defvar-local elmenu-cached-items nil)
(defvar-local elmenu-cached-items-buffer-tick nil
  "Buffer modified tick.")

(defun elmenu-buffer ()
  "Elisp find items in buffer.
IN BUFFER"
  (let ((tick (buffer-modified-tick)))
    (if (and
         elmenu-cached-items-buffer-tick
         (eq elmenu-cached-items-buffer-tick tick)
         elmenu-cached-items)
        elmenu-cached-items
      (setq elmenu-cached-items-buffer-tick tick)
      (setq elmenu-cached-items
            (elmenu--buffer)))))

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
(defun elmenu-minibuffer-jump-to-item ()
  "Call ACTION with minibuffer candidate in its original window."
  (interactive)
  (pcase-let ((`(,_category . ,current)
               (elmenu-minibuffer-get-current-candidate)))
    (with-minibuffer-selected-window
      (when-let ((cell (assq (intern-soft current)
                             (elmenu-buffer-to-alist))))
        (elmenu-buffer-jump-to-form (plist-get (cdr cell) :type)
                                        (car cell))
        (elmenu-highlight-sexp-at-point)))))

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

(defun elmenu-buffer-to-alist ()
  "Elisp find items in buffer.
IN BUFFER"
  (mapcar (lambda (it)
            (let ((sym (cadr it))
                  (type (car it))
                  (args (nth 2 it))
                  (doc (elmenu-get-doc-from-sexp it))
                  (pl))
              (setq pl (list
                        :doc doc
                        :args (when (assq type elmenu-func-types)
                                args)
                        :interactive
                        (and (memq :interactive
                                   it)
                             t)
                        :type type
                        :autoload (car (seq-drop (memq :autoload it) 1))))
              (cons sym (elmenu-plist-remove-nils pl))))
          (elmenu-buffer)))


(defvar elmenu-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j")
                #'elmenu-minibuffer-jump-to-item)
    (define-key map (kbd "C-c TAB")
                #'elmenu-insert-and-exit)
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


(defun elmenu-jump-completing-read ()
  "Scan buffer and jump to item."
  (let* ((alist (elmenu-buffer-to-alist))
         (max-len (1+ (apply #'max (mapcar (lambda (it)
                                             (length (symbol-name (car it))))
                                           alist))))
         (annotf (lambda (str)
                   (let* ((pl (cdr (assq
                                    (intern-soft str)
                                    alist)))
                          (type (propertize
                                 (capitalize
                                  (format "%s"
                                          (plist-get pl
                                                     :type)))
                                 'face
                                 'font-lock-keyword-face))
                          (interactivep
                           (and (plist-get pl :interactive)
                                (propertize "Command" 'face
                                            'completions-annotations)))
                          (autloaded (and (plist-get pl :autoload)
                                          (propertize
                                           "AUTOLOAD"
                                           'face 'font-lock-warning-face))))
                     (concat
                      (propertize " " 'display `(space :align-to ,max-len))
                      (string-join
                       (remove nil
                               (list type
                                     interactivep
                                     autloaded
                                     (replace-regexp-in-string
                                      "[\n\r\f]" "\s"
                                      (or (plist-get pl
                                                     :doc)
                                          ""))))
                       " ")))))
         (category 'symbol))
    (minibuffer-with-setup-hook
        (lambda ()
          (when (minibufferp)
            (let ((map (make-composed-keymap elmenu-minibuffer-map
                                             (current-local-map))))
              (use-local-map map))))
      (assq
       (intern-soft (completing-read "Symbol: "
                                     (lambda (str pred action)
                                       (if (eq action 'metadata)
                                           `(metadata
                                             (annotation-function . ,annotf)
                                             (category . ,category))
                                         (complete-with-action action alist
                                                               str pred)))))
       alist))))


;;;###autoload
(defun elmenu-jump ()
  "Scan buffer and jump to item."
  (interactive)
  (let ((cell (elmenu-jump-completing-read)))
    (elmenu-buffer-jump-to-form (plist-get (cdr cell) :type)
                                (car cell))
    (elmenu-highlight-sexp-at-point)))

(provide 'elmenu)
;;; elmenu.el ends here
