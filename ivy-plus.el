;; ivy-plus.el --- New counsel commands or enhancement to some existing counsel commands.  -*- lexical-binding: t; -*-

;; Filename: ivy-plus.el
;; Description: New counsel commands or enhancement to some existing counsel commands.
;; Author:  zbelial <zjyzhaojiyang@gmail.com>
;; Maintainer:  zbelial <zjyzhaojiyang@gmail.com>
;; Copyright (C) 2021, zbelial, all rights reserved.
;; Created: 2021-06-03 14:08:12
;; Version: 0.1
;; URL: https://github.com/zbelial/ivy-plus.el
;; Package-Requires: ((ivy "0.13.0") (pinyinlib "0.1.1") (pyim "3.7.9"))
;; Keywords:
;; Compatibility: GNU Emacs 27.1
;;
;; Features that might be required by this library:
;;
;; Please check README
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

(require 'ivy)
(require 'counsel)
(require 'pyim)
(require 'pinyinlib)
(require 'heap)
(require 'dash)
(require 'flymake)
(require 'cl-lib)
(require 'pulse)
(eval-when-compile
  (require 'cl-macs))

(defgroup ivy-plus nil
  "New counsel commands or enhancement to some existing counsel commands."
  :prefix "ivy-plus-" :group 'ivy)

(defun ivy-plus-goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun ivy-regex-pyim (str)
  (let ((x (ivy--regex-plus str))
	(case-fold-search nil))
    (if (listp x)
        (mapcar (lambda (y)
		  (if (cdr y)
		      (list (if (equal (car y) "")
			        ""
			      (pyim-cregexp-build (car y)))
			    (cdr y))
		    (list (pyim-cregexp-build (car y)))))
	        x)
      (pyim-cregexp-build x))))

(defun ivy--pinyinlib-build-regexp-string (str)
  (progn
    (cond ((equal str ".*")
	   ".*")
	  (t
	   (pinyinlib-build-regexp-string str t)))))

(defun ivy--pinyin-regexp-helper (str)
  (cond ((equal str " ")
	 ".*")
	((equal str "")
	 nil)
	(t
	 str)))

(defun ivy--pinyin-to-utf8 (str)
  (cond ((equal 0 (length str))
	 nil)
	(t
	 (mapconcat 'ivy--pinyinlib-build-regexp-string
		    (remove nil (mapcar 'ivy--pinyin-regexp-helper (split-string str "")))
		    ""))
	nil))

(defun ivy-regex-pinyinlib (str)
  (or (ivy--pinyin-to-utf8 str)
      (ivy--regex-plus str)
      (ivy--regex-ignore-order str)))


(defvar ivy-switch-buffer+-obuf nil)
(defun ivy-switch-buffer+-update-fn ()
  (let* ((current (ivy-state-current ivy-last)))
    (with-ivy-window
      (if (get-buffer current)
	  (set-window-buffer (selected-window) current)
        (set-window-buffer (selected-window) ivy-switch-buffer+-obuf)))))

;;;###autoload
(defun ivy-switch-buffer+ (&optional initial-input)
  "Switch to another buffer."
  (interactive)
  (setq ivy-switch-buffer+-obuf (current-buffer))

  (let (res)
    (unwind-protect
        (progn
	  (setq res (ivy-read "Switch to buffer: " #'internal-complete-buffer
			      :keymap ivy-switch-buffer-map
			      :preselect (buffer-name (other-buffer (current-buffer)))
			      :action #'ivy--switch-buffer-action
			      :update-fn #'ivy-switch-buffer+-update-fn
			      :matcher #'ivy--switch-buffer-matcher
			      :caller 'ivy-switch-buffer
			      :initial-input initial-input)))
      (unless res
	(switch-to-buffer ivy-switch-buffer+-obuf t)
	(setq ivy-switch-buffer+-obuf nil)))))
(ivy-configure 'ivy-switch-buffer+
  :parent 'internal-complete-buffer
  :occur #'ivy-switch-buffer-occur)

;;;###autoload
(defun ivy-switch-buffer+-same-mode (&optional initial-input)
  "Switch to another buffer."
  (interactive)
  (setq ivy-switch-buffer+-obuf (current-buffer))

  (let ((mode (with-current-buffer ivy-switch-buffer+-obuf
                major-mode))
        buffers
        res)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eq major-mode mode)
          (cl-pushnew buffer buffers))))
    (unwind-protect
        (progn
          (setq buffers (mapcar (lambda (r) (buffer-name r)) (nreverse buffers)))
          
	  (setq res (ivy-read "Switch to buffer: " buffers
			      :keymap ivy-switch-buffer-map
			      :preselect 1
			      :action #'ivy--switch-buffer-action
			      :update-fn #'ivy-switch-buffer+-update-fn
			      :matcher #'ivy--switch-buffer-matcher
			      :caller 'ivy-switch-buffer
			      :initial-input initial-input)))
      (unless res
	(switch-to-buffer ivy-switch-buffer+-obuf t)
	(setq ivy-switch-buffer+-obuf nil)))))

(defvar counsel-imenu+-opoint nil)
(defun counsel-imenu+-update-fn ()
  (let ((current (ivy-state-current ivy-last))
	item
	pos)
    (with-ivy-window
      (when (not (string-empty-p current))
        (setq item (nth (get-text-property 0 'idx current) (ivy-state-collection ivy-last)))
        (setq pos (cddr item))
        (goto-char pos)
        (recenter)
        (let ((pulse-delay 0.05))
	  (pulse-momentary-highlight-one-line (point))
	  )))))

(defun counsel-imenu+-action (x)
  (with-ivy-window
    (imenu (cdr x))
    (recenter)))

;;;###autoload
(defun counsel-imenu+ ()
  "Jump to a buffer position indexed by imenu."
  (interactive)
  (setq counsel-imenu+-opoint (point))

  (let* ((imenu-create-index-function
          (cond
           ((eq major-mode 'python-mode)
            #'python-imenu-create-index)
           (t
            #'imenu-default-create-index-function)))
         (items (counsel--imenu-candidates))
         (preselect 0)
         (current-pos (point))
         (idx -1)
         (min (buffer-size))
         pos
         res)
    (dolist (item items)
      (setq idx (1+ idx))
      (let ((marker (cddr item)))
        (when marker
	  (setq pos (marker-position marker))
	  (when (and (<= pos current-pos) (< (- current-pos pos) min))
	    (setq min (- current-pos pos))
	    (setq preselect idx)))))

    (unwind-protect
        (and 
         (setq res (ivy-read "imenu items: " items
        		     :preselect preselect
        		     :require-match t
        		     :action #'counsel-imenu+-action
        		     :keymap counsel-imenu-map
        		     :history 'counsel-imenu-history
        		     :update-fn #'counsel-imenu+-update-fn
        		     :caller 'counsel-imenu+))
         (point))
      (unless res
        (when counsel-imenu+-opoint
          (goto-char counsel-imenu+-opoint))))))

(defvar counsel-outline+-opoint nil)

(defun counsel-outline+-update-fn ()
  (let ((current (ivy-state-current ivy-last))
	item
	marker)
    (with-ivy-window
      (when (not (string-empty-p current))
        (setq item (nth (get-text-property 0 'idx current) (ivy-state-collection ivy-last)))
        (setq marker (cdr item))
        (goto-char marker)
        (recenter)
        (let ((pulse-delay 0.05))
	  (pulse-momentary-highlight-one-line (point)))))))

(defun counsel-outline+-action (x)
  "Go to outline X."
  (goto-char (cdr x))
  (recenter))

;;;###autoload
(defun counsel-outline+ ()
  "Jump to an outline heading with completion."
  (interactive)
  (setq counsel-outline+-opoint (point))

  (let ((settings (cdr (assq major-mode counsel-outline-settings)))
	res)
    (unwind-protect
        (and
	 (setq res (ivy-read "Outline: " (counsel-outline-candidates settings)
			     :action (or (plist-get settings :action)
					 #'counsel-outline+-action)
			     :history (or (plist-get settings :history)
					  'counsel-outline-history)
			     :preselect (max (1- counsel-outline--preselect) 0)
			     :update-fn #'counsel-outline+-update-fn
			     :caller (or (plist-get settings :caller)
					 'counsel-outline+)))
	 (point))
      (unless res
	(goto-char counsel-outline+-opoint)))))

(defun counsel-rg+-remove-boundaries (text)
  (when (string-prefix-p "\\_<" text)
    (setq text (substring text 3)))
  (when (string-suffix-p "\\_>" text)
    (setq text (substring text 0 (- (length text) 3))))
  text)

;;;###autoload
(defun counsel-rg+ (&optional initial-input initial-directory extra-rg-args rg-prompt)
  "Search upwards in the directory tree."
  (interactive)
  (let ((text (or ivy-text initial-input)))
    (ivy-quit-and-run
      (counsel-rg (counsel-rg+-remove-boundaries text) initial-directory extra-rg-args rg-prompt))))

(defvar ivy-plus--swiper-flag 0)
(defun swiper-before-set-flag (&optional initial-input)
  (setq ivy-plus--swiper-flag 1))
(advice-add #'swiper :before #'swiper-before-set-flag)
(advice-add #'swiper-isearch :before #'swiper-before-set-flag)

;;;###autoload
(defun counsel-rg-upwards (&optional initial-input initial-directory extra-rg-args rg-prompt)
  "Search upwards in the directory tree."
  (interactive)
  (let ((dir (file-name-directory (directory-file-name (or initial-directory default-directory))))
	(text (or ivy-text initial-input)))
    (when (= ivy-plus--swiper-flag 1)
      (setq dir default-directory)
      (setq ivy-plus--swiper-flag 0))
    (ivy-quit-and-run
      (counsel-rg (counsel-rg+-remove-boundaries text) dir extra-rg-args rg-prompt))))

(defcustom counsel-frequent-buffer-limit 10
  "How many buffers displayed when switching."
  :type 'integer
  :group 'ivy)

(defcustom counsel-frequent-buffer-auto-fallback nil
  "Whether automatically call `counsel-frequent-buffer-fallback' when no match."
  :type 'boolean
  :group 'ivy)

(cl-defstruct buf-freq "记录buffer访问次数"
	      bname count bfile-name)

(defun counsel-frequent-buffer--compare (a b)
  (let ((count-a (buf-freq-count a))
	(count-b (buf-freq-count b)))
    (> count-a count-b)))
;; (counsel-frequent-buffer--compare  (list "a" 1) (list "b" 3))

(defvar counsel-frequent-buffer--frequency (make-heap #'counsel-frequent-buffer--compare))
(defvar counsel-frequent-buffer--visited-count (make-hash-table :test #'equal))
(defvar counsel-frequent-buffer--current (make-buf-freq))

(defun counsel-frequent-buffer--match-function (record)
  (let ((current-key (buf-freq-bfile-name counsel-frequent-buffer--current))
	(heap-key (buf-freq-bfile-name record)))
    (and current-key (string-equal current-key heap-key))))

(defun counsel-frequent-buffer--visit-buffer (&optional arg)
  (let ((buffer (window-buffer))
	(count 0)
	(total-count (heap-size counsel-frequent-buffer--frequency))
	root
	bname
	bfile-name)
    (unless (minibufferp buffer)
      (setq bname (buffer-name buffer))
      (setq bfile-name (or (buffer-file-name buffer) bname))

      ;; (message "bname %s, bfile-name %s" bname bfile-name)

      (setq count (1+ (gethash bfile-name counsel-frequent-buffer--visited-count 0)))
      ;; 最近访问的buffer后续还有可能会被访问，所以提升其count到比访问数最大的buffer(非自身的话)的count大1点。
      (setq root (heap-root counsel-frequent-buffer--frequency))
      (when root
	(setq count (1+ (buf-freq-count root))))
      (puthash bfile-name count counsel-frequent-buffer--visited-count)

      (setq counsel-frequent-buffer--current (make-buf-freq :bname bname :count count :bfile-name bfile-name))

      (when (not (heap-modify counsel-frequent-buffer--frequency #'counsel-frequent-buffer--match-function counsel-frequent-buffer--current))
	(heap-add counsel-frequent-buffer--frequency counsel-frequent-buffer--current))

      ;; (message "heap %S" counsel-frequent-buffer--frequency)
      ;; (message "map %S" counsel-frequent-buffer--visited-count)
      )))

(defun counsel-frequent-buffer--kill-buffer ()
  (let ((buffer (current-buffer))
	(count 0)
	bname
	bfile-name)
    (setq bname (buffer-name buffer))
    (setq bfile-name (or (buffer-file-name buffer) bname))

    (remhash bfile-name counsel-frequent-buffer--visited-count)

    (setq counsel-frequent-buffer--current (make-buf-freq :bname bname :count count :bfile-name bfile-name))
    (heap-modify counsel-frequent-buffer--frequency #'counsel-frequent-buffer--match-function counsel-frequent-buffer--current)))

(add-hook 'window-buffer-change-functions #'counsel-frequent-buffer--visit-buffer)
;; (remove-hook 'window-buffer-change-functions #'counsel-frequent-buffer--visit-buffer)

(add-hook 'find-file-hook #'counsel-frequent-buffer--visit-buffer)
;; (remove-hook 'find-file-hook #'counsel-frequent-buffer--visit-buffer)

(add-hook 'kill-buffer-hook #'counsel-frequent-buffer--kill-buffer)
;; (remove-hook 'kill-buffer-hook #'counsel-frequent-buffer--kill-buffer)


(defvar counsel-frequent-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-s") 'counsel-frequent-buffer-fallback)
    map)
  "Keymap for `counsel-frequent-buffer'.")

;;;###autoload
(defun counsel-frequent-buffer-fallback ()
  (interactive)
  (let ((text (or ivy-text initial-input)))
    (ivy-quit-and-run
      (ivy-switch-buffer+ text))))

(defvar counsel-frequent-buffer-obuf nil)
(defvar counsel-frequent-buffer-tmp-selected-buf nil)
(defun counsel-frequent-buffer-update-fn ()
  ;; (message "counsel-frequent-buffer-update-fn %d" (time-convert nil 'integer))
  (let* ((current (ivy-state-current ivy-last))
	 item
	 buffer)
    ;; (message "current %s" current)
    (with-ivy-window
      (if (not (string-empty-p current))
	  (progn
	    (setq item (nth (get-text-property 0 'idx current) (ivy-state-collection ivy-last)))
	    (setq buffer (buf-freq-bname (cdr item)))
	    (when (or (null counsel-frequent-buffer-tmp-selected-buf)
		      (and
		       buffer
		       (not (eq buffer counsel-frequent-buffer-tmp-selected-buf))))
	      (setq counsel-frequent-buffer-tmp-selected-buf buffer)
	      (if (get-buffer buffer)
	          (set-window-buffer (selected-window) buffer)
	        (set-window-buffer (selected-window) counsel-frequent-buffer-obuf)
	        )))
        (when counsel-frequent-buffer-auto-fallback
          (counsel-frequent-buffer-fallback))))))

(defun counsel-frequent-buffer-action (s)
  (let ((bname (buf-freq-bname (cdr s)))
	(bfile-name (buf-freq-bfile-name (cdr s))))
    (if (bufferp bname)
        (switch-to-buffer bname)
      (cl-dolist (b (buffer-list))
	(with-current-buffer b
	  (when (and (buffer-file-name b)
		     (string-equal (buffer-file-name b) bfile-name))
	    (setq bname b)
	    (cl-return nil))))
      (switch-to-buffer bname))))

;;;###autoload
(defun counsel-frequent-buffer ()
  "Switch to frequently visited buffers."
  (interactive)
  (setq counsel-frequent-buffer-obuf (current-buffer))

  (let (
	(total-count (heap-size counsel-frequent-buffer--frequency))
	(buffers nil)
	(count 0)
	(cand-count 0)
        (copy-data (heap-copy counsel-frequent-buffer--frequency))
	record
        iter
        root)

    (while (and (< cand-count counsel-frequent-buffer-limit)
                (< count total-count))
      (setq root (heap-delete-root copy-data))
      (when (> (buf-freq-count root) 0)
        (setq buffers (cl-pushnew root buffers))
        (setq cand-count (1+ cand-count)))
      (setq count (1+ count)))
    (heap-clear copy-data)

    (setq buffers (mapcar (lambda (r) (cons (format "  %-48s    %s" (buf-freq-bname r) (buf-freq-bfile-name r)) r)) (nreverse buffers)))

    (let (res)
      (unwind-protect
	  (setq res (ivy-read "Frequent Visited: " buffers
			      :action #'counsel-frequent-buffer-action
			      :preselect 1
			      :keymap counsel-frequent-buffer-map
			      :update-fn #'counsel-frequent-buffer-update-fn
			      :caller #'counsel-frequent-buffer))          
	(unless res
	  (switch-to-buffer counsel-frequent-buffer-obuf t)
	  (setq counsel-frequent-buffer-obuf nil))))))

(defun counsel-flymake--format-type (type)
  (let (face
	display-type
	(type (symbol-name type)))
    (cond
     ((string-suffix-p "note" type)
      (setq display-type "note")
      (setq face 'success))
     ((string-suffix-p "warning" type)
      (setq display-type "warning")
      (setq face 'warning))
     ((string-suffix-p "error" type)
      (setq display-type "error")
      (setq face 'error))
     (t
      (setq display-type "note")
      (setq face 'warning)))
    (propertize (format "%s" display-type) 'face face)))

(defun counsel-flymake--formatter (diag)
  (let* (msg
	 (beg (flymake--diag-beg diag))
	 (end (flymake--diag-end diag))
	 (type (flymake--diag-type diag))
	 (text (flymake--diag-text diag))
	 (line (line-number-at-pos beg)))
    (setq msg (format "%-8d  %-12s    %s" line (counsel-flymake--format-type type) text))
    (cons msg (list :line line :type type :text text :beg beg :end end))))


(defun counsel-flymake--update-fn ()
  (let ((current (ivy-state-current ivy-last))
	item
	pos)
    (with-ivy-window
      (when (not (string-empty-p current))
        (setq item (nth (get-text-property 0 'idx current) (ivy-state-collection ivy-last)))
        (setq pos (plist-get (cdr item) :beg))
        (goto-char pos)
        (recenter)
        (let ((pulse-delay 0.05))
	  (pulse-momentary-highlight-one-line (point)))))))

(defvar counsel-flymake--opoint nil)
;;;###autoload
(defun counsel-flymake ()
  ""
  (interactive)
  (setq counsel-flymake--opoint (point))

  (let ((diagnostics (flymake-diagnostics))
	records
	res)
    (setq records (mapcar #'counsel-flymake--formatter diagnostics))
    (unwind-protect
        (setq res (ivy-read "Diagnostics: " records
			    :action '(1
				      ("v" (lambda (record)
					     (let ((diag (cdr record)))
					       (goto-char (plist-get diag :beg))
					       (recenter)
					       (let ((pulse-delay 0.05))
					         (pulse-momentary-highlight-one-line (point)))))))
			    :update-fn #'counsel-flymake--update-fn))
      (unless res
	(goto-char counsel-flymake--opoint)
	(setq counsel-flymake--opoint nil)))))


(defcustom counsel-el-definition-macro-names
  '(defun defun* cl-defun defmacro defmacro* cl-defmacro defcustom
          defvar defvar-local defconst defsubst defsubst* cl-defsubst)
  "Lists the function, macro and variable definition forms in Elisp.
Used when searching for usages across the whole buffer."
  :group 'ivy)

(cl-defstruct counsel-el-ref file line col identifier type form)

(defun counsel--beginning-of-defun ()
  "A safe version of `beginning-of-defun'.
Attempts to find an enclosing defun form first, rather than
relying on indentation."
  (or
   ;; Search for known defun form enclosing point.
   (cl-loop
    while (ignore-errors (backward-up-list) t)
    do (when (thing-at-point-looking-at
              (rx-to-string `(seq "(" (or ,@(-map 'symbol-name counsel-el-definition-macro-names)))))
         (cl-return (point))))
   ;; Fall back to using indentation.
   (ignore-errors
     (beginning-of-thing 'defun))))

(defun counsel--line-str ()
  "Return the contents of the current line."
  (buffer-substring (line-beginning-position)
                    (line-end-position)))

(cl-defun counsel--line-matches? (regex &optional (point (point)))
  "Non-nil if POINT is on a line that matches REGEX."
  (save-excursion
    (goto-char point)
    (s-matches? regex (counsel--line-str))))

(defun counsel--autoload-directive-exsts-above-defun? ()
  "Non-nil if the current defun is preceeded by an autoload directive."
  (save-excursion
    (counsel--beginning-of-defun)
    (forward-line -1)
    (counsel--line-matches? (rx bol (* space) ";;;###autoload" (* space) eol))))

(defun counsel--looking-at-string? ()
  "Return non-nil if point is inside a string."
  (save-excursion (nth 3 (syntax-ppss))))

(defun counsel--looking-at-comment? (&optional pos)
  "Non-nil if POS is on a comment."
  (save-excursion
    (nth 4 (syntax-ppss pos))))

(defun counsel--interactive-form-p (form)
  "Does FORM contain an (interactive) expression?"
  ;; (defun foo () x y ...) -> (x y ...)
  (let ((body (-drop 3 form)))
    ;; Ignore docstring, if present.
    (when (stringp (car body))
      (setq body (-drop 1 body)))

    (eq (car-safe (car body))
        'interactive)))

(defun counsel--def-name (definition-form)
  "Given a DEFINITION-FORM such as defvar/defun/..., return its name."
  (let* ((form-name (nth 1 definition-form)))
    (when (symbolp form-name)
      form-name)))

(defun counsel--def-find-usages (definition-form)
  "Find the usages for a given DEFINITION-FORM symbol.

Returns a list of conses, where the car is the line number and
the cdr is the usage form."
  (-when-let (sym (counsel--def-name definition-form))
    ;; Search the buffer for usages of `sym'. Remove the definition form
    ;; from the results.
    (let (acc)
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp
                (rx-to-string `(seq symbol-start ,(symbol-name sym) symbol-end))
                nil t)
          (-when-let (form (list-at-point))
            (unless (or (equal definition-form form)
                        (counsel--looking-at-comment?)
                        (counsel--looking-at-string?))
              ;; Add this usage to `acc', unless it is the original definition.
              (push (cons (line-number-at-pos) form) acc)))))
      (nreverse acc))))


(defun counsel--find-unused-defs (&optional buffer)
  "Return a list of all unused definitions in the buffer.
The result is a list of `counsel-el-ref'."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (save-excursion
        (let (acc)
          (goto-char (point-min))

          ;; Find definitions in this buffer.
          ;;
          ;; This will search the buffer for known defun forms. As a special
          ;; cases, forms with a preceding autoload directive are ignored.
          (while (search-forward-regexp
                  (rx-to-string `(seq "(" (or ,@(-map 'symbol-name counsel-el-definition-macro-names))
                                      symbol-end))
                  nil t)
            (unless (or (counsel--looking-at-string?)
                        (counsel--looking-at-comment?)
                        (counsel--autoload-directive-exsts-above-defun?))
              ;; Collect definitions that do not have usages.
              (-when-let* ((form (list-at-point))
                           (col  (save-excursion
                                   (counsel--beginning-of-defun)
                                   (current-column))))
                (unless (or
                         ;; Consider interactive forms to be used.
                         (counsel--interactive-form-p form)
                         (counsel--def-find-usages form))
                  (push
                   (make-counsel-el-ref :file (buffer-file-name)
                                        :line (line-number-at-pos)
                                        :col  col
                                        :type (car form)
                                        :identifier (nth 1 form)
                                        :form form)
                   acc)))))
          (nreverse acc))))))

(defun counsel-unused-definitions-update-fn ()
  (let ((current (ivy-state-current ivy-last))
	item
        line)
    (with-ivy-window
      (when (not (string-empty-p current))
        (setq item (nth (get-text-property 0 'idx current) (ivy-state-collection ivy-last)))
        (setq line (counsel-el-ref-line (cdr item)))
        (ivy-plus-goto-line line)
        (recenter)
        (let ((pulse-delay 0.05))
	  (pulse-momentary-highlight-one-line (point)))))))

(defvar counsel-unused-definitions-opoint nil)
;;;###autoload
(defun counsel-unused-definitions ()
  (interactive)

  (setq counsel-unused-definitions-opoint (point))
  
  (let ((unused (counsel--find-unused-defs))
        cands
        res)
    (setq cands (mapcar (lambda (u) (cons (format "%-6s   %-12s    %s" (counsel-el-ref-line u) (counsel-el-ref-type u) (counsel-el-ref-identifier u))
                                          u))
                        unused))

    (unwind-protect
        (setq res (ivy-read "Unused: " cands
                            :action (lambda (u) (ivy-plus-goto-line (counsel-el-ref-line (cdr u))))
                            :update-fn #'counsel-unused-definitions-update-fn
                            :caller #'counsel-unused-definitions))
      (unless res
	(goto-char counsel-unused-definitions-opoint)
	(setq counsel-unused-definitions-opoint nil)))))

(provide 'ivy-plus)
