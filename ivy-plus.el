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

(defgroup ivy-plus nil
  "New counsel commands or enhancement to some existing counsel commands."
  :prefix "ivy-plus-" :group 'ivy)

(defvar ivy-switch-buffer+-obuf nil)
(defun ivy-switch-buffer+-update-fn ()
  (with-ivy-window
    (let* ((current (ivy-state-current ivy-last))
           )
      (if (get-buffer current)
          (set-window-buffer (selected-window) current)
        (set-window-buffer (selected-window) ivy-switch-buffer+-obuf)
        )
      )))

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
                              :initial-input initial-input
                              ))
          )
      (unless res
        (switch-to-buffer ivy-switch-buffer+-obuf t)
        (setq ivy-switch-buffer+-obuf nil)
        )
      )))
(ivy-configure 'ivy-switch-buffer+
  :parent 'internal-complete-buffer
  :occur #'ivy-switch-buffer-occur)


(defvar counsel-imenu+-opoint nil)
(defun counsel-imenu+-update-fn ()
  (with-ivy-window
    (let ((current (ivy-state-current ivy-last))
          item
          marker
          )
      (when (not (string-empty-p current))
        (setq item (nth (get-text-property 0 'idx current) (ivy-state-collection ivy-last)))
        (setq marker (cddr item))
        (goto-char marker)
        (recenter)
        (let ((pulse-delay 0.75))
          (pulse-momentary-highlight-one-line (point))
          )
        )
      )))
(defun counsel-imenu+-action (x)
  (with-ivy-window
    (imenu (cdr x))
    (recenter)))

;;;###autoload
(defun counsel-imenu+ ()
  "Jump to a buffer position indexed by imenu."
  (interactive)
  (setq counsel-imenu+-opoint (point))

  (let ((items (counsel--imenu-candidates))
        (preselect 0)
        (current-pos (point))
        (idx -1)
        (min (buffer-size))
        res)
    (dolist (item items)
      (setq idx (1+ idx))
      (let ((marker (cddr item))
            marker-pos)
        (when marker
          (setq marker-pos (marker-position marker))
          (when (and (<= marker-pos current-pos) (< (- current-pos marker-pos) min))
            (setq min (- current-pos marker-pos))
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
          (goto-char counsel-imenu+-opoint))
        )
      )
    ))

(defvar counsel-outline+-opoint nil)

(defun counsel-outline+-update-fn ()
  (with-ivy-window
    (let ((current (ivy-state-current ivy-last))
          item
          marker
          )
      (when (not (string-empty-p current))
        (setq item (nth (get-text-property 0 'idx current) (ivy-state-collection ivy-last)))
        (setq marker (cdr item))
        (goto-char marker)
        (recenter)
        (let ((pulse-delay 0.75))
          (pulse-momentary-highlight-one-line (point))
          )
        ))))

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
                                         #'counsel-outline-action)
                             :history (or (plist-get settings :history)
                                          'counsel-outline-history)
                             :preselect (max (1- counsel-outline--preselect) 0)
                             :update-fn #'counsel-outline+-update-fn
                             :caller (or (plist-get settings :caller)
                                         'counsel-outline+)))
         (point)
         )
      (unless res
        (goto-char counsel-outline+-opoint)
        )
      )))

(defun counsel-rg+-remove-boundaries (text)
  (when (string-prefix-p "\\_<" text)
    (setq text (substring text 3))
    )
  (when (string-suffix-p "\\_>" text)
    (setq text (substring text 0 (- (length text) 3)))
    )
  text
  )

(defvar ivy-plus--swiper-flag 0)

(defun swiper-before-set-flag (&optional initial-input)
  (setq ivy-plus--swiper-flag 1)
  )
(add-hook #'swiper :before #'swiper-before-set-flag)

;;;###autoload
(defun counsel-rg+ (&optional initial-input initial-directory extra-rg-args rg-prompt)
  "Search upwards in the directory tree."
  (interactive)
  (let ((text (or ivy-text initial-input))
        )
    (ivy-quit-and-run
      (counsel-rg (counsel-rg+-remove-boundaries text) initial-directory extra-rg-args rg-prompt)
      )
    )
  )


;;;###autoload
(defun counsel-rg-upwards (&optional initial-input initial-directory extra-rg-args rg-prompt)
  "Search upwards in the directory tree."
  (interactive)
  (let ((dir (file-name-directory (directory-file-name (or initial-directory default-directory))))
        (text (or ivy-text initial-input))
        )
    (when (= ivy-plus--swiper-flag 1)
      (setq dir default-directory)
      (setq ivy-plus--swiper-flag 0)
      )
    (ivy-quit-and-run
      (counsel-rg (counsel-rg+-remove-boundaries text) dir extra-rg-args rg-prompt)
      )
    )
  )

(defcustom counsel-frequent-buffer-limit 10
  "How many buffers displayed when switching."
  :type 'integer
  :group 'ivy)

(defun counsel-frequent-buffer--compare (a b)
  (let ((count-a (nth 1 a))
        (count-b (nth 1 b)))
    (> count-a count-b)
    )
  )
;; (counsel-frequent-buffer--compare  (list "a" 1) (list "b" 3))

(defvar counsel-frequent-buffer--frequency (make-heap #'counsel-frequent-buffer--compare))
(defvar counsel-frequent-buffer--visited-count (make-hash-table :test #'equal))

(defvar counsel-frequent-buffer--current nil)
(defun counsel-frequent-buffer--match-function (record)
  (let ((current-key (nth 2 counsel-frequent-buffer--current))
        (heap-key (nth 2 record)))
    (and current-key (string-equal current-key heap-key))
    )
  )

(defun counsel-frequent-buffer--visit-buffer (&optional arg)
  (let ((buffer (current-buffer))
        (count 0)
        bname
        bfile-name
        )
    (unless (minibufferp buffer)
      (setq bname (buffer-name buffer))
      (setq bfile-name (or (buffer-file-name buffer) bname))

      (setq count (1+ (gethash bfile-name counsel-frequent-buffer--visited-count 0)))

      (puthash bfile-name count counsel-frequent-buffer--visited-count)

      (setq counsel-frequent-buffer--current (list bname count bfile-name))

      (when (not (heap-modify counsel-frequent-buffer--frequency #'counsel-frequent-buffer--match-function counsel-frequent-buffer--current))
        (heap-add counsel-frequent-buffer--frequency counsel-frequent-buffer--current)
        )
      )
    )
  )

(defun counsel-frequent-buffer--kill-buffer (&optional arg)
  (let ((buffer (current-buffer))
        (count 0)
        bname
        bfile-name
        )
    (setq bname (buffer-name buffer))
    (setq bfile-name (or (buffer-file-name buffer) bname))

    (remhash bfile-name counsel-frequent-buffer--visited-count)

    (setq counsel-frequent-buffer--current (list bname count bfile-name))

    (heap-modify counsel-frequent-buffer--frequency #'counsel-frequent-buffer--match-function counsel-frequent-buffer--current)
    )
  )

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
;; (define-key counsel-frequent-buffer-map (kbd "C-s") 'counsel-frequent-buffer-fallback)

;;;###autoload
(defun counsel-frequent-buffer-fallback ()
  (interactive)
  (let ((text (or ivy-text initial-input))
        )
    (ivy-quit-and-run
      (ivy-switch-buffer+ text)
      )))

(defvar counsel-frequent-buffer-obuf nil)
(defun counsel-frequent-buffer-update-fn ()
  (with-ivy-window
    (let* ((current (ivy-state-current ivy-last))
           item
           buffer
           )
      (when (not (string-empty-p current))
        (setq item (nth (get-text-property 0 'idx current) (ivy-state-collection ivy-last)))
        (setq buffer (nth 0 (cdr item)))
        (if (get-buffer buffer)
            (set-window-buffer (selected-window) buffer)
          (set-window-buffer (selected-window) counsel-frequent-buffer-obuf)
          )))
    )
  )

;;;###autoload
(defun counsel-frequent-buffer ()
  "Switch to frequently visited buffers."
  (interactive)
  (setq counsel-frequent-buffer-obuf (current-buffer))
  
  (let ((iter (heap-iter counsel-frequent-buffer--frequency))
        (total-count (heap-size counsel-frequent-buffer--frequency))
        (buffers nil)
        (count 0)
        res
        record
        )
    (while (and (< count counsel-frequent-buffer-limit)
                (< count total-count)
                )
      (setq record (iter-next iter))
      (when (> (nth 1 record) 0)
        (setq buffers (cl-pushnew record buffers))
        )

      (setq count (1+ count))
      )
    (setq buffers (mapcar (lambda (r) (cons (nth 2 r) r)) (nreverse buffers)))

    (let (res)
      (unwind-protect
          (setq res (ivy-read "Frequent Visited: " buffers
                              :action '(1
                                        ("v" (lambda (s)
                                               (switch-to-buffer (nth 0 (cdr s)))
                                               ))
                                        )
                              :keymap counsel-frequent-buffer-map
                              :update-fn #'counsel-frequent-buffer-update-fn
                              :caller #'counsel-frequent-buffer
                              )
                )          
        (unless res
          (switch-to-buffer counsel-frequent-buffer-obuf t)
          (setq counsel-frequent-buffer-obuf nil)
          )
        )
      )))

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
      (ivy--regex-ignore-order str)
      ))

(provide 'ivy-plus)
