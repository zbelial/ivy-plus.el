;; ivy-plus.el --- New counsel commands or enhancement to some existing counsel commands.  -*- lexical-binding: t; -*-

;; Filename: ivy-plus.el
;; Description: New counsel commands or enhancement to some existing counsel commands.
;; Author:  zbelial <zjyzhaojiyang@gmail.com>
;; Maintainer:  zbelial <zjyzhaojiyang@gmail.com>
;; Copyright (C) 2021, zbelial, all rights reserved.
;; Created: 2021-06-03 14:08:12
;; Version: 0.1
;; URL: https://github.com/zbelial/ivy-plus.el
;; Package-Requires: ((ivy "0.13.0"))
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
(defun ivy-switch-buffer+ ()
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
                              :caller 'ivy-switch-buffer+))
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
      ;; (message "current %S" current)
      (when (not (string-empty-p current))
        (setq item (nth (get-text-property 0 'idx current) (ivy-state-collection ivy-last)))
        (setq marker (cddr item))
        (goto-char (marker-position marker))
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
    
    ;; (message "preselect %d" preselect)
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

(provide 'ivy-plus)
