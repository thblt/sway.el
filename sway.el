;;; sway.el --- Emacs 🖤 Sway  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (c) 2020 Thibault Polge <thibault@thb.lt>

;; Author: Thibault Polge <thibault@thb.lt>
;; Maintainer: Thibault Polge <thibault@thb.lt>
;;
;; Keywords: convenience
;; Homepage: https://github.com/thblt/sway.el
;; Version: 0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a very rudimentary library to talk to Sway from Emacs.  Its
;; main use case is in combination with Shackle or some other popup
;; managers, to 1) use frames instead of windows while still 2) giving
;; focus to existing frames instead of duplicating them.

;;; Code:

(require 'json)

(defcustom sway-swaymsg-binary (executable-find "swaymsg")
  "Path to `swaymsg' or a compatible program.")

(defconst sway-focus-message-format "[con_id=%s] focus"
  "The format of the message to send to swaymsh to focus a
  window.")

(defun sway-tree ()
  "Get the Sway tree as an elisp object."
  (with-temp-buffer
    (call-process sway-swaymsg-binary nil (current-buffer) nil "-t" "get_tree")
    (goto-char (point-min))
    (json-parse-buffer :null-object nil :false-object nil)))

(defun sway-list-windows (tree &optional visible-only)
  "Walk TREE and return windows."
  ;; @TODO What this actually does is list terminal containers that
  ;; aren't workspaces.  The latter criterion is to eliminate
  ;; __i3_scratch, which is a potentially empty workspace.  It works,
  ;; but could maybe be improved.
  (let ((next-tree (gethash "nodes" tree)))
    (if (and
         (zerop (length next-tree))
         (not (string= "workspace" (gethash "type" tree)))
         (if visible-only (gethash "visible" tree) t))
        tree ; Collect
      (-flatten
       (mapcar
        (lambda (t) (sway-list-windows t visible-only))
        next-tree)))))

(defun sway-find-frame (tree)
  "Return the Emacs frame corresponding to TREE, a Sway window."
  (let ((parent (gethash "window" tree)))
    (cl-some (lambda (frame)
               (let ((owi (frame-parameter frame 'outer-window-id)))
                 (and owi
                      (eq parent (string-to-number owi))
                      frame)))
             (frame-list))))

(defun sway-get-id (tree)
  (gethash "id" tree))

(defun sway-find-frames (tree &optional visible-only)
  "Find all visible Emacs frames in TREE, and return an alist
of (FRAME-OBJECT . SWAY-ID)"
  (let* ((wins (sway-list-windows tree visible-only)))
    (seq-filter  (lambda (x) (car x))
                 (-zip (mapcar 'sway-find-frame wins) (mapcar 'sway-get-id wins)))))

(defun sway-frame-displays-buffer-p (frame buffer)
  "Determine if FRAME displays BUFFER."
  (cl-some
   (lambda (w) (eq (window-buffer w) buffer))
   (window-list frame nil)))

(defun sway-find-frame-for-buffer (buffer &optional visible-only)
  "Find which frame displays BUFFER.

Return either nil if there's none, or a pair of (FRAME-OBJECT
. SWAY-ID)"
  (cl-some (lambda (f)
             (when (sway-frame-displays-buffer-p (car f) buffer)
               f))
           (sway-find-frames (sway-tree) visible-only)))


(defun sway-shackle-display-buffer-frame (buffer alist plist)
  "Show BUFFER in a new Emacs frame, unless one is already
  visible on current workspace"
  (message "Got %s for %s (bufferp? %s)" (sway-find-frame-for-buffer buffer t) buffer (bufferp buffer))
  (if-let ((f (sway-find-frame-for-buffer buffer t)))
      (let ((frame (funcall pop-up-frame-function)))
        (shackle--window-display-buffer
         buffer  (frame-selected-window frame) 'frame alist))))

;;(sway-find-frame-for-buffer (get-file-buffer (expand-file-name "~/.emacs.d/init.el")) t)

(provide 'sway)

;;; sway.el ends here
