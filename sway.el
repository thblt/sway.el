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

(require 'dash)
(require 'json)

(defcustom sway-swaymsg-binary (executable-find "swaymsg")
  "Path to `swaymsg' or a compatible program.")

(defconst sway-focus-message-format "[con_id=%s] focus"
  "The format of the message to send to swaymsh to focus a
  window.")

(defun sway-tree (&optional frame)
  "Get the Sway tree as an elisp object, using environment of FRAME.

If FRAME is nil, use the value of (selected-frame)."
  (with-temp-buffer
    (let ((process-environment (frame-parameter frame 'environment)))
      (call-process sway-swaymsg-binary nil (current-buffer) nil "-t" "get_tree"))
    (goto-char (point-min))
    (json-parse-buffer :null-object nil :false-object nil)))

(defun sway-list-windows (tree &optional visible-only focused-only)
  "Walk TREE and return windows."
  ;; @TODO What this actually does is list terminal containers that
  ;; aren't workspaces.  The latter criterion is to eliminate
  ;; __i3_scratch, which is a potentially empty workspace.  It works,
  ;; but could maybe be improved.
  (let ((next-tree (gethash "nodes" tree)))
    (if (and
         (zerop (length next-tree))
         (not (string= "workspace" (gethash "type" tree)))
         (if visible-only (gethash "visible" tree) t)
         (if focused-only (gethash "focused" tree) t))
        tree ; Collect
      (-flatten ; Or recurse
       (mapcar
        (lambda (t) (sway-list-windows t visible-only focused-only))
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

(defun sway-find-frames (tree &optional visible-only focused-only)
  "Find all visible Emacs frames in TREE, and return an alist
of (FRAME-OBJECT . SWAY-ID)"
  (let* ((wins (sway-list-windows tree visible-only focused-only)))
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
             (and (sway-frame-displays-buffer-p (car f) buffer) f))
           (sway-find-frames (sway-tree) visible-only)))

;;;; Shackle integration

(defun sway-shackle-display-buffer-frame (buffer alist plist)
  "Show BUFFER in a new Emacs frame, unless one is already
  visible on current workspace"
  (let* ((previous-frame (selected-frame))
         (sway (sway-find-frame-for-buffer buffer t))
         (frame (or (car sway)
                    (funcall pop-up-frame-function))))

    ;; Display buffer if frame doesn't already.
    (if (sway-frame-displays-buffer-p frame buffer)
        ;; Select existing window
        (set-frame-selected-window frame (get-buffer-window buffer frame))
      ;; Show buffer in current window
      (set-window-buffer (frame-selected-window frame) buffer))

    ;; (message "buffer=%s\nalist=%s\nplist=%s" buffer alist plist)

    ;; Give focus back to previous window.
    (select-frame-set-input-focus previous-frame)

    ;; Mark as killable for undertaker mode
    ;; @TODO Make this a configuration option.
    (unless sway
      ;; (message "Marking %s sway-dedicated to %s" frame buffer)
      (set-frame-parameter frame 'sway-dedicated buffer))

    ;; Return the window displaying buffer.
    (frame-selected-window frame)))
;;(let ((process-environment (frame-parameter frame 'environment)))
;;(call-process sway-swaymsg-binary nil nil nil (format sway-focus-message-format focused))))

;;;; The Undertaker: A stupid mode to make it easier to kill frames on bury-buffer

;; Another little trick, technically independant from sway.  Some
;; frames shouldn't last, but sometimes we reuse them.  sway.el marks
;; frames it creates with the `sway-dedicated' frame parameter, whose
;; value is a buffer.  As long as this frame keeps displaying this
;; buffer, we kill the whole frame if the buffer gets buried.

(defvar sway-undertaker-killer-commands
  (list 'bury-buffer
        'cvs-bury-buffer
        'magit-log-bury-buffer
        'magit-mode-bury-buffer)
  "Commands whose invocation will kill the frame if it's still
dedicated.")

(defun sway-undertaker-protect (&optional frame)
  "Remove the `sway-killable' parameter of FRAME."

  (if (and (frame-parameter frame 'sway-dedicated)
           (member last-command sway-undertaker-killer-commands))
      ;; kill the frame
      (delete-frame frame)
    ;; otherwise, drop the sway-dedicated parameter if its contents have changed.
    (when-let ((buffer (frame-parameter nil 'sway-dedicated))
               (windows (window-list frame 'never)))
      (unless (and (= 1 (length windows))
                   (eq buffer (window-buffer (car windows))))
        ;; (message "Frame %s is now safe from The Undertaker because of %s." (or frame (selected-frame)) windows)
        (set-frame-parameter frame 'sway-dedicated nil)))))

(define-minor-mode sway-undertaker-mode
  "Remove the `sway-killable' parameter of frames on `window-configuration-change-hook'"
  :global t
  (if sway-undertaker-mode
      ;; Install
      (add-hook 'window-configuration-change-hook 'sway-undertaker-protect)
    (remove-hook 'window-configuration-change-hook 'sway-undertaker-protect)))

;;;; Tracking minor mode

(defun sway-after-make-frame-function (f)
  "When creating a new frame, copy the environment parameter from the selected frame."
  (let ((oldenv(frame-parameter nil 'environment)))
    (when (and oldenv (not (frame-parameter f 'environment)))
      (set-frame-parameter f 'environment oldenv))))

(define-minor-mode swaysock-tracker-mode
  "A minor mode to track the value of SWAYSOCK on newly created
frames.  This is a best effort approach, and remains probably
very fragile."
  :global t
  (if swaysock-tracker-mode
      (add-hook 'after-make-frame-functions 'sway-after-make-frame-function)
    (remove-hook 'after-make-frame-functions 'sway-after-make-frame-function)))

(provide 'sway)

;;; sway.el ends here
