;;; cursor-contraster.el --- Auto-generate and apply contrasting cursor colors -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Jordan Mandel
;; Author: Jordan Mandel
;; Version: 0.3
;; Package-Requires: ((emacs "24.1"))
;; Keywords: convenience, faces, cursor
;; URL: https://github.com/jam1015/cursor-contraster
;; License: GPL-3.0-or-later
;;
;;; Commentary:
;; `cursor-contraster` generates cursor colors that adapt
;; to your theme background (light or dark) for visibility.
;;
;;; Code:

(require 'cl-lib)
(require 'color)

(defvar cursor-contraster--registered-specs nil
  "Specs registered for auto-updating cursors.")

(defun cursor-contraster--get-bg ()
  "Retrieve the current default face background."
  (or (face-background 'default nil) "#000000"))

(defun cursor-contraster--background-bright-p ()
  "Return t if background color is bright."
  (let* ((bg (cursor-contraster--get-bg))
         (rgb (color-name-to-rgb bg)))
    (when rgb
      (> (apply '+ rgb) 1.5))))  ;; crude: >1.5 = bright

;;;###autoload
(defun cursor-contraster-generate-palette (&optional count)
  "Generate COUNT contrasting colors adapted to background brightness."
  (let ((n (or count 16))
        (bright (cursor-contraster--background-bright-p)))
    (cl-loop for i from 0 below n
             collect
             (apply #'color-rgb-to-hex
                    (color-hsl-to-rgb
                     (/ i (float n))
                     (if bright 0.6 0.8)
                     (if bright 0.35 0.65))))))

;;;###autoload
(defun cursor-contraster-apply-cursors (specs &optional palette)
  "Apply cursor color SPECS using PALETTE.
SPECS is a list of plists (:var SYMBOL :shape SHAPE :index IDX).
If PALETTE is nil, regenerate via `cursor-contraster-generate-palette'."
  (let ((pal (or palette (cursor-contraster-generate-palette))))
    (dolist (s specs)
      (let* ((var   (plist-get s :var))
             (shape (plist-get s :shape))
             (idx   (plist-get s :index))
             (col   (nth idx pal)))
        (when (and var shape (numberp idx) col)
          (set var (list shape col)))))))

(defun cursor-contraster--run-update ()
  "Internal: regenerate palette and reapply cursors."
  (when cursor-contraster--registered-specs
    (cursor-contraster-apply-cursors cursor-contraster--registered-specs)))

;;;###autoload
(define-minor-mode cursor-contraster-mode
  "Global minor mode to auto-update cursor colors when theme changes."
  :global t
  :group 'cursor-contraster
  (if cursor-contraster-mode
      (progn
        (add-hook 'after-load-theme-hook #'cursor-contraster--run-update t)
        (cursor-contraster--run-update))
    (remove-hook 'after-load-theme-hook #'cursor-contraster--run-update)))

;;;###autoload
(defun cursor-contraster-setup-with-specs (specs)
  "Apply SPECS now and register them to update on theme changes.
SPECS is a list of plists: (:var SYMBOL :shape SHAPE :index IDX)."
  (setq cursor-contraster--registered-specs specs)
  (cursor-contraster-apply-cursors specs))

(provide 'cursor-contraster)
;;; cursor-contraster.el ends here
