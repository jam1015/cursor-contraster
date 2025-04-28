;;; cursor-contraster.el --- Auto-generate and apply contrasting cursor colors -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Jordan Mandel
;; Author: Jordan Mandel
;; Version: 0.1
;; Package-Requires: ((emacs "24.1") (cl-lib "0.5") (color "1.0"))
;; Keywords: convenience, faces, cursor
;; URL: https://github.com/jam1015/cursor-contraster
;; License: GPL-3.0-or-later
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;;; Commentary:
;; `cursor-contraster` generates a set of highly distinguishable colors
;; using evenly spaced hues on the HSL color wheel, ensuring each
;; cursor stands out against typical backgrounds.
;;
;; **Features:**
;; • `cursor-contraster-generate-palette` – return N distinct hex colors.
;; • `cursor-contraster-apply-cursors` – apply a list of (:var SYMBOL :shape SHAPE :index IDX) specs.
;; • `cursor-contraster-setup-with-specs` – apply specs now and register update hook.
;; • `cursor-contraster-mode` – auto-update on theme changes.
;;
;; **Usage Example:**
;;
;; ;; Require and setup specs for cursor variables:
;; (require 'cursor-contraster)
;; (cursor-contraster-setup-with-specs
;;  '((:var evil-god-state-cursor     :shape box    :index 8)
;;    (:var evil-god-off-state-cursor :shape bar    :index 9)
;;    (:var evil-insert-state-cursor  :shape bar    :index 10)
;;    (:var evil-visual-state-cursor  :shape hollow :index 11)
;;    (:var evil-normal-state-cursor  :shape hollow :index 12)))
;;
;; ;; Or enable automatic theme-based updates for all specs:
;; (cursor-contraster-mode 1)
;;
;;; Code:


(defun cursor-contraster--get-bg ()
  "Retrieve the current default face background."  
  (or (face-background 'default nil) "#000000"))

;;;###autoload
(defun cursor-contraster-generate-palette (&optional count)
  "Generate COUNT distinct contrasting colors as hex strings.
Defaults to 16.  Colors are evenly spaced in HSL space with fixed
saturation and lightness for maximum visibility."
  (let ((n (or count 16)))
    (cl-loop for i from 0 below n
             collect
             (apply #'color-rgb-to-hex
                    (color-hsl-to-rgb
                     (/ i (float n))   ; hue
                     0.8               ; saturation
                     0.65)))))        ; lightness

;;;###autoload
(defun cursor-contraster-apply-cursors (specs &optional palette)
  "Apply cursor color SPECS using PALETTE.
SPECS is a list of plists (:var SYMBOL :shape SHAPE :index IDX).
Each entry sets SYMBOL to a list (SHAPE . COLOR).
If PALETTE is nil, it is regenerated via `cursor-contraster-generate-palette'."
  (let ((pal (or palette (cursor-contraster-generate-palette))))
    (dolist (s specs)
      (let* ((var   (plist-get s :var))
             (shape (plist-get s :shape))
             (idx   (plist-get s :index))
             (col   (nth idx pal)))
        (when (and var shape (numberp idx) col)
          (set var (list shape col)))))))

(defvar cursor-contraster-update-hook nil
  "Hook run with one argument: the latest palette list.
Use this to apply your own cursor mappings via
`cursor-contraster-apply-cursors`.

Example:
  (add-hook 'cursor-contraster-update-hook
            (lambda (palette)
              (cursor-contraster-apply-cursors
               '((:var evil-normal-state-cursor :shape box :index 0))
               palette)))")

(defun cursor-contraster--run-update ()
  "Internal: regenerate palette and run `cursor-contraster-update-hook'."
  (run-hook-with-args 'cursor-contraster-update-hook
                      (cursor-contraster-generate-palette)))

;;;###autoload
(define-minor-mode cursor-contraster-mode
  "Global mode to auto-update contrasting cursor colors on theme changes."
  :global t
  :group 'cursor-contraster
  (if cursor-contraster-mode
      (progn
        (add-hook 'after-load-theme-hook #'cursor-contraster--run-update t)
        (cursor-contraster--run-update))
    (remove-hook 'after-load-theme-hook #'cursor-contraster--run-update)))

;;;###autoload
(defun cursor-contraster-setup-with-specs (specs)
  "Apply SPECS now and register them to run on theme changes.
SPECS is a list of plists (:var SYMBOL :shape SHAPE :index IDX).

1) Applies SPECS via `cursor-contraster-apply-cursors` immediately.
2) Adds a hook to `cursor-contraster-update-hook` to reapply SPECS on updates."
  (cursor-contraster-apply-cursors specs)
  (add-hook 'cursor-contraster-update-hook
            (lambda (palette)
              (cursor-contraster-apply-cursors specs palette))
            t))

(provide 'cursor-contraster)
;;; cursor-contraster.el ends here
