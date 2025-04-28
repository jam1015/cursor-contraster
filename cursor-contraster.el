;;; cursor-contraster.el --- Auto-generate and apply contrasting cursor colors -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Jordan Mandel
;; Author: Jordan Mandel
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") )
;; Keywords: convenience, faces, cursor
;; URL: https://github.com/jam1015/cursor-contraster
;; License: GPL-3.0-or-later
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;;; Commentary:
;; `cursor-contraster` generates a set of highly distinguishable colors
;; using evenly spaced hues on the HSL color wheel, ensuring each
;; cursor stands out against typical backgrounds.  It exposes custom
;; settings for HSL parameters and contrast threshold, validates inputs,
;; and restores original cursors when disabled.
;;
;; **Usage Example:**
;;
;; (require 'cursor-contraster)
;; (cursor-contraster-setup-with-specs
;;  '((:var evil-god-state-cursor     :shape box    :index 8)
;;    (:var evil-god-off-state-cursor :shape bar    :index 9)
;;    (:var evil-insert-state-cursor  :shape bar    :index 10)
;;    (:var evil-visual-state-cursor  :shape hollow :index 11)
;;    (:var evil-normal-state-cursor  :shape hollow :index 12)))
;; (cursor-contraster-mode 1)
;;
;;; Code:
(require 'cl-lib)
(require 'color)
(require 'seq)

;; Ensure after-load-theme-hook actually runs
(unless (fboundp 'run-after-load-theme-hook)
  (defvar after-load-theme-hook nil
    "Hook run after `load-theme` is called.")
  (advice-add
   'load-theme :after
   (lambda (&rest _args)
     (run-hooks 'after-load-theme-hook))))


(defgroup cursor-contraster nil
  "Automatically generate contrasting cursor colors."
  :group 'convenience)

(defcustom cursor-contraster-saturation 0.8
  "Saturation component for HSL color generation."
  :type 'number
  :group 'cursor-contraster)

(defcustom cursor-contraster-lightness 0.65
  "Lightness component for HSL color generation."
  :type 'number
  :group 'cursor-contraster)

(defcustom cursor-contraster-contrast-threshold 4.5
  "Minimum WCAG contrast ratio between cursor and background."
  :type 'number
  :group 'cursor-contraster)

(defvar cursor-contraster--current-specs nil
  "Currently active cursor-contraster specs.")

(defvar cursor-contraster--original-cursor-values nil
  "Alist of (VAR . ORIGINAL-VALUE) for restoration on disable.")

(defun cursor-contraster--get-bg ()
  "Retrieve the current default face background."  
  (or (face-background 'default nil) "#000000"))

;;;###autoload
(defun cursor-contraster-generate-palette (&optional count)
  "Generate COUNT distinct contrasting colors as hex strings.
Defaults to 16. Colors are evenly spaced in HSL space using
`cursor-contraster-saturation` and `cursor-contraster-lightness`, and
filtered to meet `cursor-contraster-contrast-threshold` if possible."  
  (let* ((n (or count 16))
         (bg (cursor-contraster--get-bg))
         (raw (cl-loop for i below (* n 2)
                       collect (apply #'color-rgb-to-hex
                                      (color-hsl-to-rgb
                                       (/ i (float (* n 2)))
                                       cursor-contraster-saturation
                                       cursor-contraster-lightness))))
         (filtered
          (seq-filter (lambda (hex)
                        (>= (color-contrast-ratio bg hex)
                            cursor-contraster-contrast-threshold))
                      raw)))
    (if (>= (length filtered) n)
        (cl-subseq filtered 0 n)
      (message "cursor-contraster: only %d/%d colors meet contrast threshold; using first %d raw colors"
               (length filtered) n n)
      (cl-subseq raw 0 n))))

;;;###autoload
(defun cursor-contraster-apply-cursors (specs &optional palette)
  "Apply cursor SPECs using PALETTE.
SPECS is a list of plists (:var SYMBOL :shape SHAPE :index IDX).
Valid SHAPE values are 'box, 'bar, or 'hollow. IDX must be within palette.
If PALETTE is nil, it is regenerated via `cursor-contraster-generate-palette'."
  (let ((pal (or palette (cursor-contraster-generate-palette (length specs)))))
    (dolist (s specs)
      (let* ((var   (plist-get s :var))
             (shape (plist-get s :shape))
             (idx   (plist-get s :index))
             (col   (nth idx pal)))
        (unless (memq shape '(box bar hollow))
          (user-error "cursor-contraster: invalid shape %S" shape))
        (unless (and (numberp idx) (< idx (length pal)))
          (user-error "cursor-contraster: index %S out of bounds (0..%d)"
                      idx (1- (length pal))))
        (when (and var shape col)
          (set var (list shape col)))))))

(defvar cursor-contraster-update-hook nil
  "Hook run with one argument: the latest palette list.
Use this to apply your own cursor mappings via
`cursor-contraster-apply-cursors`.")

(defun cursor-contraster--run-update ()
  "Internal: regenerate palette and run `cursor-contraster-update-hook`."
  (run-hook-with-args 'cursor-contraster-update-hook
                      (cursor-contraster-generate-palette)))

(defun cursor-contraster--reapply-specs (palette)
  "Reapply saved `cursor-contraster--current-specs` using PALETTE."
  (when cursor-contraster--current-specs
    (cursor-contraster-apply-cursors cursor-contraster--current-specs palette)))

;;;###autoload
(defun cursor-contraster-setup-with-specs (specs)
  "Apply SPECS now and register them to run on theme changes.
SPECS is a list of plists (:var SYMBOL :shape SHAPE :index IDX).

1) Save original cursor values if not already saved.
2) Applies SPECS via `cursor-contraster-apply-cursors` immediately.
3) Removes and adds `cursor-contraster--reapply-specs` to `cursor-contraster-update-hook`."
  (unless cursor-contraster--original-cursor-values
    (setq cursor-contraster--original-cursor-values
          (mapcar (lambda (s)
                    (let ((v (plist-get s :var)))
                      (cons v (symbol-value v))))
                  specs)))
  (setq cursor-contraster--current-specs specs)
  (remove-hook 'cursor-contraster-update-hook #'cursor-contraster--reapply-specs)
  (add-hook 'cursor-contraster-update-hook #'cursor-contraster--reapply-specs t)
  ;; Apply immediately:
  (cursor-contraster-apply-cursors specs))

;;;###autoload
(define-minor-mode cursor-contraster-mode
  "Global mode to auto-update contrasting cursor colors on theme changes."
  :global t
  :group 'cursor-contraster
  (if cursor-contraster-mode
      (progn
        (add-hook 'after-load-theme-hook #'cursor-contraster--run-update t)
        (cursor-contraster--run-update))
    ;; disabling: remove hook and restore originals
    (remove-hook 'after-load-theme-hook #'cursor-contraster--run-update)
    (when cursor-contraster--original-cursor-values
      (dolist (pair cursor-contraster--original-cursor-values)
        (set (car pair) (cdr pair)))
      (setq cursor-contraster--original-cursor-values nil
            cursor-contraster--current-specs nil))))

(provide 'cursor-contraster)
;;; cursor-contraster.el ends here
