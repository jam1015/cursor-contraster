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


;;;###autoload
(defun color-contrast-ratio (c1 c2)
  "Return the WCAG contrast ratio between colors C1 and C2.
C1 and C2 are color names or RGB hex strings (e.g. \"#rrggbb\").
Contrast ratio is (L₁ + 0.05) / (L₂ + 0.05), where L₁ is the
lighter and L₂ the darker relative luminance :contentReference[oaicite:2]{index=2}."
  (cl-labels
      ((lum (col)
         "Compute the relative luminance of COLOR (name or hex)."
         (let* ((rgb   (apply #'color-name-to-rgb col))
                (r     (nth 0 rgb))
                (g     (nth 1 rgb))
                (b     (nth 2 rgb))
                ;; sRGB → linear
                (r-lin (if (<= r 0.03928)
                           (/ r 12.92)
                         (expt (/ (+ r 0.055) 1.055) 2.4)))
                (g-lin (if (<= g 0.03928)
                           (/ g 12.92)
                         (expt (/ (+ g 0.055) 1.055) 2.4)))
                (b-lin (if (<= b 0.03928)
                           (/ b 12.92)
                         (expt (/ (+ b 0.055) 1.055) 2.4))))
           ;; luminance L = 0.2126*R + 0.7152*G + 0.0722*B
           (+ (* 0.2126 r-lin)
              (* 0.7152 g-lin)
              (* 0.0722 b-lin)))))
    (let* ((L1 (funcall #'lum c1))
           (L2 (funcall #'lum c2))
           (lighter  (max L1 L2))
           (darker   (min L1 L2)))
      ;; ratio ∈ [1,21], e.g. 4.5:1 for normal text :contentReference[oaicite:3]{index=3}
      (/ (+ lighter 0.05)
         (+ darker  0.05)))))


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
