;;; cursor-contraster.el --- Auto-generate and apply contrasting cursor colors -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 Jordan Mandel
;; Author: Jordan Mandel
;; Version: 0.2
;; Package-Requires: ((emacs "24.1"))
;; Keywords: convenience, faces, cursor
;; URL: https://github.com/jam1015/cursor-contraster
;; License: GPL-3.0-or-later
;;
;;; Commentary:
;; `cursor-contraster` generates a set of cursor colors that adapt
;; to your theme background (light or dark) for better visibility.
;;
;; Features:
;; - `cursor-contraster-generate-palette` – generates a background-sensitive palette.
;; - `cursor-contraster-apply-cursors` – assigns shapes/colors to cursor variables.
;; - `cursor-contraster-setup-with-specs` – easy setup and auto-refresh.
;; - `cursor-contraster-mode` – automatically update when theme changes.
;;
;; Usage:
;; (require 'cursor-contraster)
;; (cursor-contraster-setup-with-specs
;;  '((:var evil-normal-state-cursor :shape box    :index 0)
;;    (:var evil-insert-state-cursor :shape bar    :index 1)
;;    (:var evil-visual-state-cursor :shape hollow :index 2)))
;; (cursor-contraster-mode 1)
;;
;;; Code:

(require 'cl-lib)
(require 'color)

(defun cursor-contraster--get-bg ()
  "Retrieve the current default face background."
  (or (face-background 'default nil) "#000000"))

(defun cursor-contraster--background-bright-p ()
  "Return t if background color is bright."
  (let* ((bg (cursor-contraster--get-bg))
         (rgb (color-name-to-rgb bg)))
    (when rgb
      (> (apply '+ rgb) 1.5))))  ;; crude: if total brightness > 1.5, call it light

;;;###autoload
(defun cursor-contraster-generate-palette (&optional count)
  "Generate COUNT contrasting colors adapted to background brightness."
  (let ((n (or count 16))
        (bright (cursor-contraster--background-bright-p)))
    (cl-loop for i from 0 below n
             collect
             (apply #'color-rgb-to-hex
                    (color-hsl-to-rgb
                     (/ i (float n))        ;; hue evenly spaced
                     (if bright 0.6 0.8)    ;; less saturation for light bg
                     (if bright 0.35 0.65)))))) ;; darker for light bg

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

(defvar cursor-contraster-update-hook nil
  "Hook run with one argument: the latest palette.
Use this to apply your own cursor mappings.")

(defun cursor-contraster--run-update ()
  "Internal: regenerate palette and run `cursor-contraster-update-hook'."
  (run-hook-with-args 'cursor-contraster-update-hook
                      (cursor-contraster-generate-palette)))

;;;###autoload
(define-minor-mode cursor-contraster-mode
  "Global minor mode to auto-update cursor colors when the theme changes."
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
  (cursor-contraster-apply-cursors specs)
  (add-hook 'cursor-contraster-update-hook
            (lambda (palette)
              (cursor-contraster-apply-cursors specs palette))
            t))

(provide 'cursor-contraster)
;;; cursor-contraster.el ends here
