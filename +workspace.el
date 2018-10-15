;;; ~/.doom.d/+workspace.el -*- lexical-binding: t; -*-

(defvar doom-default-workspace-name "main"
   " name of the default layout.")

 (defvar doom-last-selected-workspace doom-default-workspace-name
   "previously selected layout.")

 (defun +workspace/save-name(name frame)
   (setq doom-last-selected-workspace persp-last-persp-name)
   (message (format "persp-last: %s" persp-last-persp-name))
 )

 (add-hook 'persp-before-switch-functions #'+workspace/save-name)
