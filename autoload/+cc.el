;;; ~/.doom.d/autoload/+cc.el -*- lexical-binding: t; -*-

;;;###autoload
(defun me/create-clang-format-at (dir)
  "make a copy of clang-format file to specified directory"
  (interactive)
  (copy-file (format "%s/my.clang-format" +showgood-dir) (format "%s/.clang-format" dir))
)

;;;###autoload
(defun me/clang-format ()
  "1. copy .clang-format file to current project root directory. requires projectile
   2. format region if there is active region
   3. otherwise format whole file, but only do so if user confirms."
  (interactive)
  (let ((project-root (projectile-project-root)))
    (message "project root: %s" project-root)
    (unless (file-exists-p (format "%s.clang-format" project-root))
      (me/create-clang-format-at project-root)
      )

    (if (use-region-p)
        (call-interactively 'clang-format-region)
      (when (y-or-n-p (format "clang-format file: %s ?" (buffer-file-name)))
        (clang-format-buffer))
)))
