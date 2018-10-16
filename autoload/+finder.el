;;; ~/~/.doom.d/autoload/++finder.el -*- lexical-binding: t; -*-

(defmacro +hlissner-def-+finder! (name dir)
  "Define a pair of find-file and browse functions."
  `(progn
     (defun ,(intern (format "showgood/find-in-%s" name)) ()
       (interactive)
       (let ((default-directory ,dir)
             projectile-project-name
             projectile-require-project-root
             projectile-cached-buffer-file-name
             projectile-cached-project-root)
         (call-interactively (command-remapping #'projectile-find-file))))
     (defun ,(intern (format "showgood/browse-%s" name)) ()
       (interactive)
       (let ((default-directory ,dir))
         (call-interactively (command-remapping #'find-file))))))

;;;###autoload (autoload 'showgood/find-in-templates "autoload/+finder"  nil t)
;;;###autoload (autoload 'showgood/browse-templates "autoload/+finder"  nil t)
(+hlissner-def-+finder! templates +file-templates-dir)

;;;###autoload (autoload 'showgood/find-in-snippets "~/.doom.d/autoload/+finder"  nil t)
;;;###autoload (autoload 'showgood/browse-snippets "~/.doom.d/autoload/+finder"  nil t)
(+hlissner-def-+finder! snippets my-snippets-dir)

;;;###autoload (autoload 'showgood/find-in-dotfiles "~/.doom.d/autoload/+finder"  nil t)
;;;###autoload (autoload 'showgood/browse-dotfiles "~/.doom.d/autoload/+finder"  nil t)
(+hlissner-def-+finder! dotfiles (expand-file-name "dotfiles" "~"))

;;;autoload (autoload 'showgood/find-in-emacsd "~/.doom.d/autoload/+finder"  nil t)
;;;###autoload (autoload 'showgood/browse-emacsd "~/.doom.d/autoload/+finder"  nil t)
(+hlissner-def-+finder! emacsd doom-private-dir)

;;;###autoload (autoload 'showgood/find-in-notes "autoload/+finder"  nil t)
;;;###autoload (autoload 'showgood/browse-notes "autoload/+finder"  nil t)
(+hlissner-def-+finder! notes +notes-dir)
