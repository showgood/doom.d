;;; ~/.doom.d/+lsp.el -*- lexical-binding: t; -*-

(def-package! lsp-mode
  :commands (lsp-mode))

(def-package! lsp-ui
:hook (lsp-mode . lsp-ui-mode)
:config
(setq
 lsp-ui-sideline-enable nil
 lsp-enable-completion-at-point t
 lsp-ui-doc-position 'at-point
 lsp-ui-doc-header nil
 lsp-ui-doc-include-signature t
 lsp-ui-peek-fontify 'always
 ;; lsp-ui-doc-border (doom-color 'fg)
)

(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
(define-key lsp-ui-peek-mode-map (kbd "j") 'lsp-ui-peek--select-next)
(define-key lsp-ui-peek-mode-map (kbd "k") 'lsp-ui-peek--select-prev)
(define-key lsp-ui-mode-map (kbd "C-i") 'lsp-ui-peek-jump-forward)
(define-key lsp-ui-mode-map (kbd "C-o") 'lsp-ui-peek-jump-backward)
)

(def-package! company-lsp
  :config
    (setq company-lsp-enable-recompletion t)
    (push 'company-lsp company-backends)
)

(defun my-set-projectile-root ()
  (when lsp--cur-workspace
    (setq projectile-project-root (lsp--workspace-root lsp--cur-workspace))))
(add-hook 'lsp-before-open-hook #'my-set-projectile-root)
