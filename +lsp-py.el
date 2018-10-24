;;; ~/.doom.d/+py-lsp.el -*- lexical-binding: t; -*-

(def-package! lsp-python
    :hook (python-mode . lsp-python-enable)
    :config
    (setq python-indent-guess-indent-offset-verbose nil)
    ;; (set! :company-backend '(python-mode) '(company-lsp company-files company-yasnippet))

    ;; get lsp-python-enable defined
    ;; NB: use either projectile-project-root or ffip-get-project-root-directory
    ;;     or any other function that can be used to find the root directory of a project
    (lsp-define-stdio-client lsp-python "python"
                            #'projectile-project-root
                            '("pyls"))

    ;; NB: only required if you prefer flake8 instead of the default
    ;; send pyls config via lsp-after-initialize-hook -- harmless for
    ;; other servers due to pyls key, but would prefer only sending this
    ;; when pyls gets initialised (:initialize function in
    ;; lsp-define-stdio-client is invoked too early (before server
    ;; start)) -- cpbotha
    (defun lsp-set-cfg ()
        (let ((lsp-cfg `(:pyls (:configurationSources ("flake8")))))
        ;; TODO: check lsp--cur-workspace here to decide per server / project
        (lsp--set-configuration lsp-cfg)))

    (add-hook 'lsp-after-initialize-hook 'lsp-set-cfg)
)

(general-define-key
 :prefix ","
 :states '(normal visual)
 :keymaps 'python-mode-map
 "d" '(xref-find-definitions :which-key "find definition")
 "f" '(lsp-format-buffer :which-key "format code")
 "r" '(xref-find-references :which-key "xref-find-references")
 "R" '(lsp-rename :which-key "lsp rename")
 "D" '(elpy-doc :which-key "elpy-doc")
 "m" '(elpy-multiedit :which-key "elpy-multiedit")
 "M" '(elpy-multiedit-stop :which-key "elpy-multiedit-stop")
 "t" '(elpy-test :which-key "elpy-test")
 )
