;;; ~/.doom.d/+elpy.el -*- lexical-binding: t; -*-

(add-hook 'python-mode-hook
          ( lambda ()
            ( define-key python-mode-map ( kbd "RET" ) 'newline-and-indent ) ) )

;; ensure:
;;; pip install jedi
;;  pip install flake8
;;  pip install importmagic
;;  pip install autopep8
;;  pip install yapf
(def-package! elpy
  :after python
  :config
  (elpy-enable)
  (setq elpy-rpc-python-command "~/.pyenv/shims/python")
  ;; prevent elpy because too sluggish
  (setq eldoc-idle-delay 2)
)

;; NOTE: do NOT set to jupyter, otherwise ob-ipython would break
;; set to ipython
;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt")

(general-define-key
 :prefix ","
 :states '(normal visual)
 :keymaps 'elpy-mode-map
 "d" '(elpy-goto-definition :which-key "elpy-goto-definition")
 "D" '(elpy-doc :which-key "elpy-doc")
 "f" '(elpy-format-code :which-key "elpy-format-code")
 "r" '(xref-find-references :which-key "xref-find-references")
 "m" '(elpy-multiedit :which-key "elpy-multiedit")
 "M" '(elpy-multiedit-stop :which-key "elpy-multiedit-stop")
 "t" '(elpy-test :which-key "elpy-test")
 )
