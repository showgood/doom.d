;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq doom-font (font-spec :family "SF Mono Powerline" :size 16)
      doom-variable-pitch-font (font-spec :family "SF Mono Powerline")
      doom-unicode-font (font-spec :family "SF Mono Powerline")
      doom-big-font (font-spec :family "SF Mono Powerline" :size 20))

(require 'company)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 3)

;(load! "+bindings")
(load! "+evil-bindings.el")

(setq evil-escape-key-sequence "jf")

;; NOTE: this needs to happen before require the bookmark+ package
(setq bookmark-default-file (expand-file-name "~/bookmarks"))

(use-package! bookmark+
 :demand t
)

(use-package! hl-anything
  :defer t
  :config
    (hl-highlight-mode)
)

(use-package! dash-at-point
  :defer t
  :commands dash-at-point
)

(use-package! tldr
  :defer t
  :commands (tldr)
  :config
  (setq tldr-directory-path "~/tldr"
        tldr-enabled-categories (append '("bb" "personal") tldr-enabled-categories))
)

;; disable it since it seems caused some undesired side effect
;; (setq auto-revert-tail-mode nil)

; it causes issue for magit
; (global-visual-line-mode 1)

(setq dired-recursive-deletes 'always)
;; try suggesting dired targets
(setq dired-dwim-target t)

;; do NOT put --group-directories-first
;; otherwise will trigger error:
;; Listing directory failed but 'access-file' worked
(setq dired-listing-switches "-aBhl")

;; https://emacsbliss.com/annoyance-with-paste-in-evil-visual-mode/
(setq evil-kill-on-visual-paste nil)

;; do not use company-ispell as backend, too much noise most of the time
;; (set-company-backend! 'text-mode '(company-capf company-yasnippet company-dabbrev))

(use-package! deadgrep
  :defer t)

(use-package! tiny
  :defer t
  :config
  (tiny-setup-default)
)

;; wand can't have :defer t
;; (use-package! wand
;;  :config
;;    (wand:add-rule-by-pattern :match "https?://"
;;                            :capture :whole
;;                            :action browse-url)

;;    (wand:add-rule-by-pattern :match "file:"
;;                          :capture :after
;;                          :action find-file)
;; )

(use-package! elpa-mirror
  :defer t
  :config
  (setq elpamr-default-output-directory "~/myelpa")
)

(setq +org-dir (concat (substitute-in-file-name "$HOME/") "org"))
(setq +notes-dir (concat (substitute-in-file-name "$HOME/") "notes"))
(defvar my-snippets-dir (expand-file-name "snippets/" doom-private-dir))
(defvar my-templates-dir (expand-file-name "templates/" doom-private-dir))

(after! yasnippet
  (setq yas-snippet-dirs
        (append (list 'my-snippets-dir 'my-templates-dir)
                (delq 'yas-installed-snippets-dir yas-snippet-dirs)))
)

(mapc (lambda (x) (push x +file-templates-alist))
    '(
        ("\\.feature$" :trigger "__" :mode feature-mode)
        ("\\reveal.org$" :trigger "__reveal.org" :mode org-mode)
        ;; ("\\.org$" :trigger "__" :mode org-mode)
     )
)

(use-package! vlf
  :defer t
  :config
  (require 'vlf-setup)
)

(use-package! super-save
  :config
  (super-save-mode +1)
  (setq super-save-remote-files nil)
)

;; I don't like typing space to enable fuzzy search
;; too much interruption
;; https://oremacs.com/2016/01/06/ivy-flx/
;; update: can't use this, cause too much noise in rg search
;; (setq ivy-re-builders-alist
;;       '((ivy-switch-buffer . ivy--regex-plus)
;;         (t . ivy--regex-fuzzy)))

;; can not put :defer t for this one
(use-package! feature-mode
  :config
  (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode))
)

(use-package! eacl
  :defer t
)

(use-package! org-brain
  :defer t
  :init
  ;; For Evil users
  (with-eval-after-load 'evil
    (evil-set-initial-state 'org-brain-visualize-mode 'emacs))
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/org/brain/.org-id-locations")
  (push '("b" "Brain" plain (function org-brain-goto-end)
          "* %i%?" :empty-lines 1)
        org-capture-templates)
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12))

(use-package! company-tabnine
  :defer t
  :config
  (setq company-idle-delay 0)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t))

(after! (:any js2-mode rjsx-mode web-mode)
    (set-company-backend! 'js2-mode
    'company-tabnine))

(add-hook! 'lsp-mode-hook
  (defun me/lsp-keybinding()
    (map!
      :n ",c" #'flycheck-list-errors
      :n ",C" #'lsp-ui-flycheck-list
      :n ",d" #'lsp-find-definition
      :n ",r" #'lsp-find-references
      :n ",R" #'lsp-rename
      :n ",f" #'lsp-format-buffer
      :n ",D" #'lsp-describe-thing-at-point
      :v ",f" #'lsp-format-region)
  ))

(defvar me/ivy-separator "\\$\\$\\$")

(after! vterm
  (add-to-list 'vterm-eval-cmds '("ediff-files" ediff-files))
  (add-to-list 'vterm-eval-cmds '("magit-diff" magit-diff-dwim))
  (add-to-list 'vterm-eval-cmds '("magit-diff-staged" magit-diff-staged))
  (add-to-list 'vterm-eval-cmds '("magit-status-here" magit-status-here))
  (add-to-list 'vterm-eval-cmds '("magit-status" magit-status))
  (add-to-list 'vterm-eval-cmds '("counsel-find-file" counsel-find-file))
)
