;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "SF Mono" :size 16)
      doom-variable-pitch-font (font-spec :family "SF Mono")
      doom-unicode-font (font-spec :family "SF Mono")
      doom-big-font (font-spec :family "SF Mono" :size 20))

; my key bindings
(load! "+bindings")

(def-package! feature-mode
  :mode "\\.feature$")

(def-package! general
  :demand t
  :config
  (general-evil-setup t)
  (general-override-mode)
)

(def-package! hl-anything
  :config
    (hl-highlight-mode)
)

(def-package! ox-reveal
  :config
  (setq org-reveal-root (format "file://%s/reveal.js" (substitute-in-file-name "$HOME"))
        org-reveal-title-slide nil )
)

(def-package! tldr
  :commands (tldr)
  :config
  (setq tldr-directory-path "~/tldr"
        tldr-enabled-categories (append '("bb" "personal") tldr-enabled-categories))
)

(def-package! dash-at-point
  :commands dash-at-point
)

(setq evil-escape-key-sequence "jf")
(setq +org-dir (concat (substitute-in-file-name "$HOME/") "org"))
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
        ;; ("\\.org$" :trigger "__" :mode org-mode)
     )
)

(setq bookmark-default-file (expand-file-name "~/bookmarks"))

;; set this so search is performed on all buffers,
;; not just current buffer
(setq avy-all-windows t)

 ;; i want to switch window across frame
(setq aw-scope 'global)

;; allow to select from kill-ring history while in minibuffer
(setq enable-recursive-minibuffers t)

;; disable it since it seems caused some undesired side effect
;; (setq auto-revert-tail-mode nil)

; proper line wrapping
(global-visual-line-mode 1)

;; look before jump! so C-o/C-i works
(evil-add-command-properties #'counsel-imenu :jump t)
(evil-add-command-properties #'+jump/definition :jump t)
(evil-add-command-properties #'+jump/references :jump t)
(evil-add-command-properties #'counsel-etags-find-tag-at-point :jump t)

(load! "+term.el")
(load! "+ivy.el")
(load! "+workspace.el")

(toggle-frame-maximized)
