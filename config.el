;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "SF Mono" :size 16)
      doom-variable-pitch-font (font-spec :family "SF Mono")
      doom-unicode-font (font-spec :family "SF Mono")
      doom-big-font (font-spec :family "SF Mono" :size 20))

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
  :no-require t
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

;; NOTE: this needs to happen before require the bookmark+ package
(setq bookmark-default-file (expand-file-name "~/bookmarks"))

(def-package! bookmark+
  :demand t
)

(load! "site-lisp/key-chord.el")

(def-package! key-chord
  :config
  (key-chord-mode 1)
  ;; :disabled
)

;; NOTE: make sure +bindings is loaded after `key-chord'
(load! "+bindings")

(setq evil-escape-key-sequence "jf")
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

(setq dired-recursive-deletes 'always)
;; try suggesting dired targets
(setq dired-dwim-target t)

;; https://emacsbliss.com/annoyance-with-paste-in-evil-visual-mode/
(setq evil-kill-on-visual-paste nil)

;; look before jump! so C-o/C-i works
(evil-add-command-properties #'counsel-imenu :jump t)
(evil-add-command-properties #'+jump/definition :jump t)
(evil-add-command-properties #'+jump/references :jump t)
(evil-add-command-properties #'counsel-etags-find-tag-at-point :jump t)

;; lsp-python is too slow for me.. back to elpy
;; (load! "+lsp-py.el")
(load! "+elpy.el")

;; not working..not sure why..
;; (after! ivy-posframe
;;   (setq ivy-display-function #'ivy-posframe-display-at-point)
;; )

(add-to-list 'auto-mode-alist '("\\.xml" . web-mode))

;; do not use company-ispell as backend, too much noise most of the time
(set-company-backend! 'text-mode '(company-capf company-yasnippet))

;; ONLY turn on this when local repository for package needs to be updated
;; then run: M-x elpamr-create-mirror-for-installed
(def-package! elpa-mirror
  :config
  (setq elpamr-default-output-directory "~/myelpa")
)

;; override printer to print json path in the way I want
(setq jsons-path-printer 'me/jsons-print-path-as-list)

(setenv "PATH"
  (concat
   (expand-file-name "~/.pyenv/shims")
   ":" (getenv "PATH")
  )
)

(def-package! anki-editor
  :no-require t)

(def-package! keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
)

(def-package! deadgrep
  :no-require t)

(def-package! isend-mode
  :no-require t)

(def-package! powerthesaurus
  :no-require t)

(def-package! define-word
  :no-require t)

(def-package! vlf
  :no-require t
  :config
  (require 'vlf-setup)
)

(def-package! engine-mode
  :no-require t
  :config
  (engine-mode t)
)

(defengine github
  "https://github.com/search?ref=simplesearch&q=%s")

(defengine stack-overflow
  "https://stackoverflow.com/search?q=%s")
;; (toggle-frame-maximized)

(load! "+term.el")
(load! "+ivy.el")
(load! "+workspace.el")
(load! "+org.el")
(load! "+lsp.el")
(load! "+cc.el")

(def-package! tiny
  :config
  (tiny-setup-default)
)

(def-package! super-save
  :config
  (super-save-mode +1)
  (setq super-save-remote-files nil)
)

;; http://emacsredux.com/blog/2014/07/25/configure-the-scratch-buffers-mode/
; set scratch buffer default mode to org-mode
(setq initial-major-mode 'org-mode)
;; (setq initial-scratch-message "hello world")


;; https://www.reddit.com/r/emacs/comments/8kz8dv/tip_how_i_use_orgjournal_to_improve_my/
(def-package! org-journal
  ;; NOTE: :config won't work, need to use :custom
  ;; https://github.com/bastibe/org-journal/issues/9
  :custom
    (org-journal-dir "~/org/journal/2018/")
    (org-journal-file-format "%Y%m%d")
    (org-journal-date-format "%e %b %Y (%A)")
)
