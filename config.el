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

(load! "+bindings")

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

;; look before jump! so C-o/C-i works
(evil-add-command-properties #'counsel-imenu :jump t)
(evil-add-command-properties #'+jump/definition :jump t)
(evil-add-command-properties #'+jump/references :jump t)
(evil-add-command-properties #'counsel-etags-find-tag-at-point :jump t)
(evil-add-command-properties #'wand:execute :jump t)

;; do not use company-ispell as backend, too much noise most of the time
(set-company-backend! 'text-mode '(company-capf company-yasnippet company-dabbrev))

;; ONLY turn on this when local repository for package needs to be updated
;; then run: M-x elpamr-create-mirror-for-installed
(def-package! elpa-mirror
  :config
  (setq elpamr-default-output-directory "~/myelpa")
)

(setenv "PATH"
  (concat
   (expand-file-name "~/.pyenv/shims")
   ":" (getenv "PATH")
  )
)

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

(load! "+ivy.el")

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

(def-package! wand
  :config
    (wand:add-rule-by-pattern :match "https?://"
                            :capture :whole
                            :action browse-url)

    (wand:add-rule-by-pattern :match "file:"
                          :capture :after
                          :action find-file)
)

;; I prefer web-mode for xml and xsd file
(with-eval-after-load "web-mode-autoloads"
  (add-to-list 'auto-mode-alist '("\\.xsd\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.xml\\'" . web-mode)))


;; (setq counsel-grep-base-command
;;  "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
