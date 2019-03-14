;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq doom-font (font-spec :family "SF Mono" :size 16)
      doom-variable-pitch-font (font-spec :family "SF Mono")
      doom-unicode-font (font-spec :family "SF Mono")
      doom-big-font (font-spec :family "SF Mono" :size 20))

(require 'company)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 3)

(def-package! general
  :demand t
  :config
  (general-evil-setup t)
  (general-override-mode)
)

(load! "+bindings")

;; NOTE: this needs to happen before require the bookmark+ package
(setq bookmark-default-file (expand-file-name "~/bookmarks"))

(def-package! bookmark+
  :demand t
)

(def-package! hl-anything
  :defer t
  :config
    (hl-highlight-mode)
)

(def-package! dash-at-point
  :defer t
  :commands dash-at-point
)

(def-package! tldr
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

(def-package! deadgrep
  :defer t)

(def-package! tiny
  :defer t
  :config
  (tiny-setup-default)
)

;; wand can't have :defer t
(def-package! wand
  :config
    (wand:add-rule-by-pattern :match "https?://"
                            :capture :whole
                            :action browse-url)

    (wand:add-rule-by-pattern :match "file:"
                          :capture :after
                          :action find-file)
)

(def-package! elpa-mirror
  :defer t
  :config
  (setq elpamr-default-output-directory "~/myelpa")
)

(setq ivy-count-format "(%d/%d) "
      ;; http://oremacs.com/2017/11/30/ivy-0.10.0/
      ivy-use-selectable-prompt t)

;; http://oremacs.com/2017/04/09/ivy-0.9.0/
(setq counsel-yank-pop-separator "\n-------------------------------------------------------\n")


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

(def-package! engine-mode
  :defer t
  :config
  (engine-mode t)
  ;; this is not working
  ;; (setq engine/keybinding-prefix (kbd "gl"))
)

(defengine google
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  :keybinding "g")

(def-package! vlf
  :defer t
  :config
  (require 'vlf-setup)
)

(def-package! super-save
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
