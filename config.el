;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

(setq doom-font (font-spec :family "SF Mono" :size 16)
      doom-variable-pitch-font (font-spec :family "SF Mono")
      doom-unicode-font (font-spec :family "SF Mono")
      doom-big-font (font-spec :family "SF Mono" :size 20))

(setq evil-escape-key-sequence "jf")

(def-package! general
  :demand t
  :config
  (general-evil-setup t)
  (general-override-mode)
)

(load! "+bindings") ; my key bindings
(toggle-frame-maximized)
