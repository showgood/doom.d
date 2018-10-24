;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! general)
(package! bookmark+)
(package! hl-anything)
(package! tldr)
(package! dash-at-point)
(package! feature-mode)

(package! anki-editor)

(package! lsp-mode)
(package! lsp-ui)
(package! company-lsp)

;; for some reason, editing is really slow
;; for python with lsp mode, so back to elpy
;; (package! lsp-python)
(package! elpy)

(package! ccls)
(package! clang-format)

(package! org-attach-screenshot)
(package! deadgrep)
