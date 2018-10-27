;;; ~/.doom.d/+term.el -*- lexical-binding: t; -*-

(setq multi-term-dedicated-select-after-open-p t)

(defun setup-my-term-mode()
  (setq-local global-hl-line-mode nil)
  ;; (setq-local beacon-mode nil)
  (setq term-buffer-maximum-size 0)
  (key-chord-define term-raw-map ",," 'me/back-to-term-normal)
  (key-chord-define term-mode-map ",," 'me/back-to-term-normal)
)

(add-hook 'term-mode-hook #'setup-my-term-mode)

;; NOTE: need to disable evil-collection for term
;; otherwise my keybindings won't work
(push 'term +evil-collection-disabled-list)

(defun evil-collection-term-char-mode-insert ()
  "Switch to `term-char-mode' and enter insert state."
  (interactive)
  (term-char-mode)
  (evil-insert-state))

;; (load! "+evil-term.el")
;; (require 'evil-term)

(general-define-key
:states 'normal
:keymaps '(term-raw-map term-mode-map)
"p" '(me/paste-in-term-mode :which-key "paste")
"i" '(evil-collection-term-char-mode-insert :which-key "insert")
"a" '(evil-insert-state :which-key "insert")
"C-y" '(me/paste-in-term-mode :which-key "paste")
"C-z" '(comint-clear-buffer :which-key "clear buffer")
"C-h" '(evil-window-left :which-key "left window")
"C-j" '(evil-window-down :which-key "down window")
"C-k" '(evil-window-up :which-key "up window")
"C-l" '(evil-window-right :which-key "right window")
)

(defun me/back-to-term-normal ()
  (interactive)
  (term-line-mode)
  (evil-normal-state)
)

(general-define-key
:states '(insert emacs)
:keymaps '(term-raw-map term-mode-map)
"C-;" '(me/back-to-term-normal :which-key "escape")
"C-y" '(term-paste :which-key "paste")
"C-k" '(term-send-up :which-key "up")
"C-j" '(term-send-down :which-key "<down>")
"C-z" '(comint-clear-buffer :which-key "clear buffer")
;; this also works by simulating the key as up/down
;; "C-k" (general-simulate-key "<up>")
;; "C-j" (general-simulate-key "<down>")
)
