;;; ~/.doom.d/+term.el -*- lexical-binding: t; -*-

(setq multi-term-dedicated-select-after-open-p t)

(defun setup-my-term-mode()
  (setq-local global-hl-line-mode nil)
  ;; (setq-local beacon-mode nil)
  (setq term-buffer-maximum-size 0)

    (general-define-key
    :states 'normal
    :keymaps 'term-raw-map
    "p" '(me/paste-in-term-mode :which-key "paste")
    "i" '(evil-emacs-state :which-key "insert")
    "a" '(evil-emacs-state :which-key "insert")
    "C-y" '(me/paste-in-term-mode :which-key "paste")
    "C-z" '(comint-clear-buffer :which-key "clear buffer")
    "C-h" '(evil-window-left :which-key "left window")
    "C-j" '(evil-window-down :which-key "down window")
    "C-k" '(evil-window-up :which-key "up window")
    "C-l" '(evil-window-right :which-key "right window")
    )

    (general-define-key
    :states '(insert emacs)
    :keymaps 'term-raw-map
    "C-;" '(evil-normal-state :which-key "escape")
    "C-y" '(me/paste-in-term-mode :which-key "paste")
    "C-k" '(term-send-up :which-key "up")
    "C-j" '(term-send-down :which-key "<down>")
    "C-z" '(comint-clear-buffer :which-key "clear buffer")
    ;; this also works by simulating the key as up/down
    ;; "C-k" (general-simulate-key "<up>")
    ;; "C-j" (general-simulate-key "<down>")
    )
)

(add-hook 'term-mode-hook #'setup-my-term-mode)
