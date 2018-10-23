;;; ~/.doom.d/+org.el -*- lexical-binding: t; -*-

(setq org-todo-keywords '((sequence "☛ TODO(t)" "|" "✔ DONE(d)")
                            (sequence "⚑ WAITING(w)" "|")
                            (sequence "|" "✘ CANCELED(c)")))

(setq org-agenda-files '("~/org/gtd/"
                           "~/org/Inbox.org" ))

 ;; https://emacs.stackexchange.com/questions/5889/how-to-highlight-text-permanently-in-org-mode
(add-to-list 'org-emphasis-alist
    '("*" (:emphasis t :foreground "red")))

; Targets include this file and any file contributing to the agenda
; - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                              (org-agenda-files :maxlevel . 9))))
(setq org-refile-use-outline-path 'file)

;; prettify the exported table in HTML, add border and column divider etc
(setq org-html-table-default-attributes '(:border "2" :rules "all" :frame "border"))
