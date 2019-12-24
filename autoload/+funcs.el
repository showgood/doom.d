;;; ~/.doom.d/autoload/+funcs.el -*- lexical-binding: t; -*-

;; from prelude
;;;###autoload
(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))


;; https://emacs.stackexchange.com/questions/5371/how-to-change-emacs-windows-from-vertical-split-to-horizontal-split
;;;###autoload
(defun window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

;;;###autoload
(defun me/create-rg-ignore-file ()
  "make a copy of rg ignore file, syntax refer to https://oremacs.com/2018/03/05/grep-exclude/"
  (interactive)
  (copy-file (format "%s/rg-ignore" doom-private-dir) (format "%s/.ignore" default-directory))
)

;;;###autoload
(defun me/open-module-init ()
  (interactive)
  (find-file (format "%s/config.el" doom-private-dir))
)

;;;###autoload
(defun me/browse-notes ()
  (interactive) (doom-project-browse +notes-dir))

;;;###autoload
(defun me/find-in-notes ()
  (interactive) (doom-project-find-file +notes-dir))

;;;###autoload
(defun me/open-message-buffer ()
  (interactive)
  (switch-to-buffer "*Messages*"))

;;;###autoload
;;;http://emacsredux.com/blog/2013/03/27/copy-filename-to-the-clipboard/
(defun copy-file-path-to-clipboard ()
  "Copy the current buffer file full path to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;;;###autoload
;;;https://jblevins.org/log/clipboard
;;; do not pollute the killring
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))


;;;###autoload
(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (kill-new (buffer-name))
  (message "Copied file name '%s' to clipboard." (buffer-name))
)

;;;###autoload
;; http://blog.binchen.org/posts/convert-multiple-line-into-one-big-string-in-emacs.html
(defun strip-convert-lines-into-one-big-string (beg end)
"strip and convert selected lines into one big string which is copied into kill ring.
When transient-mark-mode is enabled, if no region is active then only the
current line is acted upon.

If the region begins or ends in the middle of a line, that entire line is
copied, even if the region is narrowed to the middle of a line.

Current position is preserved."
  (interactive "r")
  (let (str (orig-pos (point-marker)))
  (save-restriction
    (widen)
    (when (and transient-mark-mode (not (use-region-p)))
      (setq beg (line-beginning-position)
            end (line-beginning-position 2)))

    (goto-char beg)
    (setq beg (line-beginning-position))
    (goto-char end)
    (unless (= (point) (line-beginning-position))
      (setq end (line-beginning-position 2)))

    (goto-char beg)
    (setq str (replace-regexp-in-string "[ \t]*\n" "" (replace-regexp-in-string "^[ \t]+" "" (buffer-substring-no-properties beg end))))
    ;; (message "str=%s" str)
    (kill-new str)
    (goto-char orig-pos)))
  )

;;;###autoload
(defun get-kill-ring()
  (substring-no-properties (car kill-ring))
)

;;;###autoload
(defun my-format-date (date)
  "convert a date in string `2019-05-30' to `30 May 2019 (Thursday)'"
  (let* ((time (parse-time-string date))
         (day (nth 3 time))
         (month (nth 4 time))
         (year (nth 5 time)))
    (format-time-string "%d %b %Y (%A)"
      (encode-time 0 0 0 day month year))))

;;;###autoload
(defun xah-clean-empty-lines ()
  "Replace repeated blank lines to just 1.
Works on whole buffer or text selection, respects `narrow-to-region'.

URL `http://ergoemacs.org/emacs/elisp_compact_empty_lines.html'
Version 2017-09-22"
  (interactive)
  (let ($begin $end)
    (if (region-active-p)
        (setq $begin (region-beginning) $end (region-end))
      (setq $begin (point-min) $end (point-max)))
    (save-excursion
      (save-restriction
        (narrow-to-region $begin $end)
        (progn
          (goto-char (point-min))
          (while (re-search-forward "\n\n\n+" nil "move")
            (replace-match "\n\n")))))))


;;;###autoload
(defun me/trunknotes-to-org-journal(file)
  "export trunknotes markdown file from `file' argument to org-journal file"

(interactive)

(message "process file: %s" file)

(with-temp-buffer file
  (insert-file-contents file)
  (xah-clean-empty-lines)

  (let* ((case-fold-search t)
        (working-dir (file-name-directory file))
        (input-file-name (file-name-nondirectory file))
        (ext (file-name-extension input-file-name))
        (date (replace-regexp-in-string (concat "." ext) "" input-file-name))
        (target-file-path (concat working-dir date ".org"))
        (time-pattern "^= *\\(.*\\) *=")
        (time nil)
        (hour nil)
        (minute nil))

    (unless (file-exists-p target-file-path)
      (write-region (format "*  %s\n" (my-format-date date)) nil target-file-path)
    )
    (goto-char (point-min))
    (while (search-forward-regexp time-pattern nil t)

      (when (match-string 0)
        (setq time (match-string 1)))
      (setq hour (substring time 0 2))
      (setq minute (substring time 2 4))

      (replace-match (format "** %s:%s" hour minute)))
      (append-to-file (point-min) (point-max) target-file-path)
  )))

;;;###autoload
(defun me/export-current-trunknote-file-to-org-journal ()
  "export current trunknote markdown file to org-journal file"
  (interactive)

  (me/trunknotes-to-org-journal (expand-file-name (buffer-file-name))))

;;;###autoload
(defun me/export-trunknotes-dir-to-org-journal (dir)
  "export all trunknotes markdown files at dir `dir' to org-journal"
  (interactive)
  (require 'find-lisp)
  (mapc 'me/trunknotes-to-org-journal (find-lisp-find-files dir "\\.markdown$")))

;;;###autoload
(defun me/join-string-with-comma (str)
  (mapconcat (lambda (x) (format "'%s'" x)) (split-string str " " t) ",")
)

;;;###autoload
(defun jk-org-kwds ()
  "parse the buffer and return a cons list of (property . value)
from lines like:
#+PROPERTY: value"
  (org-element-map (org-element-parse-buffer 'element) 'keyword
                   (lambda (keyword) (cons (org-element-property :key keyword)
                                           (org-element-property :value keyword)))))

;;;###autoload
(defun jk-org-kwd (KEYWORD)
  "get the value of a KEYWORD in the form of #+KEYWORD: value"
  (cdr (assoc KEYWORD (jk-org-kwds))))

;;;###autoload
(defun buffer-string* (buffer)
  (with-current-buffer buffer
    (buffer-string)))

;;;###autoload
(defun me/read-lines (filePath)
  "Return a list of lines of a file at filePath."
  (with-temp-buffer
    (insert-file-contents filePath)
    (split-string (buffer-string) "\n" t)))

;;;###autoload
(defun me/send-to-project-term (cmd)
  "send cmd to the project terminal"
  (interactive)
  (let ( (bf-name (format "%s-term" (+workspace-current-name))) )
    (comint-send-string bf-name (format "%s\n" cmd) )
    )
  )

;;;###autoload
(defun me/get-key (x separator)
  (kill-new (string-trim (car (split-string x separator t))))
)

;;;###autoload
(defun me/get-value (x separator)
  (kill-new (string-trim (cadr (split-string x separator t))))
)

;;;###autoload
(defun me/ivy-transform-with-separator (s)
  (replace-regexp-in-string me/ivy-separator "   " s)
)

;;;###autoload
(defun me/ivy-from-file (file)
  "read lines from file and display two column list by using me/ivy-separator"
  (interactive)
  (ivy-read "options: " (me/read-lines file)
            :action '(1
                      ("o" (lambda (x) (me/get-value x me/ivy-separator)) "get value")
                      ("j" (lambda (x) (me/get-key x me/ivy-separator)) "get key"))))

;; for eg, below is an example for using this me/ivy-from-file
;; (defvar me/ivy-separator "\\$\\$\\$")

;; (defun me/test-ivy ()
;;   (interactive)
;;   (ivy-set-display-transformer 'me/test-ivy 'me/ivy-transform-with-separator)
;;   (me/ivy-from-file "~/.doom.d/test.txt"))
