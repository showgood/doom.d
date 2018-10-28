;; adopted from
;; https://github.com/ganmacs/emacs-dayone/blob/master/emacs-dayone.el

;; author: showgood
;; This package requires following:
;; - ox-gfm for exporting org mode content to markdown
;; - dayone2 cli tool
;; dayone2 cli tools can be installed via:
;; sudo bash /Applications/Day\ One.app/Contents/Resources/install_cli.sh
;; for more information on dayone2 cli:
;; http://help.dayoneapp.com/tips-and-tutorials/command-line-interface-cli

(defvar emacs-dayone-cmd "dayone2 new")
(defvar emacs-dayone-succes-reg "^Created new entry with uuid:")

;;;###autoload
(defun emacs-dayone2/save (&optional b e)
  "Save current region to DayOne 2"
  (interactive "r")
  (let* ((start (if mark-active b (point-min)))
         (end   (if mark-active e (point-max)))
         (contents (buffer-substring start end))
         (tmp-file (make-temp-file "emacs-dayone")))
    (with-temp-file tmp-file (insert contents))
    (message (number-to-string  start))
    (emacs-dayone2/save-from-file tmp-file)))

;;;###autoload
(defun emacs-dayone/export-to-md ()
  (org-gfm-export-to-markdown)
  (concat (buffer-file-name) ".md")
)

;;;###autoload
(defun emacs-dayone2/save-as-md ()
  "Export current region as markdown and
   save to DayOne"
  (interactive)
  (emacs-dayone2/save-from-file (emacs-dayone/export-to-md))
)

;;;###autoload
(defun emacs-dayone2/save-from-file (tmp-file)
  "Save the content from tmp-file to DayOne"
  (interactive "r")
   (if (string-match emacs-dayone-succes-reg
          (shell-command-to-string
             (format "%s < %s" emacs-dayone-cmd tmp-file)))
        (message "Success: contents saved")
      (message "Failed: can't saved"))
    (delete-file tmp-file))

(provide 'emacs-dayone2)
