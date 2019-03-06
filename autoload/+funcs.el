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
