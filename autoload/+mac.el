;;; ~/.doom.d/autoload/+mac.el -*- lexical-binding: t; -*-

;; https://sam217pa.github.io/2016/09/01/emacs-iterm-integration/
;;;###autoload
(defun open-in-iterm ()
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
    " tell application \"iTerm2\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"cd %s\" \n"
            ;; string escaping madness for applescript
            (replace-regexp-in-string "\\\\" "\\\\\\\\"
                                      (shell-quote-argument (or default-directory "~"))))
    "   end tell\n"
    " end tell\n"
    " do shell script \"open -a iTerm\"\n"
    ))
)

;; https://sam217pa.github.io/2016/09/01/emacs-iterm-integration/
;;;###autoload
(defun iterm-focus ()
  (interactive)
  (do-applescript
   " do shell script \"open -a iTerm\"\n"
   ))

;; https://sam217pa.github.io/2016/09/01/emacs-iterm-integration/
;;;###autoload
(defun iterm-repeat-last-command ()
  (interactive)
  (do-applescript
   (concat
    "tell application \"iTerm2\"\n"
    "  activate\n"
    "    tell current session of current window\n"
    "      tell application \"System Events\" to keystroke (ASCII character 30)\n" ;; up arrow
    "      tell application \"System Events\" to key code 36\n" ;; return
    "    end tell\n"
    "end tell\n")))

;;;###autoload
(defun get-file-dir-or-home ()
  "If inside a file buffer, return the directory, else return home"
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
	"~/"
      (file-name-directory filename))))

;;;###autoload
(defun iterm-goto-filedir-or-home ()
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
    " tell application \"iTerm2\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"cd %s\" \n" (get-file-dir-or-home))
    "   end tell\n"
    " end tell\n"
    " do shell script \"open -a iTerm\"\n"
    ))
  )
