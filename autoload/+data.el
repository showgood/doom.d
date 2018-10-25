;;; ~/.doom.d/autoload/+data.el -*- lexical-binding: t; -*-

;; depends on package json-snatcher
;;;###autoload
(defun me/jsons-print-path-as-list ()
  "Print the python path to the JSON value under point, and save it in the kill ring.
   output: ['a', 'b', 'c'] instead of [\"a\"][\"b\"][\"c\"]"
  (let ((path (jsons-get-path))
        (i 0)
        (python_str ""))
    (setq path (reverse path))
    (while (< i (length path))
      (if (numberp (elt path i))
          (progn
            (setq python_str (concat python_str (number-to-string (elt path i)) ","))
            (setq i (+ i 1)))
        (progn
          (if (equal i (- (length path) 1))
          (setq python_str (concat python_str (elt path i)))
          (setq python_str (concat python_str (elt path i) ","))
              )
          (setq i (+ i 1)))))
    (unless (string-empty-p python_str)
      (setq python_str (concat "[" python_str "]"))
    )
    (setq python_str (s-replace "\"" "\'" python_str))
    (kill-new python_str)
    (princ python_str)
))

;; print out xpath and copy to clipboard
;;;###autoload
(defun nxml-where ()
  "Display the hierarchy of XML elements the point is on as a
path. from http://www.emacswiki.org/emacs/NxmlMode"
  (interactive)
  (let ((path nil))
    (save-excursion
      (save-restriction
        (widen)
        (while
            (and (< (point-min) (point)) ;; Doesn't error if point is at
                                         ;; beginning of buffer
                 (condition-case nil
                     (progn
                       (nxml-backward-up-element) ; always returns nil
                       t)
                   (error nil)))
          (setq path (cons (xmltok-start-tag-local-name) path)))
        (if (called-interactively-p t)
            (progn
                (message "/%s" (mapconcat 'identity path "/"))
                (kill-new (format "/%s" (mapconcat 'identity path "/"))))
          (format "/%s" (mapconcat 'identity path "/")))))))


;;;###autoload
(defun xml-format ()
  "format the xml file using external tool - `xmllint`"
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "xmllint --format -" (buffer-name) t)
))
