;;; ~/.doom.d/autoload/+anki.el -*- lexical-binding: t; -*-

;;;###autoload
(defun me/parent-title (entry)
  "Extract the *direct* parent title."
  (let ((parent (org-element-property :parent entry)))
    (if (equal (car entry) 'item)
        (org-element-property :raw-value (org-element-property :parent (org-element-property :parent parent)))
        (org-element-property :raw-value (org-element-property :parent parent))
        )))

;;;###autoload
(defun me/org-create-anki-card (input deck)
  "Create one anki card for input. input is a cons (title . content)"
  (let ((prop '(":PROPERTIES:"
                ":ANKI_DECK: %s"
                ":ANKI_NOTE_TYPE: Basic"
                ":END:"))
        (heading (format "* %s" (car input)))
        (front "** Front")
        (front-card  (car input))
        (back "** Back")
        (back-card (cdr input))
        )
    (s-join "\n" (list heading
                       (format (s-join "\n" prop) deck)
                       front front-card back back-card))))

;;;###autoload
(defun me/anki-org-file-name (org-file)
  "figure out the export file name"
  (let ((base-path (f-dirname org-file))
        (base-name (f-base org-file))
        )

    (format "%s/%s-anki.org" base-path base-name)
  )
)

;;;###autoload
(defun me/org-create-anki-cards (deck)
  "save all the anki cards to deck `DECK` in exported file rule: "
  (interactive)
  (let ((content nil)
        (all-cards "")
        (out-file (me/anki-org-file-name (buffer-file-name)))
        )

    (org-element-map (org-element-parse-buffer) '(paragraph src-block item)
      (lambda (p)
        (let ((elt (assoc (me/parent-title p) content))
              (org-content (org-element-interpret-data p))
              )
          (if elt
              (setf (cdr elt) (concat (cdr elt) org-content))
            (if (me/parent-title p)
                ;; keep the order of cards
                (add-to-list 'content (cons (me/parent-title p) org-content) t)
              (message "parent title is nil.. ignore")
              )
            )
          )
        )
      )

    (dolist (obj content)
      (setq all-cards (concat all-cards (me/org-create-anki-card obj deck)))
      )

    (f-write-text all-cards 'utf-8 out-file)
    (message "all cards exported to file: %s" out-file)
    )
  )

;;;###autoload
(defun me/org-export-to-anki (deck)
  "transform current org file into an org file in format `anki-editor` can use"
  (interactive "sanki deck: ")
  (me/org-create-anki-cards deck)
)
