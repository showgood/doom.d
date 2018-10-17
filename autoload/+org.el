;;; ~/.doom.d/autoload/+org.el -*- lexical-binding: t; -*-

;;;###autoload
(defun me/org-sum-rows (begin-row)
  "insert a org-table formula for sum all the rows for current column
    at the cell under cursor"
  (interactive "p")
  (cond ((or (<= begin-row 0)
             (>= begin-row (org-table-current-dline)) )
         (error "invalid row!"))
        (t
         (let ((cell (format "@%d$%d" (org-table-current-dline) (org-table-current-column))))
           (if (= begin-row 1)
               (me/org-append-formula cell
                                      (format "vsum(@2..@%d)" (- (org-table-current-dline) 1)))
             (me/org-append-formula cell
                                    (format "vsum(@%d..@%d)" begin-row (- (org-table-current-dline) 1)))
             )
           )
         )
        ))

;;;###autoload
(defun me/org-sum-cols (begin-column)
  "insert a org-table formula for sum columns begin-column -- current-column -1
   for current row at the cell under cursor"
  (interactive "p")
  (cond ((or (<= begin-column 0)
             (>= begin-column (org-table-current-column)) )
         (error "invalid column!"))
        (t
         (let (
               (cell (format "@%d$%d" (org-table-current-dline) (org-table-current-column)) )
               (sum-formula (format "vsum($%d..$%d)" begin-column (- (org-table-current-column) 1) )))
           (me/org-append-formula cell sum-formula)
           )
         )
        )
  )

;;;###autoload
(defun me/org-add-index-column ()
  "insert a index column to the table under cursor."
  (interactive)
  (org-table-goto-column 1)
  (org-table-insert-column)
  (me/org-append-formula "$1" "@#-1")
)

;;;###autoload
(defun me/create-org-table-from-clipboard ()
  "create a table in org mode with content from clipboard"
  (interactive)
  (let* ((buf (current-buffer)))
    (with-temp-buffer
      (switch-to-buffer (current-buffer) nil t)
      (insert (get-kill-ring))
      (mark-whole-buffer)
      (org-table-create-or-convert-from-region nil)
      (org-table-insert-hline)
      (goto-char (point-min))
      (open-line 1)
      (insert "#+tblname:")
      (append-to-buffer buf (point-min) (point-max))
      )))
