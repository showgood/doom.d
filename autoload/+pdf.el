;; https://gist.github.com/myrjola/15585e3461b4d3178953

(defun pdf-annot-markups-as-org-text (pdfpath &optional title level)
  "Acquire highlight annotations as text"

  (interactive "fPath to PDF: ")
  (let* ((outputstring "") ;; the text to be return
         (title (or title (replace-regexp-in-string "-" " " (file-name-base pdfpath ))))
         (level (or level (1+ (org-current-level)))) ;; I guess if we're not in an org-buffer this will fail
         (levelstring (make-string level ?*))
         (pdf-image-buffer (get-buffer-create "*temp pdf image*"))
         )
    (with-temp-buffer ;; use temp buffer to avoid opening zillions of pdf buffers
      (insert-file-contents pdfpath)
      (pdf-view-mode)
      (pdf-annot-minor-mode t)
      (ignore-errors (pdf-outline-noselect (current-buffer)))

      (setq outputstring (concat levelstring " Annotations from " title "\n\n")) ;; create heading

      (let* ((annots (sort (pdf-annot-getannots nil (list 'square 'highlight)  nil)
                           'pdf-annot-compare-annotations))
             (last-outline-page -1))
        (mapc
         (lambda (annot) ;; traverse all annotations
           (message "%s" annot)
           (let* ((page (assoc-default 'page annot))
                  (height (nth 1 (assoc-default 'edges annot)))
                  (type (assoc-default 'type annot))
                  (id (symbol-name (assoc-default 'id annot)))
                  (text (pdf-info-gettext page (assoc-default 'edges annot)))
                  (imagefile (concat id ".png"))
                  (region (assoc-default 'edges annot))
                  ;; use pdfview link directly to page number
                  (linktext (concat "[[pdfview:" pdfpath "::" (number-to-string page)
                                    "++" (number-to-string height) "][" title  "]]" ))
                  ;; The default export is for highlight annotations
                  (annotation-as-org (concat text "\n(" linktext ", " (number-to-string page) ")\n\n"))
                  )

             ;; Square annotations are written to images and displayed inline
             (when (eq type 'square)
               (pdf-view-extract-region-image (list region) page (cons 1000 1000) pdf-image-buffer)
               (with-current-buffer pdf-image-buffer
                 (write-file imagefile))
               (setq annotation-as-org (concat "[[file:" imagefile "]]" "\n\n(" linktext ", " (number-to-string page) ")\n\n")))

             ;; Insert outline heading if not already inserted
             (let* ((outline-info (ignore-errors
                                    (with-current-buffer (pdf-outline-buffer-name)
                                      (pdf-outline-move-to-page page)
                                      (pdf-outline-link-at-pos))))
                    (outline-page (when outline-info (number-to-string (assoc-default 'page outline-info)))))
               (when outline-info
                 (unless (equal last-outline-page outline-page)
                   (setq outputstring (concat outputstring
                                              (make-string (+ level (assoc-default 'depth outline-info)) ?*)
                                              " "
                                              (assoc-default 'title outline-info)
                                              ", "
                                              outline-page
                                              "\n\n"))
                   (setq last-outline-page outline-page))))

             (setq outputstring (concat outputstring annotation-as-org)))
           )

         annots))
      )
    (insert outputstring)
    ))
