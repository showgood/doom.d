;;; ob-diagrams.el --- org-babel support for diagrams evaluation

;; Copyright (C) 2018 Alexei Nunez

;; Author: Alexei Nunez <alexeirnunez@gmail.com>
;; URL: https://github.com/arnm/ob-mermaid
;; Keywords: lisp
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating diagrams.

;;; Requirements:

;; diagrams | https://github.com/francoislaberge/diagrams

;;; Code:
(require 'ob)
(require 'ob-eval)

(defvar org-babel-default-header-args:diagrams
  '((:results . "file") (:type . "type") (:exports . "results"))
  "Default arguments for evaluatiing a mermaid source block.")

(defcustom ob-diagrams-cli-path nil
  "Path to diagrams.cli executable."
  :group 'org-babel
  :type 'string)

(defun org-babel-execute:diagrams (body params)
  (let* ((out-file (or (cdr (assoc :file params))
                       (error "diagrams requires a \":file\" header argument")))
         (chart-type (or (cdr (assoc :type params))
                       (error "diagrams requires a \":type\" header argument")))
         (temp-file (org-babel-temp-file "diagrams-"))
         (cmd (if (not ob-diagrams-cli-path)
                  (error "`ob-diagrams-cli-path' is not set")
                (concat (shell-quote-argument (expand-file-name ob-diagrams-cli-path))
                        " " chart-type " "
                        (org-babel-process-file-name temp-file)
                        " "
                        (org-babel-process-file-name out-file)))))
    (unless (file-exists-p ob-diagrams-cli-path)
      (error "could not find diagrams.cli executable at %s" ob-diagrams-cli-path))
    (with-temp-file temp-file (insert body))
    (message "%s" cmd)
    (org-babel-eval cmd "")
    nil))

(provide 'ob-diagrams)


;;; ob-diagrams.el ends here
