;;; merita-tabula.el --- Tabula integration for Merita -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: Paul H. McClelland
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 Paul H. McClelland

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (merita "0.1.0") (tabula "0.3.0"))
;; Keywords: bib, data
;; URL: https://codeberg.org/phmcc/merita

;;; Commentary:

;; This module registers the Merita database with Tabula, allowing you
;; to browse, search, and edit your scholarly records using Tabula's
;; tabulated-list interface, hydra menus, and form-based entry.
;;
;; Quick start (first time):
;;   (require 'merita-tabula)
;;   M-x merita-tabula-setup
;;
;; This initializes the database, writes the .schema.el file that
;; Tabula expects, optionally seeds data from your CV, and opens the
;; Tabula view.
;;
;; Subsequent sessions (schema file already on disk):
;;   M-x tabula-open RET merita RET
;;
;; This is a personal bridge module.  The core merita package does not
;; depend on tabula — this file is optional glue for users who want
;; both packages to work together.

;;; Code:

(require 'merita)
(require 'tabula)

(defcustom merita-tabula-quick-entry-fields
  '(type title authors year journal author_position status)
  "Fields shown in tabula quick-entry mode for merita."
  :type '(repeat symbol)
  :group 'merita)

;;;###autoload
(defun merita-tabula-register ()
  "Register the Merita database with Tabula's schema system.
After calling this, you can open the merita database with
`tabula-open' and use all of tabula's browsing and editing features."
  (interactive)
  ;; Ensure merita database exists
  (merita-init)
  ;; Register with tabula
  (let ((schema
         `("merita"
           :file ,(merita--db-file)
           :quick-entry-fields ,merita-tabula-quick-entry-fields
           :default-sort desc
           :views ((:name "Chronological"
                    :default t
                    :columns (status type year month day
                              title authors journal volume pages
                              doi pmid author_position
                              impact_factor citations links date_added)
                    :sort ((year . desc) (month . desc))))
           :fields
           (;; -- Primary key --
            (:name id
             :type integer :primary t :prompt "ID"
             :width 5)

            ;; -- Date fields --
            (:name year
             :type integer :prompt "Year" :width 5)
            (:name month
             :type integer :prompt "Month (1-12)" :width 3)
            (:name day
             :type integer :prompt "Day (1-31)" :width 3 :hidden t)

            ;; -- Core bibliographic fields --
            (:name type
             :type choice :prompt "Type" :required t :width 18
             :choices ("journal-article" "review-article" "editorial"
                       "letter" "case-report" "book" "book-chapter"
                       "conference-paper" "preprint" "technical-report"
                       "thesis-doctoral" "thesis-masters"
                       "podium" "poster" "invited-talk" "keynote"
                       "workshop" "abstract" "patent" "grant" "award"
                       "dataset" "software" "media" "other"))
            (:name title
             :type text :prompt "Title" :required t :width 50)
            (:name authors
             :type text :prompt "Authors" :width 40)

            ;; -- Journal fields --
            (:name journal
             :type text :prompt "Journal" :width 30
             :complete historical)
            (:name journal_abbrev
             :type text :prompt "Journal (abbrev)" :hidden t
             :complete historical)
            (:name volume
             :type text :prompt "Volume" :width 6)
            (:name issue
             :type text :prompt "Issue" :width 6 :hidden t)
            (:name pages
             :type text :prompt "Pages" :width 12)
            (:name eid
             :type text :prompt "Article ID" :hidden t)

            ;; -- Book fields --
            (:name book_title
             :type text :prompt "Book title" :hidden t)
            (:name publisher
             :type text :prompt "Publisher" :hidden t
             :complete historical)
            (:name edition
             :type text :prompt "Edition" :hidden t)
            (:name editors
             :type text :prompt "Editor(s)" :hidden t)
            (:name series
             :type text :prompt "Series" :hidden t)
            (:name isbn
             :type text :prompt "ISBN" :hidden t)

            ;; -- Conference/presentation fields --
            (:name conference
             :type text :prompt "Conference" :hidden t
             :complete historical)
            (:name conference_location
             :type text :prompt "Location (city)" :hidden t)
            (:name conference_date
             :type text :prompt "Presentation date" :hidden t)
            (:name organization
             :type text :prompt "Organization" :hidden t
             :complete historical)

            ;; -- Thesis --
            (:name school
             :type text :prompt "School" :hidden t)

            ;; -- Identifiers --
            (:name doi
             :type text :prompt "DOI" :width 20)
            (:name pmid
             :type text :prompt "PMID" :width 10)
            (:name pmcid
             :type text :prompt "PMCID" :hidden t)
            (:name arxiv_id
             :type text :prompt "arXiv ID" :hidden t)
            (:name url
             :type text :prompt "URL" :hidden t)

            ;; -- Merita-specific --
            (:name author_position
             :type choice :prompt "Author position" :width 8
             :choices ("first" "co-first" "second" "third"
                       "senior" "co-senior" "corresponding" "middle" "sole"))
            (:name author_count
             :type integer :prompt "Author count" :hidden t)
            (:name impact_factor
             :type number :prompt "Impact factor" :width 6)
            (:name citations
             :type integer :prompt "Citations" :width 5)
            (:name altmetric
             :type integer :prompt "Altmetric score" :hidden t)
            (:name status
             :type choice :prompt "Status" :width 12
             :choices ("published" "in-press" "accepted" "revision"
                       "review" "submitted" "preparation" "retracted"))
            (:name peer_reviewed
             :type choice :prompt "Peer reviewed?" :hidden t
             :choices ("1" "0"))

            ;; -- Content --
            (:name abstract
             :type text :prompt "Abstract" :hidden t)
            (:name keywords
             :type text :prompt "Keywords (;-delimited)" :hidden t)
            (:name mesh_terms
             :type text :prompt "MeSH terms (;-delimited)" :hidden t)

            ;; -- Grant/funding --
            (:name funding
             :type text :prompt "Grant number(s)" :hidden t)
            (:name grant_role
             :type choice :prompt "Grant role" :hidden t
             :choices ("PI" "Co-PI" "Co-I" "Mentor" "Trainee" "Other"))
            (:name grant_amount
             :type number :prompt "Grant amount" :hidden t)
            (:name grant_period
             :type text :prompt "Grant period" :hidden t)

            ;; -- Awards --
            (:name awards
             :type text :prompt "Awards (;-delimited)" :hidden t)
            (:name award_body
             :type text :prompt "Awarding body" :hidden t)

            ;; -- Notes/metadata --
            (:name notes
             :type text :prompt "Notes" :hidden t)
            (:name tags
             :type text :prompt "Tags (;-delimited)" :hidden t
             :complete historical)
            (:name bibtex_key
             :type text :prompt "BibTeX key" :hidden t)
            (:name bibtex_type
             :type text :prompt "BibTeX type" :hidden t)

            ;; -- Software/dataset fields --
            (:name repo_url
             :type text :prompt "Repository URL" :hidden t)
            (:name repo_language
             :type text :prompt "Language (R, Python, etc.)" :hidden t
             :complete historical)
            (:name pkg_registry
             :type choice :prompt "Package registry" :hidden t
             :choices ("" "CRAN" "PyPI" "MELPA" "npm" "Bioconductor"))
            (:name pkg_name
             :type text :prompt "Package name" :hidden t)
            (:name pkg_version
             :type text :prompt "Version" :hidden t)

            ;; -- Linkage --
            (:name links
             :type integer :prompt "Links" :width 5)

            ;; -- Timestamps --
            (:name date_added
             :type text :prompt "Date added" :width 19)
            (:name date_modified
             :type text :prompt "Date modified" :hidden t)))))

    ;; Add to tabula-schemas, replacing if already present
    (setq tabula-schemas
          (cons schema
                (cl-remove-if (lambda (s) (equal (car s) "merita"))
                              tabula-schemas)))
    ;; Write the .schema.el file so tabula can find it in future sessions
    (tabula--save-schema-to-file "merita")
    (message "Merita registered with Tabula.  Schema written to %s"
             (concat (file-name-sans-extension (merita--db-file)) ".schema.el"))))

;;;###autoload
(defun merita-tabula-setup ()
  "One-time setup: initialize the database, register with Tabula, and open.
If the database is empty, offers to run `merita-seed'."
  (interactive)
  (merita-tabula-register)
  (let ((count (merita--count)))
    (when (zerop count)
      (if (and (fboundp 'merita-seed)
               (y-or-n-p "Database is empty.  Run merita-seed to populate from CV? "))
          (progn
            (merita-seed)
            (message "Seeded %d entries." (merita--count)))
        (message "Database is empty.  Use merita-add, merita-add-from-doi, or merita-import-* to populate."))))
  (tabula-open "merita"))

;; Auto-register on load
(merita-tabula-register)

(provide 'merita-tabula)

;;; merita-tabula.el ends here