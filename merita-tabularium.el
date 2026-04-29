;;; merita-tabularium.el --- Tabularium integration for Merita -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2026 Paul H. McClelland

;; Author: Paul H. McClelland <paulhmcclelland@protonmail.com>
;; Maintainer: Paul H. McClelland <paulhmcclelland@protonmail.com>
;; Version: 0.5.1
;; Package-Requires: ((emacs "29.1") (merita "0.1.0") (tabularium "0.4.5"))
;; Keywords: bib, data
;; URL: https://codeberg.org/phmcc/merita
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This module registers the Merita database with Tabularium, allowing
;; browsing, searching, and editing of scholarly records using
;; Tabularium's interface.  Linked entries from `related_entries' are
;; displayed in form buffers.
;;
;; Loading: add to the merita use-package :init block:
;;   (with-eval-after-load 'tabularium
;;     (require 'merita-tabularium nil t))
;;
;; This registers the hook at startup so the bridge activates when
;; either package loads, even with deferred loading.
;;
;; First time (creates the .schema.el file):
;;   M-x merita-tabularium-setup
;;
;; Subsequent sessions:
;;   M-x tabularium-open RET merita RET

;;; Code:

(require 'merita)
(require 'tabularium)

;;; * 1. Schema Registration

(defcustom merita-tabularium-quick-entry-fields
  '(type title authors year journal author_position status)
  "Fields shown in tabularium quick-entry mode for merita."
  :type '(repeat symbol)
  :group 'merita)

;;;###autoload
(defun merita-tabularium-register ()
  "Register the Merita database with `tabularium'.
After calling this, you can open the merita database with
`tabularium-open' and use all of tabularium's browsing and editing features."
  (interactive)
  ;; Ensure merita database exists
  (merita-init)
  ;; Register with tabularium
  (let ((schema
         `("merita"
           :file ,(merita--db-file)
           :quick-entry-fields ,merita-tabularium-quick-entry-fields
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
                   :choice ("journal-article" "review-article" "editorial"
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
                   :complete (:type related :field journal :autofill t))
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
                   :complete (:type related :field journal :autofill t))
            (:name edition
                   :type text :prompt "Edition" :hidden t)
            (:name editors
                   :type text :prompt "Editor(s)" :hidden t)
            (:name series
                   :type text :prompt "Series" :hidden t)
            (:name isbn
                   :type text :prompt "ISBN" :hidden t
                   :complete (:type related :field book_title :autofill t))
            (:name issn
                   :type text :prompt "ISSN" :hidden t
                   :complete (:type related :field journal :autofill t))

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
                   :choice ("first" "co-first" "second" "third"
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
                   :choice ("published" "in-press" "accepted" "revision"
                             "review" "submitted" "preparation" "retracted"))
            (:name peer_reviewed
                   :type choice :prompt "Peer reviewed?" :hidden t
                   :choice ("1" "0"))

            ;; -- Content --
            (:name abstract
                   :type text :prompt "Abstract" :hidden t)
            (:name keywords
                   :type text :prompt "Keywords" :hidden t)
            (:name mesh_terms
                   :type text :prompt "MeSH terms" :hidden t)

            ;; -- Grant/funding --
            (:name funding
                   :type text :prompt "Grant number(s)" :hidden t)
            (:name grant_role
                   :type choice :prompt "Grant role" :hidden t
                   :choice ("PI" "Co-PI" "Co-I" "Mentor" "Trainee" "Other"))
            (:name grant_amount
                   :type number :prompt "Grant amount" :hidden t)
            (:name grant_period
                   :type text :prompt "Grant period" :hidden t)

            ;; -- Awards --
            (:name awards
                   :type text :prompt "Awards" :hidden t)
            (:name award_body
                   :type text :prompt "Awarding body" :hidden t)

            ;; -- Notes/metadata --
            (:name notes
                   :type text :prompt "Notes" :hidden t)
            (:name tags
                   :type text :prompt "Tags" :hidden t
                   :complete historical)
            (:name bibtex_key
                   :type text :prompt "BibTeX key" :hidden t)
            (:name bibtex_type
                   :type text :prompt "BibTeX type" :hidden t)

            ;; -- Software/dataset fields --
            (:name repo_url
                   :type text :prompt "Repository URL" :hidden t)
            (:name repo_language
                   :type text :prompt "Language" :hidden t
                   :complete historical)
            (:name pkg_registry
                   :type choice :prompt "Package registry" :hidden t
                   :choice ("" "CRAN" "PyPI" "MELPA" "npm" "Bioconductor"))
            (:name pkg_name
                   :type text :prompt "Package name" :hidden t)
            (:name pkg_version
                   :type text :prompt "Version" :hidden t)

            ;; -- Local file --
            (:name file_path
                   :type text :prompt "File path" :hidden t)

            ;; -- Linkage --
            (:name links
                   :type integer :prompt "Links" :width 5)

            ;; -- Timestamps --
            (:name date_added
                   :type text :prompt "Date added" :width 19)
            (:name date_modified
                   :type text :prompt "Date modified" :hidden t)))))

    ;; Add to tabularium-schemas, replacing if already present
    (setq tabularium-schemas
          (cons schema
                (cl-remove-if (lambda (s) (equal (car s) "merita"))
                              tabularium-schemas)))
    ;; Write the .schema.el file so tabularium can find it in future sessions
    (tabularium--save-schema-to-file "merita")
    (message "Merita registered with Tabularium.  Schema written to %s"
             (concat (file-name-sans-extension (merita--db-file)) tabularium-schema-file-suffix))))

;;;###autoload
(defun merita-tabularium-setup ()
  "One-time setup: initialize the database, register with `tabularium', and open.
If the database is empty, offers to run `merita-seed'."
  (interactive)
  (merita-tabularium-register)
  (let ((count (merita--count)))
    (when (zerop count)
      (if (and (fboundp 'merita-seed)
               (y-or-n-p "Database is empty.  Run merita-seed to populate from CV? "))
          (progn
            (merita-seed)
            (message "Seeded %d entries." (merita--count)))
        (message "Database is empty.  Use merita-new-entry, merita-add-from-doi, or merita-import-* to populate."))))
  (tabularium-open "merita"))

;; Auto-register on load
(merita-tabularium-register)

;;; * 2. Linked Entries in Form Buffer

(defun merita-tabularium--on-merita-p ()
  "Return non-nil if the current form buffer is editing a merita entry."
  (and (bound-and-true-p tabularium-entry-schema-name)
       (equal tabularium-entry-schema-name "merita")))

(defun merita-tabularium--on-link-p ()
  "Return non-nil if point is on a merita link line."
  (and (merita-tabularium--on-merita-p)
       (get-text-property (point) 'merita-link-id)))

(defun merita-tabularium--pre-render-hook ()
  "Install merita's citation header for merita form buffers.
Added to `tabularium-entry-pre-render-hook' so the function is in
place before the layout pass consults it."
  (when (merita-tabularium--on-merita-p)
    (setq tabularium-entry-header-function
          #'merita-tabularium--header-string)))

(defun merita-tabularium--render-hook ()
  "Inject linked entries and hint line into the merita form buffer.
Added to `tabularium-entry-render-hook'."
  (when (merita-tabularium--on-merita-p)
    (when (bound-and-true-p tabularium-entry-editing-id)
      (let ((id tabularium-entry-editing-id)
            (w 80))
        ;; Inject links before the footer
        (let ((related (merita--get-related id)))
          (when related
            (save-excursion
              (goto-char tabularium-entry-footer-start)
              (insert "  " (propertize "Linked entries:"
                                       'face 'font-lock-type-face) "\n")
              (dolist (rel related)
                (let* ((rtype (or (alist-get 'relation_type rel) ""))
                       (etype (or (alist-get 'type rel) ""))
                       (short (merita--entry-short-title rel))
                       (rtitle (or (alist-get 'title rel) ""))
                       (fixed 12)
                       (avail (- w 2 fixed
                                 (length rtype) (length etype) (length short)))
                       (title-str (merita--truncate rtitle (max 10 avail)))
                       (line-start (point)))
                  (insert (format "  {%s} [%s] %s. \"%s\"\n"
                                  rtype etype short title-str))
                  (put-text-property line-start (point)
                                     'merita-linked-entry-id
                                     (alist-get 'related_id rel))
                  (put-text-property line-start (point)
                                     'merita-link-id
                                     (alist-get 'id rel))
                  (put-text-property line-start (point)
                                     'tabularium-navigable t)))
              (insert "\n")
              (setq tabularium-entry-footer-start (point)))))
        ;; Append merita hint line after the standard hints
        (goto-char (point-max))
        (insert "\n\n  " (propertize "── Merita ──" 'face '(:weight bold)) "\n")
        (insert "  "
                (propertize "L" 'face 'help-key-binding) " Link  "
                (propertize "l" 'face 'help-key-binding) " Show link  "
                (propertize "b" 'face 'help-key-binding) " URL  "
                (propertize "O" 'face 'help-key-binding) " File  "
                (propertize "M-RET" 'face 'help-key-binding) " Edit link")))))

(defun merita-tabularium--header-string ()
  "Return the citation header for the current merita form buffer.
Used as `tabularium-entry-header-function' to render a wrapped
citation preview at the top of the form.  Returns nil when the
entry has no data to render (e.g. a brand-new empty form)."
  (when (merita-tabularium--on-merita-p)
    (let* ((entry (cl-loop for (k . v) in tabularium-entry--values
                           collect (cons k v)))
           (w 80)
           (preview-entry (cons (cons 'doi nil)
                                (assq-delete-all 'doi (copy-alist entry))))
           (fmt (let ((merita--latex-context nil))
                  (funcall (merita--resolve-citation-style
                            merita-default-citation-style)
                           preview-entry))))
      (when (and fmt
                 (not (string-empty-p fmt))
                 ;; Suppress when the formatted citation contains no actual
                 ;; content — only punctuation, brackets, and whitespace.
                 (not (string-empty-p
                       (replace-regexp-in-string
                        "[[:punct:][:space:]]+" "" fmt))))
        (with-temp-buffer
          (let ((start (point))
                (fill-prefix "  ")
                (fill-column (- w 2)))
            (insert "  " fmt)
            (fill-region start (point))
            (put-text-property start (point) 'face 'font-lock-doc-face))
          (buffer-string))))))

(defun merita-tabularium--new-hook ()
  "Auto-populate merita-specific fields on new entry creation.
Sets initial values for Citations, Links, and timestamp fields so
the user does not have to fill in bookkeeping defaults manually."
  (when (merita-tabularium--on-merita-p)
    (let ((today (format-time-string "%Y-%m-%d %H:%M:%S")))
      (dolist (default '((citations . "0")
                         (links . "0")
                         (date_added . NEEDS-TODAY)
                         (date_modified . NEEDS-TODAY)))
        (let ((field (car default))
              (val (cdr default)))
          ;; Only set if currently empty (don't overwrite user input)
          (let ((current (alist-get field tabularium-entry--values)))
            (when (or (null current)
                      (and (stringp current) (string-empty-p current)))
              (setf (alist-get field tabularium-entry--values)
                    (if (eq val 'NEEDS-TODAY) today val)))))))))

(defun merita-tabularium--pre-submit-hook ()
  "Update merita-specific bookkeeping fields on every save.
Sets =date_modified= to the current timestamp so the field tracks
the most recent edit, and recomputes =author_count= from the
=authors= field."
  (when (merita-tabularium--on-merita-p)
    ;; Always refresh date_modified
    (setf (alist-get 'date_modified tabularium-entry--values)
          (format-time-string "%Y-%m-%d %H:%M:%S"))
    ;; Recompute author_count from authors (comma-separated).
    ;; Empty authors → 0; otherwise count commas + 1.
    (let* ((authors (alist-get 'authors tabularium-entry--values))
           (authors-str (if authors (format "%s" authors) ""))
           (count (if (string-empty-p (string-trim authors-str))
                      0
                    (1+ (cl-count ?, authors-str)))))
      (setf (alist-get 'author_count tabularium-entry--values)
            (number-to-string count)))))

(defconst merita-tabularium--type-required-fields
  '(("journal-article"   . (authors year journal))
    ("review-article"    . (authors year journal))
    ("editorial"         . (authors year journal))
    ("letter"            . (authors year journal))
    ("case-report"       . (authors year journal))
    ("book"              . (authors year publisher))
    ("book-chapter"      . (authors year book_title publisher))
    ("conference-paper"  . (authors year conference))
    ("preprint"          . (authors year))
    ("technical-report"  . (authors year))
    ("thesis-doctoral"   . (authors year))
    ("thesis-masters"    . (authors year))
    ("podium"            . (authors year conference))
    ("poster"            . (authors year conference))
    ("invited-talk"      . (authors year conference))
    ("keynote"           . (authors year conference))
    ("workshop"          . (authors year conference))
    ("abstract"          . (authors year))
    ("patent"            . (authors year))
    ("grant"             . (authors year))
    ("award"             . (authors year))
    ("dataset"           . (authors year))
    ("software"          . (authors year))
    ("media"             . (authors year))
    ("other"             . (authors year)))
  "Per-type required fields for merita entries.
Each entry maps an entry type (string from the schema's `:choice'
list) to a list of field names (symbols) that are required when
that type is selected.  Used by `merita-tabularium--required-p'.

These required-by-convention fields are in addition to the static
`:required t' fields in the schema (currently `type' and `title'),
which are required regardless of entry type.")

(defun merita-tabularium--required-p (field-name values)
  "Return non-nil if FIELD-NAME is required for the current entry type.
Consults `merita-tabularium--type-required-fields' against the
=type= field in VALUES.  Used as a member of
`tabularium-entry-required-field-functions'."
  (when (merita-tabularium--on-merita-p)
    (let* ((type (alist-get 'type values))
           (type-str (when type (format "%s" type)))
           (required-fields (cdr (assoc type-str
                                        merita-tabularium--type-required-fields))))
      (memq field-name required-fields))))

;;; ** 2.1. Link Commands

(defun merita-tabularium-jump-to-link ()
  "Show the linked merita entry at point in the merita native view."
  (interactive)
  (let ((linked-id (get-text-property (point) 'merita-linked-entry-id)))
    (if linked-id
        (let ((entry (merita--get linked-id)))
          (if entry
              (merita--display-entry entry)
            (message "Linked entry not found.")))
      (message "Not on a linked-entry line."))))

(defun merita-tabularium-edit-link ()
  "Open the linked merita entry at point in a tabularium form buffer.
Prompts for unsaved changes if the current form has them."
  (interactive)
  (let ((linked-id (get-text-property (point) 'merita-linked-entry-id)))
    (if linked-id
        (tabularium-new-entry linked-id)
      (message "Not on a linked-entry line."))))

(defun merita-tabularium-edit-link-at-point ()
  "Edit the link type at point in a tabularium form buffer."
  (interactive)
  (let ((link-id (get-text-property (point) 'merita-link-id)))
    (when link-id
      (merita-edit-link-type link-id)
      (tabularium-entry-render))))

(defun merita-tabularium-remove-link-at-point ()
  "Remove the link at point in a tabularium form buffer."
  (interactive)
  (let ((link-id (get-text-property (point) 'merita-link-id)))
    (when link-id
      (when (yes-or-no-p "Remove this link? ")
        (merita--delete-relation link-id)
        (tabularium-entry-render)
        (message "Link removed.")))))

(defun merita-tabularium-new-link ()
  "Link the current entry to another merita entry."
  (interactive)
  (unless (merita-tabularium--on-merita-p)
    (user-error "Not a merita entry"))
  (unless (bound-and-true-p tabularium-entry-editing-id)
    (user-error "Save the entry before linking"))
  (let ((rid (merita--read-entry-id "Link to: "))
        (rtype (completing-read "Link type: "
                                merita-relation-types nil t)))
    (merita--insert-relation tabularium-entry-editing-id rid rtype)
    (tabularium-entry-render)))

(defun merita-tabularium-open-url ()
  "Open the DOI, PMID, or URL of the current merita entry."
  (interactive)
  (unless (merita-tabularium--on-merita-p)
    (user-error "Not a merita entry"))
  (when (bound-and-true-p tabularium-entry-editing-id)
    (merita--open-url tabularium-entry-editing-id)))

(defun merita-tabularium-open-file ()
  "Open the local file for the current merita entry."
  (interactive)
  (unless (merita-tabularium--on-merita-p)
    (user-error "Not a merita entry"))
  (when (bound-and-true-p tabularium-entry-editing-id)
    (merita--open-file tabularium-entry-editing-id)))

;;; ** 2.2. Context-Aware Advice

(defun merita-tabularium--around-edit-field (orig-fn &rest args)
  "Advice for `tabularium-entry-edit-field'.
On a merita link line, edit the link type.  Otherwise call ORIG-FN."
  (if (merita-tabularium--on-link-p)
      (merita-tabularium-edit-link-at-point)
    (apply orig-fn args)))

(defun merita-tabularium--around-reset-field (orig-fn &rest args)
  "Advice for `tabularium-entry-reset-field'.
On a merita link line, remove the link.  Otherwise call ORIG-FN."
  (if (merita-tabularium--on-link-p)
      (merita-tabularium-remove-link-at-point)
    (apply orig-fn args)))

;;; ** 2.3. Setup

(defun merita-tabularium--setup-keys ()
  "Add merita keybindings to `tabularium-entry-mode-map'."
  ;; l: show the linked entry in the merita native view
  (define-key tabularium-entry-mode-map (kbd "l")
    (lambda ()
      (interactive)
      (if (merita-tabularium--on-link-p)
          (merita-tabularium-jump-to-link)
        (message "Not on a linked-entry line."))))
  ;; M-RET: open the linked entry in a tabularium form buffer
  (define-key tabularium-entry-mode-map (kbd "M-RET")
    (lambda ()
      (interactive)
      (if (merita-tabularium--on-link-p)
          (merita-tabularium-edit-link)
        (message "Not on a linked-entry line."))))
  ;; L: create a new link
  (define-key tabularium-entry-mode-map (kbd "L")
    (lambda ()
      (interactive)
      (if (merita-tabularium--on-merita-p)
          (merita-tabularium-new-link)
        (message "Not a merita entry."))))
  ;; b: open URL
  (define-key tabularium-entry-mode-map (kbd "b")
    (lambda ()
      (interactive)
      (if (merita-tabularium--on-merita-p)
          (merita-tabularium-open-url)
        (message "Not a merita entry."))))
  ;; f: open local file
  (define-key tabularium-entry-mode-map (kbd "O")
    (lambda ()
      (interactive)
      (if (merita-tabularium--on-merita-p)
          (merita-tabularium-open-file)
        (message "Not a merita entry.")))))

;; Wire up
(advice-add 'tabularium-entry-edit-field :around
            #'merita-tabularium--around-edit-field)
(advice-add 'tabularium-entry-reset-field :around
            #'merita-tabularium--around-reset-field)
(add-hook 'tabularium-entry-pre-render-hook
          #'merita-tabularium--pre-render-hook)
(add-hook 'tabularium-entry-render-hook
          #'merita-tabularium--render-hook)
(add-hook 'tabularium-entry-new-hook
          #'merita-tabularium--new-hook)
(add-hook 'tabularium-entry-pre-submit-hook
          #'merita-tabularium--pre-submit-hook)
(add-hook 'tabularium-entry-required-field-functions
          #'merita-tabularium--required-p)
(merita-tabularium--setup-keys)

(provide 'merita-tabularium)

;;; merita-tabularium.el ends here
