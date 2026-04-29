;;; merita.el --- Academic achievement database for Emacs -*- lexical-binding: t; -*-

;; Author: Paul H. McClelland
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 Paul H. McClelland

;; Version: 0.5.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: bib, data, convenience
;; URL: https://codeberg.org/phmcc/merita

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Merita (Latin: "things earned") is a personal scholarly achievement
;; database.  It manages publications, presentations, grants,
;; and awards in a queryable SQLite database with multi-format export.
;;
;; Merita differs from a standard reference manager like Zotero, `citar',
;; and `org-ref' in that it manages an individual researcher's own work,
;; tracking career-specific metadata no citation format captures: author
;; position, citation metrics, impact factors, and publication trajectories.
;;
;; Features:
;; - SQLite database with 50+ fields covering all scholarly output
;; - DOI-based entry via the Crossref API (free, no key required)
;; - ORCID import for bulk population from a public ORCID record
;; - Citation metrics from OpenAlex and Semantic Scholar
;; - BibTeX, CSV, and TSV import
;; - LaTeX CV export with Vancouver (medical) citation style
;; - Plain text export in APA, IEEE, Chicago, NLM, and Vancouver styles
;; - BibTeX, TSV, CSV, and RIS export for interoperability
;; - Abstract/presentation linking (one work, two CV sections)
;; - Career statistics: h-index, i10-index, FWCI, authorship breakdown
;; - Optional Tabula integration for interactive browsing/editing
;;
;; See the README for full documentation and example configurations.

;;; Code:

;;; * 0. Prerequisites

(require 'sqlite)
(require 'cl-lib)
(require 'json)
(require 'url)

(defvar merita--latex-context)
(defvar merita-text-formatter)
(defvar merita--style-formatter-map)
(defvar merita-org-type-aliases)

;;; * 1. Foundation

;;; ** 1.1. Customization

(defgroup merita nil
  "Academic achievement database."
  :group 'applications
  :prefix "merita-")

(defcustom merita-database-file "~/.emacs.d/merita.db"
  "Path to the Merita SQLite database file."
  :type 'file
  :group 'merita)

(defcustom merita-user-name nil
  "Author name as it appears in publication author lists.
Used for bolding in LaTeX export and statistics.
Set to nil to disable author name bolding."
  :type '(choice (const :tag "None" nil) string)
  :group 'merita)

(defcustom merita-user-name-variants nil
  "Alternative forms of the author's name (maiden name, abbreviations, etc.)."
  :type '(repeat string)
  :group 'merita)

(defcustom merita-default-export-directory "~/"
  "Default directory for export files."
  :type 'directory
  :group 'merita)

(defcustom merita-latex-formatter #'merita-format-vancouver
  "Function to format a single entry for LaTeX output.
Called with an entry alist; should return a LaTeX string."
  :type 'function
  :group 'merita)

(defcustom merita-default-citation-style 'vancouver
  "Default citation style for export and display.
Used by the export dispatcher and the entry view buffer.
Valid values: vancouver, apa, ieee, chicago, nlm, plain."
  :type '(choice (const :tag "Vancouver/AMA" vancouver)
                 (const :tag "APA 7th edition" apa)
                 (const :tag "IEEE" ieee)
                 (const :tag "Chicago author-date" chicago)
                 (const :tag "NLM" nlm)
                 (const :tag "Plain (minimal)" plain))
  :group 'merita)

(defcustom merita-latex-bold-author t
  "If non-nil, bold the user's name in LaTeX export."
  :type 'boolean
  :group 'merita)

(defcustom merita-crossref-mailto nil
  "Email for Crossref polite API access.
Optional but recommended for faster response times."
  :type '(choice (const :tag "None" nil) string)
  :group 'merita)

(defcustom merita-stats-sort-by 'all
  "Column used to rank sections in `merita-stats'.
When `pr', sections are sorted by peer-reviewed count.
When `all', sections are sorted by total count."
  :type '(choice (const :tag "Peer-reviewed" pr)
                 (const :tag "All entries" all))
  :group 'merita)

(defcustom merita-entry-display 'buffer
  "How to display the entry view buffer.
When `buffer', replaces the current window.
When `side', opens in a right-side window."
  :type '(choice (const :tag "Full buffer" buffer)
                 (const :tag "Side window" side))
  :group 'merita)

(defcustom merita-entry-show-empty-fields t
  "Whether to show empty fields in the entry view buffer.
When non-nil (the default), all fields are displayed with empty
values shown as blank.  When nil, only fields with non-empty
values are shown."
  :type 'boolean
  :group 'merita)

;;; ** 1.2. Command Map

;;;###autoload
(defvar merita-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") #'merita-init)
    (define-key map (kbd "N") #'merita-new-entry)
    (define-key map (kbd "d") #'merita-add-from-doi)
    (define-key map (kbd "f") #'merita-find)
    (define-key map (kbd "/") #'merita-search)
    (define-key map (kbd "l") #'merita-browse)
    (define-key map (kbd "e") #'merita-edit)
    (define-key map (kbd "D") #'merita-delete)
    (define-key map (kbd "x") #'merita-export-latex)
    (define-key map (kbd "b") #'merita-export-bibtex)
    (define-key map (kbd "t") #'merita-export-tsv)
    (define-key map (kbd "r") #'merita-export-ris)
    (define-key map (kbd "c") #'merita-export-csv)
    (define-key map (kbd "B") #'merita-bibliography)
    (define-key map (kbd "K") #'merita-biosketch)
    (define-key map (kbd "T") #'merita-list-tags)
    (define-key map (kbd "o") #'merita-import-orcid)
    (define-key map (kbd "s") #'merita-stats)
    (define-key map (kbd "L") #'merita-link-entries)
    (define-key map (kbd "U") #'merita-unlink-entries)
    (define-key map (kbd "R") #'merita-list-related)
    (define-key map (kbd "q") #'merita-close)
    map)
  "Keymap for Merita commands.
Bind this to a prefix key, e.g.:
  (global-set-key (kbd \"C-c v\") merita-command-map)")

;;; ** 1.3. Entry Types

(defconst merita-entry-types
  '(journal-article review-article editorial letter case-report
    book book-chapter conference-paper preprint technical-report
    thesis-doctoral thesis-masters
    podium poster invited-talk keynote workshop
    abstract patent grant award dataset software media other)
  "Valid entry types for scholarly output.")

(defconst merita-entry-type-bibtex-map
  '((journal-article  . "article")
    (review-article   . "article")
    (editorial        . "article")
    (letter           . "article")
    (case-report      . "article")
    (book             . "book")
    (book-chapter     . "incollection")
    (conference-paper . "inproceedings")
    (preprint         . "unpublished")
    (technical-report . "techreport")
    (thesis-doctoral  . "phdthesis")
    (thesis-masters   . "mastersthesis")
    (podium           . "misc")
    (poster           . "misc")
    (invited-talk     . "misc")
    (keynote          . "misc")
    (workshop         . "misc")
    (abstract         . "misc")
    (patent           . "misc")
    (grant            . "misc")
    (award            . "misc")
    (dataset          . "misc")
    (software         . "misc")
    (media            . "misc")
    (other            . "misc"))
  "Mapping from Merita entry types to BibTeX entry types.")

(defconst merita-author-positions
  '(first co-first second third senior co-senior corresponding middle sole)
  "Valid author position values, ordered by significance.")

(defconst merita-status-values
  '(published in-press accepted revision review submitted
    preparation retracted)
  "Valid publication status values.")

(defconst merita--active-status-sql
  "status IN ('published','in-press','accepted',
              'revision','review','submitted','preparation')"
  "SQL WHERE fragment for non-retracted entries.")

(defcustom merita-peer-reviewed-types
  '(journal-article review-article book book-chapter conference-paper)
  "Entry types counted as peer-reviewed for statistics.
Follows NIH biosketch conventions.  The summary dashboard shows
both peer-reviewed and total counts."
  :type '(repeat symbol)
  :group 'merita)

;;; * 2. Database

;;; ** 2.1. Schema

(defconst merita--schema-version 2
  "Current database schema version.")

(defconst merita--create-table-sql
  "CREATE TABLE IF NOT EXISTS data (
  id INTEGER PRIMARY KEY AUTOINCREMENT,

  -- Core bibliographic fields
  type TEXT NOT NULL,
  title TEXT NOT NULL,
  authors TEXT,
  year INTEGER,
  month INTEGER,
  day INTEGER,

  -- Journal fields
  journal TEXT,
  journal_abbrev TEXT,
  volume TEXT,
  issue TEXT,
  pages TEXT,
  eid TEXT,

  -- Book fields
  book_title TEXT,
  publisher TEXT,
  edition TEXT,
  editors TEXT,
  series TEXT,
  isbn TEXT,
  issn TEXT,

  -- Conference/presentation fields (primary venue)
  conference TEXT,
  conference_location TEXT,
  conference_date TEXT,
  organization TEXT,

  -- Thesis fields
  school TEXT,

  -- Identifiers
  doi TEXT UNIQUE,
  pmid TEXT,
  pmcid TEXT,
  arxiv_id TEXT,
  url TEXT,

  -- Merita-specific fields
  author_position TEXT,
  author_count INTEGER,
  impact_factor REAL,
  citations INTEGER DEFAULT 0,
  altmetric INTEGER,
  status TEXT DEFAULT 'published',
  peer_reviewed INTEGER DEFAULT 1,

  -- Content
  abstract TEXT,
  keywords TEXT,
  mesh_terms TEXT,

  -- Grant/funding
  funding TEXT,
  grant_role TEXT,
  grant_amount REAL,
  grant_period TEXT,

  -- Awards
  awards TEXT,
  award_body TEXT,

  -- Notes and metadata
  notes TEXT,
  tags TEXT,
  bibtex_key TEXT,
  bibtex_type TEXT,

  -- Software/dataset fields
  repo_url TEXT,
  repo_language TEXT,
  pkg_registry TEXT,
  pkg_name TEXT,
  pkg_version TEXT,

  -- Local file
  file_path TEXT,

  -- Linkage (denormalized count, updated by merita--refresh-link-count)
  links INTEGER DEFAULT 0,

  -- Timestamps
  date_added TEXT DEFAULT (datetime('now')),
  date_modified TEXT DEFAULT (datetime('now'))
)"
  "SQL to create the main data table.")

(defconst merita--create-related-table-sql
  "CREATE TABLE IF NOT EXISTS related_entries (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  entry_id INTEGER NOT NULL,
  related_id INTEGER NOT NULL,
  relation_type TEXT NOT NULL,
  notes TEXT,
  FOREIGN KEY (entry_id) REFERENCES data(id) ON DELETE CASCADE,
  FOREIGN KEY (related_id) REFERENCES data(id) ON DELETE CASCADE,
  UNIQUE(entry_id, related_id, relation_type)
)"
  "SQL to create the related entries linkage table.
Links are stored symmetrically (both directions).
See `merita-relation-types' for valid link types.")

(defconst merita-relation-types
  '("software-paper" "related" "pipeline" "erratum"
    "commentary" "derived" "supersedes")
  "Valid relation types for linked entries.")

(defconst merita--create-meta-table-sql
  "CREATE TABLE IF NOT EXISTS meta (
  key TEXT PRIMARY KEY,
  value TEXT
)"
  "SQL to create the metadata table.")

(defconst merita--create-indexes-sql
  '("CREATE INDEX IF NOT EXISTS idx_type ON data(type)"
    "CREATE INDEX IF NOT EXISTS idx_year ON data(year)"
    "CREATE INDEX IF NOT EXISTS idx_author_position ON data(author_position)"
    "CREATE INDEX IF NOT EXISTS idx_status ON data(status)"
    "CREATE INDEX IF NOT EXISTS idx_journal ON data(journal)"
    "CREATE INDEX IF NOT EXISTS idx_doi ON data(doi)"
    "CREATE INDEX IF NOT EXISTS idx_bibtex_key ON data(bibtex_key)"
    "CREATE INDEX IF NOT EXISTS idx_related_entry ON related_entries(entry_id)"
    "CREATE INDEX IF NOT EXISTS idx_related_related ON related_entries(related_id)")
  "SQL to create indexes on commonly queried columns.")

;;; ** 2.2. Connection

(defvar merita--db nil
  "Active SQLite database connection.")

(defun merita--db-file ()
  "Return the expanded path to the database file."
  (expand-file-name merita-database-file))

(defun merita--ensure-db ()
  "Ensure the database is open and initialized.  Return the connection."
  (unless (and merita--db (sqlitep merita--db))
    (merita-init))
  merita--db)

;;;###autoload
(defun merita-init ()
  "Initialize the Merita database.
Creates the database file and tables if they don't exist.
If a connection is already open to the same database, reuses it."
  (interactive)
  (let ((db-file (merita--db-file)))
    (if (and merita--db (sqlitep merita--db)
             (equal (expand-file-name db-file)
                    (expand-file-name merita-database-file)))
        (message "Merita database already open: %s" db-file)
      ;; Close stale connection to a different file, if any
      (when (and merita--db (sqlitep merita--db))
        (sqlite-close merita--db)
        (setq merita--db nil))
      (let ((dir (file-name-directory db-file)))
        (unless (file-exists-p dir)
          (make-directory dir t)))
      (setq merita--db (sqlite-open db-file))
      (sqlite-execute merita--db "PRAGMA journal_mode=WAL")
      (sqlite-execute merita--db "PRAGMA foreign_keys=ON")
      (sqlite-execute merita--db merita--create-table-sql)
      (sqlite-execute merita--db merita--create-meta-table-sql)
      (sqlite-execute merita--db merita--create-related-table-sql)
      ;; Migrations (must run before indexes for column renames)
      (condition-case nil
          (sqlite-execute merita--db
            "ALTER TABLE data ADD COLUMN links INTEGER DEFAULT 0")
        (error nil))
      (condition-case nil
          (sqlite-execute merita--db
            "ALTER TABLE data ADD COLUMN day INTEGER")
        (error nil))
      (condition-case nil
          (sqlite-execute merita--db
            "ALTER TABLE data ADD COLUMN issn TEXT")
        (error nil))
      (condition-case nil
          (sqlite-execute merita--db
            "ALTER TABLE data ADD COLUMN file_path TEXT")
        (error nil))
      (condition-case nil
          (sqlite-execute merita--db
            "ALTER TABLE data RENAME COLUMN entry_type TO type")
        (error nil))
      (condition-case nil
          (sqlite-execute merita--db
            "DROP INDEX IF EXISTS idx_entry_type")
        (error nil))
      ;; Indexes (after migrations so column names are current)
      (dolist (sql merita--create-indexes-sql)
        (sqlite-execute merita--db sql))
      (sqlite-execute merita--db
        "INSERT OR REPLACE INTO meta (key, value) VALUES ('schema_version', ?)"
        (list (number-to-string merita--schema-version)))
      (message "Merita database initialized: %s" db-file))))

;;;###autoload
(defun merita-close ()
  "Close the Merita database connection.
Forces a WAL checkpoint before closing to ensure all writes are
flushed to the main database file."
  (interactive)
  (when (and merita--db (sqlitep merita--db))
    (condition-case nil
        (sqlite-execute merita--db "PRAGMA wal_checkpoint(TRUNCATE)")
      (error nil))
    (sqlite-close merita--db)
    (setq merita--db nil)
    (message "Merita database closed.")))

;;; ** 2.3. Core CRUD

(defun merita--maybe-refresh-browse ()
  "Refresh the *merita-browse* buffer if it exists and is alive."
  (let ((buf (get-buffer "*merita-browse*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (merita-browse--refresh)))))

(defun merita--insert (alist)
  "Insert an entry from ALIST.  Return the new row ID."
  (let* ((db (merita--ensure-db))
         (fields (mapcar #'car alist))
         (values (mapcar #'cdr alist))
         (columns (mapconcat (lambda (f) (symbol-name f)) fields ", "))
         (placeholders (mapconcat (lambda (_) "?") fields ", "))
         (sql (format "INSERT INTO data (%s) VALUES (%s)"
                      columns placeholders)))
    (sqlite-execute db sql values)
    (caar (sqlite-select db "SELECT last_insert_rowid()"))))

(defun merita--update (id alist)
  "Update entry ID with fields from ALIST."
  (let* ((db (merita--ensure-db))
         (set-clause (mapconcat
                      (lambda (pair)
                        (format "%s = ?" (symbol-name (car pair))))
                      alist ", "))
         (values (append (mapcar #'cdr alist) (list id)))
         (sql (format "UPDATE data SET %s, date_modified = datetime('now') WHERE id = ?"
                      set-clause)))
    (sqlite-execute db sql values)
    (merita--maybe-refresh-browse)))

(defun merita--delete (id)
  "Delete entry with ID and all associated relations."
  (let ((db (merita--ensure-db)))
    (merita--delete-relations-for-entry id)
    (sqlite-execute db "DELETE FROM data WHERE id = ?" (list id))
    (merita--maybe-refresh-browse)))

(defun merita--get (id)
  "Get entry with ID as an alist, or nil."
  (let* ((db (merita--ensure-db))
         (rows (sqlite-select db "SELECT * FROM data WHERE id = ?"
                              (list id) 'full)))
    (when (and rows (> (length rows) 1))
      (let ((columns (mapcar #'intern (car rows)))
            (values (cadr rows)))
        (cl-mapcar #'cons columns values)))))

(defun merita--get-by-doi (doi)
  "Get entry with DOI as an alist, or nil."
  (let* ((db (merita--ensure-db))
         (rows (sqlite-select db "SELECT * FROM data WHERE doi = ?"
                              (list doi) 'full)))
    (when (and rows (> (length rows) 1))
      (let ((columns (mapcar #'intern (car rows)))
            (values (cadr rows)))
        (cl-mapcar #'cons columns values)))))

(defun merita--query (sql &optional params)
  "Execute SQL with PARAMS, return list of alists."
  (let* ((db (merita--ensure-db))
         (rows (sqlite-select db sql params 'full)))
    (when (and rows (> (length rows) 1))
      (let ((columns (mapcar #'intern (car rows))))
        (mapcar (lambda (row) (cl-mapcar #'cons columns row))
                (cdr rows))))))

(defun merita--count (&optional where params)
  "Count entries, optionally filtered by WHERE clause and PARAMS."
  (let* ((db (merita--ensure-db))
         (sql (if where
                  (format "SELECT COUNT(*) FROM data WHERE %s" where)
                "SELECT COUNT(*) FROM data")))
    (caar (sqlite-select db sql params))))

;;; ** 2.4. Related Entries (Cross-Links)

(defun merita--refresh-link-count (&rest entry-ids)
  "Update the `links' column for each id in ENTRY-IDS.
If called with no arguments, refreshes all entries."
  (let ((db (merita--ensure-db)))
    (if entry-ids
        (dolist (eid entry-ids)
          (let ((n (or (caar (sqlite-select db
                               "SELECT COUNT(*) FROM related_entries
                                WHERE entry_id = ?"
                               (list eid))) 0)))
            (sqlite-execute db "UPDATE data SET links = ? WHERE id = ?"
                            (list n eid))))
      ;; Bulk refresh all
      (sqlite-execute db
        "UPDATE data SET links = (
           SELECT COUNT(*) FROM related_entries
           WHERE related_entries.entry_id = data.id)"))))

(defun merita--insert-relation (entry-id related-id relation-type &optional notes)
  "Link ENTRY-ID and RELATED-ID with RELATION-TYPE.
Inserts both directions so queries from either side find the link.
NOTES is an optional annotation.  Signals an error if the link
already exists or if the entry would link to itself."
  (when (equal entry-id related-id)
    (user-error "Cannot link an entry to itself"))
  (let ((db (merita--ensure-db)))
    ;; Check for existing link (any type) between the two entries
    (let ((existing (caar (sqlite-select db
                            "SELECT relation_type FROM related_entries
                             WHERE entry_id = ? AND related_id = ?"
                            (list entry-id related-id)))))
      (when existing
        (user-error "Link already exists between these entries (type: %s)"
                    existing)))
    (sqlite-execute db
      "INSERT INTO related_entries (entry_id, related_id, relation_type, notes)
       VALUES (?, ?, ?, ?)"
      (list entry-id related-id relation-type notes))
    (sqlite-execute db
      "INSERT INTO related_entries (entry_id, related_id, relation_type, notes)
       VALUES (?, ?, ?, ?)"
      (list related-id entry-id relation-type notes))
    (merita--refresh-link-count entry-id related-id)
    (caar (sqlite-select db "SELECT last_insert_rowid()"))))

(defun merita--get-related (entry-id &optional relation-type)
  "Get all entries related to ENTRY-ID.
If RELATION-TYPE is non-nil, filter by that type.
Returns list of alists."
  (let* ((db (merita--ensure-db))
         (sql (concat
               "SELECT r.id, r.related_id, r.relation_type, r.notes,
                       d.title, d.type, d.year, d.month,
                       d.authors, d.pkg_name, d.pkg_registry
                FROM related_entries r
                JOIN data d ON d.id = r.related_id
                WHERE r.entry_id = ?"
               (when relation-type " AND r.relation_type = ?")
               " ORDER BY d.year ASC"))
         (params (if relation-type
                     (list entry-id relation-type)
                   (list entry-id)))
         (rows (sqlite-select db sql params 'full)))
    (when (and rows (> (length rows) 1))
      (let ((columns (mapcar #'intern (car rows))))
        (mapcar (lambda (row) (cl-mapcar #'cons columns row))
                (cdr rows))))))

(defun merita--delete-relation (relation-id)
  "Delete the relation with RELATION-ID and its reverse."
  (let* ((db (merita--ensure-db))
         (row (car (sqlite-select db
                     "SELECT entry_id, related_id, relation_type
                      FROM related_entries WHERE id = ?"
                     (list relation-id)))))
    (when row
      (let ((eid (nth 0 row))
            (rid (nth 1 row))
            (rtype (nth 2 row)))
        (sqlite-execute db
          "DELETE FROM related_entries
           WHERE (entry_id = ? AND related_id = ? AND relation_type = ?)
              OR (entry_id = ? AND related_id = ? AND relation_type = ?)"
          (list eid rid rtype rid eid rtype))
        (merita--refresh-link-count eid rid)))))

(defun merita--delete-relations-for-entry (entry-id)
  "Delete all relations involving ENTRY-ID (both directions).
Refreshes link counts for all formerly-linked entries."
  (let* ((db (merita--ensure-db))
         (peers (mapcar #'car
                        (sqlite-select db
                          "SELECT DISTINCT related_id FROM related_entries
                           WHERE entry_id = ?"
                          (list entry-id)))))
    (sqlite-execute db
      "DELETE FROM related_entries
       WHERE entry_id = ? OR related_id = ?"
      (list entry-id entry-id))
    (dolist (peer peers)
      (merita--refresh-link-count peer))))

;;;###autoload
(defun merita-link-entries (entry-id related-id relation-type &optional notes)
  "Link two entries as related scholarly works.
ENTRY-ID and RELATED-ID are database IDs.  RELATION-TYPE is one of
`merita-relation-types'."
  (interactive
   (let* ((eid (merita--read-entry-id "First entry: "))
          (rid (merita--read-entry-id "Related entry: "))
          (rtype (completing-read "Link type: "
                                  merita-relation-types nil t))
          (notes (merita--read-field "Notes (optional)")))
     (list eid rid rtype (unless (string-empty-p (or notes "")) notes))))
  (let ((e1 (merita--get entry-id))
        (e2 (merita--get related-id)))
    (unless e1 (user-error "First entry not found"))
    (unless e2 (user-error "Second entry not found"))
    (merita--insert-relation entry-id related-id relation-type notes)
    (message "Linked: %s <-> %s [%s]"
             (merita--truncate (or (alist-get 'title e1) "") 30)
             (merita--truncate (or (alist-get 'title e2) "") 30)
             relation-type)))

;;;###autoload
(defun merita-unlink-entries (entry-id related-id)
  "Remove the link between ENTRY-ID and RELATED-ID."
  (interactive
   (list (merita--read-entry-id "First entry: ")
         (merita--read-entry-id "Second entry: ")))
  (let ((db (merita--ensure-db)))
    (sqlite-execute db
      "DELETE FROM related_entries
       WHERE (entry_id = ? AND related_id = ?)
          OR (entry_id = ? AND related_id = ?)"
      (list entry-id related-id related-id entry-id))
    (merita--refresh-link-count entry-id related-id)
    (message "Unlinked.")))

;;;###autoload
(defun merita-list-related (entry-id)
  "Display all entries related to ENTRY-ID."
  (interactive
   (list (merita--read-entry-id "Show relations for: ")))
  (let* ((entry (merita--get entry-id))
         (related (merita--get-related entry-id)))
    (if (null related)
        (message "No related entries.")
      (let ((buf (get-buffer-create "*merita-related*")))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "=== Related entries: %s ===\n\n"
                            (merita--truncate (or (alist-get 'title entry) "") 55)))
            (dolist (rel related)
              (insert (format "  [%-15s] %s\n"
                              (or (alist-get 'relation_type rel) "")
                              (merita--entry-short-title rel))))
            (goto-char (point-min))
            (special-mode)))
        (pop-to-buffer buf)))))

;;; * 3. Data Entry

;;; ** 3.1. Input Helpers

(defun merita--read-field (prompt &optional default history require)
  "Read a string with PROMPT.
DEFAULT, HISTORY, and REQUIRE behave as in `read-string'."
  (let* ((p (if default (format "%s [%s]: " prompt default)
              (format "%s: " prompt)))
         (val (read-string p nil history default)))
    (if (and require (string-empty-p val))
        (user-error "%s is required" prompt)
      (if (string-empty-p val) nil val))))

(defun merita--read-number-field (prompt)
  "Read a numeric field with PROMPT.  Return number or nil."
  (let ((val (read-string (format "%s: " prompt))))
    (if (string-empty-p val) nil (string-to-number val))))

(defun merita--read-entry-type ()
  "Read an entry type with completion."
  (intern (completing-read "Entry type: "
                           (mapcar #'symbol-name merita-entry-types)
                           nil t)))

(defun merita--read-author-position ()
  "Read an author position with completion."
  (intern (completing-read "Author position: "
                           (mapcar #'symbol-name merita-author-positions)
                           nil t)))

(defun merita--read-status ()
  "Read a status value with completion."
  (intern (completing-read "Status: "
                           (mapcar #'symbol-name merita-status-values)
                           nil t nil nil "published")))

(defun merita--read-journal-with-completion (prompt)
  "Read journal name with PROMPT, completing from existing journals."
  (let* ((db (merita--ensure-db))
         (journals (mapcar #'car
                           (sqlite-select db
                             "SELECT DISTINCT journal FROM data
                              WHERE journal IS NOT NULL ORDER BY journal"))))
    (completing-read (format "%s: " prompt) journals nil nil)))

;;; ** 3.2. Interactive Entry

;;;###autoload
(defun merita-new-entry ()
  "Add a new entry interactively."
  (interactive)
  (merita--ensure-db)
  (let* ((entry-type (merita--read-entry-type))
         (title (merita--read-field "Title" nil nil t))
         (authors (merita--read-field "Authors (comma-separated)"))
         (year (merita--read-number-field "Year"))
         (position (merita--read-author-position))
         (status (merita--read-status))
         (alist (list (cons 'type (symbol-name entry-type))
                      (cons 'title title))))
    (when authors (push (cons 'authors authors) alist))
    (when year (push (cons 'year year) alist))
    (push (cons 'author_position (symbol-name position)) alist)
    (push (cons 'status (symbol-name status)) alist)
    ;; Type-specific fields
    (pcase entry-type
      ((or 'journal-article 'review-article 'editorial 'letter 'case-report)
       (let ((journal (merita--read-journal-with-completion "Journal"))
             (volume (merita--read-field "Volume"))
             (issue (merita--read-field "Issue"))
             (pages (merita--read-field "Pages"))
             (doi (merita--read-field "DOI"))
             (pmid (merita--read-field "PMID")))
         (when journal (push (cons 'journal journal) alist))
         (when volume (push (cons 'volume volume) alist))
         (when issue (push (cons 'issue issue) alist))
         (when pages (push (cons 'pages pages) alist))
         (when doi (push (cons 'doi doi) alist))
         (when pmid (push (cons 'pmid pmid) alist))))
      ('book-chapter
       (let ((book-title (merita--read-field "Book title" nil nil t))
             (editors (merita--read-field "Editor(s)"))
             (publisher (merita--read-field "Publisher"))
             (pages (merita--read-field "Pages"))
             (isbn (merita--read-field "ISBN"))
             (doi (merita--read-field "DOI")))
         (push (cons 'book_title book-title) alist)
         (when editors (push (cons 'editors editors) alist))
         (when publisher (push (cons 'publisher publisher) alist))
         (when pages (push (cons 'pages pages) alist))
         (when isbn (push (cons 'isbn isbn) alist))
         (when doi (push (cons 'doi doi) alist))))
      ('book
       (let ((publisher (merita--read-field "Publisher"))
             (edition (merita--read-field "Edition"))
             (isbn (merita--read-field "ISBN"))
             (doi (merita--read-field "DOI")))
         (when publisher (push (cons 'publisher publisher) alist))
         (when edition (push (cons 'edition edition) alist))
         (when isbn (push (cons 'isbn isbn) alist))
         (when doi (push (cons 'doi doi) alist))))
      ((or 'podium 'poster 'invited-talk 'keynote 'workshop)
       (let ((conference (merita--read-field "Conference"))
             (location (merita--read-field "Location"))
             (conf-date (merita--read-field "Presentation date"))
             (org (merita--read-field "Sponsoring organization")))
         (when conference (push (cons 'conference conference) alist))
         (when location (push (cons 'conference_location location) alist))
         (when conf-date (push (cons 'conference_date conf-date) alist))
         (when org (push (cons 'organization org) alist))))
      ((or 'thesis-doctoral 'thesis-masters)
       (let ((school (merita--read-field "School"))
             (doi (merita--read-field "DOI")))
         (when school (push (cons 'school school) alist))
         (when doi (push (cons 'doi doi) alist))))
      ('grant
       (let ((funding (merita--read-field "Grant number(s)"))
             (role (merita--read-field "Role (PI, Co-PI, etc.)"))
             (amount (merita--read-number-field "Grant amount"))
             (period (merita--read-field "Grant period"))
             (org (merita--read-field "Funding organization")))
         (when funding (push (cons 'funding funding) alist))
         (when role (push (cons 'grant_role role) alist))
         (when amount (push (cons 'grant_amount amount) alist))
         (when period (push (cons 'grant_period period) alist))
         (when org (push (cons 'organization org) alist))))
      ('award
       (let ((body (merita--read-field "Awarding body"))
             (org (merita--read-field "Organization"))
             (awards-text (merita--read-field "Award description(s) (;-delimited)")))
         (when body (push (cons 'award_body body) alist))
         (when org (push (cons 'organization org) alist))
         (when awards-text (push (cons 'awards awards-text) alist))))
      ((or 'software 'dataset)
       (let ((repo-url (merita--read-field "Repository URL (e.g., GitHub)"))
             (entry-url (merita--read-field "Project URL"))
             (language (merita--read-field "Programming language"))
             (registry (merita--read-field "Package registry (cran, pypi, melpa, npm)"))
             (pkg-name (merita--read-field "Package name in registry"))
             (pkg-version (merita--read-field "Current version"))
             (doi (merita--read-field "DOI (if published)")))
         (when repo-url (push (cons 'repo_url repo-url) alist))
         (when entry-url (push (cons 'url entry-url) alist))
         (when language (push (cons 'repo_language language) alist))
         (when registry (push (cons 'pkg_registry registry) alist))
         (when pkg-name (push (cons 'pkg_name pkg-name) alist))
         (when pkg-version (push (cons 'pkg_version pkg-version) alist))
         (when doi (push (cons 'doi doi) alist)))))
    ;; Optional fields for all types
    (when (y-or-n-p "Add optional fields? ")
      (let ((month (merita--read-number-field "Month (1-12)"))
            (day (merita--read-number-field "Day (1-31)"))
            (impact-factor (merita--read-number-field "Impact factor"))
            (keywords (merita--read-field "Keywords (;-delimited)"))
            (awards-text (merita--read-field "Awards (;-delimited, if any)"))
            (notes (merita--read-field "Notes"))
            (bibtex-key (merita--read-field "BibTeX key")))
        (when month (push (cons 'month month) alist))
        (when day (push (cons 'day day) alist))
        (when impact-factor (push (cons 'impact_factor impact-factor) alist))
        (when keywords (push (cons 'keywords keywords) alist))
        (when awards-text (push (cons 'awards awards-text) alist))
        (when notes (push (cons 'notes notes) alist))
        (when bibtex-key (push (cons 'bibtex_key bibtex-key) alist))))
    ;; Insert
    (let ((id (merita--insert alist)))
      (merita--maybe-refresh-browse)
      (merita--display-entry (merita--get id))
      (message "Added: %s" title)
      id)))

;;; ** 3.3. Edit and Delete

(defconst merita--editable-fields
  '(type title authors year month day journal journal_abbrev volume issue
    pages doi pmid pmcid url author_position author_count status
    peer_reviewed keywords notes tags awards award_body
    conference conference_location conference_date organization
    school publisher edition editors book_title isbn issn
    funding grant_role grant_amount grant_period
    impact_factor abstract bibtex_key
    repo_url repo_language pkg_registry pkg_name pkg_version
    file_path)
  "Fields available for interactive editing.")

(defconst merita--choice-fields
  `((type             . ,(mapcar #'symbol-name merita-entry-types))
    (author_position . ,(mapcar #'symbol-name merita-author-positions))
    (status          . ,(mapcar #'symbol-name merita-status-values))
    (peer_reviewed   . ("1" "0"))
    (grant_role      . ("PI" "Co-PI" "Co-I" "Mentor" "Trainee" "Other"))
    (pkg_registry    . ("" "CRAN" "PyPI" "MELPA" "npm" "Bioconductor")))
  "Alist mapping field names to valid choices for completion.")

(defun merita--read-field-value (field current-str)
  "Prompt for a value for FIELD, pre-populated with CURRENT-STR.
Uses `completing-read' for choice fields, `read-string' otherwise.
The minibuffer is seeded with CURRENT-STR so minor edits can be
made in place."
  (let ((choices (alist-get field merita--choice-fields)))
    (if choices
        (completing-read (format "%s: " field)
                         choices nil t nil nil current-str)
      (read-string (format "%s: " field)
                   current-str))))

;;;###autoload
(defun merita-edit-field (id &optional field)
  "Edit a single FIELD of entry ID.
When called interactively, prompts for the entry and field."
  (interactive
   (let* ((id (merita--read-entry-id "Edit entry: "))
          (field (intern (completing-read "Field: "
                           (mapcar #'symbol-name merita--editable-fields)
                           nil t))))
     (list id field)))
  (let* ((entry (merita--get id))
         (current (alist-get field entry))
         (current-str (if current (format "%s" current) ""))
         (new-val (merita--read-field-value field current-str)))
    (unless (equal new-val current-str)
      (merita--update id (list (cons field
                                     (if (string-empty-p new-val) nil new-val))))
      (message "Updated %s." field))))

;;;###autoload
(defun merita-edit (id)
  "Edit entry ID interactively.
Prompts for which fields to edit, then steps through each one."
  (interactive (list (merita--read-entry-id "Edit entry: ")))
  (let* ((entry (merita--get id))
         (changes '()))
    (unless entry (user-error "Entry not found"))
    ;; Show current entry in message area
    (message "Editing: %s (%s)"
             (merita--truncate (or (alist-get 'title entry) "") 50)
             (or (alist-get 'year entry) ""))
    ;; Select fields to edit
    (let* ((field-names (mapcar #'symbol-name merita--editable-fields))
           (selected (completing-read-multiple
                      "Fields to edit (comma-separated): "
                      field-names nil t)))
      (dolist (field-str selected)
        (let* ((field (intern field-str))
               (current (alist-get field entry))
               (current-str (if current (format "%s" current) ""))
               (new-val (merita--read-field-value field current-str)))
          (unless (equal new-val current-str)
            (push (cons field (if (string-empty-p new-val) nil new-val))
                  changes)))))
    (if changes
        (progn
          (merita--update id changes)
          (message "Updated (%d fields changed)." (length changes)))
      (message "No changes."))))

;;;###autoload
(defun merita-delete (id)
  "Delete entry ID after confirmation."
  (interactive (list (merita--read-entry-id "Delete entry: ")))
  (let ((entry (merita--get id)))
    (unless entry (user-error "Entry not found"))
    (when (yes-or-no-p
           (format "Delete: %s? "
                   (merita--truncate (alist-get 'title entry) 50)))
      (merita--delete id)
      (message "Deleted."))))

;;; * 4. Search & Display

;;; ** 4.1. Completion Interface

(defun merita--entry-candidates ()
  "Generate completing-read candidates as (DISPLAY . ID) alist.
Title and authors are given generous width so that minibuffer
narrowing can match on any substring."
  (let ((entries (merita--query
                  "SELECT id, type, title, authors, year, journal
                   FROM data ORDER BY year DESC, title ASC")))
    (mapcar (lambda (entry)
              (let* ((id (alist-get 'id entry))
                     (type (alist-get 'type entry))
                     (title (or (alist-get 'title entry) ""))
                     (authors (or (alist-get 'authors entry) ""))
                     (year (or (alist-get 'year entry) ""))
                     (journal (or (alist-get 'journal entry) "")))
                (cons (format "[%s] %s (%s) %s — %s"
                              type authors year title journal)
                      id)))
            entries)))

(defun merita--read-entry-id (prompt)
  "Read an entry ID using completing-read with PROMPT."
  (let* ((candidates (merita--entry-candidates))
         (selected (completing-read prompt candidates nil t)))
    (cdr (assoc selected candidates))))

;;; ** 4.2. Search

;;;###autoload
(defun merita-find ()
  "Find and display an entry using completing-read."
  (interactive)
  (let ((id (merita--read-entry-id "Find entry: ")))
    (merita--display-entry (merita--get id))))

;;;###autoload
(defun merita-search (query)
  "Search entries matching QUERY by opening browse with a filter."
  (interactive "sSearch: ")
  (merita-browse query))

;;; ** 4.3. Display Helpers

(defun merita--truncate (str max-len)
  "Truncate STR to MAX-LEN with ellipsis."
  (if (> (length str) max-len)
      (concat (substring str 0 (1- max-len)) "…")
    str))

(defun merita--entry-short-title (entry)
  "Return a short citation label for ENTRY, e.g. \"McClelland et al. (2024-09)\"."
  (let* ((authors (or (alist-get 'authors entry) ""))
         (year-raw (alist-get 'year entry))
         (month-raw (alist-get 'month entry))
         (year (and year-raw (not (equal year-raw "")) (if (stringp year-raw) (string-to-number year-raw) year-raw)))
         (month (and month-raw (not (equal month-raw "")) (if (stringp month-raw) (string-to-number month-raw) month-raw)))
         (first-author (if (string-match "\\`\\([^,]+\\)" authors)
                           (match-string 1 authors)
                         "Unknown"))
         (author-count (if (string-empty-p authors) 0
                         (1+ (cl-count ?, authors))))
         (author-str (if (> author-count 1)
                         (format "%s et al." first-author)
                       first-author))
         (date-str (cond
                    ((and year month) (format "%d-%02d" year month))
                    (year (format "%d" year))
                    (t "n.d."))))
    (format "%s (%s)" author-str date-str)))

(defun merita--open-url (id)
  "Open the DOI or PMID for entry ID in a browser."
  (let ((entry (merita--get id)))
    (if entry
        (let ((doi (alist-get 'doi entry))
              (pmid (alist-get 'pmid entry))
              (url (alist-get 'url entry)))
          (cond
           ((and doi (stringp doi) (not (string-empty-p doi)))
            (let ((target (format "https://doi.org/%s" doi)))
              (message "Opening %s" target)
              (browse-url target)))
           ((and pmid (not (eq pmid :null))
                 (not (equal pmid ""))
                 (not (equal pmid 0)))
            (let ((target (format "https://pubmed.ncbi.nlm.nih.gov/%s" pmid)))
              (message "Opening %s" target)
              (browse-url target)))
           ((and url (stringp url) (not (string-empty-p url)))
            (message "Opening %s" url)
            (browse-url url))
           (t (message "No DOI, PMID, or URL for this entry."))))
      (message "Entry not found."))))

(defun merita--open-file (id)
  "Open the local file for entry ID.
Falls back to `merita--open-url' when no file_path is set."
  (let ((entry (merita--get id)))
    (if entry
        (let ((path (alist-get 'file_path entry)))
          (if (and path (stringp path) (not (string-empty-p path)))
              (if (file-exists-p (expand-file-name path))
                  (progn
                    (message "Opening %s" path)
                    (find-file (expand-file-name path)))
                (message "File not found: %s" path))
            (message "No file_path for this entry.")))
      (message "Entry not found."))))

;;;###autoload
(defun merita-open-file (id)
  "Open the local file associated with entry ID.
With no file_path set, report that none exists."
  (interactive (list (merita--read-entry-id "Open file for: ")))
  (merita--open-file id))

;;; ** 4.4. Entry View Mode

(defvar-local merita--entry-id nil
  "ID of the entry displayed in the current buffer.")

(defvar merita-entry-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'merita-entry-edit-at-point)
    (define-key map (kbd "M-RET") #'merita-entry-jump-to-link)
    (define-key map (kbd "E") #'merita-entry-edit)
    (define-key map (kbd "e") #'merita-entry-export)
    (define-key map (kbd "x") #'merita-entry-clear-at-point)
    (define-key map (kbd "D") #'merita-entry-delete)
    (define-key map (kbd "n") #'merita-entry-next-field)
    (define-key map (kbd "p") #'merita-entry-prev-field)
    (define-key map (kbd "TAB") #'merita-entry-next-field)
    (define-key map (kbd "<backtab>") #'merita-entry-prev-field)
    (define-key map (kbd "<down>") #'merita-entry-next-field)
    (define-key map (kbd "<up>") #'merita-entry-prev-field)
    (define-key map (kbd "M-n") #'merita-entry-next)
    (define-key map (kbd "M-p") #'merita-entry-prev)
    (define-key map (kbd "N") #'merita-new-entry)
    (define-key map (kbd "b") #'merita-entry-open-url)
    (define-key map (kbd "O") #'merita-entry-open-file)
    (define-key map (kbd "m") #'merita-entry-metrics)
    (define-key map (kbd "L") #'merita-entry-link)
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'merita-entry-refresh)
    (define-key map (kbd "=") #'merita-entry-refresh)
    map)
  "Keymap for `merita-entry-mode'.")

(define-derived-mode merita-entry-mode special-mode "Merita Entry"
  "Mode for viewing a single Merita entry.
\\{merita-entry-mode-map}")

(defconst merita--entry-field-order
  '(type title authors year month day
    journal journal_abbrev volume issue pages eid
    book_title publisher edition editors series isbn issn
    conference conference_location conference_date organization
    school doi pmid pmcid arxiv_id url
    author_position author_count impact_factor citations altmetric
    status peer_reviewed abstract keywords mesh_terms
    funding grant_role grant_amount grant_period
    awards award_body notes tags
    bibtex_key bibtex_type
    repo_url repo_language pkg_registry pkg_name pkg_version
    file_path links date_added date_modified)
  "Display order for fields in the entry view buffer.")

(defun merita--display-entry (entry)
  "Display ENTRY in the entry view buffer."
  (let ((buf (get-buffer-create "*merita-entry*"))
        (id (alist-get 'id entry))
        (w 80))  ; display width
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Header: short citation label
        (insert (propertize (merita--entry-short-title entry)
                            'face '(:weight bold :height 1.2)))
        (insert "\n" (make-string w ?─) "\n\n")
        ;; Formatted citation preview — word-wrapped to width
        ;; Strip DOI from the preview since it appears in the fields below
        (let* ((merita--latex-context nil)
               (preview-entry (cons (cons 'doi nil)
                                    (assq-delete-all 'doi (copy-alist entry))))
               (fmt (funcall (merita--resolve-citation-style
                              merita-default-citation-style)
                             preview-entry)))
          (when (and fmt (not (string-empty-p fmt)))
            (let ((start (point))
                  (fill-prefix "  ")
                  (fill-column (- w 2)))
              (insert "  " fmt "\n")
              (fill-region start (point))
              (put-text-property start (point) 'face 'font-lock-doc-face))
            (insert "\n")))
        ;; Field/value pairs in database column order
        (let ((val-width (- w 25))  ; 2 indent + 20 field + 1 space + val + 2 trailing
              (first-field-pos nil))
          (dolist (key merita--entry-field-order)
            (let* ((val (alist-get key entry))
                   (filled (and val (not (equal val "")) (not (eq val 0))
                                (not (eq val :null)))))
              (when (or filled merita-entry-show-empty-fields)
                (let ((line-start (point)))
                  (unless first-field-pos
                    (setq first-field-pos line-start))
                  (insert (format "  %-20s %s\n"
                                  (propertize (symbol-name key)
                                              'face 'font-lock-keyword-face)
                                  (if filled
                                      (merita--truncate (format "%s" val)
                                                        val-width)
                                    "")))
                  (put-text-property line-start (point)
                                     'merita-field key)))))
          ;; Show linked entries
          (let ((related (merita--get-related id)))
            (when related
              (insert "\n  " (propertize "Linked entries:"
                                         'face 'font-lock-type-face) "\n")
              (dolist (rel related)
                (let* ((rtype (or (alist-get 'relation_type rel) ""))
                       (etype (or (alist-get 'type rel) ""))
                       (short (merita--entry-short-title rel))
                       (rtitle (or (alist-get 'title rel) ""))
                       ;; Fixed overhead: "  {" "} " "[" "] " ". " "\"" "\""
                       (fixed 12)
                       (avail (- w 2 fixed
                                 (length rtype) (length etype) (length short)))
                       (title-str (merita--truncate rtitle (max 10 avail)))
                       (line-start (point)))
                  (insert (format "  {%s} [%s] %s. \"%s\"\n"
                                  rtype etype short title-str))
                  (put-text-property line-start (point)
                                     'merita-link-id (alist-get 'id rel))
                  (put-text-property line-start (point)
                                     'merita-linked-entry-id
                                     (alist-get 'related_id rel))))))
          (insert "\n" (make-string w ?─) "\n")
          (insert "  "
                  (propertize "TAB" 'face 'help-key-binding) "/"
                  (propertize "S-TAB" 'face 'help-key-binding) " Navigate  "
                  (propertize "n" 'face 'help-key-binding) "/"
                  (propertize "p" 'face 'help-key-binding) " Line ↓/↑  "
                  (propertize "RET" 'face 'help-key-binding) " Edit  "
                  (propertize "x" 'face 'help-key-binding) " Clear/Unlink  "
                  (propertize "M-RET" 'face 'help-key-binding) " Jump\n")
          (insert "  "
                  (propertize "M-n" 'face 'help-key-binding) "/"
                  (propertize "M-p" 'face 'help-key-binding) " Entry ↓/↑  "
                  (propertize "N" 'face 'help-key-binding) " New  "
                  (propertize "L" 'face 'help-key-binding) " Link  "
                  (propertize "E" 'face 'help-key-binding) " Edit (multi)  "
                  (propertize "D" 'face 'help-key-binding) " Delete\n")
          (insert "  "
                  (propertize "e" 'face 'help-key-binding) " Export  "
                  (propertize "b" 'face 'help-key-binding) " URL  "
                  (propertize "O" 'face 'help-key-binding) " File  "
                  (propertize "m" 'face 'help-key-binding) " Metrics  "
                  (propertize "g" 'face 'help-key-binding) " Refresh  "
                  (propertize "q" 'face 'help-key-binding) " Quit\n")
          ;; Position cursor on first field line
          (goto-char (or first-field-pos (point-min))))
        (merita-entry-mode)
        (setq merita--entry-id id)))
    (merita--display-entry-buffer buf)))

(defun merita--display-entry-buffer (buf)
  "Display entry buffer BUF according to `merita-entry-display'."
  (pcase merita-entry-display
    ('side
     (display-buffer buf
                     '(display-buffer-in-direction
                       (direction . right)
                       (window-width . 84))))
    (_ (pop-to-buffer buf))))

(defun merita-entry-refresh ()
  "Refresh the current entry view."
  (interactive)
  (when merita--entry-id
    (let ((entry (merita--get merita--entry-id)))
      (if entry
          (merita--display-entry entry)
        (message "Entry no longer exists.")))))

(defun merita-entry-edit ()
  "Edit the entry displayed in this buffer.
Prompts for which fields to edit, then steps through each one."
  (interactive)
  (when merita--entry-id
    (merita-edit merita--entry-id)
    (merita-entry-refresh)))

(defun merita--entry-field-at-point ()
  "Return the field symbol at point, or nil.
Reads the `merita-field' text property set by `merita--display-entry'."
  (get-text-property (point) 'merita-field))

(defun merita--entry-link-at-point ()
  "Return the relation ID at point, or nil.
Reads the `merita-link-id' text property set by `merita--display-entry'."
  (get-text-property (point) 'merita-link-id))

(defun merita--entry-linked-entry-at-point ()
  "Return the linked entry ID at point, or nil."
  (get-text-property (point) 'merita-linked-entry-id))

(defun merita--entry-navigable-at (pos)
  "Return non-nil if POS has a field or link text property."
  (or (get-text-property pos 'merita-field)
      (get-text-property pos 'merita-link-id)))

(defun merita--entry-goto-field (field)
  "Move point to the line for FIELD in the entry view buffer.
Searches for the `merita-field' text property.  Returns non-nil
if the field was found."
  (let ((pos (point-min))
        found)
    (while (and (not found) (< pos (point-max)))
      (if (eq (get-text-property pos 'merita-field) field)
          (setq found pos)
        (setq pos (next-single-property-change pos 'merita-field nil (point-max)))))
    (when found
      (goto-char found)
      (beginning-of-line)
      t)))

(defun merita--entry-next-navigable-pos (&optional backward)
  "Return the position of the next navigable line, or nil.
Navigable lines are data fields and linked-entry lines.
When BACKWARD is non-nil, search backward instead."
  (save-excursion
    (let* ((step (if backward -1 1))
           (pos (if backward
                    (line-beginning-position)
                  (line-end-position))))
      ;; Scan line by line
      (goto-char pos)
      (let (found)
        (while (and (not found)
                    (if backward (> (point) (point-min)) (< (point) (point-max))))
          (forward-line step)
          (when (merita--entry-navigable-at (point))
            (setq found (point))))
        found))))

(defun merita--entry-first-navigable-pos ()
  "Return the position of the first navigable line in the buffer."
  (save-excursion
    (goto-char (point-min))
    (if (merita--entry-navigable-at (point))
        (point)
      (let (found)
        (while (and (not found) (< (point) (point-max)))
          (forward-line 1)
          (when (merita--entry-navigable-at (point))
            (setq found (point))))
        found))))

(defun merita--entry-last-navigable-pos ()
  "Return the position of the last navigable line in the buffer."
  (save-excursion
    (goto-char (point-max))
    (let (found)
      (while (and (not found) (> (point) (point-min)))
        (forward-line -1)
        (when (merita--entry-navigable-at (point))
          (setq found (point))))
      found)))

(defun merita-entry-next-field ()
  "Move to the next field or linked-entry line.
Wraps to the first field when past the last."
  (interactive)
  (let ((pos (or (merita--entry-next-navigable-pos)
                 (merita--entry-first-navigable-pos))))
    (when pos
      (goto-char pos)
      (beginning-of-line))))

(defun merita-entry-prev-field ()
  "Move to the previous field or linked-entry line.
Wraps to the last field when before the first."
  (interactive)
  (let ((pos (or (merita--entry-next-navigable-pos t)
                 (merita--entry-last-navigable-pos))))
    (when pos
      (goto-char pos)
      (beginning-of-line))))

(defun merita-entry-edit-at-point ()
  "Edit the field or link at point in the entry view.
On a data field line, prompt for a new value.  On a linked-entry
line, prompt to change the link type.  Otherwise, fall back to
`merita-entry-edit' (multi-field selection)."
  (interactive)
  (unless merita--entry-id
    (user-error "No entry in this buffer"))
  (let ((field (merita--entry-field-at-point))
        (link-id (merita--entry-link-at-point)))
    (cond
     ;; On a link line — edit the link type
     (link-id
      (merita-edit-link-type link-id)
      (merita-entry-refresh))
     ;; On an editable field line
     ((and field (memq field merita--editable-fields))
      (merita-edit-field merita--entry-id field)
      (merita-entry-refresh)
      (merita--entry-goto-field field))
     ;; Fallback
     (t (merita-entry-edit)))))

(defun merita-entry-clear-at-point ()
  "Clear the field or remove the link at point.
On a data field line, set the field to nil.  On a linked-entry
line, remove the link after confirmation."
  (interactive)
  (unless merita--entry-id
    (user-error "No entry in this buffer"))
  (let ((field (merita--entry-field-at-point))
        (link-id (merita--entry-link-at-point)))
    (cond
     ;; On a link line — remove it
     (link-id
      (when (yes-or-no-p "Remove this link? ")
        (merita--delete-relation link-id)
        (merita-entry-refresh)
        (message "Link removed.")))
     ;; On a data field line
     ((not field)
      (message "Not on a field or link line."))
     ((not (memq field merita--editable-fields))
      (message "Field `%s' is not editable." field))
     (t
      (when (yes-or-no-p (format "Clear %s? " field))
        (merita--update merita--entry-id (list (cons field nil)))
        (merita-entry-refresh)
        (merita--entry-goto-field field)
        (message "Cleared %s." field))))))

(defun merita-entry-jump-to-link ()
  "Jump to the linked entry at point.
When point is on a linked-entry line, display that entry."
  (interactive)
  (let ((linked-id (merita--entry-linked-entry-at-point)))
    (if linked-id
        (let ((entry (merita--get linked-id)))
          (if entry
              (merita--display-entry entry)
            (message "Linked entry not found.")))
      (message "Not on a linked-entry line."))))

(defun merita-entry-delete ()
  "Delete the entry displayed in this buffer."
  (interactive)
  (when merita--entry-id
    (merita-delete merita--entry-id)))

(defun merita-entry-open-url ()
  "Open the DOI, PMID, or URL of the displayed entry in a browser."
  (interactive)
  (when merita--entry-id
    (merita--open-url merita--entry-id)))

(defun merita-entry-open-file ()
  "Open the local file for the displayed entry."
  (interactive)
  (when merita--entry-id
    (merita--open-file merita--entry-id)))

(defun merita-entry-metrics ()
  "Show metrics for the displayed entry."
  (interactive)
  (when merita--entry-id
    (if (fboundp 'merita-metrics-format-entry)
        (message "%s" (merita-metrics-format-entry merita--entry-id))
      (message "Metrics module not loaded."))))

;;; ** 4.5. Link Management (Entry View)

(defun merita-edit-link-type (relation-id)
  "Change the relation type for link RELATION-ID.
Prompts for a new type from `merita-relation-types'."
  (interactive
   (list (or (merita--entry-link-at-point)
             (read-number "Relation ID: "))))
  (let* ((db (merita--ensure-db))
         (row (car (sqlite-select db
                     "SELECT entry_id, related_id, relation_type
                      FROM related_entries WHERE id = ?"
                     (list relation-id)))))
    (unless row (user-error "Relation not found"))
    (let* ((eid (nth 0 row))
           (rid (nth 1 row))
           (old-type (nth 2 row))
           (new-type (completing-read
                      (format "Link type [%s]: " old-type)
                      merita-relation-types nil t nil nil old-type)))
      (unless (equal new-type old-type)
        ;; Update both directions
        (sqlite-execute db
          "UPDATE related_entries SET relation_type = ?
           WHERE entry_id = ? AND related_id = ? AND relation_type = ?"
          (list new-type eid rid old-type))
        (sqlite-execute db
          "UPDATE related_entries SET relation_type = ?
           WHERE entry_id = ? AND related_id = ? AND relation_type = ?"
          (list new-type rid eid old-type))
        (message "Changed link type: %s → %s" old-type new-type)))))

;;;###autoload
(defun merita-edit-link (entry-id)
  "Edit a link associated with ENTRY-ID.
Prompts for which link to edit, then for the new relation type."
  (interactive (list (merita--read-entry-id "Edit link for: ")))
  (let ((related (merita--get-related entry-id)))
    (if (null related)
        (message "No links for this entry.")
      (let* ((candidates
              (mapcar (lambda (rel)
                        (cons (format "[%s] %s"
                                      (or (alist-get 'relation_type rel) "")
                                      (merita--entry-short-title rel))
                              (alist-get 'id rel)))
                      related))
             (choice (completing-read "Edit link: " candidates nil t))
             (rel-id (cdr (assoc choice candidates))))
        (merita-edit-link-type rel-id)))))

;;;###autoload
(defun merita-remove-link (entry-id)
  "Remove a link associated with ENTRY-ID.
Prompts for which link to remove."
  (interactive (list (merita--read-entry-id "Remove link from: ")))
  (let ((related (merita--get-related entry-id)))
    (if (null related)
        (message "No links for this entry.")
      (let* ((candidates
              (mapcar (lambda (rel)
                        (cons (format "[%s] %s"
                                      (or (alist-get 'relation_type rel) "")
                                      (merita--entry-short-title rel))
                              (alist-get 'id rel)))
                      related))
             (choice (completing-read "Remove link: " candidates nil t))
             (rel-id (cdr (assoc choice candidates))))
        (when (yes-or-no-p (format "Remove link %s? " choice))
          (merita--delete-relation rel-id)
          (message "Link removed."))))))

(defun merita-entry-link ()
  "Link the displayed entry to another."
  (interactive)
  (when merita--entry-id
    (let ((rid (merita--read-entry-id "Link to: "))
          (rtype (completing-read "Link type: "
                                  merita-relation-types nil t)))
      (merita--insert-relation merita--entry-id rid rtype)
      (merita-entry-refresh))))

;;; ** 4.6. Entry Export & Navigation

(defun merita-entry-export ()
  "Export the displayed entry via the format dispatcher."
  (interactive)
  (when merita--entry-id
    (merita-export-entry merita--entry-id)))

(defun merita--resolve-citation-style (style-sym)
  "Return the formatter function for STYLE-SYM."
  (or (cdr (assoc (symbol-name style-sym) merita--style-formatter-map))
      (user-error "Unknown citation style: %s" style-sym)))

(defun merita-export-entry (id)
  "Export entry ID in a chosen format and citation style.
Prompts for format (plain text, LaTeX, BibTeX, RIS), then for
citation style if applicable.  Text and LaTeX are copied to the
kill ring; BibTeX and RIS prompt for a filename."
  (interactive (list (merita--read-entry-id "Export entry: ")))
  (let* ((entry (merita--get id))
         (format-choices '("plain text" "latex" "bibtex" "ris"))
         (fmt (completing-read "Export format: " format-choices nil t))
         (style-names (mapcar #'car merita--style-formatter-map))
         (default-style (symbol-name merita-default-citation-style)))
    (pcase fmt
      ("plain text"
       (let* ((style (completing-read
                       (format "Citation style [%s]: " default-style)
                       style-names nil t nil nil default-style))
              (formatter (cdr (assoc style merita--style-formatter-map)))
              (merita--latex-context nil)
              (text (funcall formatter entry)))
         (kill-new text)
         (message "Copied %s citation to kill ring." style)))
      ("latex"
       (let* ((style (completing-read
                       (format "Citation style [%s]: " default-style)
                       style-names nil t nil nil default-style))
              (formatter (cdr (assoc style merita--style-formatter-map)))
              (merita--latex-context t)
              (text (funcall formatter entry)))
         (kill-new text)
         (message "Copied LaTeX (%s) citation to kill ring." style)))
      ("bibtex"
       (let* ((text (merita--entry-to-bibtex entry))
              (filename (read-file-name "Save BibTeX to: "
                                        merita-default-export-directory nil nil
                                        (format "entry-%d.bib" id))))
         (with-temp-file filename
           (insert text "\n"))
         (message "Exported to %s" filename)))
      ("ris"
       (let* ((text (merita--entry-to-ris entry))
              (filename (read-file-name "Save RIS to: "
                                        merita-default-export-directory nil nil
                                        (format "entry-%d.ris" id))))
         (with-temp-file filename
           (insert text "\n"))
         (message "Exported to %s" filename))))))

(defun merita--entry-date-val (val fallback)
  "Coerce VAL to an integer for date comparison.
Returns FALLBACK for nil, empty strings, and :null."
  (cond
   ((null val) fallback)
   ((eq val :null) fallback)
   ((and (stringp val) (string-empty-p val)) fallback)
   ((stringp val) (string-to-number val))
   ((numberp val) val)
   (t fallback)))

(defconst merita--sort-year-sql
  "CASE WHEN year IS NULL OR year = '' THEN 9999 ELSE year END"
  "SQL expression for year sort key (nulls sort last).")

(defconst merita--sort-month-sql
  "CASE WHEN month IS NULL OR month = '' THEN 99 ELSE month END"
  "SQL expression for month sort key (nulls sort last).")

(defconst merita--sort-day-sql
  "CASE WHEN day IS NULL OR day = '' THEN 99 ELSE day END"
  "SQL expression for day sort key (nulls sort last).")

(defun merita-entry-next ()
  "Display the next entry forward in time (newer, then undated)."
  (interactive)
  (when merita--entry-id
    (let* ((db (merita--ensure-db))
           (cur (merita--get merita--entry-id))
           (yr (merita--entry-date-val (alist-get 'year cur) 9999))
           (mo (merita--entry-date-val (alist-get 'month cur) 99))
           (dy (merita--entry-date-val (alist-get 'day cur) 99))
           (row (caar (sqlite-select db
                        (format "SELECT id FROM data
                         WHERE (%s, %s, %s, id) > (?, ?, ?, ?)
                         ORDER BY %s ASC, %s ASC, %s ASC, id ASC
                         LIMIT 1"
                                merita--sort-year-sql merita--sort-month-sql merita--sort-day-sql
                                merita--sort-year-sql merita--sort-month-sql merita--sort-day-sql)
                        (list yr mo dy merita--entry-id)))))
      (if row
          (merita--display-entry (merita--get row))
        (message "Most recent entry.")))))

(defun merita-entry-prev ()
  "Display the next entry backward in time (older)."
  (interactive)
  (when merita--entry-id
    (let* ((db (merita--ensure-db))
           (cur (merita--get merita--entry-id))
           (yr (merita--entry-date-val (alist-get 'year cur) 9999))
           (mo (merita--entry-date-val (alist-get 'month cur) 99))
           (dy (merita--entry-date-val (alist-get 'day cur) 99))
           (row (caar (sqlite-select db
                        (format "SELECT id FROM data
                         WHERE (%s, %s, %s, id) < (?, ?, ?, ?)
                         ORDER BY %s DESC, %s DESC, %s DESC, id DESC
                         LIMIT 1"
                                merita--sort-year-sql merita--sort-month-sql merita--sort-day-sql
                                merita--sort-year-sql merita--sort-month-sql merita--sort-day-sql)
                        (list yr mo dy merita--entry-id)))))
      (if row
          (merita--display-entry (merita--get row))
        (message "Oldest entry.")))))

;;; ** 4.7. Browse Mode

(defcustom merita-browse-columns
  '((status . 12) (type . 18) (year . 5) (month . 3)
    (title . 65) (authors . 45) (journal . 30) (doi . 25)
    (author_position . 8) (citations . 4))
  "Columns displayed in `merita-browse-mode'.
Each element is (COLUMN . WIDTH).  Any database field is valid."
  :type '(repeat (cons (symbol :tag "Column") (integer :tag "Width")))
  :group 'merita)

(defvar-local merita-browse--filter-layers nil
  "List of active filter layers for browse mode.
Each layer is a plist with :kind (`text' or `tag'), :value (search
term or tag string), :join (nil, and, or, and-not, or-not),
matching tabularium's filter conventions.  Layers without :kind
are treated as text filters for backward compatibility.")

(defconst merita-browse--search-fields
  '("title" "authors" "journal" "keywords")
  "Fields searched by browse mode text filters.")

(defconst merita-browse--join-symbols
  '((nil . "") (and . " ∧ ") (or . " ∨ ") (and-not . " ∧¬ ") (or-not . " ∨¬ "))
  "Alist mapping join types to display symbols.")

(defun merita-browse--filter-layer-sql (layer)
  "Return a SQL condition for a single filter LAYER.
Tag layers use boundary-safe matching against the tags column;
text layers (the default) match across title, authors, journal,
and keywords."
  (pcase (or (plist-get layer :kind) 'text)
    ('tag (merita--tag-match-clause))
    (_    (let ((conditions (mapcar (lambda (f)
                                      (format "%s LIKE ? COLLATE NOCASE" f))
                                    merita-browse--search-fields)))
            (format "(%s)" (string-join conditions " OR "))))))

(defun merita-browse--filter-layer-params (layer)
  "Return SQL parameters for a single filter LAYER."
  (pcase (or (plist-get layer :kind) 'text)
    ('tag (list (merita--tag-match-param (plist-get layer :value))))
    (_    (let ((pat (format "%%%s%%" (plist-get layer :value))))
            (make-list (length merita-browse--search-fields) pat)))))

(defun merita-browse--build-filter-clause ()
  "Build a SQL WHERE clause and params from filter layers.
Returns (WHERE-STRING . PARAMS-LIST), or nil if no filters."
  (when merita-browse--filter-layers
    (let ((parts '())
          (params '()))
      (dolist (layer merita-browse--filter-layers)
        (let ((sql (merita-browse--filter-layer-sql layer))
              (join (plist-get layer :join)))
          (push (concat (pcase join
                          ('and " AND ")
                          ('or " OR ")
                          ('and-not " AND NOT ")
                          ('or-not " OR NOT ")
                          (_ ""))
                        sql)
                parts))
        (setq params (append params (merita-browse--filter-layer-params layer))))
      (cons (format "WHERE %s" (string-join (nreverse parts)))
            params))))

(defun merita-browse--filter-description ()
  "Return a human-readable description of all filter layers."
  (when merita-browse--filter-layers
    (let ((parts '()))
      (dolist (layer merita-browse--filter-layers)
        (let ((join-sym (alist-get (plist-get layer :join)
                                   merita-browse--join-symbols))
              (val (plist-get layer :value))
              (kind (or (plist-get layer :kind) 'text)))
          (push (format "%s%s%s"
                        join-sym
                        (if (eq kind 'tag) "#" "")
                        val)
                parts)))
      (string-join (nreverse parts)))))

(defun merita-browse--filter-prompt-join ()
  "Prompt for join operator when filters already exist."
  (when merita-browse--filter-layers
    (let ((choice (completing-read "Join logic: "
                                   '("AND" "OR" "AND NOT" "OR NOT")
                                   nil t nil nil "AND")))
      (cdr (assoc choice '(("AND" . and) ("OR" . or)
                           ("AND NOT" . and-not)
                           ("OR NOT" . or-not)))))))

;;;###autoload
(defun merita-browse (&optional filter)
  "Browse all entries in a tabulated list.
When FILTER is non-nil, apply it as the initial text filter."
  (interactive)
  (merita--ensure-db)
  (let ((buf (get-buffer-create "*merita-browse*")))
    (with-current-buffer buf
      (merita-browse-mode)
      (when filter
        (setq merita-browse--filter-layers
              (list (list :kind 'text :value filter :join nil))))
      (merita-browse--refresh))
    (switch-to-buffer buf)))

(defvar merita-browse-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'merita-browse-view)
    (define-key map (kbd "e") #'merita-browse-export)
    (define-key map (kbd "E") #'merita-browse-edit)
    (define-key map (kbd "D") #'merita-browse-delete)
    (define-key map (kbd "N") #'merita-browse-new-entry)
    (define-key map (kbd "d") #'merita-browse-add-doi)
    (define-key map (kbd "b") #'merita-browse-open-url)
    (define-key map (kbd "O") #'merita-browse-open-file)
    (define-key map (kbd "f") #'merita-browse-filter)
    (define-key map (kbd "/") #'merita-browse-filter)
    (define-key map (kbd "t") #'merita-browse-filter-tag)
    (define-key map (kbd "F") #'merita-browse-filter-clear)
    (define-key map (kbd "m") #'merita-browse-metrics)
    (define-key map (kbd "i") #'merita-browse-import)
    (define-key map (kbd "g") #'merita-browse-refresh)
    (define-key map (kbd "=") #'merita-browse-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `merita-browse-mode'.")

(defconst merita--browse-column-headers
  '((status . "Status") (type . "Type") (year . "Year") (month . "Mo")
    (day . "Day") (title . "Title") (authors . "Authors")
    (journal . "Journal") (journal_abbrev . "J.Abbrev")
    (volume . "Vol") (issue . "Iss") (pages . "Pages") (eid . "ArtID")
    (book_title . "Book") (publisher . "Publisher") (edition . "Ed")
    (editors . "Editors") (series . "Series") (isbn . "ISBN")
    (issn . "ISSN")
    (conference . "Conference") (conference_location . "Conf.Loc")
    (conference_date . "Conf.Date") (organization . "Org")
    (school . "School")
    (doi . "DOI") (pmid . "PMID") (pmcid . "PMCID")
    (arxiv_id . "arXiv") (url . "URL")
    (author_position . "Pos") (author_count . "N.Auth")
    (impact_factor . "IF") (citations . "Citations") (altmetric . "Altm")
    (status . "Status") (peer_reviewed . "PR")
    (abstract . "Abstract") (keywords . "Keywords")
    (mesh_terms . "MeSH")
    (funding . "Funding") (grant_role . "G.Role")
    (grant_amount . "G.Amt") (grant_period . "G.Period")
    (awards . "Awards") (award_body . "Awd.Body")
    (notes . "Notes") (tags . "Tags")
    (bibtex_key . "BibKey") (bibtex_type . "BibType")
    (repo_url . "Repo") (repo_language . "Lang")
    (pkg_registry . "Registry") (pkg_name . "Pkg") (pkg_version . "PkgVer")
    (file_path . "File")
    (links . "Links")
    (date_added . "Added") (date_modified . "Modified"))
  "Alist mapping field symbols to column header strings.")

(defconst merita--browse-integer-fields
  '(year month day citations altmetric author_count links peer_reviewed)
  "Fields that hold integer values for formatting and sorting.")

(defconst merita--browse-real-fields
  '(impact_factor grant_amount)
  "Fields that hold real/float values.")

(defun merita--browse-column-header (field)
  "Return the header string for FIELD."
  (or (alist-get field merita--browse-column-headers)
      (capitalize (replace-regexp-in-string "_" " " (symbol-name field)))))

(defun merita--browse-format-value (field value width)
  "Format VALUE for display in a browse column.
FIELD is the field symbol, WIDTH is the column width."
  (cond
   ((null value) "")
   ((eq value :null) "")
   ((memq field merita--browse-real-fields)
    (if (and (numberp value) (> value 0))
        (format "%.1f" value) ""))
   ((memq field merita--browse-integer-fields)
    (if (and (numberp value) (> value 0))
        (format "%d" value) ""))
   ((memq field '(date_added date_modified))
    (let ((s (format "%s" value)))
      (substring s 0 (min 10 (length s)))))
   (t
    (merita--truncate (format "%s" value) (max 1 (1- width))))))

(defun merita--browse-make-numeric-sorter (idx)
  "Return a numeric comparator for column at position IDX."
  (lambda (a b)
    (< (string-to-number (or (aref (cadr a) idx) "0"))
       (string-to-number (or (aref (cadr b) idx) "0")))))

(defun merita--browse-build-format ()
  "Build `tabulated-list-format' from `merita-browse-columns'."
  (let ((idx -1))
    (vconcat
     (cl-loop for pair in merita-browse-columns
              for col = (car pair)
              for width = (cdr pair)
              do (cl-incf idx)
              collect (list (merita--browse-column-header col)
                            width
                            (if (or (memq col merita--browse-integer-fields)
                                    (memq col merita--browse-real-fields))
                                (merita--browse-make-numeric-sorter idx)
                              t))))))

(define-derived-mode merita-browse-mode tabulated-list-mode "Merita Browse"
  "Major mode for browsing Merita entries.
\\{merita-browse-mode-map}"
  (setq tabulated-list-format (merita--browse-build-format))
  ;; Don't set tabulated-list-sort-key — SQL controls the order
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun merita-browse--refresh ()
  "Refresh the browse buffer from the database."
  (interactive)
  (merita--ensure-db)
  (setq tabulated-list-format (merita--browse-build-format))
  (tabulated-list-init-header)
  (let* ((cols (mapcar #'car merita-browse-columns))
         (field-list (string-join
                      (delete-dups (cons "id" (mapcar #'symbol-name cols)))
                      ", "))
         ;; Build WHERE clause from filter layers
         (filter-clause (merita-browse--build-filter-clause))
         (sql (format "SELECT %s FROM data %s ORDER BY %s DESC, %s DESC, %s DESC, id DESC"
                      field-list
                      (or (car filter-clause) "")
                      merita--sort-year-sql
                      merita--sort-month-sql
                      merita--sort-day-sql))
         (entries (merita--query sql (cdr filter-clause))))
    (setq tabulated-list-entries
          (mapcar (lambda (e)
                    (let ((id (alist-get 'id e)))
                      (list id
                            (vconcat
                             (cl-loop for pair in merita-browse-columns
                                      for col = (car pair)
                                      for width = (cdr pair)
                                      collect (merita--browse-format-value
                                               col (alist-get col e) width))))))
                  entries))
    (tabulated-list-print t)
    (if merita-browse--filter-layers
        (progn
          (setq mode-name (format "Merita Browse [%s]"
                                  (merita-browse--filter-description)))
          (message "%d entries matching %s."
                   (length entries) (merita-browse--filter-description)))
      (setq mode-name "Merita Browse")
      (message "%d entries." (length entries)))))

(defun merita-browse-refresh ()
  "Refresh the browse view."
  (interactive)
  (merita-browse--refresh))

(defun merita-browse--id-at-point ()
  "Return the entry ID at point in the browse list."
  (tabulated-list-get-id))

(defun merita-browse-view ()
  "View the entry at point."
  (interactive)
  (let ((id (merita-browse--id-at-point)))
    (when id (merita--display-entry (merita--get id)))))

(defun merita-browse-export ()
  "Export the entry at point via the format dispatcher."
  (interactive)
  (let ((id (merita-browse--id-at-point)))
    (when id (merita-export-entry id))))

(defun merita-browse-edit ()
  "Edit the entry at point."
  (interactive)
  (let ((id (merita-browse--id-at-point)))
    (when id
      (merita-edit id))))

(defun merita-browse-delete ()
  "Delete the entry at point."
  (interactive)
  (let ((id (merita-browse--id-at-point)))
    (when id
      (merita-delete id))))

(defun merita-browse-new-entry ()
  "Add a new entry, then refresh the browse list."
  (interactive)
  (call-interactively #'merita-new-entry))

(defun merita-browse-add-doi ()
  "Add an entry by DOI, then refresh the browse list."
  (interactive)
  (call-interactively #'merita-add-from-doi)
  (merita-browse--refresh))

(defun merita-browse-import ()
  "Import entries from a file, prompting for format.
Supported formats: BibTeX, CSV, TSV, ORCID."
  (interactive)
  (let* ((choices '("bibtex" "csv" "tsv" "orcid"))
         (fmt (completing-read "Import format: " choices nil t)))
    (pcase fmt
      ("bibtex" (call-interactively #'merita-import-bibtex))
      ("csv"    (call-interactively #'merita-import-csv))
      ("tsv"    (call-interactively #'merita-import-tsv))
      ("orcid"  (call-interactively #'merita-import-orcid)))
    (merita-browse--refresh)))

(defun merita-browse-filter (&optional kind)
  "Add a filter layer to the browse view.
With no prefix argument, prompts for a free-text search term that
matches across title, authors, journal, and keywords.  With a
prefix argument, KIND is `tag' and the prompt accepts a single tag
to match against the tags column.  When filters already exist,
also prompts for join logic (AND, OR, AND NOT, OR NOT)."
  (interactive (list (if current-prefix-arg 'tag 'text)))
  (let* ((kind (or kind 'text))
         (term (read-string
                (format "%s%s: "
                        (if (eq kind 'tag) "Tag filter" "Filter")
                        (if merita-browse--filter-layers
                            (format " [%s] +"
                                    (merita-browse--filter-description))
                          "")))))
    (unless (string-empty-p (string-trim term))
      (let ((join (merita-browse--filter-prompt-join))
            (value (if (eq kind 'tag)
                       (downcase (string-trim term))
                     (string-trim term))))
        (setq merita-browse--filter-layers
              (append merita-browse--filter-layers
                      (list (list :kind kind :value value :join join))))
        (merita-browse--refresh)))))

;;;###autoload
(defun merita-browse-filter-tag ()
  "Add a tag filter layer to the browse view.
Equivalent to invoking `merita-browse-filter' with a prefix argument."
  (interactive)
  (merita-browse-filter 'tag))

(defun merita-browse-filter-clear ()
  "Clear all filter layers from the browse view."
  (interactive)
  (setq merita-browse--filter-layers nil)
  (setq mode-name "Merita Browse")
  (merita-browse--refresh))

(defun merita-browse-filter-delete ()
  "Delete a specific filter layer by selection."
  (interactive)
  (if (null merita-browse--filter-layers)
      (message "No filters active.")
    (let* ((descs (cl-loop for layer in merita-browse--filter-layers
                           for i from 1
                           collect (format "%d: %s%s" i
                                           (let ((sym (alist-get (plist-get layer :join)
                                                                 merita-browse--join-symbols)))
                                             (if (string-empty-p sym) "" sym))
                                           (plist-get layer :value))))
           (choice (completing-read "Delete filter: " descs nil t))
           (idx (1- (string-to-number (car (split-string choice ":"))))))
      (setq merita-browse--filter-layers
            (cl-remove-if (let ((i -1))
                            (lambda (_) (= (cl-incf i) idx)))
                          merita-browse--filter-layers))
      ;; Fix join on new first layer
      (when (and merita-browse--filter-layers
                 (plist-get (car merita-browse--filter-layers) :join))
        (setq merita-browse--filter-layers
              (cons (plist-put (copy-sequence (car merita-browse--filter-layers)) :join nil)
                    (cdr merita-browse--filter-layers))))
      (merita-browse--refresh))))

(defun merita-browse-metrics ()
  "Show metrics for the entry at point."
  (interactive)
  (let ((id (merita-browse--id-at-point)))
    (when id
      (if (fboundp 'merita-metrics-format-entry)
          (message "%s" (merita-metrics-format-entry id))
        (message "Metrics module not loaded.")))))

(defun merita-browse-open-url ()
  "Open the DOI or PMID of the entry at point in a browser."
  (interactive)
  (let ((id (merita-browse--id-at-point)))
    (if id
        (merita--open-url id)
      (message "No entry at point."))))

(defun merita-browse-open-file ()
  "Open the local file for the entry at point."
  (interactive)
  (let ((id (merita-browse--id-at-point)))
    (if id
        (merita--open-file id)
      (message "No entry at point."))))

;;; * 5. Statistics

;;; ** 5.1. H-Index and I10-Index

;;;###autoload
(defun merita-h-index ()
  "Calculate and return the h-index from citation data."
  (interactive)
  (let* ((db (merita--ensure-db))
         (cites (mapcar #'car
                        (sqlite-select db
                          "SELECT citations FROM data
                           WHERE citations > 0
                           ORDER BY citations DESC")))
         (h 0))
    (cl-loop for c in cites
             for i from 1
             while (>= c i)
             do (setq h i))
    (when (called-interactively-p 'any)
      (message "h-index: %d" h))
    h))

;;;###autoload
(defun merita-i10-index ()
  "Calculate and return the i10-index (publications with ≥10 citations)."
  (interactive)
  (let* ((db (merita--ensure-db))
         (n (or (caar (sqlite-select db
                        "SELECT COUNT(*) FROM data WHERE citations >= 10")) 0)))
    (when (called-interactively-p 'any)
      (message "i10-index: %d" n))
    n))

;;; ** 5.2. Statistics Dashboard

;;;###autoload
(defun merita-stats ()
  "Display a comprehensive statistics dashboard."
  (interactive)
  (let* ((db (merita--ensure-db))
         (total (merita--count))
         (pr-total (merita-count-peer-reviewed-pubs))
         (pr-where (merita--pr-where))
         (buf (get-buffer-create "*merita-stats*"))
         ;; Layout: 78 inner width (80 with box borders)
         ;; Label=56, two number columns of 8 each, gaps of 2
         (lw   56)
         (bw   78)
         (hdr  (format "  %%-%ds  %%8s  %%8s\n" lw))
         (row2 (format "  %%-%ds  %%8d  %%8d\n" lw))
         (row1 (format "  %%-%ds            %%8d\n" lw))
         (rowf (format "  %%-%ds  %%18s\n" lw))
         (sep  (format "  %%-%ds  %%8s  %%8s\n" lw)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (let* ((title " MERITA — Career Statistics ")
               (pad (max 0 (- bw (length title))))
               (lpad (/ pad 2))
               (rpad (- pad lpad)))
          (insert "╔" (make-string bw ?═) "╗\n")
          (insert "║" (make-string lpad ?\s) title (make-string rpad ?\s) "║\n")
          (insert "╚" (make-string bw ?═) "╝\n\n"))

        ;; ── Key Metrics ──
        (insert (format row1 "Total entries" total))
        (insert (format row1 "Peer-reviewed publications" pr-total))
        (let* ((h (merita-h-index))
               (i10 (merita-i10-index))
               (total-cites (or (caar (sqlite-select db
                                        "SELECT SUM(citations) FROM data")) 0))
               (influential-cites (or (caar (sqlite-select db
                                              "SELECT SUM(influential_citation_count) FROM data
                                               WHERE influential_citation_count IS NOT NULL")) 0))
               (mean-fwci (caar (sqlite-select db
                                  (format "SELECT ROUND(AVG(fwci), 2) FROM data
                                           WHERE %s AND fwci IS NOT NULL"
                                          pr-where))))
               (mean-cites (or (caar (sqlite-select db
                                       (format "SELECT ROUND(AVG(citations), 1) FROM data
                                                WHERE %s AND citations IS NOT NULL"
                                               pr-where))) 0))
               (median-cites
                (or (caar (sqlite-select db
                            (format "SELECT citations FROM data
                                     WHERE %s AND citations IS NOT NULL
                                     ORDER BY citations
                                     LIMIT 1 OFFSET (
                                       SELECT COUNT(*) / 2 FROM data
                                       WHERE %s AND citations IS NOT NULL)"
                                    pr-where pr-where))) 0))
               ;; Career span
               (first-year (caar (sqlite-select db
                                   "SELECT MIN(year) FROM data
                                    WHERE year IS NOT NULL AND year != ''")))
               (last-year (caar (sqlite-select db
                                  "SELECT MAX(year) FROM data
                                   WHERE year IS NOT NULL AND year != ''")))
               (cur-year (nth 5 (decode-time)))
               (career-years (if first-year (1+ (- cur-year first-year)) 0))
               (cites-per-year (if (> career-years 0)
                                   (/ (float total-cites) career-years) 0)))
          (insert (format row1 "h-index" h))
          (insert (format row1 "i10-index" i10))
          (insert (format row1 "Total citations" total-cites))
          (when (> influential-cites 0)
            (insert (format row1 "Influential citations" influential-cites)))
          (when mean-fwci
            (insert (format rowf "Mean FWCI (peer-reviewed)"
                            (format "%.2f" mean-fwci))))
          (insert (format rowf "Mean citations (peer-reviewed)"
                          (format "%.1f" mean-cites)))
          (insert (format row1 "Median citations (peer-reviewed)" median-cites))
          (when (> career-years 0)
            (insert (format rowf "Citations per year"
                            (format "%.1f" cites-per-year)))
            (insert (format rowf "Career span"
                            (format "%s–%s (%d yrs)"
                                    (or first-year "?") (or last-year "?")
                                    career-years)))))

        ;; ── Impact Factor ──
        (let* ((if-count (or (caar (sqlite-select db
                                     "SELECT COUNT(*) FROM data
                                      WHERE impact_factor IS NOT NULL
                                        AND impact_factor > 0")) 0))
               (if-mean (or (caar (sqlite-select db
                                    "SELECT ROUND(AVG(impact_factor), 2) FROM data
                                     WHERE impact_factor IS NOT NULL
                                       AND impact_factor > 0")) 0))
               (if-median
                (or (caar (sqlite-select db
                            "SELECT impact_factor FROM data
                             WHERE impact_factor IS NOT NULL AND impact_factor > 0
                             ORDER BY impact_factor
                             LIMIT 1 OFFSET (
                               SELECT COUNT(*) / 2 FROM data
                               WHERE impact_factor IS NOT NULL
                                 AND impact_factor > 0)")) 0))
               (if-max-row (car (sqlite-select db
                                  "SELECT COALESCE(NULLIF(journal_abbrev, ''), journal),
                                          impact_factor
                                   FROM data
                                   WHERE impact_factor IS NOT NULL
                                   ORDER BY impact_factor DESC LIMIT 1")))
               (if-max-journal (when if-max-row (car if-max-row)))
               (if-max-val (when if-max-row (cadr if-max-row))))
          (when (> if-count 0)
            (insert "\n  ── Impact Factor ──\n")
            (insert (format rowf "Mean IF" (format "%.2f" if-mean)))
            (insert (format rowf "Median IF" (format "%.1f" if-median)))
            (when if-max-journal
              (insert (format rowf "Highest IF"
                              (format "%.1f (%s)" if-max-val if-max-journal))))
            (insert (format row1 "Entries with IF" if-count))))

        ;; ── Most Cited ──
        (let ((most-cited (sqlite-select db
                            "SELECT title, citations, year FROM data
                             WHERE citations > 0
                             ORDER BY citations DESC LIMIT 5")))
          (when most-cited
            (insert "\n  ── Most Cited ──\n")
            (dolist (mc-row most-cited)
              (insert (format "  [%4d] %s (%s)\n"
                              (nth 1 mc-row)
                              (merita--truncate (nth 0 mc-row) (- lw 14))
                              (or (nth 2 mc-row) "n.d."))))))

        ;; ── Links ──
        (let ((link-count (or (caar (sqlite-select db
                                      "SELECT COUNT(DISTINCT entry_id) FROM related_entries")) 0))
              (rel-count (or (caar (sqlite-select db
                                     "SELECT COUNT(*) / 2 FROM related_entries")) 0)))
          (when (> rel-count 0)
            (insert "\n  ── Links ──\n")
            (insert (format row1 "Entries with links" link-count))
            (insert (format row1 "Total links" rel-count))
            (let ((by-rtype (sqlite-select db
                              "SELECT relation_type, COUNT(*) / 2
                               FROM related_entries
                               GROUP BY relation_type
                               ORDER BY COUNT(*) DESC")))
              (dolist (r-row by-rtype)
                (insert (format row1 (format "  %s" (car r-row)) (cadr r-row)))))))

        ;; ── By Type ──
        (insert "\n  ── Type ──\n")
        (let* ((by-type (sqlite-select db
                          "SELECT type, COUNT(*) FROM data
                           GROUP BY type ORDER BY COUNT(*) DESC"))
               (sort-pr (eq merita-stats-sort-by 'pr))
               (typed-rows
                (mapcar (lambda (type-row)
                          (let* ((etype (car type-row))
                                 (all-n (cadr type-row))
                                 (pr-n (caar (sqlite-select db
                                               (format "SELECT COUNT(*) FROM data
                                                        WHERE type = ? AND %s" pr-where)
                                               (list etype)))))
                            (list etype (or pr-n 0) all-n)))
                        by-type)))
          (when sort-pr
            (setq typed-rows (sort typed-rows (lambda (a b) (> (nth 1 a) (nth 1 b))))))
          (insert (format hdr "" "PR" "All"))
          (insert (format sep "" "────────" "────────"))
          (dolist (tr typed-rows)
            (insert (format row2 (car tr) (nth 1 tr) (nth 2 tr)))))

        ;; ── By Status ──
        (insert "\n  ── Status ──\n")
        (insert (format hdr "" "PR" "All"))
        (insert (format sep "" "────────" "────────"))
        (let* ((by-status (sqlite-select db
                            "SELECT status, COUNT(*) FROM data
                             GROUP BY status ORDER BY COUNT(*) DESC"))
               (sort-pr (eq merita-stats-sort-by 'pr))
               (status-rows
                (mapcar (lambda (s-row)
                          (let* ((status (car s-row))
                                 (all-n (cadr s-row))
                                 (pr-n (caar (sqlite-select db
                                               (format "SELECT COUNT(*) FROM data
                                                        WHERE status = ? AND %s" pr-where)
                                               (list status)))))
                            (list status (or pr-n 0) all-n)))
                        by-status)))
          (when sort-pr
            (setq status-rows (sort status-rows (lambda (a b) (> (nth 1 a) (nth 1 b))))))
          (dolist (sr status-rows)
            (insert (format row2 (car sr) (nth 1 sr) (nth 2 sr)))))

        ;; ── Authorship ──
        (insert "\n  ── Authorship ──\n")
        (insert (format hdr "" "PR" "All"))
        (insert (format sep "" "────────" "────────"))
        (let* ((first-all (merita-count-first-author))
               (first-pr (merita-count-first-author-pr))
               (second-all (merita-count-second-author))
               (second-pr (merita--count
                           (format "%s AND author_position = 'second'" pr-where)))
               (third-all (merita-count-third-author))
               (third-pr (merita--count
                          (format "%s AND author_position = 'third'" pr-where)))
               (senior-all (merita-count-senior-author))
               (senior-pr (merita--count
                           (format "%s AND author_position IN ('senior','co-senior')"
                                   pr-where)))
               (sig-all (merita-count-significant-author))
               (sig-pr (merita--count
                        (format "%s AND author_position IN ('first','co-first','second','third','senior','co-senior','corresponding','sole')"
                                pr-where))))
          (insert (format row2 "First/co-first" first-pr first-all))
          (insert (format row2 "Second" second-pr second-all))
          (insert (format row2 "Third" third-pr third-all))
          (insert (format row2 "Senior/co-senior" senior-pr senior-all))
          (insert (format row2 "Significant (1st+2nd+3rd+Sr)" sig-pr sig-all)))

        ;; ── By Year ──
        (insert "\n  ── Year ──\n")
        (insert (format hdr "" "PR" "All"))
        (insert (format sep "" "────────" "────────"))
        (let ((by-year (sqlite-select db
                         "SELECT CASE WHEN year IS NULL OR year = '' THEN 'n.d.' ELSE year END AS yr,
                                 COUNT(*) FROM data
                          GROUP BY yr ORDER BY
                            CASE WHEN year IS NULL OR year = '' THEN 9999 ELSE year END DESC")))
          (dolist (y-row by-year)
            (let* ((yr (car y-row))
                   (all-n (cadr y-row))
                   (pr-n (caar (sqlite-select db
                                 (format "SELECT COUNT(*) FROM data
                                          WHERE CASE WHEN year IS NULL OR year = '' THEN 'n.d.' ELSE year END = ?
                                          AND %s" pr-where)
                                 (list yr)))))
              (insert (format row2 yr (or pr-n 0) all-n)))))

        ;; ── Top Journals ──
        (insert "\n  ── Top Journals ──\n")
        (insert (format hdr "" "PR" "All"))
        (insert (format sep "" "────────" "────────"))
        (let* ((journals (sqlite-select db
                           "SELECT COALESCE(NULLIF(journal_abbrev, ''), journal),
                                   COUNT(*) FROM data
                            WHERE journal IS NOT NULL
                            GROUP BY COALESCE(NULLIF(journal_abbrev, ''), journal)
                            ORDER BY COUNT(*) DESC LIMIT 10"))
               (sort-pr (eq merita-stats-sort-by 'pr))
               (journal-rows
                (mapcar (lambda (j-row)
                          (let* ((jname (car j-row))
                                 (all-n (cadr j-row))
                                 (pr-n (caar (sqlite-select db
                                               (format "SELECT COUNT(*) FROM data
                                                        WHERE COALESCE(NULLIF(journal_abbrev, ''), journal) = ?
                                                        AND %s" pr-where)
                                               (list jname)))))
                            (list jname (or pr-n 0) all-n)))
                        journals)))
          (when sort-pr
            (setq journal-rows (sort journal-rows (lambda (a b) (> (nth 1 a) (nth 1 b))))))
          (dolist (jr journal-rows)
            (insert (format row2 (merita--truncate (car jr) lw) (nth 1 jr) (nth 2 jr)))))

        (insert "\n")
        (goto-char (point-min))
        (special-mode)))
    (pop-to-buffer buf)))

;;; * 6. Import

;;; ** 6.1. DOI Import (Crossref API)

(defun merita--crossref-url (doi)
  "Return the Crossref API URL for DOI."
  (format "https://api.crossref.org/works/%s"
          (url-hexify-string doi)))

(defun merita--crossref-fetch (doi)
  "Fetch metadata for DOI from Crossref.  Return parsed JSON."
  (let* ((url (merita--crossref-url doi))
         (url-request-extra-headers
          (append '(("Accept" . "application/json"))
                  (when merita-crossref-mailto
                    `(("User-Agent" .
                       ,(format "merita.el/0.1 (mailto:%s)"
                                merita-crossref-mailto))))))
         (buf (url-retrieve-synchronously url t t 30)))
    (when buf
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (re-search-forward "^$" nil t)
            (let ((json-object-type 'alist)
                  (json-array-type 'list)
                  (json-key-type 'symbol))
              (condition-case nil
                  (json-read)
                (json-error
                 (user-error "Failed to parse Crossref response for DOI: %s" doi)))))
        (kill-buffer buf)))))

(defun merita--crossref-parse (data)
  "Parse Crossref API response DATA into a Merita entry alist."
  (let* ((message (alist-get 'message data))
         (result '()))
    (cl-flet ((add (key val) (when val (push (cons key val) result))))
      (add 'title (car (alist-get 'title message)))
      (let ((authors (alist-get 'author message)))
        (when authors
          (add 'authors
               (mapconcat (lambda (a)
                            (let ((family (alist-get 'family a))
                                  (given (alist-get 'given a)))
                              (if given (format "%s %s" family given) family)))
                          authors ", "))
          (add 'author_count (length authors))))
      (let ((cr-type (alist-get 'type message)))
        (add 'type
             (pcase cr-type
               ("journal-article" "journal-article")
               ("book-chapter" "book-chapter")
               ("book" "book")
               ("proceedings-article" "conference-paper")
               ("posted-content" "preprint")
               ("report" "technical-report")
               ("dissertation" "thesis-doctoral")
               ("monograph" "book")
               ("edited-book" "book")
               ("review" "review-article")
               (_ "journal-article"))))
      (add 'journal (car (alist-get 'container-title message)))
      (add 'journal_abbrev (car (alist-get 'short-container-title message)))
      (let* ((issued (alist-get 'issued message))
             (date-parts (car (alist-get 'date-parts issued))))
        (when date-parts
          (add 'year (nth 0 date-parts))
          (when (nth 1 date-parts) (add 'month (nth 1 date-parts)))
          (when (nth 2 date-parts) (add 'day (nth 2 date-parts)))))
      (add 'volume (alist-get 'volume message))
      (add 'issue (alist-get 'issue message))
      (add 'pages (alist-get 'page message))
      (add 'doi (alist-get 'DOI message))
      (add 'url (alist-get 'URL message))
      (add 'isbn (car (alist-get 'ISBN message)))
      (add 'publisher (alist-get 'publisher message))
      (let ((abstract (alist-get 'abstract message)))
        (when abstract
          (add 'abstract (replace-regexp-in-string "<[^>]+>" "" abstract))))
      (let ((subjects (alist-get 'subject message)))
        (when subjects
          (add 'keywords (mapconcat #'identity subjects "; "))))
      (let ((refs (alist-get 'is-referenced-by-count message)))
        (when (and refs (> refs 0)) (add 'citations refs)))
      (add 'status "published"))
    result))

;;;###autoload
(defun merita-add-from-doi (doi)
  "Add an entry by fetching metadata from Crossref for DOI."
  (interactive "sDOI: ")
  (setq doi (replace-regexp-in-string "^https?://doi\\.org/" "" doi))
  (when (merita--get-by-doi doi)
    (user-error "Entry with DOI %s already exists" doi))
  (message "Fetching metadata for %s..." doi)
  (let* ((data (merita--crossref-fetch doi))
         (entry (merita--crossref-parse data)))
    (unless entry (user-error "No metadata found for DOI: %s" doi))
    (message "Found: %s (%s) %s, %s"
             (or (alist-get 'authors entry) "?")
             (or (alist-get 'year entry) "?")
             (or (alist-get 'title entry) "?")
             (or (alist-get 'journal entry) "?"))
    (let ((position (merita--read-author-position)))
      (push (cons 'author_position (symbol-name position)) entry))
    (let ((peer-reviewed (y-or-n-p "Peer-reviewed? ")))
      (push (cons 'peer_reviewed (if peer-reviewed 1 0)) entry))
    (when (y-or-n-p "Add impact factor? ")
      (let ((if-val (merita--read-number-field "Impact factor")))
        (when if-val (push (cons 'impact_factor if-val) entry))))
    (let ((id (merita--insert entry)))
      (merita--maybe-refresh-browse)
      (merita--display-entry (merita--get id))
      (message "Added: %s" (alist-get 'title entry))
      id)))

;;;###autoload
(defun merita-add-from-doi-batch ()
  "Add multiple entries by DOI.  Empty input finishes."
  (interactive)
  (let ((count 0))
    (catch 'done
      (while t
        (let ((doi (read-string (format "DOI #%d (empty to finish): " (1+ count)))))
          (if (string-empty-p doi) (throw 'done nil)
            (condition-case err
                (progn (merita-add-from-doi doi) (cl-incf count))
              (error (message "Error adding %s: %s" doi (error-message-string err))))))))
    (message "Added %d entries." count)))

;;; ** 6.2. BibTeX Import

(defun merita--bibtex-type-to-merita (bibtex-type)
  "Map BIBTEX-TYPE string to a Merita entry type symbol."
  (pcase (downcase bibtex-type)
    ("article" 'journal-article)
    ("book" 'book)
    ("incollection" 'book-chapter)
    ("inbook" 'book-chapter)
    ("inproceedings" 'conference-paper)
    ("conference" 'conference-paper)
    ("phdthesis" 'thesis-doctoral)
    ("mastersthesis" 'thesis-masters)
    ("techreport" 'technical-report)
    ("unpublished" 'preprint)
    ("misc" 'other)
    (_ 'other)))

(defun merita--parse-bibtex-file (filename)
  "Parse a BibTeX file at FILENAME.  Returns list of alists."
  (if (require 'parsebib nil t)
      (merita--parse-bibtex-with-parsebib filename)
    (merita--parse-bibtex-builtin filename)))

(declare-function parsebib-parse "parsebib")

(defun merita--parse-bibtex-with-parsebib (filename)
  "Parse FILENAME using parsebib."
  (let* ((entries (parsebib-parse filename))
         (result '()))
    (maphash (lambda (key entry)
               (let ((alist (merita--convert-parsebib-entry key entry)))
                 (when alist (push alist result))))
             entries)
    (nreverse result)))

(defun merita--convert-parsebib-entry (key entry)
  "Convert a parsebib ENTRY with KEY to a Merita alist."
  (let ((result '())
        (bib-type (gethash "=type=" entry)))
    (cl-flet ((add (merita-field bib-field)
                (let ((val (gethash bib-field entry)))
                  (when (and val (not (string-empty-p val)))
                    (push (cons merita-field val) result)))))
      (push (cons 'type
                   (symbol-name (merita--bibtex-type-to-merita (or bib-type "misc"))))
            result)
      (add 'title "title") (add 'authors "author") (add 'journal "journal")
      (add 'volume "volume") (add 'issue "number") (add 'pages "pages")
      (add 'doi "doi") (add 'url "url") (add 'publisher "publisher")
      (add 'edition "edition") (add 'editors "editor")
      (add 'book_title "booktitle") (add 'isbn "isbn") (add 'series "series")
      (add 'school "school") (add 'organization "institution")
      (add 'abstract "abstract") (add 'keywords "keywords") (add 'notes "note")
      (let ((year (gethash "year" entry)))
        (when year (push (cons 'year (string-to-number year)) result)))
      (let ((month (gethash "month" entry)))
        (when month
          (let ((num (merita--parse-month month)))
            (when num (push (cons 'month num) result)))))
      (let ((day (gethash "day" entry)))
        (when day (push (cons 'day (string-to-number day)) result)))
      (push (cons 'bibtex_key key) result)
      (push (cons 'bibtex_type (or bib-type "misc")) result)
      (push (cons 'status "published") result))
    result))

(defun merita--parse-bibtex-builtin (filename)
  "Simple built-in BibTeX parser for FILENAME."
  (let ((entries '()))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (while (re-search-forward "@\\([a-zA-Z]+\\){\\s-*\\([^,]+\\)," nil t)
        (let* ((bib-type (match-string 1))
               (key (string-trim (match-string 2)))
               (start (point))
               (fields '()))
          (unless (member (downcase bib-type) '("string" "comment" "preamble"))
            (backward-char)
            (when (condition-case nil (progn (forward-sexp 1) t) (scan-error nil))
              (let ((field-text (buffer-substring-no-properties start (1- (point)))))
                (with-temp-buffer
                  (insert field-text)
                  (goto-char (point-min))
                  (while (re-search-forward "\\([a-zA-Z_-]+\\)\\s-*=\\s-*" nil t)
                    (let ((field-name (downcase (match-string 1)))
                          (value (merita--bibtex-read-value)))
                      (when value (push (cons field-name value) fields))))))
              (let ((entry (merita--bibtex-fields-to-merita bib-type key (nreverse fields))))
                (when entry (push entry entries))))))))
    (nreverse entries)))

(defun merita--bibtex-read-value ()
  "Read a BibTeX field value at point."
  (skip-chars-forward " \t\n")
  (cond
   ((looking-at "{")
    (let ((start (1+ (point))))
      (condition-case nil
          (progn (forward-sexp 1)
                 (let ((val (buffer-substring-no-properties start (1- (point)))))
                   (skip-chars-forward " \t\n,") val))
        (scan-error nil))))
   ((looking-at "\"")
    (let ((start (1+ (point))))
      (forward-char 1)
      (when (search-forward "\"" nil t)
        (let ((val (buffer-substring-no-properties start (1- (point)))))
          (skip-chars-forward " \t\n,") val))))
   ((looking-at "[a-zA-Z0-9]+")
    (let ((val (match-string 0)))
      (goto-char (match-end 0))
      (skip-chars-forward " \t\n,") val))
   (t nil)))

(defun merita--bibtex-fields-to-merita (bib-type key fields)
  "Convert BibTeX FIELDS alist with BIB-TYPE and KEY to Merita alist."
  (let ((result '()))
    (cl-flet ((add (merita-field bib-field)
                (let ((val (cdr (assoc bib-field fields))))
                  (when (and val (not (string-empty-p val)))
                    (push (cons merita-field val) result))))
              (get (field) (cdr (assoc field fields))))
      (push (cons 'type
                   (symbol-name (merita--bibtex-type-to-merita bib-type)))
            result)
      (add 'title "title") (add 'authors "author") (add 'journal "journal")
      (add 'volume "volume") (add 'issue "number") (add 'pages "pages")
      (add 'doi "doi") (add 'url "url") (add 'publisher "publisher")
      (add 'edition "edition") (add 'editors "editor")
      (add 'book_title "booktitle") (add 'isbn "isbn") (add 'series "series")
      (add 'school "school") (add 'organization "institution")
      (add 'abstract "abstract") (add 'keywords "keywords") (add 'notes "note")
      (let ((year (get "year")))
        (when year (push (cons 'year (string-to-number year)) result)))
      (let ((month (get "month")))
        (when month
          (let ((num (merita--parse-month month)))
            (when num (push (cons 'month num) result)))))
      (push (cons 'bibtex_key key) result)
      (push (cons 'bibtex_type bib-type) result)
      (push (cons 'status "published") result))
    result))

(defun merita--parse-month (month-str)
  "Parse MONTH-STR to integer 1-12, or nil."
  (let ((m (downcase (string-trim month-str))))
    (cond
     ((string-match-p "^[0-9]+$" m) (string-to-number m))
     ((string-prefix-p "jan" m) 1)  ((string-prefix-p "feb" m) 2)
     ((string-prefix-p "mar" m) 3)  ((string-prefix-p "apr" m) 4)
     ((string-prefix-p "may" m) 5)  ((string-prefix-p "jun" m) 6)
     ((string-prefix-p "jul" m) 7)  ((string-prefix-p "aug" m) 8)
     ((string-prefix-p "sep" m) 9)  ((string-prefix-p "oct" m) 10)
     ((string-prefix-p "nov" m) 11) ((string-prefix-p "dec" m) 12)
     (t nil))))

;;;###autoload
(defun merita-import-bibtex (filename)
  "Import entries from a BibTeX FILENAME.  Duplicate DOIs are skipped."
  (interactive "fBibTeX file: ")
  (let* ((entries (merita--parse-bibtex-file filename))
         (imported 0) (skipped 0))
    (dolist (entry entries)
      (let ((doi (alist-get 'doi entry)))
        (if (and doi (merita--get-by-doi doi))
            (progn (cl-incf skipped)
                   (message "Skipping duplicate DOI: %s" doi))
          (progn
            (merita--insert entry)
            (cl-incf imported)
            (message "Imported: %s" (or (alist-get 'title entry) "Untitled"))))))
    (message "BibTeX import: %d imported, %d skipped" imported skipped)
    (when (and (> imported 0)
               (y-or-n-p "Annotate author positions for imported entries? "))
      (merita-annotate-author-positions))))

;;; ** 6.3. CSV/TSV Import

(defun merita--parse-delimited-file (filename separator)
  "Parse a delimited FILENAME with SEPARATOR.  Returns list of alists."
  (let ((entries '()))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (let* ((header-line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
             (columns (mapcar #'string-trim
                              (split-string header-line separator)))
             (col-syms (mapcar #'intern columns)))
        (forward-line 1)
        (while (not (eobp))
          (let* ((line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))))
            (unless (string-empty-p (string-trim line))
              (let* ((values (split-string line separator))
                     (alist '()))
                (cl-loop for col in col-syms
                         for val in values
                         do (let ((trimmed (string-trim val)))
                              (unless (string-empty-p trimmed)
                                (push (cons col
                                            (if (and (memq col '(id year month
                                                                 citations altmetric
                                                                 author_count
                                                                 peer_reviewed))
                                                     (string-match-p "^-?[0-9]+$" trimmed))
                                                (string-to-number trimmed)
                                              (if (and (memq col '(impact_factor
                                                                   grant_amount))
                                                       (string-match-p "^-?[0-9.]+$" trimmed))
                                                  (string-to-number trimmed)
                                                trimmed)))
                                      alist))))
                (when alist (push (nreverse alist) entries)))))
          (forward-line 1))))
    (nreverse entries)))

;;;###autoload
(defun merita-import-csv (filename)
  "Import entries from a CSV FILENAME."
  (interactive "fCSV file: ")
  (merita--import-delimited filename ","))

;;;###autoload
(defun merita-import-tsv (filename)
  "Import entries from a TSV FILENAME."
  (interactive "fTSV file: ")
  (merita--import-delimited filename "\t"))

(defun merita--import-delimited (filename separator)
  "Import entries from delimited FILENAME with SEPARATOR."
  (let* ((entries (merita--parse-delimited-file filename separator))
         (imported 0) (skipped 0))
    (dolist (entry entries)
      (let ((clean (cl-remove-if
                    (lambda (pair)
                      (memq (car pair) '(id date_added date_modified)))
                    entry))
            (doi (alist-get 'doi entry)))
        (if (and doi (merita--get-by-doi doi))
            (progn (cl-incf skipped)
                   (message "Skipping duplicate DOI: %s" doi))
          (condition-case err
              (progn
                (merita--insert clean)
                (cl-incf imported))
            (error (message "Error importing row: %s" (error-message-string err))
                   (cl-incf skipped))))))
    (message "%s import: %d imported, %d skipped"
             (if (equal separator ",") "CSV" "TSV")
             imported skipped)))

;;; ** 6.4. Author Position Annotation

;;;###autoload
(defun merita-annotate-author-positions ()
  "Walk through entries missing author positions and annotate them."
  (interactive)
  (let* ((entries (merita--query
                   "SELECT id, title, authors, author_position FROM data
                    WHERE author_position IS NULL
                    ORDER BY year DESC"))
         (count 0)
         (total (length entries)))
    (if (null entries)
        (message "All entries have author positions annotated.")
      (dolist (entry entries)
        (cl-incf count)
        (let* ((id (alist-get 'id entry))
               (title (alist-get 'title entry))
               (authors (alist-get 'authors entry)))
          (message "[%d/%d] %s\n  Authors: %s"
                   count total
                   (merita--truncate (or title "Untitled") 60)
                   (or authors "Unknown"))
          (let ((position (merita--read-author-position)))
            (merita--update id (list (cons 'author_position
                                          (symbol-name position)))))))
      (message "Annotated %d entries." total))))

;;; ** 6.5. ORCID Import

(defgroup merita-orcid nil
  "ORCID integration for Merita."
  :group 'merita
  :prefix "merita-orcid-")

(defcustom merita-orcid-id nil
  "ORCID iD for the researcher (format: 0000-0000-0000-0000)."
  :type '(choice (const :tag "None" nil) string)
  :group 'merita-orcid)

(defcustom merita-orcid-auto-position nil
  "If non-nil, skip author position prompts during ORCID import.
When set to a symbol from `merita-author-positions', that position
is assigned to all imported entries without prompting."
  :type '(choice (const :tag "Prompt for each entry" nil)
                 (symbol :tag "Default position"))
  :group 'merita-orcid)

(defconst merita-orcid--api-url "https://pub.orcid.org/v3.0"
  "Base URL for the ORCID public API.")

(defun merita-orcid--fetch-works (orcid-id)
  "Fetch the public works list for ORCID-ID."
  (let* ((url (format "%s/%s/works" merita-orcid--api-url orcid-id))
         (url-request-extra-headers
          '(("Accept" . "application/json")
            ("User-Agent" . "merita.el/0.2 (Emacs; academic CV tool)")))
         (url-show-status nil))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url t nil 30)
          (goto-char (point-min))
          (prog1
              (when (re-search-forward "\n\n" nil t)
                (let ((json-object-type 'alist)
                      (json-array-type 'vector)
                      (json-key-type 'symbol))
                  (condition-case nil
                      (json-read)
                    (json-error nil))))
            (kill-buffer)))
      (error
       (message "merita-orcid: failed to fetch works for %s: %s"
                orcid-id err)
       nil))))

(defun merita-orcid--extract-works (data)
  "Extract a list of works from ORCID API response DATA."
  (let ((groups (cdr (assq 'group data)))
        (results '()))
    (when (vectorp groups)
      (seq-doseq (group groups)
        (let* ((summaries (cdr (assq 'work-summary group)))
               (summary (when (vectorp summaries) (aref summaries 0))))
          (when summary
            (let* ((title-obj (cdr (assq 'title summary)))
                   (title-val (cdr (assq 'value (cdr (assq 'title title-obj)))))
                   (journal-obj (cdr (assq 'journal-title summary)))
                   (journal-val (cdr (assq 'value journal-obj)))
                   (pub-date (cdr (assq 'publication-date summary)))
                   (year-obj (cdr (assq 'year pub-date)))
                   (year-val (cdr (assq 'value year-obj)))
                   (month-obj (cdr (assq 'month pub-date)))
                   (month-val (cdr (assq 'value month-obj)))
                   (day-obj (cdr (assq 'day pub-date)))
                   (day-val (cdr (assq 'value day-obj)))
                   (type (cdr (assq 'type summary)))
                   (ext-ids (cdr (assq 'external-ids summary)))
                   (ext-id-list (cdr (assq 'external-id ext-ids)))
                   (doi nil))
              (when (vectorp ext-id-list)
                (seq-doseq (eid ext-id-list)
                  (when (equal (cdr (assq 'external-id-type eid)) "doi")
                    (setq doi (cdr (assq 'external-id-value eid))))))
              (push (list (cons 'doi doi)
                          (cons 'title title-val)
                          (cons 'journal journal-val)
                          (cons 'year (when year-val
                                        (string-to-number year-val)))
                          (cons 'month (when month-val
                                         (string-to-number month-val)))
                          (cons 'day (when day-val
                                       (string-to-number day-val)))
                          (cons 'orcid-type type))
                    results))))))
    (nreverse results)))

(defun merita-orcid--classify-type (orcid-type)
  "Map ORCID work type string to a Merita entry type symbol."
  (pcase (downcase (or orcid-type ""))
    ("journal-article" 'journal-article)
    ("book-review" 'review-article)
    ("book" 'book)
    ("book-chapter" 'book-chapter)
    ("conference-paper" 'conference-paper)
    ("conference-abstract" 'abstract)
    ("conference-poster" 'poster)
    ("dissertation" 'thesis-doctoral)
    ("dissertation-thesis" 'thesis-doctoral)
    ("edited-book" 'book)
    ("encyclopedia-entry" 'book-chapter)
    ("letter" 'letter)
    ("magazine-article" 'other)
    ("manual" 'technical-report)
    ("online-resource" 'other)
    ("patent" 'patent)
    ("preprint" 'preprint)
    ("report" 'technical-report)
    ("research-technique" 'other)
    ("review" 'review-article)
    ("supervised-student-publication" 'journal-article)
    ("test" 'other)
    ("translation" 'other)
    ("working-paper" 'preprint)
    (_ 'other)))

;;;###autoload
(defun merita-import-orcid (&optional orcid-id)
  "Import publications from ORCID into Merita.
Fetches the public works list for ORCID-ID (defaults to
`merita-orcid-id'), identifies DOIs not yet in the database,
and imports them via Crossref."
  (interactive
   (list (or merita-orcid-id
             (read-string "ORCID iD (0000-0000-0000-0000): "))))
  (unless orcid-id
    (user-error "No ORCID iD provided; set `merita-orcid-id' or pass one interactively"))
  (unless (string-match-p "^[0-9]\\{4\\}-[0-9]\\{4\\}-[0-9]\\{4\\}-[0-9X]\\{4\\}$" orcid-id)
    (user-error "Invalid ORCID iD format: %s (expected 0000-0000-0000-0000)" orcid-id))
  (merita--ensure-db)
  (message "Fetching works from ORCID %s..." orcid-id)
  (let* ((data (merita-orcid--fetch-works orcid-id))
         (works (when data (merita-orcid--extract-works data))))
    (unless works
      (user-error "No works found for ORCID %s (or the record is not public)" orcid-id))
    (let ((new-dois '())
          (existing '())
          (no-doi '()))
      (dolist (work works)
        (let ((doi (alist-get 'doi work)))
          (cond
           ((or (null doi) (string-empty-p doi))
            (push work no-doi))
           ((merita--get-by-doi doi)
            (push work existing))
           (t
            (push work new-dois)))))
      (setq new-dois (nreverse new-dois)
            existing (nreverse existing)
            no-doi (nreverse no-doi))
      (message "ORCID %s: %d works total — %d new, %d existing, %d without DOI"
               orcid-id (length works)
               (length new-dois) (length existing) (length no-doi))
      (if (null new-dois)
          (message "All works with DOIs are already in the database.")
        (merita-orcid--preview-import new-dois no-doi existing)
        (when (yes-or-no-p
               (format "Import %d new entries from ORCID? " (length new-dois)))
          (merita-orcid--do-import new-dois))))))

(defun merita-orcid--preview-import (new-dois no-doi existing)
  "Display a preview buffer of NEW-DOIS, NO-DOI, and EXISTING works."
  (let ((buf (get-buffer-create "*merita-orcid-preview*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "ORCID Import Preview\n%s\n\n"
                        (make-string 50 ?─)))
        (insert (format "Will import (%d):\n" (length new-dois)))
        (dolist (w new-dois)
          (insert (format "  + [%s] %s\n    %s — %s\n"
                          (or (alist-get 'year w) "?")
                          (or (alist-get 'title w) "Untitled")
                          (or (alist-get 'doi w) "")
                          (or (alist-get 'journal w) ""))))
        (when no-doi
          (insert (format "\nSkipping — no DOI (%d):\n" (length no-doi)))
          (dolist (w no-doi)
            (insert (format "  - [%s] %s\n"
                            (or (alist-get 'year w) "?")
                            (or (alist-get 'title w) "Untitled")))))
        (when existing
          (insert (format "\nAlready in database (%d):\n" (length existing)))
          (dolist (w existing)
            (insert (format "  = [%s] %s\n"
                            (or (alist-get 'year w) "?")
                            (or (alist-get 'title w) "Untitled")))))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(defun merita-orcid--do-import (works)
  "Import WORKS via Crossref.  Falls back to stubs for missing metadata."
  (let ((imported 0)
        (failed 0)
        (total (length works)))
    (dolist (work works)
      (let ((doi (alist-get 'doi work))
            (title (or (alist-get 'title work) "Untitled")))
        (condition-case err
            (progn
              (message "[%d/%d] Importing: %s..." (+ imported failed 1) total title)
              (let* ((data (merita--crossref-fetch doi))
                     (entry (merita--crossref-parse data)))
                (if entry
                    (progn
                      (let ((position
                             (if merita-orcid-auto-position
                                 (symbol-name merita-orcid-auto-position)
                               (progn
                                 (message "  %s (%s) — %s"
                                          (or (alist-get 'authors entry) "?")
                                          (or (alist-get 'year entry) "?")
                                          title)
                                 (symbol-name (merita--read-author-position))))))
                        (push (cons 'author_position position) entry))
                      (push (cons 'peer_reviewed 1) entry)
                      (merita--insert entry)
                      (cl-incf imported))
                  (let* ((orcid-type (alist-get 'orcid-type work))
                         (stub (list (cons 'type
                                           (symbol-name
                                            (merita-orcid--classify-type orcid-type)))
                                     (cons 'title title)
                                     (cons 'doi doi)
                                     (cons 'journal (alist-get 'journal work))
                                     (cons 'year (alist-get 'year work))
                                     (cons 'status "published"))))
                    (let ((position
                           (if merita-orcid-auto-position
                               (symbol-name merita-orcid-auto-position)
                             (progn
                               (message "  [stub — no Crossref data] %s" title)
                               (symbol-name (merita--read-author-position))))))
                      (push (cons 'author_position position) stub))
                    (merita--insert stub)
                    (cl-incf imported)
                    (message "  (imported as stub — Crossref had no data for %s)" doi)))))
          (error
           (cl-incf failed)
           (message "  Error importing %s: %s" doi (error-message-string err))))))
    (message "ORCID import complete: %d imported, %d failed" imported failed)))

;;;###autoload
(defun merita-orcid-diff ()
  "Show works on ORCID that are not yet in the database."
  (interactive)
  (let ((orcid-id (or merita-orcid-id
                      (read-string "ORCID iD: "))))
    (unless orcid-id
      (user-error "No ORCID iD provided"))
    (merita--ensure-db)
    (message "Fetching works from ORCID %s..." orcid-id)
    (let* ((data (merita-orcid--fetch-works orcid-id))
           (works (when data (merita-orcid--extract-works data)))
           (missing '()))
      (unless works
        (user-error "No works found for ORCID %s" orcid-id))
      (dolist (w works)
        (let ((doi (alist-get 'doi w)))
          (when (or (null doi) (string-empty-p doi)
                    (not (merita--get-by-doi doi)))
            (push w missing))))
      (if (null missing)
          (message "Database is in sync with ORCID — no missing works.")
        (merita-orcid--preview-import
         (cl-remove-if (lambda (w)
                         (let ((d (alist-get 'doi w)))
                           (or (null d) (string-empty-p d))))
                       missing)
         (cl-remove-if-not (lambda (w)
                             (let ((d (alist-get 'doi w)))
                               (or (null d) (string-empty-p d))))
                           missing)
         nil)
        (message "%d works not in database." (length missing))))))

;;; * 7. Export

;;; ** 7.1. LaTeX Export

;;; *** 7.1.1. LaTeX Customization

(defcustom merita-latex-section-order
  '(journal-article review-article book book-chapter
    editorial letter case-report conference-paper
    podium invited-talk keynote poster abstract
    thesis-doctoral thesis-masters grant award
    software dataset)
  "Order in which entry types appear in LaTeX CV export."
  :type '(repeat symbol)
  :group 'merita)

(defcustom merita-latex-section-titles
  '((journal-article  . "Peer-Reviewed Journal Articles")
    (review-article   . "Review Articles")
    (book             . "Books")
    (book-chapter     . "Book Chapters")
    (editorial        . "Editorials \\& Commentaries")
    (letter           . "Letters to the Editor")
    (case-report      . "Case Reports")
    (conference-paper . "Conference Papers")
    (podium           . "Podium Presentations")
    (invited-talk     . "Invited Lectures")
    (keynote          . "Keynote Addresses")
    (poster           . "Poster Presentations")
    (abstract         . "Published Abstracts")
    (thesis-doctoral  . "Doctoral Dissertation")
    (thesis-masters   . "Master's Thesis")
    (preprint         . "Preprints")
    (technical-report . "Technical Reports")
    (grant            . "Funded Grants")
    (award            . "Awards \\& Honors")
    (patent           . "Patents")
    (dataset          . "Published Datasets")
    (software         . "Software")
    (workshop         . "Workshops")
    (media            . "Media")
    (other            . "Other"))
  "Alist mapping entry types to LaTeX section titles."
  :type '(alist :key-type symbol :value-type string)
  :group 'merita)

(defcustom merita-latex-preamble
  "%% Generated by Merita (https://codeberg.org/phmcc/merita)
%% Date: %s
%%
%% Include this file in a CV with: \\input{publications.tex}
%% Requires: \\usepackage{enumitem} for list formatting

"
  "Preamble template for LaTeX export.  %s is replaced with timestamp."
  :type 'string
  :group 'merita)

(defcustom merita-latex-enumerate-style t
  "If non-nil, use numbered enumerate environment."
  :type 'boolean
  :group 'merita)

(defcustom merita-latex-reverse-numbering t
  "If non-nil, number publications in reverse chronological order."
  :type 'boolean
  :group 'merita)

;;; *** 7.1.2. Unified Text Helpers
;;
;; All citation formatters use these context-aware helpers.
;; `merita--latex-context' is bound to t by LaTeX export functions;
;; when nil, output is plain text.

(defvar merita--latex-context nil
  "Non-nil when formatters are producing LaTeX output.
Set dynamically by export functions.  Controls whether helpers
emit LaTeX markup or plain text.")

(defun merita--emphasize-name (author-string)
  "Emphasize the user's name in AUTHOR-STRING.
In LaTeX context, wraps in \\textbf{}.  In plain text, uses ALL CAPS.
Matches longest names first; skips variants that are substrings
of already-matched longer names."
  (if (and merita-latex-bold-author merita-user-name)
      (let* ((all-names (cons merita-user-name
                              (or merita-user-name-variants '())))
             (sorted (sort (copy-sequence all-names)
                           (lambda (a b) (> (length a) (length b)))))
             (matched '())
             (result author-string))
        (dolist (name sorted)
          (unless (cl-some (lambda (m) (string-match-p (regexp-quote name) m))
                           matched)
            (when (string-match-p (regexp-quote name) result)
              (setq result (replace-regexp-in-string
                            (regexp-quote name)
                            (if merita--latex-context
                                (format "\\textbf{%s}" name)
                              (upcase name))
                            result t t))
              (push name matched))))
        result)
    author-string))

(defun merita--escape (str)
  "Escape STR for the current output context.
In LaTeX context, escapes special characters.  In plain text, identity."
  (if (and merita--latex-context str)
      (merita--latex-escape str)
    (or str "")))

(defun merita--emph (str)
  "Italicize STR (e.g., journal name) for the current output context.
In LaTeX context, wraps in \\emph{}.  In plain text, returns as-is."
  (if (and merita--latex-context str)
      (format "\\emph{%s}" (merita--latex-escape str))
    (or str "")))

(defun merita--mono (str)
  "Monospace STR (e.g., package name) for the current output context.
In LaTeX context, wraps in \\texttt{}.  In plain text, returns as-is."
  (if (and merita--latex-context str)
      (format "\\texttt{%s}" (merita--latex-escape str))
    (or str "")))

(defun merita--href (url)
  "Format URL for the current output context.
In LaTeX context, wraps in \\url{}.  In plain text, returns as-is."
  (if (and merita--latex-context url)
      (format "\\url{%s}" url)
    (or url "")))

(defun merita--fmt-doi (doi)
  "Format DOI for the current output context."
  (when doi
    (if merita--latex-context
        (format "doi: %s" doi)
      (format "https://doi.org/%s" doi))))

(defun merita--latex-escape (str)
  "Escape special LaTeX characters in STR."
  (when str
    (let ((result str))
      (dolist (pair '(("\\\\" . "\\textbackslash{}")
                      ("&" . "\\&") ("%" . "\\%") ("\\$" . "\\$")
                      ("#" . "\\#") ("_" . "\\_") ("{" . "\\{")
                      ("}" . "\\}") ("~" . "\\textasciitilde{}")
                      ("\\^" . "\\textasciicircum{}")))
        (setq result (replace-regexp-in-string (car pair) (cdr pair) result t t)))
      result)))

(defun merita-format-vancouver (entry)
  "Format ENTRY in Vancouver (medical) citation style.
Uses unified helpers; works in both LaTeX and plain text contexts."
  (let* ((authors (alist-get 'authors entry))
         (title (alist-get 'title entry))
         (journal (alist-get 'journal entry))
         (journal-abbrev (alist-get 'journal_abbrev entry))
         (year (alist-get 'year entry))
         (volume (alist-get 'volume entry))
         (issue (alist-get 'issue entry))
         (pages (alist-get 'pages entry))
         (doi (alist-get 'doi entry))
         (book-title (alist-get 'book_title entry))
         (publisher (alist-get 'publisher entry))
         (editors (alist-get 'editors entry))
         (conference (alist-get 'conference entry))
         (location (alist-get 'conference_location entry))
         (conf-date (alist-get 'conference_date entry))
         (school (alist-get 'school entry))
         (entry-type (intern (or (alist-get 'type entry) "other")))
         (parts '()))
    (when authors
      (push (format "%s." (merita--emphasize-name (merita--escape authors))) parts))
    (when title
      (push (format "%s." (merita--escape title)) parts))
    (pcase entry-type
      ((or 'journal-article 'review-article 'editorial 'letter 'case-report 'abstract)
       (when (or journal-abbrev journal)
         (push (format "%s." (merita--emph (or journal-abbrev journal))) parts))
       (when year
         (let ((vol-str (cond
                         ((and volume issue) (format "%s;%s(%s)" year volume issue))
                         (volume (format "%s;%s" year volume))
                         (t (format "%s" year)))))
           (push (if pages (format "%s:%s." vol-str pages) (format "%s." vol-str)) parts))))
      ('book-chapter
       (when book-title
         (push (format "In: %s%s"
                       (if editors
                           (format "%s, ed%s. " (merita--escape editors)
                                   (if (string-match-p "," (or editors "")) "s" ""))
                         "")
                       (format "%s." (merita--emph book-title)))
               parts))
       (when publisher (push (format "%s;" (merita--escape publisher)) parts))
       (when year (push (format "%s." year) parts))
       (when pages (push (format "p. %s." pages) parts)))
      ('book
       (when publisher (push (format "%s;" (merita--escape publisher)) parts))
       (when year (push (format "%s." year) parts)))
      ('conference-paper
       (when book-title (push (format "In: %s." (merita--emph book-title)) parts))
       (when publisher (push (format "%s;" (merita--escape publisher)) parts))
       (when year (push (format "%s." year) parts))
       (when pages (push (format "p. %s." pages) parts)))
      ((or 'podium 'poster 'invited-talk 'keynote 'workshop)
       (when conference
         (push (format "Presented at %s." (merita--escape conference)) parts))
       (when location (push (format "%s." (merita--escape location)) parts))
       (when (or conf-date year) (push (format "%s." (or conf-date year)) parts)))
      ((or 'thesis-doctoral 'thesis-masters)
       (push (format "[%s]." (if (eq entry-type 'thesis-doctoral)
                                 "Doctoral dissertation" "Master's thesis")) parts)
       (when school (push (format "%s;" (merita--escape school)) parts))
       (when year (push (format "%s." year) parts)))
      ('grant
       (let ((role (alist-get 'grant_role entry))
             (amount (alist-get 'grant_amount entry))
             (period (alist-get 'grant_period entry))
             (funding (alist-get 'funding entry))
             (org (alist-get 'organization entry)))
         (when role (push (format "Role: %s." role) parts))
         (when org (push (format "%s." (merita--escape org)) parts))
         (when funding (push (format "Grant %s." (merita--escape funding)) parts))
         (when amount (push (format "$%s." (merita--format-currency amount)) parts))
         (when period (push (format "%s." period) parts))))
      ('award
       (let ((body (alist-get 'award_body entry))
             (org (alist-get 'organization entry)))
         (when (or body org)
           (push (format "%s." (merita--escape (or body org))) parts))
         (when year (push (format "%s." year) parts))))
      ('software
       (let ((repo (or (alist-get 'repo_url entry) (alist-get 'url entry)))
             (lang (alist-get 'repo_language entry))
             (registry (alist-get 'pkg_registry entry))
             (pkg-name (alist-get 'pkg_name entry)))
         (when lang (push (format "[%s]." (merita--escape lang)) parts))
         (when (and registry pkg-name)
           (push (format "%s package %s."
                         (upcase registry) (merita--mono pkg-name))
                 parts))
         (when year (push (format "%s." year) parts))
         (when repo (push (merita--href repo) parts))
         (let ((xrefs (merita--get-related (alist-get 'id entry) "software-paper")))
           (when xrefs
             (dolist (rel xrefs)
               (push (format "See also: %s%s."
                             (merita--escape (or (alist-get 'title rel) ""))
                             (if (alist-get 'year rel)
                                 (format " (%s)" (alist-get 'year rel)) ""))
                     parts))))))
      ('dataset
       (when publisher (push (format "%s;" (merita--escape publisher)) parts))
       (when year (push (format "%s." year) parts))
       (let ((u (or (alist-get 'url entry) doi)))
         (when u (push (merita--href u) parts))))
      (_ (when year (push (format "%s." year) parts))))
    (let ((awards (alist-get 'awards entry)))
      (when (and awards (not (string-empty-p awards)))
        (push (format "%s." (merita--emph (merita--escape awards))) parts)))
    ;; Status indicator
    (let ((entry-status (alist-get 'status entry)))
      (when (and entry-status (not (member entry-status '("published" nil))))
        (push (format "[%s]" (capitalize (replace-regexp-in-string "-" " " entry-status)))
              parts)))
    ;; DOI
    (when doi (push (merita--fmt-doi doi) parts))
    (string-join (nreverse parts) " ")))

(defun merita--format-currency (amount)
  "Format AMOUNT with comma separators."
  (let* ((str (if (integerp amount) (number-to-string amount) (format "%.0f" amount)))
         (result "") (len (length str)))
    (dotimes (i len)
      (when (and (> i 0) (= (mod (- len i) 3) 0))
        (setq result (concat result ",")))
      (setq result (concat result (substring str i (1+ i)))))
    result))

;;; *** 7.1.3. LaTeX Presentation Formatter

(defun merita--format-presentation-line (entry)
  "Format a presentation line from ENTRY.
Uses unified helpers; works in both LaTeX and plain text contexts."
  (let* ((authors (alist-get 'authors entry))
         (title (alist-get 'title entry))
         (conference (alist-get 'conference entry))
         (location (alist-get 'conference_location entry))
         (date (alist-get 'conference_date entry))
         (pres-type (pcase (intern (or (alist-get 'type entry) "other"))
                      ('podium "podium") ('poster "poster")
                      ('invited-talk "invited") ('keynote "keynote") (_ nil)))
         (parts '()))
    (when authors (push (format "%s." (merita--emphasize-name (merita--escape authors))) parts))
    (when title (push (format "%s." (merita--escape title)) parts))
    (when pres-type
      (push (format "%s:" (pcase pres-type
                            ("podium" "Oral presentation") ("poster" "Poster presented")
                            ("invited" "Invited lecture") ("keynote" "Keynote address")
                            (_ "Presented")))
            parts))
    (when conference (push (format "%s." (merita--escape conference)) parts))
    (when location (push (format "%s." (merita--escape location)) parts))
    (when date (push (format "%s." date) parts))
    (string-join (nreverse parts) " ")))

(defun merita--format-award-line (entry)
  "Format an award line from ENTRY for honors lists.
Produces \"YYYY  Title, Body — Notes\" suitable for NIH biosketch
Section B or any chronological honors list.  Works in both LaTeX
and plain text contexts via the unified escape helpers."
  (let* ((year (alist-get 'year entry))
         (title (alist-get 'title entry))
         (body (alist-get 'award_body entry))
         (notes (alist-get 'notes entry))
         (parts '()))
    (when year (push (format "%s" year) parts))
    (when title
      (push (if body
                (format "%s, %s"
                        (merita--escape title)
                        (merita--escape body))
              (merita--escape title))
            parts))
    (when (and notes (not (string-empty-p notes)))
      (push (format "%s %s" (if merita--latex-context "---" "—")
                    (merita--escape notes))
            parts))
    (string-join (nreverse parts) "  ")))

;;; *** 7.1.4. LaTeX File Generation

;;;###autoload
(defun merita-export-latex (&optional filename)
  "Export publications to a LaTeX file."
  (interactive
   (list (read-file-name "Export to: " merita-default-export-directory
                         nil nil "publications.tex")))
  (merita--ensure-db)
  (let* ((merita--latex-context t)
         (filename (or filename (expand-file-name "publications.tex"
                                                  merita-default-export-directory)))
         (all-entries (merita--query
                       (format "SELECT * FROM data WHERE %s
                        ORDER BY year DESC, month DESC, title ASC"
                               merita--active-status-sql)))
         (grouped (merita--group-by-type all-entries))
         (ordered-types (merita--ordered-types grouped)))
    (with-temp-file filename
      (insert (format merita-latex-preamble (format-time-string "%Y-%m-%d %H:%M")))
      (dolist (type ordered-types)
        (let* ((entries (alist-get type grouped))
               (section-title (or (alist-get type merita-latex-section-titles)
                                  (symbol-name type)))
               (total-count (length entries)))
          (when (> total-count 0)
            (insert (format "\\subsection*{%s (%d)}\n" section-title total-count))
            (if merita-latex-enumerate-style
                (if merita-latex-reverse-numbering
                    (insert (format "\\begin{enumerate}[start=%d,label={\\arabic*.}]\n"
                                    total-count))
                  (insert "\\begin{enumerate}\n"))
              (insert "\\begin{itemize}\n"))
            (let ((sorted (if merita-latex-reverse-numbering (reverse entries) entries)))
              (dolist (entry sorted)
                (insert (format "  \\item %s\n\n"
                                (cond
                                 ((eq type 'award)
                                  (merita--format-award-line entry))
                                 ((memq type '(podium poster invited-talk keynote workshop))
                                  (merita--format-presentation-line entry))
                                 (t (funcall merita-latex-formatter entry)))))))
            (insert (if merita-latex-enumerate-style
                        "\\end{enumerate}\n\n" "\\end{itemize}\n\n"))))))
    (message "Exported %d entries to %s" (length all-entries) filename)))

(defun merita--group-by-type (entries)
  "Group ENTRIES by type.  Returns alist."
  (let ((groups nil))
    (dolist (entry entries)
      (let* ((type (intern (or (alist-get 'type entry) "other")))
             (existing (assq type groups)))
        (if existing
            (setcdr existing (append (cdr existing) (list entry)))
          (push (cons type (list entry)) groups))))
    groups))

(defun merita--ordered-types (grouped)
  "Return entry types from GROUPED in display order."
  (let ((present-types (mapcar #'car grouped))
        (result nil))
    (dolist (type merita-latex-section-order)
      (when (memq type present-types) (push type result)))
    (dolist (type present-types)
      (unless (memq type result) (push type result)))
    (nreverse result)))

;;; ** 7.2. BibTeX Export

;;;###autoload
(defun merita-export-bibtex (&optional filename)
  "Export all entries to a BibTeX file."
  (interactive
   (list (read-file-name "Export to: " merita-default-export-directory
                         nil nil "merita.bib")))
  (let* ((filename (or filename (expand-file-name "merita.bib"
                                                  merita-default-export-directory)))
         (entries (merita--query "SELECT * FROM data ORDER BY year DESC, title ASC"))
         (count 0))
    (with-temp-file filename
      (insert (format "%% Generated by Merita — %s\n" (format-time-string "%Y-%m-%d %H:%M")))
      (insert "%% https://codeberg.org/phmcc/merita\n\n")
      (dolist (entry entries)
        (insert (merita--entry-to-bibtex entry))
        (insert "\n")
        (cl-incf count)))
    (message "Exported %d entries to %s" count filename)))

(defun merita--generate-bibtex-key (entry)
  "Generate a BibTeX key for ENTRY."
  (or (alist-get 'bibtex_key entry)
      (let* ((authors (or (alist-get 'authors entry) "Unknown"))
             (year (or (alist-get 'year entry) "XXXX"))
             (surname (car (split-string authors "[, ]+" t)))
             (clean (replace-regexp-in-string "[^a-zA-Z]" "" surname)))
        (format "%s%s" clean year))))

(defun merita--entry-to-bibtex (entry)
  "Convert ENTRY alist to a BibTeX string."
  (let* ((entry-type (intern (or (alist-get 'type entry) "other")))
         (bibtex-type (or (alist-get entry-type merita-entry-type-bibtex-map) "misc"))
         (key (merita--generate-bibtex-key entry))
         (fields '()))
    (cl-flet ((add-field (bib-name merita-name)
                (let ((val (alist-get merita-name entry)))
                  (when (and val (not (equal val "")) (not (eq val 0)))
                    (push (cons bib-name (if (numberp val) (number-to-string val)
                                           (format "{%s}" val)))
                          fields)))))
      (add-field "author" 'authors) (add-field "title" 'title)
      (add-field "journal" 'journal) (add-field "year" 'year)
      (add-field "month" 'month) (add-field "volume" 'volume)
      (add-field "number" 'issue) (add-field "pages" 'pages)
      (add-field "doi" 'doi) (add-field "url" 'url)
      (add-field "publisher" 'publisher) (add-field "edition" 'edition)
      (add-field "editor" 'editors) (add-field "booktitle" 'book_title)
      (add-field "isbn" 'isbn) (add-field "series" 'series)
      (add-field "school" 'school) (add-field "institution" 'organization)
      (add-field "abstract" 'abstract) (add-field "keywords" 'keywords)
      (add-field "note" 'notes)
      (add-field "merita-type" 'type)
      (add-field "merita-author-position" 'author_position)
      (add-field "merita-citations" 'citations)
      (add-field "merita-impact-factor" 'impact_factor)
      (add-field "merita-repo-url" 'repo_url)
      (add-field "merita-pkg-registry" 'pkg_registry)
      (add-field "merita-pkg-name" 'pkg_name)
      (add-field "merita-language" 'repo_language))
    (format "@%s{%s,\n%s\n}\n" bibtex-type key
            (string-join (mapcar (lambda (f) (format "  %s = %s" (car f) (cdr f)))
                                 (nreverse fields))
                         ",\n"))))

;;; ** 7.3. TSV Export

;;;###autoload
(defun merita-export-tsv (&optional filename)
  "Export all entries to a TSV file."
  (interactive
   (list (read-file-name "Export to: " merita-default-export-directory
                         nil nil "merita.tsv")))
  (let* ((db (merita--ensure-db))
         (filename (or filename (expand-file-name "merita.tsv"
                                                  merita-default-export-directory)))
         (rows (sqlite-select db "SELECT * FROM data ORDER BY id" nil 'full))
         (columns (car rows))
         (data (cdr rows)))
    (with-temp-file filename
      (insert (string-join columns "\t") "\n")
      (dolist (row data)
        (insert (string-join
                 (mapcar (lambda (val)
                           (cond ((null val) "")
                                 ((numberp val) (number-to-string val))
                                 (t (replace-regexp-in-string
                                     "[\t\n\r]" " " (format "%s" val)))))
                         row) "\t") "\n")))
    (message "Exported %d entries to %s" (length data) filename)))

;;; ** 7.4. Citation Formatters

(defun merita--entry-fields (entry)
  "Extract common fields from ENTRY as a plist for formatters."
  (list :authors    (alist-get 'authors entry)
        :title      (alist-get 'title entry)
        :journal    (alist-get 'journal entry)
        :journal-ab (alist-get 'journal_abbrev entry)
        :year       (alist-get 'year entry)
        :volume     (alist-get 'volume entry)
        :issue      (alist-get 'issue entry)
        :pages      (alist-get 'pages entry)
        :doi        (alist-get 'doi entry)
        :book-title (alist-get 'book_title entry)
        :publisher  (alist-get 'publisher entry)
        :editors    (alist-get 'editors entry)
        :school     (alist-get 'school entry)
        :type       (intern (or (alist-get 'type entry) "other"))
        :status     (alist-get 'status entry)
        :url        (alist-get 'url entry)
        :repo-url   (alist-get 'repo_url entry)
        :pkg-reg    (alist-get 'pkg_registry entry)
        :pkg-name   (alist-get 'pkg_name entry)
        :language   (alist-get 'repo_language entry)))

;;; *** 7.4.1. APA 7th Edition

(defun merita-format-apa (entry)
  "Format ENTRY in APA 7th edition style."
  (let* ((f (merita--entry-fields entry))
         (authors (plist-get f :authors))
         (title (plist-get f :title))
         (journal (or (plist-get f :journal) ""))
         (year (plist-get f :year))
         (volume (plist-get f :volume))
         (issue (plist-get f :issue))
         (pages (plist-get f :pages))
         (doi (plist-get f :doi))
         (book-title (plist-get f :book-title))
         (publisher (plist-get f :publisher))
         (editors (plist-get f :editors))
         (school (plist-get f :school))
         (type (plist-get f :type))
         (status (plist-get f :status))
         (parts '()))
    (when authors
      (push (merita--emphasize-name (merita--escape authors)) parts))
    (push (format "(%s)." (or year "n.d.")) parts)
    (when title (push (format "%s." (merita--escape title)) parts))
    (pcase type
      ((or 'journal-article 'review-article 'editorial 'letter
           'case-report 'abstract)
       (let ((vol-str (cond
                       ((and volume issue) (format "%s(%s)" volume issue))
                       (volume volume)
                       (t nil))))
         (when (not (string-empty-p journal))
           (push (format "%s%s%s."
                         (merita--emph journal)
                         (if vol-str (format ", %s" vol-str) "")
                         (if pages (format ", %s" pages) ""))
                 parts))))
      ('book-chapter
       (when editors (push (format "In %s (Eds.)," (merita--escape editors)) parts))
       (when book-title (push (merita--emph book-title) parts))
       (when pages (push (format "(pp. %s)." pages) parts))
       (when publisher (push (format "%s." (merita--escape publisher)) parts)))
      ('book
       (when publisher (push (format "%s." (merita--escape publisher)) parts)))
      ('conference-paper
       (when book-title (push (format "In %s." (merita--emph book-title)) parts))
       (when publisher (push (format "%s." (merita--escape publisher)) parts)))
      ((or 'thesis-doctoral 'thesis-masters)
       (push (format "[%s]."
                      (if (eq type 'thesis-doctoral)
                          "Doctoral dissertation" "Master's thesis"))
             parts)
       (when school (push (format "%s." (merita--escape school)) parts)))
      ((or 'software 'dataset)
       (let ((lang (plist-get f :language))
             (registry (plist-get f :pkg-reg))
             (pkg (plist-get f :pkg-name))
             (url (or (plist-get f :repo-url) (plist-get f :url))))
         (when lang (push (format "[%s]." (merita--escape lang)) parts))
         (when (and registry pkg)
           (push (format "%s package %s." (upcase registry) (merita--mono pkg)) parts))
         (when url (push (merita--href url) parts))))
      (_ nil))
    (when (and status (not (member status '("published" nil))))
      (push (format "[%s]" (capitalize status)) parts))
    (when doi (push (merita--fmt-doi doi) parts))
    (string-join (nreverse parts) " ")))

;;; *** 7.4.2. IEEE

(defun merita-format-ieee (entry)
  "Format ENTRY in IEEE citation style."
  (let* ((f (merita--entry-fields entry))
         (authors (plist-get f :authors))
         (title (plist-get f :title))
         (journal (or (plist-get f :journal) ""))
         (year (plist-get f :year))
         (volume (plist-get f :volume))
         (issue (plist-get f :issue))
         (pages (plist-get f :pages))
         (doi (plist-get f :doi))
         (book-title (plist-get f :book-title))
         (publisher (plist-get f :publisher))
         (school (plist-get f :school))
         (type (plist-get f :type))
         (parts '()))
    (when authors
      (push (format "%s," (merita--emphasize-name (merita--escape authors))) parts))
    (when title (push (format "\"%s,\"" (merita--escape title)) parts))
    (pcase type
      ((or 'journal-article 'review-article 'editorial 'letter
           'case-report 'abstract)
       (when (not (string-empty-p journal))
         (push (format "%s," (merita--emph journal)) parts))
       (when volume (push (format "vol. %s," volume) parts))
       (when issue (push (format "no. %s," issue) parts))
       (when pages (push (format "pp. %s," pages) parts))
       (when year (push (format "%s." year) parts)))
      ('book-chapter
       (push "in" parts)
       (when book-title (push (format "%s," (merita--emph book-title)) parts))
       (when publisher (push (format "%s," (merita--escape publisher)) parts))
       (when year (push (format "%s," year) parts))
       (when pages (push (format "pp. %s." pages) parts)))
      ('book
       (when publisher (push (format "%s," (merita--escape publisher)) parts))
       (when year (push (format "%s." year) parts)))
      ('conference-paper
       (push "in" parts)
       (when book-title (push (format "%s," (merita--emph book-title)) parts))
       (when year (push (format "%s," year) parts))
       (when pages (push (format "pp. %s." pages) parts)))
      ((or 'thesis-doctoral 'thesis-masters)
       (push (format "\"%s,\"" (if (eq type 'thesis-doctoral)
                                   "Ph.D. dissertation" "M.S. thesis"))
             parts)
       (when school (push (format "%s," (merita--escape school)) parts))
       (when year (push (format "%s." year) parts)))
      ((or 'software 'dataset)
       (let ((lang (plist-get f :language))
             (registry (plist-get f :pkg-reg))
             (pkg (plist-get f :pkg-name))
             (url (or (plist-get f :repo-url) (plist-get f :url))))
         (when lang (push (format "[%s]," (merita--escape lang)) parts))
         (when (and registry pkg)
           (push (format "%s package %s," (upcase registry) (merita--mono pkg)) parts))
         (when year (push (format "%s." year) parts))
         (when url (push (format "[Online]. Available: %s" (merita--href url)) parts))))
      (_ (when year (push (format "%s." year) parts))))
    (when doi (push (format "%s." (merita--fmt-doi doi)) parts))
    (string-join (nreverse parts) " ")))

;;; *** 7.4.3. Chicago (Author-Date, 17th Edition)

(defun merita-format-chicago (entry)
  "Format ENTRY in Chicago author-date style (17th edition)."
  (let* ((f (merita--entry-fields entry))
         (authors (plist-get f :authors))
         (title (plist-get f :title))
         (journal (or (plist-get f :journal) ""))
         (year (plist-get f :year))
         (volume (plist-get f :volume))
         (issue (plist-get f :issue))
         (pages (plist-get f :pages))
         (book-title (plist-get f :book-title))
         (publisher (plist-get f :publisher))
         (editors (plist-get f :editors))
         (school (plist-get f :school))
         (type (plist-get f :type))
         (parts '()))
    (when authors
      (push (format "%s." (merita--emphasize-name (merita--escape authors))) parts))
    (push (format "%s." (or year "n.d.")) parts)
    (when title (push (format "\"%s.\"" (merita--escape title)) parts))
    (pcase type
      ((or 'journal-article 'review-article 'editorial 'letter
           'case-report 'abstract)
       (when (not (string-empty-p journal))
         (let ((vol-str (cond
                         ((and volume issue) (format "%s (%s)" volume issue))
                         (volume (format "%s" volume))
                         (t nil))))
           (push (format "%s%s%s."
                         (merita--emph journal)
                         (if vol-str (format " %s" vol-str) "")
                         (if pages (format ": %s" pages) ""))
                 parts))))
      ('book-chapter
       (push "In" parts)
       (when book-title (push (format "%s," (merita--emph book-title)) parts))
       (when editors (push (format "edited by %s," (merita--escape editors)) parts))
       (when pages (push (format "%s." pages) parts))
       (when publisher (push (format "%s." (merita--escape publisher)) parts)))
      ('book
       (when publisher (push (format "%s." (merita--escape publisher)) parts)))
      ((or 'thesis-doctoral 'thesis-masters)
       (push (format "%s thesis,"
                      (if (eq type 'thesis-doctoral) "PhD" "Master's"))
             parts)
       (when school (push (format "%s." (merita--escape school)) parts)))
      ((or 'software 'dataset)
       (let ((lang (plist-get f :language))
             (registry (plist-get f :pkg-reg))
             (pkg (plist-get f :pkg-name))
             (url (or (plist-get f :repo-url) (plist-get f :url))))
         (when lang (push (format "[%s]." (merita--escape lang)) parts))
         (when (and registry pkg)
           (push (format "%s package %s." (upcase registry) (merita--mono pkg)) parts))
         (when url (push (format "%s." (merita--href url)) parts))))
      (_ nil))
    (string-join (nreverse parts) " ")))

;;; *** 7.4.4. NLM (National Library of Medicine)

(defun merita-format-nlm (entry)
  "Format ENTRY in NLM citation style."
  (let* ((f (merita--entry-fields entry))
         (authors (plist-get f :authors))
         (title (plist-get f :title))
         (journal (plist-get f :journal))
         (journal-ab (plist-get f :journal-ab))
         (year (plist-get f :year))
         (volume (plist-get f :volume))
         (issue (plist-get f :issue))
         (pages (plist-get f :pages))
         (doi (plist-get f :doi))
         (book-title (plist-get f :book-title))
         (publisher (plist-get f :publisher))
         (editors (plist-get f :editors))
         (school (plist-get f :school))
         (type (plist-get f :type))
         (parts '()))
    (when authors
      (push (format "%s." (merita--emphasize-name (merita--escape authors))) parts))
    (when title (push (format "%s." (merita--escape title)) parts))
    (pcase type
      ((or 'journal-article 'review-article 'editorial 'letter
           'case-report 'abstract)
       (when (or journal-ab journal)
         (push (format "%s." (merita--emph (or journal-ab journal))) parts))
       (when year
         (let ((vol-str (cond
                         ((and volume issue) (format "%s;%s(%s)" year volume issue))
                         (volume (format "%s;%s" year volume))
                         (t (format "%s" year)))))
           (push (if pages (format "%s:%s." vol-str pages)
                   (format "%s." vol-str))
                 parts))))
      ('book-chapter
       (push "In:" parts)
       (when editors (push (format "%s, editors." (merita--escape editors)) parts))
       (when book-title (push (format "%s." (merita--emph book-title)) parts))
       (when publisher (push (format "%s;" (merita--escape publisher)) parts))
       (when year (push (format "%s." year) parts))
       (when pages (push (format "p. %s." pages) parts)))
      ('book
       (when publisher (push (format "%s;" (merita--escape publisher)) parts))
       (when year (push (format "%s." year) parts)))
      ((or 'thesis-doctoral 'thesis-masters)
       (push (format "[%s]."
                      (if (eq type 'thesis-doctoral)
                          "dissertation" "master's thesis"))
             parts)
       (when school (push (format "%s;" (merita--escape school)) parts))
       (when year (push (format "%s." year) parts)))
      ((or 'software 'dataset)
       (let ((lang (plist-get f :language))
             (registry (plist-get f :pkg-reg))
             (pkg (plist-get f :pkg-name))
             (url (or (plist-get f :repo-url) (plist-get f :url))))
         (when lang (push (format "[%s]." (merita--escape lang)) parts))
         (when (and registry pkg)
           (push (format "%s package %s." (upcase registry) (merita--mono pkg)) parts))
         (when year (push (format "%s." year) parts))
         (when url (push (format "Available from: %s" (merita--href url)) parts))))
      (_ (when year (push (format "%s." year) parts))))
    (when doi (push (merita--fmt-doi doi) parts))
    (string-join (nreverse parts) " ")))

;;; *** 7.4.5. Plain Text (minimal)

(defun merita-format-plain (entry)
  "Format ENTRY as a minimal plain text citation."
  (let* ((f (merita--entry-fields entry))
         (authors (or (plist-get f :authors) ""))
         (title (or (plist-get f :title) ""))
         (journal (or (plist-get f :journal-ab)
                      (plist-get f :journal) ""))
         (year (plist-get f :year))
         (volume (plist-get f :volume))
         (issue (plist-get f :issue))
         (pages (plist-get f :pages))
         (doi (plist-get f :doi))
         (type (plist-get f :type))
         (parts '()))
    (when (not (string-empty-p authors))
      (push (format "%s." (merita--emphasize-name (merita--escape authors))) parts))
    (when (not (string-empty-p title))
      (push (format "%s." (merita--escape title)) parts))
    (if (memq type '(software dataset))
        ;; Software/dataset: language, registry, URL
        (let ((lang (plist-get f :language))
              (registry (plist-get f :pkg-reg))
              (pkg (plist-get f :pkg-name))
              (url (or (plist-get f :repo-url) (plist-get f :url))))
          (when lang (push (format "[%s]." (merita--escape lang)) parts))
          (when (and registry pkg)
            (push (format "%s: %s." (upcase registry) (merita--mono pkg)) parts))
          (when year (push (format "%s." year) parts))
          (when url (push (merita--href url) parts)))
      ;; Publications: journal, volume, pages
      (when (not (string-empty-p journal))
        (push (format "%s." (merita--emph journal)) parts))
      (when year
        (let ((vol-str (cond
                        ((and volume issue) (format "%s;%s(%s)" year volume issue))
                        (volume (format "%s;%s" year volume))
                        (t (format "%s" year)))))
          (push (if pages (format "%s:%s." vol-str pages)
                  (format "%s." vol-str))
                parts))))
    (when doi (push (merita--fmt-doi doi) parts))
    (string-join (nreverse parts) " ")))

;;; ** 7.5. Plain Text Export

(defcustom merita-text-formatter #'merita-format-apa
  "Citation formatter for plain text export."
  :type 'function
  :group 'merita)

(defcustom merita-text-numbered t
  "If non-nil, number entries in plain text export."
  :type 'boolean
  :group 'merita)

;;;###autoload
(defun merita-export-text (&optional filename formatter)
  "Export publications to a plain text file.
FORMATTER defaults to `merita-text-formatter'."
  (interactive
   (list (read-file-name "Export to: " merita-default-export-directory
                         nil nil "publications.txt")))
  (merita--ensure-db)
  (let* ((fmt (or formatter merita-text-formatter))
         (filename (or filename (expand-file-name "publications.txt"
                                                  merita-default-export-directory)))
         (all-entries (merita--query
                       (format "SELECT * FROM data WHERE %s
                        ORDER BY year DESC, month DESC, title ASC"
                               merita--active-status-sql)))
         (grouped (merita--group-by-type all-entries))
         (ordered-types (merita--ordered-types grouped))
         (global-num 0))
    (with-temp-file filename
      (insert (format "Publications — generated %s\n"
                      (format-time-string "%Y-%m-%d")))
      (insert (make-string 60 ?=) "\n\n")
      (dolist (type ordered-types)
        (let* ((entries (alist-get type grouped))
               (section-title (or (alist-get type merita-latex-section-titles)
                                  (symbol-name type))))
          (when entries
            (insert (format "%s (%d)\n" section-title (length entries)))
            (insert (make-string (+ (length section-title) 5) ?─) "\n")
            (let ((sorted (if merita-latex-reverse-numbering
                              (reverse entries) entries)))
              (dolist (entry sorted)
                (cl-incf global-num)
                (if merita-text-numbered
                    (insert (format "%3d. %s\n\n" global-num (funcall fmt entry)))
                  (insert (format "  %s\n\n" (funcall fmt entry))))))
            (insert "\n")))))
    (message "Exported %d entries to %s" global-num filename)))

;;; ** 7.6. RIS Export

(defconst merita--ris-type-map
  '((journal-article  . "JOUR")
    (review-article   . "JOUR")
    (editorial        . "JOUR")
    (letter           . "JOUR")
    (case-report      . "JOUR")
    (book             . "BOOK")
    (book-chapter     . "CHAP")
    (conference-paper . "CPAPER")
    (preprint         . "UNPB")
    (technical-report . "RPRT")
    (thesis-doctoral  . "THES")
    (thesis-masters   . "THES")
    (abstract         . "ABST")
    (patent           . "PAT")
    (grant            . "GRANT")
    (software         . "COMP")
    (dataset          . "DATA")
    (other            . "GEN"))
  "Mapping from Merita entry types to RIS type tags.")

(defun merita--entry-to-ris (entry)
  "Convert ENTRY alist to an RIS record string."
  (let* ((type (intern (or (alist-get 'type entry) "other")))
         (ris-type (or (alist-get type merita--ris-type-map) "GEN"))
         (lines '()))
    (push (format "TY  - %s" ris-type) lines)
    (let ((authors (alist-get 'authors entry)))
      (when authors
        (dolist (author (split-string authors "," t "\\s-*"))
          (push (format "AU  - %s" (string-trim author)) lines))))
    (let ((title (alist-get 'title entry)))
      (when title (push (format "TI  - %s" title) lines)))
    (let ((journal (alist-get 'journal entry)))
      (when journal (push (format "JO  - %s" journal) lines)))
    (let ((journal-ab (alist-get 'journal_abbrev entry)))
      (when journal-ab (push (format "JA  - %s" journal-ab) lines)))
    (let ((year (alist-get 'year entry)))
      (when year (push (format "PY  - %d" year) lines)))
    (let ((year (alist-get 'year entry))
          (month (alist-get 'month entry))
          (day (alist-get 'day entry)))
      (when (and year month)
        (push (format "DA  - %d/%02d/%s" year month
                      (if day (format "%02d" day) ""))
              lines)))
    (let ((volume (alist-get 'volume entry)))
      (when volume (push (format "VL  - %s" volume) lines)))
    (let ((issue (alist-get 'issue entry)))
      (when issue (push (format "IS  - %s" issue) lines)))
    (let ((pages (alist-get 'pages entry)))
      (when pages
        (let ((parts (split-string pages "[-–]")))
          (push (format "SP  - %s" (string-trim (car parts))) lines)
          (when (cadr parts)
            (push (format "EP  - %s" (string-trim (cadr parts))) lines)))))
    (let ((doi (alist-get 'doi entry)))
      (when doi (push (format "DO  - %s" doi) lines)))
    (let ((url (alist-get 'url entry)))
      (when url (push (format "UR  - %s" url) lines)))
    (let ((pmid (alist-get 'pmid entry)))
      (when pmid (push (format "AN  - %s" pmid) lines)))
    (let ((abstract (alist-get 'abstract entry)))
      (when (and abstract (not (string-empty-p abstract)))
        (push (format "AB  - %s" abstract) lines)))
    (let ((keywords (alist-get 'keywords entry)))
      (when keywords
        (dolist (kw (split-string keywords ";" t "\\s-*"))
          (push (format "KW  - %s" (string-trim kw)) lines))))
    (let ((publisher (alist-get 'publisher entry)))
      (when publisher (push (format "PB  - %s" publisher) lines)))
    (let ((isbn (alist-get 'isbn entry)))
      (when isbn (push (format "SN  - %s" isbn) lines)))
    (let ((book-title (alist-get 'book_title entry)))
      (when book-title
        (when (memq type '(book-chapter conference-paper))
          (push (format "T2  - %s" book-title) lines))))
    ;; Software-specific fields
    (let ((repo-url (alist-get 'repo_url entry)))
      (when repo-url (push (format "UR  - %s" repo-url) lines)))
    (let ((registry (alist-get 'pkg_registry entry))
          (pkg-name (alist-get 'pkg_name entry)))
      (when (and registry pkg-name)
        (push (format "N1  - %s package: %s" (upcase registry) pkg-name) lines)))
    (let ((lang (alist-get 'repo_language entry)))
      (when lang (push (format "M1  - %s" lang) lines)))
    (push "ER  - " lines)
    (concat (string-join (nreverse lines) "\n") "\n")))

;;;###autoload
(defun merita-export-ris (&optional filename)
  "Export all entries to an RIS file."
  (interactive
   (list (read-file-name "Export to: " merita-default-export-directory
                         nil nil "merita.ris")))
  (merita--ensure-db)
  (let* ((filename (or filename (expand-file-name "merita.ris"
                                                  merita-default-export-directory)))
         (entries (merita--query "SELECT * FROM data ORDER BY year DESC, title ASC"))
         (count 0))
    (with-temp-file filename
      (dolist (entry entries)
        (insert (merita--entry-to-ris entry))
        (insert "\n")
        (cl-incf count)))
    (message "Exported %d entries to %s" count filename)))

;;; ** 7.7. CSV Export

(defun merita--csv-row (fields)
  "Format FIELDS as a CSV row with proper quoting (RFC 4180)."
  (mapconcat
   (lambda (field)
     (let ((s (if (stringp field) field (format "%s" field))))
       (if (string-match-p "[,\"\n\r]" s)
           (format "\"%s\"" (replace-regexp-in-string "\"" "\"\"" s))
         s)))
   fields ","))

;;;###autoload
(defun merita-export-csv (&optional filename)
  "Export all entries to a CSV file (RFC 4180)."
  (interactive
   (list (read-file-name "Export to: " merita-default-export-directory
                         nil nil "merita.csv")))
  (let* ((db (merita--ensure-db))
         (filename (or filename (expand-file-name "merita.csv"
                                                  merita-default-export-directory)))
         (rows (sqlite-select db "SELECT * FROM data ORDER BY id" nil 'full))
         (columns (car rows))
         (data (cdr rows)))
    (with-temp-file filename
      (insert (merita--csv-row columns) "\n")
      (dolist (row data)
        (insert (merita--csv-row
                 (mapcar (lambda (val)
                           (cond ((null val) "")
                                 ((numberp val) (number-to-string val))
                                 (t (format "%s" val))))
                         row))
                "\n")))
    (message "Exported %d entries to %s" (length data) filename)))

;;; ** 7.8. Bibliography Generation

(defun merita--read-type-filter ()
  "Prompt for a type filter and return a list of type symbols, or nil for all.
Offers section aliases from `merita-org-type-aliases', individual
entry types, and <ALL> (no filter)."
  (let* ((alias-names (mapcar #'car merita-org-type-aliases))
         (type-names (mapcar #'symbol-name merita-entry-types))
         (choices (append '("<ALL>") alias-names type-names))
         (choice (completing-read "Type: " choices nil t nil nil "<ALL>")))
    (cond
     ((equal choice "<ALL>") nil)
     ((assoc choice merita-org-type-aliases)
      (cdr (assoc choice merita-org-type-aliases)))
     (t (list (intern choice))))))

(defun merita--read-year-filter (prompt default)
  "Read a year with PROMPT, defaulting to DEFAULT.
Returns the year as an integer, or nil if the user enters <ALL>."
  (let* ((choices (cons "<ALL>" (mapcar #'number-to-string
                                        (number-sequence (+ default 2) (- default 10) -1))))
         (val (completing-read (format "%s: " prompt) choices nil nil
                               nil nil (number-to-string default))))
    (if (equal val "<ALL>") nil
      (string-to-number val))))

(defun merita--read-month-filter (prompt)
  "Read a month with PROMPT.
Returns the month as an integer (1-12), or nil if the user enters <ALL>."
  (let* ((choices '("<ALL>" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11" "12"))
         (val (completing-read (format "%s: " prompt) choices nil t nil nil "<ALL>")))
    (if (equal val "<ALL>") nil
      (string-to-number val))))

(defun merita--query-bibliography (start-year start-month end-year end-month
                                    &optional types strings tags-and tags-any
                                              tags-not tags-nor)
  "Query entries matching the given filters.
START-YEAR, START-MONTH, END-YEAR, END-MONTH may each be nil to leave that
boundary unconstrained.  TYPES is an optional list of type symbols.
STRINGS is an optional list of search terms; entries must match all terms
(case-insensitive) across title, authors, journal, or keywords.
TAGS-AND, TAGS-ANY, TAGS-NOT, TAGS-NOR are tag filters with AND/OR/NAND/NOR
semantics respectively.  Returns entries in chronological order."
  (merita--query-filter-spec
   (list :start-year  start-year
         :start-month start-month
         :end-year    end-year
         :end-month   end-month
         :types       types
         :strings     strings
         :tags        tags-and
         :tags-any    tags-any
         :tags-not    tags-not
         :tags-nor    tags-nor)))

(defun merita--format-bibliography (entries heading style formatter latex-p)
  "Format ENTRIES as a bibliography, display in a buffer, and copy to kill ring.
HEADING is the title line.  FORMATTER is the citation function.
LATEX-P selects LaTeX or plain text output."
  (let ((merita--latex-context latex-p)
        (presentation-types '(podium poster invited-talk keynote workshop)))
    (cl-flet ((render (entry)
                (let ((etype (intern (or (alist-get 'type entry) "other"))))
                  (cond
                   ((eq etype 'award)
                    (merita--format-award-line entry))
                   ((memq etype presentation-types)
                    (merita--format-presentation-line entry))
                   (t (funcall formatter entry))))))
      (if (null entries)
          (progn (message "No entries found.") nil)
        (let* ((buf (get-buffer-create "*merita-bibliography*"))
               (text
                (if latex-p
                    (concat
                     (format "%% %s (%d entries)\n" heading (length entries))
                     (format "%% Style: %s\n" style)
                     (format "%% Generated: %s\n\n"
                             (format-time-string "%Y-%m-%d %H:%M"))
                     "\\begin{enumerate}\n"
                     (mapconcat
                      (lambda (entry) (format "  \\item %s\n" (render entry)))
                      entries "\n")
                     "\\end{enumerate}\n")
                  (let ((i 0))
                    (concat
                     (format "%s (%d entries)\n" heading (length entries))
                     (format "Style: %s\n\n" style)
                     (mapconcat
                      (lambda (entry)
                        (setq i (1+ i))
                        (format "%d. %s" i (render entry)))
                      entries "\n\n")
                     "\n")))))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert text)
              (goto-char (point-min))
              (special-mode)
              (visual-line-mode 1)))
          (kill-new text)
          (pop-to-buffer buf)
          (message "%d entries. Copied to kill ring." (length entries))
          text)))))

;;;###autoload
(defun merita-bibliography ()
  "Generate a bibliography with type, date-range, text, and tag filters.
Prompts for type, start/end year and month, search terms, tag filters
(AND, OR, NAND, NOR), output format, and citation style.  Use <ALL> to
leave any filter unconstrained; leave the search-term and tag prompts
blank to skip filtering.  Result displayed in *merita-bibliography* and
copied to kill ring."
  (interactive)
  (let* ((types (merita--read-type-filter))
         (cur-year (nth 5 (decode-time)))
         (sy (merita--read-year-filter "Start year" (1- cur-year)))
         (sm (when sy (merita--read-month-filter "Start month")))
         (ey (merita--read-year-filter "End year" cur-year))
         (em (when ey (merita--read-month-filter "End month")))
         ;; Text filter (comma-separated, case-insensitive)
         (raw-strings (completing-read-multiple
                       "Search terms (comma-separated, blank to skip): " nil))
         (strings (delq nil (mapcar (lambda (s)
                                      (let ((trimmed (string-trim s)))
                                        (unless (string-empty-p trimmed) trimmed)))
                                    raw-strings)))
         ;; Tag filters
         (tag-mode (merita--read-tag-mode))
         (tags (when tag-mode
                 (merita--normalize-tag-list
                  (read-string
                   (format "Tags (%s, comma-separated): "
                           (upcase (symbol-name tag-mode)))))))
         (tags-and (when (eq tag-mode 'and) tags))
         (tags-any (when (eq tag-mode 'or)  tags))
         (tags-not (when (eq tag-mode 'not) tags))
         (tags-nor (when (eq tag-mode 'nor) tags))
         ;; Style prompts
         (format-choices '("plain text" "latex"))
         (fmt (completing-read "Output format: " format-choices nil t nil nil "plain text"))
         (style-names (mapcar #'car merita--style-formatter-map))
         (default-style (symbol-name merita-default-citation-style))
         (style (completing-read
                 (format "Citation style [%s]: " default-style)
                 style-names nil t nil nil default-style))
         (formatter (cdr (assoc style merita--style-formatter-map)))
         (latex-p (equal fmt "latex"))
         ;; Query
         (entries (merita--query-bibliography sy sm ey em types strings
                                              tags-and tags-any tags-not tags-nor))
         ;; Build heading
         (type-part (if types
                        (mapconcat #'symbol-name types ", ")
                      "all"))
         (start-part (cond ((and sy sm) (format "%d-%02d" sy sm))
                           (sy (format "%d" sy))
                           (t nil)))
         (end-part (cond ((and ey em) (format "%d-%02d" ey em))
                         (ey (format "%d" ey))
                         (t nil)))
         (date-part (cond ((and start-part end-part)
                           (format ", %s to %s" start-part end-part))
                          (start-part (format ", from %s" start-part))
                          (end-part (format ", through %s" end-part))
                          (t "")))
         (filter-part (if strings
                          (format ", matching \"%s\"" (string-join strings "\", \""))
                        ""))
         (tag-part (if tags
                       (format ", tags %s {%s}"
                               (upcase (symbol-name tag-mode))
                               (string-join tags ", "))
                     ""))
         (heading (format "Bibliography: %s%s%s%s"
                          type-part date-part filter-part tag-part)))
    (merita--format-bibliography entries heading style formatter latex-p)))

(defun merita--read-tag-mode ()
  "Prompt for a tag filter mode, or nil to skip tag filtering."
  (let ((choice (completing-read "Tag filter: "
                                 '("<NONE>" "AND" "OR" "NOT (NAND)" "NOR")
                                 nil t nil nil "<NONE>")))
    (pcase choice
      ("<NONE>"     nil)
      ("AND"        'and)
      ("OR"         'or)
      ("NOT (NAND)" 'not)
      ("NOR"        'nor))))

;;; * 8. Utility

;;; ** 8.1. Counting Functions

(defun merita-count (&optional type)
  "Count entries, optionally filtered by TYPE."
  (if type
      (merita--count "type = ?" (list (symbol-name type)))
    (merita--count)))

(defun merita--pr-where ()
  "Return a SQL WHERE fragment for peer-reviewed entry types."
  (format "type IN (%s)"
          (mapconcat (lambda (typ) (format "'%s'" (symbol-name typ)))
                     merita-peer-reviewed-types ", ")))

(defun merita-count-peer-reviewed-pubs ()
  "Count entries whose type is in `merita-peer-reviewed-types'."
  (merita--count (merita--pr-where)))

(defun merita-count-first-author ()
  "Count first-author and co-first-author entries."
  (merita--count "author_position IN ('first', 'co-first')"))

(defun merita-count-first-author-pr ()
  "Count first/co-first-author peer-reviewed entries."
  (merita--count (format "%s AND author_position IN ('first', 'co-first')"
                         (merita--pr-where))))

(defun merita-count-second-author ()
  "Count second-author entries."
  (merita--count "author_position = 'second'"))

(defun merita-count-third-author ()
  "Count third-author entries."
  (merita--count "author_position = 'third'"))

(defun merita-count-senior-author ()
  "Count senior-author and co-senior-author entries."
  (merita--count "author_position IN ('senior', 'co-senior')"))

(defun merita-count-significant-author ()
  "Count entries with first, second, third, senior, or sole authorship."
  (merita--count
   "author_position IN ('first','co-first','second','third','senior',
                         'co-senior','corresponding','sole')"))

(defun merita-count-peer-reviewed ()
  "Count peer-reviewed entries."
  (merita--count "peer_reviewed = 1"))

;;; ** 8.2. Tag Filtering

;; Tags are stored as a comma-separated string in the `tags' column.
;; Matching uses boundary-safe LIKE: the column is wrapped in commas and
;; whitespace is stripped before comparison, so `cdh1' will not match
;; `cdh1-related'.  Tag names should not contain whitespace; use
;; hyphens (e.g., `peritoneal-malignancy', `til-immunotherapy').

(defun merita--normalize-tag-list (input)
  "Parse INPUT into a list of normalized (lowercased, trimmed) tag strings.
INPUT may be a string (comma-separated), a symbol, or a list of either.
Returns nil for empty or nil input."
  (cond
   ((null input) nil)
   ((listp input)
    (delete-dups
     (delq nil
           (mapcan #'merita--normalize-tag-list input))))
   ((symbolp input)
    (merita--normalize-tag-list (symbol-name input)))
   ((stringp input)
    (delete-dups
     (delq nil
           (mapcar (lambda (s)
                     (let ((trimmed (downcase (string-trim s))))
                       (unless (string-empty-p trimmed) trimmed)))
                   (split-string input "[, ]" t)))))
   (t nil)))

(defun merita--tag-match-clause ()
  "Return a SQL fragment that matches a single tag against the tags column.
The fragment expects one positional parameter (the LIKE pattern from
`merita--tag-match-param').  Whitespace in the stored tags is stripped
before comparison so users can write `cdh1, peritoneal' or `cdh1,peritoneal'."
  (concat "(',' || LOWER(REPLACE(IFNULL(tags, ''), ' ', '')) || ',') "
          "LIKE ?"))

(defun merita--tag-match-param (tag)
  "Return the LIKE pattern used by `merita--tag-match-clause' for TAG."
  (format "%%,%s,%%" (downcase (string-trim tag))))

(defun merita--tag-conditions (tags mode)
  "Build a SQL filter for TAGS combined according to MODE.
MODE is one of `and', `or', `not', or `nor':
  `and' — entry must have all of TAGS
  `or'  — entry must have at least one of TAGS
  `not' — entry must be missing at least one of TAGS (NAND)
  `nor' — entry must have none of TAGS

Returns a cons (FRAGMENT . PARAMS), or nil if TAGS is empty."
  (when tags
    (let* ((clause (merita--tag-match-clause))
           (frags (mapcar (lambda (_) clause) tags))
           (params (mapcar #'merita--tag-match-param tags)))
      (pcase mode
        ('and  (cons (concat "(" (string-join frags " AND ") ")") params))
        ('or   (cons (concat "(" (string-join frags " OR ")  ")") params))
        ('not  (cons (concat "NOT (" (string-join frags " AND ") ")") params))
        ('nor  (cons (concat "NOT (" (string-join frags " OR ")  ")") params))
        (_     (error "Unknown tag mode: %s" mode))))))

(defun merita--collect-all-tags ()
  "Return an alist (TAG . COUNT) of all tags in the database, sorted by count desc."
  (merita--ensure-db)
  (let ((freq (make-hash-table :test 'equal))
        (rows (merita--query
               (format "SELECT tags FROM data WHERE %s AND tags IS NOT NULL AND tags != ''"
                       merita--active-status-sql))))
    (dolist (row rows)
      (dolist (tag (merita--normalize-tag-list (alist-get 'tags row)))
        (puthash tag (1+ (gethash tag freq 0)) freq)))
    (let (alist)
      (maphash (lambda (k v) (push (cons k v) alist)) freq)
      (sort alist (lambda (a b)
                    (or (> (cdr a) (cdr b))
                        (and (= (cdr a) (cdr b))
                             (string< (car a) (car b)))))))))

;;;###autoload
(defun merita-list-tags ()
  "Display all tags in the database with frequency counts.
Tags are aggregated from the `tags' column across all active entries.
Selecting a tag opens `merita-browse' with a tag filter applied."
  (interactive)
  (merita--ensure-db)
  (let ((alist (merita--collect-all-tags)))
    (if (null alist)
        (message "No tags found.  Use the `tags' field on entries to add some.")
      (let* ((max-tag (apply #'max (mapcar (lambda (c) (length (car c))) alist)))
             (choices (mapcar (lambda (c)
                                (format "%-*s  (%d)"
                                        (max max-tag 12) (car c) (cdr c)))
                              alist))
             (choice (completing-read
                      (format "Tags (%d) — select to browse: " (length alist))
                      choices nil t)))
        (when (and choice (string-match "^\\([^ ]+\\)" choice))
          (merita-browse)
          (with-current-buffer "*merita-browse*"
            (setq merita-browse--filter-layers
                  (list (list :kind 'tag
                              :value (match-string 1 choice)
                              :join nil)))
            (merita-browse--refresh)))))))

;;; ** 8.3. Filter Spec and Ranking

;; A "filter spec" is a plist consumed by `merita--filter-spec-where' and
;; `merita--filter-spec-order' to build a parameterized SQL query.  It is
;; used by both the bibliography command and the org dynamic block.
;;
;; Recognized keys:
;;   :types           list of type symbols (entry types to include)
;;   :start-year      integer year (inclusive lower bound)
;;   :start-month     integer month (1-12), only meaningful with :start-year
;;   :end-year        integer year (inclusive upper bound)
;;   :end-month       integer month (1-12), only meaningful with :end-year
;;   :strings         list of free-text terms (AND across terms,
;;                    OR across title/authors/journal/keywords)
;;   :tags            list of tags (entry must have all)
;;   :tags-any        list of tags (entry must have at least one)
;;   :tags-not        list of tags (entry must be missing at least one)
;;   :tags-nor        list of tags (entry must have none)
;;   :peer-reviewed   t to require peer_reviewed = 1
;;   :author-position list of author position symbols (entry must match one)
;;   :rank            ordering: `year' (default), `citations', or `biosketch'
;;   :limit           integer maximum number of returned rows

(defconst merita--rank-orderings
  '((year      . "year DESC, month DESC, title ASC")
    (citations . "citations DESC, year DESC, title ASC")
    (biosketch . "CASE \
WHEN peer_reviewed = 1 \
 AND author_position IN ('first','co-first','senior','co-senior','sole','corresponding') \
THEN 1 \
WHEN peer_reviewed = 1 THEN 2 \
ELSE 3 END ASC, \
year DESC, citations DESC, title ASC"))
  "Mapping from `:rank' values to SQL ORDER BY fragments.
The `biosketch' tier prefers peer-reviewed first/senior-author work,
then peer-reviewed middle authorship, then everything else.")

(defun merita--filter-spec-where (spec)
  "Build a (CONDITIONS . PARAMS) pair from filter SPEC.
CONDITIONS is a list of SQL fragments to be joined with AND."
  (let ((conditions (list merita--active-status-sql))
        (params '()))
    ;; Type filter
    (when-let ((types (plist-get spec :types)))
      (let ((placeholders (mapconcat (lambda (_) "?") types ", ")))
        (setq conditions
              (append conditions (list (format "type IN (%s)" placeholders))))
        (setq params (append params (mapcar #'symbol-name types)))))
    ;; Date boundaries
    (let ((sy (plist-get spec :start-year))
          (sm (plist-get spec :start-month))
          (ey (plist-get spec :end-year))
          (em (plist-get spec :end-month)))
      (when sy
        (if sm
            (progn
              (setq conditions
                    (append conditions
                            (list "(year > ? OR (year = ? AND (month >= ? OR month IS NULL)))")))
              (setq params (append params (list sy sy sm))))
          (setq conditions (append conditions (list "year >= ?")))
          (setq params (append params (list sy)))))
      (when ey
        (if em
            (progn
              (setq conditions
                    (append conditions
                            (list "(year < ? OR (year = ? AND (month <= ? OR month IS NULL)))")))
              (setq params (append params (list ey ey em))))
          (setq conditions (append conditions (list "year <= ?")))
          (setq params (append params (list ey))))))
    ;; Free-text strings
    (dolist (term (plist-get spec :strings))
      (setq conditions
            (append conditions
                    (list (concat "(title LIKE ? COLLATE NOCASE"
                                  " OR authors LIKE ? COLLATE NOCASE"
                                  " OR journal LIKE ? COLLATE NOCASE"
                                  " OR keywords LIKE ? COLLATE NOCASE)"))))
      (let ((pat (format "%%%s%%" term)))
        (setq params (append params (list pat pat pat pat)))))
    ;; Tag filters (all four modes compose with AND)
    (dolist (cell `((,(plist-get spec :tags)     . and)
                    (,(plist-get spec :tags-any) . or)
                    (,(plist-get spec :tags-not) . not)
                    (,(plist-get spec :tags-nor) . nor)))
      (when-let ((built (merita--tag-conditions (car cell) (cdr cell))))
        (setq conditions (append conditions (list (car built))))
        (setq params (append params (cdr built)))))
    ;; Peer-reviewed flag
    (when (plist-get spec :peer-reviewed)
      (setq conditions (append conditions (list "peer_reviewed = 1"))))
    ;; Author position
    (when-let ((positions (plist-get spec :author-position)))
      (let ((placeholders (mapconcat (lambda (_) "?") positions ", ")))
        (setq conditions
              (append conditions
                      (list (format "author_position IN (%s)" placeholders))))
        (setq params (append params (mapcar #'symbol-name positions)))))
    (cons conditions params)))

(defun merita--filter-spec-order (spec)
  "Return the ORDER BY fragment for SPEC.
Defaults to chronological ascending if `:rank' is not specified, since
that's the natural CV order; the bibliography command overrides this."
  (let ((rank (plist-get spec :rank)))
    (cond
     ((null rank) "year ASC, month ASC, title ASC")
     ((symbolp rank)
      (or (alist-get rank merita--rank-orderings)
          (error "Unknown :rank value: %s" rank)))
     ((stringp rank)
      (or (alist-get (intern rank) merita--rank-orderings)
          (error "Unknown :rank value: %s" rank))))))

(defun merita--query-filter-spec (spec)
  "Run a query built from filter SPEC and return matching entries."
  (merita--ensure-db)
  (let* ((built (merita--filter-spec-where spec))
         (conditions (car built))
         (params (cdr built))
         (order (merita--filter-spec-order spec))
         (limit (plist-get spec :limit))
         (sql (format "SELECT * FROM data WHERE %s ORDER BY %s%s"
                      (string-join conditions " AND ")
                      order
                      (if limit (format " LIMIT %d" limit) ""))))
    (merita--query sql params)))

;;; * 9. Org-Mode Integration

;;; ** 9.1. Dynamic Blocks

(defconst merita-org-type-aliases
  '(("journal-articles"   . (journal-article review-article))
    ("book-chapters"      . (book-chapter))
    ("books"              . (book))
    ("other-publications" . (editorial letter case-report other media))
    ("software"           . (software dataset))
    ("presentations"      . (podium poster invited-talk keynote
                             workshop abstract conference-paper))
    ("grants"             . (grant))
    ("awards"             . (award))
    ("patents"            . (patent)))
  "Named aliases for groups of entry types.
Used in org dynamic blocks, e.g.:
  #+BEGIN: merita :section journal-articles")

(defun merita--resolve-types (params)
  "Resolve entry types from PARAMS.
Accepts either :section (alias from `merita-org-type-aliases')
or :types (comma-separated entry type names).  Returns a list of
symbols.  If neither is given, returns nil (meaning all types)."
  (let ((section (plist-get params :section))
        (types (plist-get params :types)))
    (cond
     (section
      ;; Org passes :section value as a symbol; coerce to string for lookup
      (let* ((key (if (symbolp section) (symbol-name section)
                    (format "%s" section)))
             (alias (assoc key merita-org-type-aliases)))
        (if alias (cdr alias)
          (user-error "Unknown section alias: %s" key))))
     (types
      (let ((tstr (if (symbolp types) (symbol-name types)
                    (format "%s" types))))
        (mapcar #'intern (split-string tstr ","))))
     (t nil))))

(defun merita--org-symbol-list (raw)
  "Coerce RAW (string, symbol, or list) to a list of interned symbols.
Splits on commas.  Returns nil for nil or empty input."
  (cond
   ((null raw) nil)
   ((listp raw) (mapcar (lambda (x)
                          (if (symbolp x) x
                            (intern (string-trim (format "%s" x)))))
                        raw))
   (t (let ((s (if (symbolp raw) (symbol-name raw) (format "%s" raw))))
        (mapcar #'intern (split-string s "," t " *"))))))

(defun merita--org-int (raw)
  "Coerce RAW to an integer, or nil if not coercible.
Org dynamic block parameter values arrive as integers, strings, or symbols
depending on how they were written."
  (cond
   ((integerp raw) raw)
   ((stringp raw) (string-to-number raw))
   ((symbolp raw) (string-to-number (symbol-name raw)))
   (t nil)))

(defun merita--params-to-filter-spec (params)
  "Convert org dynamic block PARAMS into a filter spec plist.
See `merita--filter-spec-where' for recognized keys."
  (list :types           (merita--resolve-types params)
        :start-year      (merita--org-int (plist-get params :since))
        :end-year        (merita--org-int (plist-get params :until))
        :tags            (merita--normalize-tag-list (plist-get params :tags))
        :tags-any        (merita--normalize-tag-list (plist-get params :tags-any))
        :tags-not        (merita--normalize-tag-list (plist-get params :tags-not))
        :tags-nor        (merita--normalize-tag-list (plist-get params :tags-nor))
        :peer-reviewed   (and (plist-member params :peer-reviewed)
                              (not (memq (plist-get params :peer-reviewed)
                                         '(nil "nil" "no"))))
        :author-position (merita--org-symbol-list (plist-get params :author-position))
        :rank            (let ((r (plist-get params :rank)))
                           (cond ((null r) nil)
                                 ((symbolp r) r)
                                 (t (intern (format "%s" r)))))
        :limit           (merita--org-int (plist-get params :limit))))

(defun merita--org-format-entries-latex (entries &optional formatter)
  "Format ENTRIES as a LaTeX enumerate block for org export.
Wraps output in #+begin_export latex / #+end_export.
Always numbers from 1 in the order ENTRIES are given.
FORMATTER overrides `merita-latex-formatter' if given."
  (let ((presentation-types '(podium poster invited-talk keynote workshop))
        (fmt (or formatter merita-latex-formatter))
        (merita--latex-context t))
    (concat
     "#+begin_export latex\n"
     "\\begin{enumerate}\n"
     (mapconcat
      (lambda (entry)
        (let ((etype (intern (or (alist-get 'type entry) "other"))))
          (format "  \\item %s\n"
                  (cond
                   ((eq etype 'award)
                    (merita--format-award-line entry))
                   ((memq etype presentation-types)
                    (merita--format-presentation-line entry))
                   (t (funcall fmt entry))))))
      entries
      "\n")
     "\\end{enumerate}\n"
     "#+end_export")))

(defun merita--org-format-entries-plain (entries &optional formatter)
  "Format ENTRIES as plain text for org.
FORMATTER is a citation formatter function (default: AMA)."
  (let ((presentation-types '(podium poster invited-talk keynote workshop))
        (fmt (or formatter merita-latex-formatter))
        (i 0))
    (mapconcat
     (lambda (entry)
       (setq i (1+ i))
       (let ((etype (intern (or (alist-get 'type entry) "other"))))
         (format "%d. %s" i
                 (cond
                  ((eq etype 'award)
                   (merita--format-award-line entry))
                  ((memq etype presentation-types)
                   (merita--format-presentation-line entry))
                  (t (funcall fmt entry))))))
     entries
     "\n\n")))

(defconst merita--style-formatter-map
  '(("vancouver" . merita-format-vancouver)
    ("apa"       . merita-format-apa)
    ("ieee"      . merita-format-ieee)
    ("chicago"   . merita-format-chicago)
    ("nlm"       . merita-format-nlm)
    ("plain"     . merita-format-plain))
  "Mapping from style names to formatter functions.")

;;;###autoload
(defun org-dblock-write:merita (params)
  "Write a Merita dynamic block.

Recognized PARAMS:
  :section ALIAS         alias from `merita-org-type-aliases'
  :types T1,T2,...       comma-separated entry type names
  :tags T1,T2,...        require all tags (AND)
  :tags-any T1,T2,...    require any tag (OR)
  :tags-not T1,T2,...    exclude entries having all tags (NAND)
  :tags-nor T1,T2,...    exclude entries having any tag (NOR)
  :since YEAR            include entries from YEAR forward
  :until YEAR            include entries through YEAR
  :peer-reviewed t       require peer_reviewed = 1
  :author-position P1,P2 require author_position to match one of P1, P2, ...
  :rank year             default; reverse-chronological for biosketches
  :rank citations        order by citations descending
  :rank biosketch        composite (peer-reviewed first/senior, then year)
  :limit N               truncate to N entries
  :format latex|plain    output format (default latex)
  :style STYLE           citation style (vancouver|apa|ieee|chicago|nlm|plain)

If no filters are given, all active entries are included in chronological
ascending order."
  (merita--ensure-db)
  (let* ((spec (merita--params-to-filter-spec params))
         ;; If no rank specified, leave default (chronological ASC) for CV-style
         ;; output. The :rank parameter overrides per dblock.
         (fmt-raw (or (plist-get params :format) "latex"))
         (fmt (if (symbolp fmt-raw) (symbol-name fmt-raw)
                (format "%s" fmt-raw)))
         ;; Resolve :style to a formatter function
         (style-raw (plist-get params :style))
         (style-str (when style-raw
                      (if (symbolp style-raw) (symbol-name style-raw)
                        (format "%s" style-raw))))
         (formatter (if style-str
                        (or (cdr (assoc style-str merita--style-formatter-map))
                            (user-error "Unknown style: %s (available: %s)"
                                        style-str
                                        (mapconcat #'car merita--style-formatter-map ", ")))
                      merita-latex-formatter))
         (entries (merita--query-filter-spec spec)))
    (if (null entries)
        (insert (if (equal fmt "latex")
                    "#+begin_export latex\n%% (no entries)\n#+end_export\n"
                  "(no entries)\n"))
      (insert
       (pcase fmt
         ("plain" (merita--org-format-entries-plain entries formatter))
         (_ (merita--org-format-entries-latex entries formatter)))
       "\n"))))

;;; ** 9.2. Biosketch Generator

(defcustom merita-biosketch-position-title nil
  "Default position title for `merita-biosketch'.
When nil, the user is prompted for a position title each time."
  :type '(choice (const :tag "Prompt every time" nil) string)
  :group 'merita)

(defcustom merita-biosketch-default-style 'nlm
  "Citation style for biosketch publication lists.
NLM is the NIH convention; other valid values include `vancouver',
`apa', `ieee', `chicago', and `plain'."
  :type 'symbol
  :group 'merita)

(defconst merita--biosketch-preamble
  "#+TITLE: NIH Biographical Sketch — %s
#+AUTHOR: %s
#+OPTIONS: toc:nil num:nil author:nil date:nil title:nil
#+LATEX_CLASS: article
#+LATEX_CLASS_OPTIONS: [11pt,letterpaper]
#+LATEX_HEADER: \\usepackage[T1]{fontenc}
#+LATEX_HEADER: \\usepackage{helvet}
#+LATEX_HEADER: \\renewcommand{\\familydefault}{\\sfdefault}
#+LATEX_HEADER: \\usepackage[margin=0.5in]{geometry}
#+LATEX_HEADER: \\usepackage{enumitem}
#+LATEX_HEADER: \\usepackage{titlesec}
#+LATEX_HEADER: \\usepackage{tabularx}
#+LATEX_HEADER: \\usepackage{booktabs}
#+LATEX_HEADER: \\usepackage{hyperref}
#+LATEX_HEADER: \\usepackage{xcolor}
#+LATEX_HEADER: \\usepackage{fancyhdr}
#+LATEX_HEADER: \\usepackage{microtype}
#+LATEX_HEADER:
#+LATEX_HEADER: \\pagestyle{fancy}
#+LATEX_HEADER: \\fancyhf{}
#+LATEX_HEADER: \\renewcommand{\\headrulewidth}{0pt}
#+LATEX_HEADER: \\rfoot{\\footnotesize Page \\thepage}
#+LATEX_HEADER: \\lfoot{\\footnotesize %s}
#+LATEX_HEADER:
#+LATEX_HEADER: \\definecolor{linkblue}{HTML}{1a5276}
#+LATEX_HEADER: \\hypersetup{colorlinks=true, urlcolor=linkblue, linkcolor=black}
#+LATEX_HEADER:
#+LATEX_HEADER: \\titleformat{\\section}{\\large\\bfseries}{\\thesection.}{0.5em}{}
#+LATEX_HEADER: \\titlespacing*{\\section}{0pt}{1.4ex plus 0.4ex minus 0.2ex}{0.8ex}
#+LATEX_HEADER: \\titleformat{\\subsection}{\\normalsize\\bfseries}{}{0pt}{}
#+LATEX_HEADER: \\titlespacing*{\\subsection}{0pt}{1.0ex plus 0.2ex}{0.4ex}
#+LATEX_HEADER:
#+LATEX_HEADER: \\setlist[enumerate]{leftmargin=2.2em, itemsep=0.4ex, parsep=0pt, topsep=0.2ex, label=\\arabic*.}
#+LATEX_HEADER:
#+LATEX_HEADER: \\newcommand{\\me}[1]{\\textbf{#1}}
#+LATEX_HEADER: \\newcommand{\\cofirst}{\\textsuperscript{\\#}}

#+BEGIN_EXPORT latex
\\noindent
\\textbf{NAME:} %s \\\\
\\textbf{POSITION TITLE:} %s
#+END_EXPORT

* EDUCATION/TRAINING

#+BEGIN_EXPORT latex
\\noindent\\begin{tabularx}{\\textwidth}{@{}p{5.5cm}p{1.6cm}p{2.0cm}X@{}}
\\toprule
INSTITUTION AND LOCATION & DEGREE & MM/YYYY & FIELD OF STUDY \\\\
\\midrule
%% TODO: Fill in education/training table.  Tracking of biographical
%% entries (positions, education, appointments) is on the merita roadmap.
 &  &  &  \\\\
\\bottomrule
\\end{tabularx}
#+END_EXPORT
"
  "Format string for the biosketch LaTeX preamble.
Substituted, in order, with: subject name (title), author name,
footer name, header NAME field, POSITION TITLE field.")

(defun merita--biosketch-tag-completing-read (prompt)
  "Prompt for a tag with completion from existing tags.
Returns the chosen tag as a string, or empty string if user enters nothing."
  (let* ((alist (merita--collect-all-tags))
         (choices (mapcar (lambda (c)
                            (format "%s (%d)" (car c) (cdr c)))
                          alist))
         (raw (completing-read prompt choices nil nil)))
    (downcase (string-trim
               (if (string-match "^\\([^ ]+\\)" raw)
                   (match-string 1 raw)
                 raw)))))

(defun merita--biosketch-prompt-groups ()
  "Prompt for 3-5 contribution-group tags and titles.
Returns a list of (TAG . TITLE) pairs."
  (let ((n (string-to-number
            (completing-read "Number of contribution groups: "
                             '("3" "4" "5") nil t nil nil "4")))
        (groups '()))
    (dotimes (i n)
      (let* ((idx (1+ i))
             (tag (merita--biosketch-tag-completing-read
                   (format "Group %d tag: " idx)))
             (title (read-string
                     (format "Group %d title: " idx))))
        (when (and tag (not (string-empty-p tag)))
          (push (cons tag title) groups))))
    (nreverse groups)))

(defun merita--biosketch-render-section-a (group-tags style)
  "Render Section A (Personal Statement) using GROUP-TAGS for citation selection.
STYLE is the citation style symbol."
  (let ((tags-csv (string-join group-tags ",")))
    (concat
     "* A. Personal Statement\n\n"
     "# TODO: Write a 3-5 sentence narrative framing your scientific trajectory,\n"
     "# how it bears on the proposed work, and why this training opportunity is\n"
     "# the right next step.  The four publications below illustrate the\n"
     "# trajectory and are pulled from the union of the contribution-group tags.\n\n"
     "** Ongoing and recently completed projects\n\n"
     "# Pulled from active grant entries (delete this subsection if none apply).\n\n"
     "#+BEGIN: merita :section grants :rank year :limit 5 :format latex :style plain\n"
     "#+END:\n\n"
     "** Citations\n\n"
     (format "#+BEGIN: merita :tags %s :tags-any t :limit 4 :rank biosketch :style %s :format latex\n"
             tags-csv (symbol-name style))
     "#+END:\n\n")))

(defun merita--biosketch-render-section-b ()
  "Render Section B (Positions, Appointments, Honors)."
  (concat
   "* B. Positions, Scientific Appointments, and Honors\n\n"
   "** Positions and Scientific Appointments\n\n"
   "# TODO: List positions in reverse chronological order.  Tracking of\n"
   "# biographical entries (positions, appointments) is on the merita roadmap;\n"
   "# fill in manually for now.\n\n"
   "#+BEGIN_EXPORT latex\n"
   "\\begin{itemize}[leftmargin=*, label={}, itemsep=0.2ex]\n"
   "  \\item YYYY--present\\quad Position, Institution, Location\n"
   "\\end{itemize}\n"
   "#+END_EXPORT\n\n"
   "** Honors\n\n"
   "#+BEGIN: merita :section awards :rank year :format latex :style plain\n"
   "#+END:\n\n"))

(defun merita--biosketch-render-contribution (idx tag title style)
  "Render one contribution-group section.
IDX is the 1-based group number, TAG the tag, TITLE the heading, STYLE the
citation style."
  (concat
   (format "** %d. %s\n\n" idx (if (string-empty-p title)
                                   (format "<title for tag %s>" tag)
                                 title))
   "# TODO: Write a paragraph describing this line of work.  Refer to the\n"
   "# publications below by item number.\n\n"
   (format "#+BEGIN: merita :tags %s :limit 4 :rank biosketch :style %s :format latex\n"
           tag (symbol-name style))
   "#+END:\n\n"))

(defun merita--biosketch-render (name position groups style)
  "Render a complete biosketch as a string.
NAME and POSITION go into the header.  GROUPS is a list of (TAG . TITLE)
pairs for Section C.  STYLE is the citation style symbol."
  (let ((group-tags (mapcar #'car groups)))
    (concat
     (format merita--biosketch-preamble
             name name name name position)
     "\n"
     (merita--biosketch-render-section-a group-tags style)
     (merita--biosketch-render-section-b)
     "* C. Contributions to Science\n\n"
     (let ((idx 0))
       (mapconcat (lambda (g)
                    (setq idx (1+ idx))
                    (merita--biosketch-render-contribution
                     idx (car g) (cdr g) style))
                  groups
                  ""))
     "* COMMENT Notes\n\n"
     "After editing the prose:\n\n"
     "1. Update all dynamic blocks: =M-x org-update-all-dblocks= (or =C-c C-x C-u=).\n"
     "2. Export to PDF: =C-c C-e l p= (or =M-x org-latex-export-to-pdf=).\n"
     "3. Verify against the funding agency's format requirements (font, margins,\n"
     "   page limit).  The default preamble uses Helvetica (~Arial), 11pt,\n"
     "   0.5\" margins, single-spaced -- consistent with NIH biosketch and most\n"
     "   institutional pilot grant RFAs.\n")))

;;;###autoload
(defun merita-biosketch (&optional outfile)
  "Generate an NIH-style biosketch scaffold as an Org file.
Prompts for position title and 3-5 thematic contribution groups (each
identified by a tag and a section title), then writes a complete Org file
to OUTFILE with LaTeX preamble, header, education table stub, Sections A
(Personal Statement), B (Positions/Honors), and C (Contributions to
Science).  Each Section C contribution and the Section A representative
publications are populated by `merita' dynamic blocks filtered on the
chosen tags.

After running this command, edit the prose where indicated by =TODO=
markers, run =M-x org-update-all-dblocks= to populate the citation lists,
then export to PDF via the org LaTeX exporter.  The default preamble
targets the NIH biosketch format conventions: Helvetica (Arial-equivalent)
11pt, 0.5\" margins, single-spaced, with a footer showing the author name
and page number on each page."
  (interactive)
  (merita--ensure-db)
  (let* ((name (or merita-user-name
                   (read-string "Author name: ")))
         (position (or merita-biosketch-position-title
                       (read-string "Position title: ")))
         (groups (merita--biosketch-prompt-groups))
         (style (intern (completing-read
                         (format "Citation style [%s]: "
                                 (symbol-name merita-biosketch-default-style))
                         (mapcar #'car merita--style-formatter-map)
                         nil t nil nil
                         (symbol-name merita-biosketch-default-style))))
         (default-name (format "biosketch-%s.org"
                               (format-time-string "%Y%m%d")))
         (file (or outfile
                   (read-file-name "Output file: "
                                   (file-name-as-directory
                                    (or merita-default-export-directory
                                        default-directory))
                                   nil nil default-name))))
    (when (null groups)
      (user-error "No contribution groups specified"))
    (with-temp-file file
      (insert (merita--biosketch-render name position groups style)))
    (find-file file)
    (message "Biosketch scaffold written to %s.  Run M-x org-update-all-dblocks to fill citation lists."
             file)))

;;; * 10. Provide

;; Provide first so that optional modules can (require 'merita) without
;; triggering a recursive load.
(provide 'merita)

(require 'merita-metrics nil t)

;;; merita.el ends here
