;;; merita-metrics.el --- Citation metrics for Merita -*- lexical-binding: t; -*-

;; Author: Paul H. McClelland
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2026 Paul H. McClelland

;; Version: 0.4.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: bib, data
;; URL: https://codeberg.org/phmcc/merita

;;; Commentary:

;; This module fetches citation metrics from OpenAlex and Semantic
;; Scholar, storing them in the Merita SQLite database alongside
;; existing publication records.
;;
;; Data sources:
;;   - OpenAlex (primary): citation counts, FWCI, OA status,
;;     citations-by-year, OpenAlex ID
;;   - Semantic Scholar (secondary): citation count, influential
;;     citation count, S2 paper ID
;;
;; The fetching is synchronous (via `url-retrieve-synchronously') and
;; typically completes in a few seconds for ~50 DOIs.  Results are
;; batched: OpenAlex supports up to 50 DOIs per request, and Semantic
;; Scholar supports up to 500 via its POST batch endpoint.
;;
;; Usage:
;;   M-x merita-metrics-update       — fetch metrics (skips recent)
;;   M-x merita-metrics-update-force — force refresh all

;;; Code:

;;; * 0. Prerequisites

(require 'url)
(require 'json)
(require 'cl-lib)

;; Merita core is a soft dependency — this module can be byte-compiled
;; independently, but requires `merita' at runtime.
(declare-function merita--ensure-db "merita")
(declare-function merita--db-file "merita")
(declare-function merita-close "merita")
(declare-function merita-init "merita")

;;; * 1. Foundation

;;; ** 1.1. Customization

(defgroup merita-metrics nil
  "Citation metrics integration for Merita."
  :group 'merita
  :prefix "merita-metrics-")

(defcustom merita-metrics-email nil
  "Email for OpenAlex polite pool (strongly recommended).
Providing an email grants access to the faster, more reliable
\"polite pool\"."
  :type '(choice (const :tag "None" nil) string)
  :group 'merita-metrics)

(defcustom merita-metrics-openalex-key nil
  "OpenAlex API key (optional; free at openalex.org/settings/api).
Without a key, the limit is 100 credits/day (testing only).
A free key provides 100,000 credits/day."
  :type '(choice (const :tag "None" nil) string)
  :group 'merita-metrics)

(defcustom merita-metrics-s2-key nil
  "Semantic Scholar API key (optional).
Without a key, the unauthenticated rate limit is shared.
Request a key at semanticscholar.org for higher limits."
  :type '(choice (const :tag "None" nil) string)
  :group 'merita-metrics)

(defcustom merita-metrics-stale-days 7
  "Number of days before metrics are considered stale and re-fetched.
Entries updated more recently than this are skipped unless
`merita-metrics-update-force' is called."
  :type 'integer
  :group 'merita-metrics)

(defcustom merita-metrics-source 'all
  "Which API source(s) to query."
  :type '(choice (const :tag "Both OpenAlex and Semantic Scholar" all)
                 (const :tag "OpenAlex only" openalex)
                 (const :tag "Semantic Scholar only" semanticscholar))
  :group 'merita-metrics)

;;; ** 1.2. Schema Migration

(defconst merita-metrics--columns
  '(;; Citation metrics (publications)
    ("cited_by_count_openalex"    "INTEGER"  nil)
    ("cited_by_count_s2"          "INTEGER"  nil)
    ("influential_citation_count" "INTEGER"  nil)
    ("fwci"                       "REAL"     nil)
    ("citations_by_year"          "TEXT"     nil)
    ("openalex_id"                "TEXT"     nil)
    ("s2_paper_id"                "TEXT"     nil)
    ("oa_status"                  "TEXT"     nil)
    ("metrics_updated"            "TEXT"     nil)
    ;; Software metrics (fetched from APIs)
    ("repo_host"                  "TEXT"     nil)   ; github, gitlab, codeberg, etc.
    ("repo_stars"                 "INTEGER"  nil)
    ("repo_forks"                 "INTEGER"  nil)
    ("repo_watchers"              "INTEGER"  nil)
    ("repo_open_issues"           "INTEGER"  nil)
    ("repo_license"               "TEXT"     nil)
    ("repo_created"               "TEXT"     nil)   ; ISO date
    ("pkg_downloads"              "INTEGER"  nil)   ; total or monthly, per registry
    ("pkg_downloads_period"       "TEXT"     nil))  ; last-month, last-week, total
  "Additional columns for citation and software metrics.
Each element is (COLUMN-NAME SQL-TYPE DEFAULT-OR-NIL).
User-editable software fields (repo_url, repo_language,
pkg_registry, pkg_name, pkg_version) are in the base schema.")

(defun merita-metrics--ensure-schema ()
  "Add any missing metrics columns to the data table."
  (let* ((db (merita--ensure-db))
         (info (sqlite-select db "PRAGMA table_info(data)"))
         (existing (mapcar (lambda (row) (nth 1 row)) info)))
    (dolist (col merita-metrics--columns)
      (let ((name (nth 0 col))
            (type (nth 1 col))
            (default (nth 2 col)))
        (unless (member name existing)
          (let ((sql (format "ALTER TABLE data ADD COLUMN %s %s%s"
                             name type
                             (if default (format " DEFAULT %s" default) ""))))
            (sqlite-execute db sql)
            (message "merita-metrics: added column %s" name)))))))

;;; * 2. Database Queries

(defun merita-metrics--dois-to-update (force)
  "Return list of (ID . DOI) pairs needing a metrics update.
If FORCE is non-nil, return all entries with DOIs."
  (let ((db (merita--ensure-db)))
    (if force
        (sqlite-select db
                       "SELECT id, lower(trim(doi)) FROM data
           WHERE doi IS NOT NULL AND doi != ''")
      (let ((cutoff (format-time-string
                     "%Y-%m-%dT%H:%M:%S"
                     (time-subtract nil (* merita-metrics-stale-days 86400)))))
        (sqlite-select db
                       "SELECT id, lower(trim(doi)) FROM data
           WHERE doi IS NOT NULL AND doi != ''
             AND (metrics_updated IS NULL OR metrics_updated < ?)"
                       (list cutoff))))))

(defun merita-metrics--write-results (doi-ids openalex-data s2-data)
  "Write fetched metrics to the database.
DOI-IDS is a list of (ID DOI) from the database.
OPENALEX-DATA and S2-DATA are hash tables mapping lowercase DOI
to alist of metrics.  Returns the number of entries updated."
  (let ((db (merita--ensure-db))
        (now (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
        (updated 0))
    (dolist (pair doi-ids)
      (let* ((entry-id (nth 0 pair))
             (doi (nth 1 pair))
             (oa (gethash doi openalex-data))
             (s2 (gethash doi s2-data)))
        (when (or oa s2)
          (let ((best-citations
                 (or (cdr (assq 'cited_by_count_openalex oa))
                     (cdr (assq 'cited_by_count_s2 s2))
                     0))
                (sets '())
                (params '()))
            ;; Unified citation count
            (push "citations = ?" sets)
            (push best-citations params)
            ;; OpenAlex fields
            (when oa
              (dolist (field '(openalex_id cited_by_count_openalex fwci oa_status))
                (let ((val (cdr (assq field oa))))
                  (when val
                    (push (format "%s = ?" field) sets)
                    (push val params))))
              (let ((cby (cdr (assq 'citations_by_year oa))))
                (when cby
                  (push "citations_by_year = ?" sets)
                  (push (json-encode cby) params))))
            ;; Semantic Scholar fields
            (when s2
              (dolist (field '(s2_paper_id cited_by_count_s2
                               influential_citation_count))
                (let ((val (cdr (assq field s2))))
                  (when val
                    (push (format "%s = ?" field) sets)
                    (push val params)))))
            ;; Timestamps
            (push "metrics_updated = ?" sets)
            (push now params)
            (push "date_modified = ?" sets)
            (push now params)
            ;; WHERE
            (push entry-id params)
            (let ((sql (format "UPDATE data SET %s WHERE id = ?"
                               (string-join (nreverse sets) ", "))))
              (sqlite-execute db sql (nreverse params))
              (cl-incf updated))))))
    updated))

(defun merita-metrics--software-to-update (force)
  "Return software entries needing metrics update.
Each row is (ID URL REPO_URL PKG_REGISTRY PKG_NAME)."
  (let ((db (merita--ensure-db)))
    (if force
        (sqlite-select db
                       "SELECT id, url, repo_url, pkg_registry, pkg_name FROM data
           WHERE type IN ('software', 'dataset')
             AND (url IS NOT NULL OR repo_url IS NOT NULL)")
      (let ((cutoff (format-time-string
                     "%Y-%m-%dT%H:%M:%S"
                     (time-subtract nil (* merita-metrics-stale-days 86400)))))
        (sqlite-select db
                       "SELECT id, url, repo_url, pkg_registry, pkg_name FROM data
           WHERE type IN ('software', 'dataset')
             AND (url IS NOT NULL OR repo_url IS NOT NULL)
             AND (metrics_updated IS NULL OR metrics_updated < ?)"
                       (list cutoff))))))

;;; * 3. API

;;; ** 3.1. HTTP Helpers

(defun merita-metrics--url-user-agent ()
  "Return a User-Agent string for API requests."
  "merita-metrics/0.1.0 (Emacs; academic CV tool)")

(defun merita-metrics--parse-response ()
  "Parse the JSON body from a `url-retrieve' response buffer.
Point should be at the start of the buffer.  Returns parsed JSON
or nil on failure.  Kills the response buffer."
  (goto-char (point-min))
  ;; Skip HTTP headers
  (when (re-search-forward "\n\n" nil t)
    (condition-case nil
        (let ((json-object-type 'alist)
              (json-array-type 'vector)
              (json-key-type 'symbol))
          (json-read))
      (json-error nil))))

(defun merita-metrics--url-get-sync (url &optional extra-headers)
  "Synchronous GET of URL, return parsed JSON or nil.
EXTRA-HEADERS is an alist of additional HTTP headers."
  (let ((url-request-extra-headers
         (append `(("User-Agent" . ,(merita-metrics--url-user-agent)))
                 extra-headers))
        (url-show-status nil))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url t nil 30)
          (prog1 (merita-metrics--parse-response)
            (kill-buffer)))
      (error
       (message "merita-metrics: GET failed for %s: %s" url err)
       nil))))

(defun merita-metrics--url-post-sync (url payload &optional extra-headers)
  "Synchronous POST of JSON PAYLOAD to URL, return parsed JSON or nil.
EXTRA-HEADERS is an alist of additional HTTP headers."
  (let ((url-request-method "POST")
        (url-request-data (encode-coding-string (json-encode payload) 'utf-8))
        (url-request-extra-headers
         (append `(("Content-Type" . "application/json")
                   ("User-Agent" . ,(merita-metrics--url-user-agent)))
                 extra-headers))
        (url-show-status nil))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url t nil 30)
          (prog1 (merita-metrics--parse-response)
            (kill-buffer)))
      (error
       (message "merita-metrics: POST failed for %s: %s" url err)
       nil))))

;;; ** 3.2. OpenAlex

(defconst merita-metrics--openalex-batch-size 50
  "Maximum DOIs per OpenAlex filter query.")

(defconst merita-metrics--openalex-fields
  "doi,id,cited_by_count,counts_by_year,fwci,open_access"
  "Fields to request from OpenAlex works endpoint.")

(defun merita-metrics--openalex-fetch-batch (dois)
  "Fetch OpenAlex data for a list of DOIS (max 50).
Returns a hash table mapping lowercase DOI -> metrics alist."
  (let* ((results (make-hash-table :test #'equal))
         (doi-urls (mapcar (lambda (d) (concat "https://doi.org/" d)) dois))
         (filter-val (string-join doi-urls "|"))
         (url (concat "https://api.openalex.org/works"
                      "?filter=doi:" (url-hexify-string filter-val)
                      "&select=" merita-metrics--openalex-fields
                      "&per_page=" (number-to-string
                                    merita-metrics--openalex-batch-size)))
         ;; Add polite pool email
         (url (if merita-metrics-email
                  (concat url "&mailto=" (url-hexify-string
                                          merita-metrics-email))
                url))
         ;; Add API key
         (url (if merita-metrics-openalex-key
                  (concat url "&api_key=" merita-metrics-openalex-key)
                url))
         (data (merita-metrics--url-get-sync url)))
    (when data
      (let ((works (cdr (assq 'results data))))
        (when (vectorp works)
          (seq-doseq (work works)
            (let* ((doi-raw (or (cdr (assq 'doi work)) ""))
                   (doi (downcase (replace-regexp-in-string
                                   "^https://doi\\.org/" "" doi-raw)))
                   ;; Parse counts_by_year
                   (cby-raw (cdr (assq 'counts_by_year work)))
                   (cby (when (vectorp cby-raw)
                          (let ((ht (list)))
                            (seq-doseq (entry cby-raw)
                              (let ((yr (cdr (assq 'year entry)))
                                    (ct (cdr (assq 'cited_by_count entry))))
                                (when yr
                                  (push (cons (number-to-string yr) ct) ht))))
                            (nreverse ht))))
                   (oa (cdr (assq 'open_access work))))
              (unless (string-empty-p doi)
                (puthash doi
                         (list (cons 'openalex_id
                                     (or (cdr (assq 'id work)) ""))
                               (cons 'cited_by_count_openalex
                                     (or (cdr (assq 'cited_by_count work)) 0))
                               (cons 'fwci
                                     (cdr (assq 'fwci work)))
                               (cons 'citations_by_year cby)
                               (cons 'oa_status
                                     (or (cdr (assq 'oa_status oa)) "")))
                         results)))))))
    results))

(defun merita-metrics--openalex-fetch-all (dois)
  "Fetch OpenAlex data for all DOIS, batching as needed.
Returns a hash table mapping lowercase DOI -> metrics alist."
  (let ((results (make-hash-table :test #'equal))
        (total (length dois))
        (i 0))
    (while (< i total)
      (let* ((end (min (+ i merita-metrics--openalex-batch-size) total))
             (batch (seq-subseq dois i end))
             (batch-results (merita-metrics--openalex-fetch-batch batch)))
        (message "merita-metrics: OpenAlex batch %d–%d of %d (got %d)"
                 (1+ i) end total (hash-table-count batch-results))
        (maphash (lambda (k v) (puthash k v results)) batch-results)
        (setq i end)
        ;; Rate-limit pause between batches
        (when (< i total) (sleep-for 0.2))))
    results))

;;; ** 3.3. Semantic Scholar

(defconst merita-metrics--s2-batch-size 500
  "Maximum paper IDs per Semantic Scholar batch request.")

(defun merita-metrics--s2-fetch-batch (dois)
  "Fetch Semantic Scholar data for a list of DOIS (max 500).
Returns a hash table mapping lowercase DOI -> metrics alist."
  (let* ((results (make-hash-table :test #'equal))
         (ids (mapcar (lambda (d) (concat "DOI:" d)) dois))
         (url (concat "https://api.semanticscholar.org/graph/v1/paper/batch"
                      "?fields=externalIds,citationCount,"
                      "influentialCitationCount,paperId"))
         (headers (when merita-metrics-s2-key
                    `(("x-api-key" . ,merita-metrics-s2-key))))
         (data (merita-metrics--url-post-sync url
                 (list (cons "ids" (vconcat ids)))
                 headers)))
    (when (vectorp data)
      (seq-doseq (paper data)
        (when paper  ; nil entries = not found
          (let* ((ext (cdr (assq 'externalIds paper)))
                 (doi (downcase (or (cdr (assq 'DOI ext)) ""))))
            (unless (string-empty-p doi)
              (puthash doi
                       (list (cons 's2_paper_id
                                   (or (cdr (assq 'paperId paper)) ""))
                             (cons 'cited_by_count_s2
                                   (or (cdr (assq 'citationCount paper)) 0))
                             (cons 'influential_citation_count
                                   (or (cdr (assq 'influentialCitationCount
                                                   paper))
                                       0)))
                       results))))))
    results))

(defun merita-metrics--s2-fetch-all (dois)
  "Fetch Semantic Scholar data for all DOIS, batching as needed.
Returns a hash table mapping lowercase DOI -> metrics alist."
  (let ((results (make-hash-table :test #'equal))
        (total (length dois))
        (i 0))
    (while (< i total)
      (let* ((end (min (+ i merita-metrics--s2-batch-size) total))
             (batch (seq-subseq dois i end))
             (batch-results (merita-metrics--s2-fetch-batch batch)))
        (message "merita-metrics: Semantic Scholar batch %d–%d of %d (got %d)"
                 (1+ i) end total (hash-table-count batch-results))
        (maphash (lambda (k v) (puthash k v results)) batch-results)
        (setq i end)
        (when (< i total) (sleep-for 0.5))))
    results))

;;; ** 3.4. GitHub

(defun merita-metrics--parse-github-url (url)
  "Extract (OWNER . REPO) from a GitHub URL, or nil."
  (when (and url (string-match
                  "github\\.com/\\([^/]+\\)/\\([^/?.#]+\\)" url))
    (cons (match-string 1 url) (match-string 2 url))))

(defun merita-metrics--github-fetch (owner repo)
  "Fetch repository metadata for OWNER/REPO from GitHub.
Returns an alist of metrics, or nil."
  (let* ((url (format "https://api.github.com/repos/%s/%s" owner repo))
         (data (merita-metrics--url-get-sync url)))
    (when data
      (list (cons 'repo_stars (cdr (assq 'stargazers_count data)))
            (cons 'repo_forks (cdr (assq 'forks_count data)))
            (cons 'repo_watchers (cdr (assq 'subscribers_count data)))
            (cons 'repo_open_issues (cdr (assq 'open_issues_count data)))
            (cons 'repo_language (cdr (assq 'language data)))
            (cons 'repo_license
                  (let ((lic (cdr (assq 'license data))))
                    (when lic (cdr (assq 'spdx_id lic)))))
            (cons 'repo_created (cdr (assq 'created_at data)))))))

;;; ** 3.5. Package Registries

(defun merita-metrics--cran-downloads (package)
  "Fetch last-month download count for CRAN PACKAGE."
  (let* ((url (format "https://cranlogs.r-pkg.org/downloads/total/last-month/%s"
                      (url-hexify-string package)))
         (data (merita-metrics--url-get-sync url)))
    (when (and data (vectorp data) (> (length data) 0))
      (let ((entry (aref data 0)))
        (cdr (assq 'downloads entry))))))

(defun merita-metrics--pypi-downloads (package)
  "Fetch recent download count for PyPI PACKAGE."
  (let* ((url (format "https://pypistats.org/api/packages/%s/recent"
                      (url-hexify-string (downcase package))))
         (data (merita-metrics--url-get-sync url)))
    (when data
      (let ((d (cdr (assq 'data data))))
        (cdr (assq 'last_month d))))))

(defun merita-metrics--fetch-software-entry (entry-id url pkg-registry pkg-name)
  "Fetch software metrics for ENTRY-ID.
URL is the repository URL.  PKG-REGISTRY and PKG-NAME identify the
package in a distribution registry (e.g., \"cran\" and \"merita\").
Returns the number of fields updated."
  (let* ((db (merita--ensure-db))
         (now (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
         (sets '())
         (params '()))
    ;; GitHub metrics
    (let ((gh (merita-metrics--parse-github-url url)))
      (when gh
        (let ((data (merita-metrics--github-fetch (car gh) (cdr gh))))
          (when data
            (push "repo_host = ?" sets) (push "github" params)
            (dolist (field '(repo_stars repo_forks repo_watchers
                                        repo_open_issues repo_language repo_license
                                        repo_created))
              (let ((val (cdr (assq field data))))
                (when val
                  (push (format "%s = ?" field) sets)
                  (push val params))))))))
    ;; Package registry downloads
    (when (and pkg-registry pkg-name)
      (let ((downloads
             (pcase (downcase pkg-registry)
               ("cran" (merita-metrics--cran-downloads pkg-name))
               ("pypi" (merita-metrics--pypi-downloads pkg-name))
               (_ nil))))
        (when downloads
          (push "pkg_downloads = ?" sets)
          (push downloads params)
          (push "pkg_downloads_period = ?" sets)
          (push "last-month" params))))
    ;; Write to database
    (when sets
      (push "metrics_updated = ?" sets)
      (push now params)
      (push entry-id params)
      (let ((sql (format "UPDATE data SET %s WHERE id = ?"
                         (string-join (nreverse sets) ", "))))
        (sqlite-execute db sql (nreverse params)))
      (length sets))))


;;; * 4. Interactive Commands

;;; ** 4.1. Update

;;;###autoload
(defun merita-metrics-update (&optional force)
  "Fetch citation metrics for publications and software entries.
Only updates entries not fetched within `merita-metrics-stale-days'.
With prefix argument FORCE, update all entries regardless."
  (interactive "P")
  (require 'merita)
  (merita--ensure-db)
  (merita-metrics--ensure-schema)
  ;; Publication metrics (DOI-based)
  (let* ((doi-ids (merita-metrics--dois-to-update force))
         (dois (mapcar #'cadr doi-ids))
         (pub-updated 0)
         (sw-updated 0))
    (when dois
      (message "Fetching metrics for %d publications%s..."
               (length dois)
               (if force " (force)" ""))
      (let ((openalex-data (make-hash-table :test #'equal))
            (s2-data (make-hash-table :test #'equal)))
        (when (memq merita-metrics-source '(all openalex))
          (setq openalex-data (merita-metrics--openalex-fetch-all dois)))
        (when (memq merita-metrics-source '(all semanticscholar))
          (setq s2-data (merita-metrics--s2-fetch-all dois)))
        (setq pub-updated (merita-metrics--write-results
                           doi-ids openalex-data s2-data))))
    ;; Software metrics (repo/registry-based)
    (let ((sw-entries (merita-metrics--software-to-update force)))
      (when sw-entries
        (message "Fetching metrics for %d software entries..." (length sw-entries))
        (dolist (row sw-entries)
          (let ((id (nth 0 row))
                (url (or (nth 1 row) (nth 2 row)))
                (registry (nth 3 row))
                (pkg-name (nth 4 row)))
            (condition-case nil
                (when (merita-metrics--fetch-software-entry
                       id url registry pkg-name)
                  (cl-incf sw-updated))
              (error nil))
            (sleep-for 0.2)))))
    ;; Summary
    (if (and (= pub-updated 0) (= sw-updated 0) (null dois))
        (message "All entries are up to date.")
      (let ((h (merita-h-index))
            (total-cit (or (caar (sqlite-select (merita--ensure-db)
                                   "SELECT SUM(citations) FROM data")) 0)))
        (message "Updated %d publications, %d software — h-index: %d, total citations: %d"
                 pub-updated sw-updated h total-cit)))))

;;;###autoload
(defun merita-metrics-update-force ()
  "Force-refresh citation metrics for all publications with DOIs."
  (interactive)
  (merita-metrics-update t))

;;; ** 4.2. Per-Entry Metrics

(defun merita-metrics-for-entry (entry-id)
  "Return an alist of metrics for ENTRY-ID, or nil."
  (let* ((db (merita--ensure-db))
         (row (car (sqlite-select db
                                  "SELECT citations, cited_by_count_openalex,
                             cited_by_count_s2, influential_citation_count,
                             fwci, oa_status, citations_by_year,
                             metrics_updated
                      FROM data WHERE id = ?"
                                  (list entry-id)))))
    (when row
      (list (cons 'citations (nth 0 row))
            (cons 'openalex (nth 1 row))
            (cons 's2 (nth 2 row))
            (cons 'influential (nth 3 row))
            (cons 'fwci (nth 4 row))
            (cons 'oa_status (nth 5 row))
            (cons 'by_year (when (nth 6 row)
                             (condition-case nil
                                 (json-read-from-string (nth 6 row))
                               (json-error nil))))
            (cons 'updated (nth 7 row))))))

(defun merita-metrics-format-entry (entry-id)
  "Return a one-line formatted string of metrics for ENTRY-ID."
  (let ((m (merita-metrics-for-entry entry-id)))
    (if m
        (let ((cit (or (cdr (assq 'citations m)) 0))
              (inf (or (cdr (assq 'influential m)) 0))
              (fwci (cdr (assq 'fwci m)))
              (oa (cdr (assq 'oa_status m)))
              (updated (cdr (assq 'updated m))))
          (format "Citations: %d (influential: %d)%s%s  [%s]"
                  cit inf
                  (if fwci (format "  FWCI: %.2f" fwci) "")
                  (if (and oa (not (string-empty-p oa)))
                      (format "  OA: %s" oa)
                    "")
                  (or updated "never")))
      "No metrics data")))

;;; * 5. Keybinding Integration

(with-eval-after-load 'merita
  (when (boundp 'merita-command-map)
    (define-key merita-command-map (kbd "M") #'merita-metrics-update)))

;;; * 6. Provide

(provide 'merita-metrics)
;;; merita-metrics.el ends here
