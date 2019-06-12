;;; org-tj.el --- Org Mode TaskJuggler 3  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Nate Dwarshuis

;; Author: Nate Dwarshuis <natedwarshuis@gmail.com>
;; Keywords: outlines
;; Homepage: https://github.com/ndwarshuis/org-tj3
;; Package-Requires: ((emacs "25") (dash "2.15"))
;; Version: 0.0.1

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

;; Controls and convenience functions for Taskjuggler 3 and Org Mode
;; This is a high level package that currently assumes workflow is
;; mostly done through org-mode files that are loaded into the
;; taskjuggler daemon and viewed in the web browser.

;;; Code:

;; code goes here

(require 's)
(require 'f)
(require 'dash)
(require 'org-element)
(require 'subr-x)
(require 'ox)
(eval-when-compile (require 'cl-lib))

;;; constants

(defconst org-tj--report-attributes-rich-text
  '(caption center epilog footer header headline left prolog right))

(defconst org-tj--property-attributes
  `((report
     ;; :TJ3_accountreport (not fully tested)
     (accountroot . :TJ3_ACCOUNTROOT)
     ;; . :TJ3_auxdir (not fully tested)
     (balance . :TJ3_BALANCE)
     (caption . ,(-partial #'org-tj--get-rich-text "caption"))
     (center . ,(-partial #'org-tj--get-rich-text "center"))
     (columns . :TJ3_COLUMNS)
     (currencyformat . :TJ3_CURRENCYFORMAT)
     (end . ,(-partial #'org-tj--get-rich-text "end"))
     (epilog . ,(-partial #'org-tj--get-rich-text "epilog"))
     ;; . :TJ3_EXPORT (not implemented)
     (flags . :TJ3_FLAGS)
     (footer . ,(-partial #'org-tj--get-rich-text "footer"))
     (formats . :TJ3_FORMATS)
     (header . ,(-partial #'org-tj--get-rich-text "header"))
     (headline . ,(-partial #'org-tj--get-rich-text "headline"))
     (height . :TJ3_HEIGHT)
     (hideaccount . :TJ3_HIDEACCOUNT)
     (hidejournalentry . :TJ3_HIDEJOURNALENTRY)
     (hideresource . :TJ3_HIDERESOURCE)
     (hidetask . :TJ3_HIDETASK)
     (journalattributes . :TJ3_JOURNALATTRIBUTES)
     (journalmode . :TJ3_JOURNALMODE)
     (left . ,(-partial #'org-tj--get-rich-text "left"))
     (loadunit . :TJ3_LOADUNIT)
     (numberformat . :TJ3_NUMBERFORMAT)
     ;; . :TJ3_OPENNODES (internal use only)
     (period . :TJ3_PERIOD)
     (prolog . ,(-partial #'org-tj--get-rich-text "prolog"))
     (purge . :TJ3_PURGE)
     (rawhtmlhead . :TJ3_RAWHTMLHEAD)
     (resourcereport . org-tj--get-resourcereports)
     (right . ,(-partial #'org-tj--get-rich-text "right"))
     (rollupaccount . :TJ3_ROLLUPACCOUNT)
     (rollupresource . :TJ3_ROLLUPRESOURCE)
     (rolluptask . :TJ3_ROLLUPTASK)
     (scenarios . :TJ3_SCENARIOS)
     (selfcontained . :TJ3_SELFCONTAINED)
     (sortaccounts . :TJ3_SORTACCOUNTS)
     (sortjournalentries . :TJ3_SORTJOURNALENTRIES)
     (sortresources . :TJ3_SORTRESOURCES)
     (sorttasks . :TJ3_SORTTASKS)
     (start . org-tj--get-start)
     (taskreport . org-tj--get-taskreports)
     (taskroot . :TJ3_TASKROOT)
     (textreport . org-tj--get-textreports)
     (timeformat . :TJ3_TIMEFORMAT)
     (timezone . :TJ3_TIMEZONE)
     (title . :TJ3_TITLE)
     ;; . :TJ3_TRACEREPORT (not implemented)
     (width . :TJ3_WIDTH))
    (task
     ;; . :TJ3_ADOPT (experimental)
     (allocate . :TJ3_ALLOCATE)
     (booking . :TJ3_BOOKING)
     (charge . :TJ3_CHARGE)
     (chargeset . :TJ3_CHARGESET)
     (complete . org-tj--get-complete)
     (depends . org-tj--get-depends)
     (duration . :TJ3_DURATION)
     (effort . org-tj--get-effort)
     ;; . :TJ3_EFFORTDONE (not fully tested)
     ;; . :TJ3_EFFORTLEFT (not fully tested)
     (end . org-tj--get-end)
     (fail . :TJ3_FAIL)
     (flags . :TJ3_FLAGS)
     (journalentry . :TJ3_JOURNALENTRY)
     (length . :TJ3_LENGTH)
     (limits . :TJ3_LIMITS)
     (maxend . :TJ3_MAXEND)
     (maxstart . :TJ3_MAXSTART)
     (milestone . org-tj--get-milestone)
     (minend . :TJ3_MINEND)
     (minstart . :TJ3_MINSTART)
     (note . :TJ3_NOTE)
     (period . :TJ3_PERIOD)
     (precedes . :TJ3_PRECEDES)
     (priority . org-tj--get-priority)
     (projectid . :TJ3_PROJECTID)
     (purge . :TJ3_PURGE)
     (responsible . :TJ3_RESPONSIBLE)
     (scheduled . :TJ3_SCHEDULED)
     (scheduling . :TJ3_SCHEDULING)
     (schedulingmode . :TJ3_SCHEDULINGMODE)
     ;; . :TJ3_SUPPLEMENT (not implemented, useless here)
     (shift . :TJ3_SHIFTS)
     (start . org-tj--get-start)
     (task . org-tj--get-inner-tasks)
     (warn . :TJ3_WARN))
    (shift
     (leaves . :TJ3_LEAVES)
     (replace . :TJ3_REPLACE)
     (timezone . :TJ3_TIMEZONE)
     (vacation . :TJ3_VACATION)
     (workinghours . :TJ3_WORKINGHOURS))
    (account
     (aggregate . :TJ3_AGGREGATE)
     (credits . :TJ3_CREDITS)
     (flags . :TJ3_FLAGS))
    (resource
     (booking . :TJ3_BOOKING)
     (chargeset . :TJ3_CHARGESET)
     (efficiency . :TJ3_EFFICIENCY)
     (email . :TJ3_EMAIL)
     (fail . :TJ3_FAIL)
     (flag . :TJ3_FLAGS)
     (journalentry . :TJ3_JOURNALENTRY)
     ;; . :TJ3_LEAVEALLOWANCE (not fully tested)
     (leaves . :TJ3_LEAVES)
     (limits . :TJ3_LIMITS)
     (managers . :TJ3_MANAGERS)
     (purge . :TJ3_PURGE)
     (rate . :TJ3_RATE)
     (shifts . :TJ3_SHIFTS)
     ;; . :TJ3_SUPPLEMENT (not implemented, useless here)
     (vacation . :TJ3_VACATION)
     (warn . :TJ3_WARN)
     (workinghours . :TJ3_WORKINGHOURS)))
  "Taskjuggler attributes that are valid as headline properties.")

;;; User Variables

(defgroup org-tj3 nil
  "Org TJ3 backend options."
  :tag "Org TJ3"
  :group 'org)

(defcustom org-tj-use-id-property nil
  "Use the :ID: property when t.
Since this will class with the org-id functions, this is off by
default and :TASK_ID: will be used instead."
  :group 'org-tj3
  :type 'boolean)

(defcustom org-tj-config-file (-> "XDG_CONFIG_HOME"
                                   getenv
                                   file-name-as-directory
                                   (concat "tj3")
                                   file-name-as-directory
                                   (concat "taskjugglerrc"))
  "The org-tj3 config file."
  :group 'org-tj3
  :type 'string)

;; TODO need a way to validate attributes? see the project manual
;; page for what is allowed
(defcustom org-tj-default-attributes
  '(("timingresolution" . "5 min"))
  "Default project attributes."
  :group 'org-tj3)

(defgroup org-export-taskjuggler nil
  "Options specific for TaskJuggler export back-end."
  :tag "Org Export TaskJuggler"
  :group 'org-export)

(defcustom org-tj-extension ".tjp"
  "Extension of TaskJuggler files."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-tj-project-tag "tj3_project"
  "Tag marking project's tasks.
This tag is used to find the tree containing all the tasks for
the project."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-tj-account-tag "tj3_acnt"
  "Tag marking project's accounts.
This tag is used to find the tree containing all the tasks for
the project."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-tj-shift-tag "tj3_shift"
  "Tag marking project's shifts.
This tag is used to find the tree containing all the tasks for
the project."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-tj-resource-tag "tj3_resource"
  "Tag marking project's resources.
This tag is used to find the tree containing all the resources
for the project."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-tj-report-tag "tj3_report"
  "Tag marking project's reports.
This tag is used to find the tree containing all the reports for
the project."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-tj-ignore-tag "tj3_ignore"
  "Tag marking tasks to be ignored when exporting.
Anything children under a marked subtree header is also ignore.
This tag has no effect when applied to the toplevel headline of
subtrees marked with `org-tj-project-tag'."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-tj-default-project-version "1.0"
  "Default version string for the project.
This value can also be set with the \":VERSION:\" property
associated to the headline defining the project."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-tj-default-project-duration 280
  "Default project duration.
The value will be used if no start and end date have been defined
in the root node of the task tree, i.e. the tree that has been
marked with `org-tj-project-tag'"
  :group 'org-export-taskjuggler
  :type 'integer)

(defcustom org-tj-default-reports
  '("textreport report \"Plan\" {
  formats html
  header '== %title =='

  center -8<-
    [#Plan Plan] | [#Resource_Allocation Resource Allocation]
    ----
    === Plan ===
    <[report id=\"plan\"]>
    ----
    === Resource Allocation ===
    <[report id=\"resourceGraph\"]>
  ->8-
}

# A traditional Gantt chart with a project overview.
taskreport plan \"\" {
  headline \"Project Plan\"
  columns bsi, name, start, end, effort, chart
  loadunit shortauto
  hideresource 1
}

# A graph showing resource allocation. It identifies whether each
# resource is under- or over-allocated for.
resourcereport resourceGraph \"\" {
  headline \"Resource Allocation Graph\"
  columns no, name, effort, weekly
  loadunit shortauto
  hidetask ~(isleaf() & isleaf_())
  sorttasks plan.start.up
}")
  "Default reports for the project.
These are sensible default reports to give a good out-of-the-box
result when exporting without defining any reports.  \"%title\"
anywhere in the reports will be replaced with the document title.
If you want to define your own reports you can change them here
or simply define the default reports so that they include an
external report definition as follows:

include reports.tji"
  :group 'org-export-taskjuggler
  :type '(repeat (string :tag "Report")))

(defcustom org-tj-default-global-header ""
  "Default global header for the project.
This goes before project declaration, and might be useful for
early macros."
  :group 'org-export-taskjuggler
  :type '(string :tag "Preamble"))

(defcustom org-tj-default-global-properties
  "shift s40 \"Part time shift\" {
  workinghours wed, thu, fri off
}
"
  "Default global properties for the project.

Here you typically define global properties such as shifts,
accounts, rates, vacation, macros and flags.  Any property that
is allowed within the TaskJuggler file can be inserted.  You
could for example include another TaskJuggler file.

The global properties are inserted after the project declaration
but before any resource and task declarations."
  :group 'org-export-taskjuggler
  :type '(string :tag "Preamble"))

(defcustom org-tj-process-command
  "tj3 --silent --no-color --output-dir %o %f"
  "Command to process a Taskjuggler file.
The command will be given to the shell as a command to process a
Taskjuggler file.  \"%f\" in the command will be replaced by the
full file name, \"%o\" by the reports directory (see
`org-tj-reports-directory')."
  :group 'org-export-taskjuggler)

(defcustom org-tj-reports-directory "reports"
  "Default directory to generate the Taskjuggler reports in.
The command `org-tj-process-command' generates the
reports and associated files such as CSS inside this directory.

If the directory is not an absolute path it is relative to the
directory of the exported file.  The directory is created if it
doesn't exist."
  :group 'org-export-taskjuggler)

(defcustom org-tj-keep-project-as-task t
  "Non-nil keeps the project headline as an umbrella task for all tasks.
Setting this to nil will allow maintaining completely separated
task buckets, while still sharing the same resources pool."
  :group 'org-export-taskjuggler
  :type 'boolean)

;;; Hooks

(defvar org-tj-final-hook nil
  "Hook run after a TaskJuggler files has been saved.
This hook is run with the name of the file as argument.")

;;; macros

;; (defmacro org-tj--with-advice (adlist &rest body)
;;   "Execute BODY with temporary advice in ADLIST.

;; Each element of ADLIST should be a list of the form
;;   (SYMBOL WHERE FUNCTION [PROPS])
;; suitable for passing to `advice-add'.  The BODY is wrapped in an
;; `unwind-protect' form, so the advice will be removed even in the
;; event of an error or nonlocal exit."
;;   (declare (debug ((&rest (&rest form)) body))
;;            (indent 1))
;;   `(progn
;;      ,@(mapcar (lambda (adform)
;;                  (cons 'advice-add adform))
;;                adlist)
;;      (unwind-protect (progn ,@body)
;;        ,@(mapcar (lambda (adform)
;;                    `(advice-remove ,(car adform) ,(nth 2 adform)))
;;                  adlist))))

;;; Unique IDs

(defun org-tj--filter-headlines (headlines)
  "Return HEADLINES that do not have `org-tj-ignore-tag'."
  (--remove (->> (org-element-property :tags it)
                 (member org-tj-ignore-tag))
            headlines))

(defun org-tj--generate-ids (headlines)
  "Generate list of unique ids from HEADLINES."
  (->> headlines
       (-reductions-from
        (lambda (a b)
          (let ((unique-id (org-tj--build-unique-id b a)))
            ;; TODO this is inefficient (building list
            ;; backwards)
            (append a (list unique-id))))
        nil)
       (-drop 1)
       (-map #'-last-item)))

(defun org-tj--assign-global-ids (headlines)
  "Assign a globally unique ID to each in HEADLINES.
HEADLINES is a list of headlines which may contain subheadlines.
Return value is an alist between headlines and their associated ID."
  (cl-labels
      ((filter-headlines
        (hs)
        (-when-let (filtered-hs (org-tj--filter-headlines hs))
          (->> (--map (filter-headlines (org-tj--subheadlines it))
                      filtered-hs)
               (apply #'append)
               (append filtered-hs)))))
    (let ((filtered-headlines (filter-headlines headlines)))
      (->> (org-tj--generate-ids filtered-headlines)
           (--zip-with (cons it other) filtered-headlines)))))

(defun org-tj--assign-local-ids (headlines)
  "Assign a locally unique ID to each in HEADLINES.
HEADLINES is a list of headlines which may contain subheadlines.
Return value is an alist between headlines and their associated ID.
IDs are hierarchical, which means are unique only among the task
siblings."
  (let* ((filtered-headlines (org-tj--filter-headlines headlines))
         (inner-ids
          (->> filtered-headlines
               (--map (->> (org-tj--subheadlines it)
                           (org-tj--assign-local-ids)))
               (apply #'append))))
    (->> (org-tj--generate-ids filtered-headlines)
         (--zip-with (cons it other) filtered-headlines)
         (append inner-ids))))

;;; Accessors

(defun org-tj--get-id (item ids)
  "Return id for task or resource ITEM and list of IDS."
  (cdr (assq item ids)))

(defun org-tj--get-name (headline)
  "Return name for task or resource HEADLINE.
ITEM is a headline.  Return value is a string."
  ;; Quote double quotes in name.
  (replace-regexp-in-string
   "\"" "\\\"" (org-element-property :raw-value headline) t t))

(defun org-tj--get-start (headline &optional _pd)
  "Return start date for task or resource HEADLINE.
ITEM is a headline.  Return value is a string or nil if HEADLINE
doesn't have any start date defined."
  (-if-let (scheduled (org-element-property :scheduled headline))
      (org-timestamp-format scheduled "%Y-%02m-%02d")
    (org-element-property :TJ3_START headline)))

(defun org-tj--get-end (headline &optional _pd)
  "Return end date for task or resource HEADLINE.
ITEM is a headline.  Return value is a string or nil if HEADLINE
doesn't have any end date defined."
  (-if-let (deadline (org-element-property :deadline headline))
      (org-timestamp-format deadline "%Y-%02m-%02d")
    (org-element-property :TJ3_END headline)))

(defun org-tj--get-effort (headline &optional _pd)
  (or (org-element-property :TJ3_EFFORT headline)
      (-some--> (org-element-property :EFFORT headline)
                (org-duration-to-minutes it)
                (/ it 60.0)
                (format "%.1fh" it))))

(defun org-tj--get-complete (headline &optional _pd)
  (if (eq (org-element-property :todo-type headline) 'done) "100"
    (org-element-property :TJ3_COMPLETE headline)))

(defun org-tj--get-milestone (headline _pd)
  (or (org-element-property :TJ3_MILESTONE headline)
      (not (or (org-tj--subheadlines headline)
               (org-element-property :TJ3_LENGTH headline)
               (org-element-property :TJ3_DURATION headline)
               (org-tj--get-effort headline)
               (and (org-tj--get-start headline)
                    (org-tj--get-end headline))
               (org-element-property :TJ3_PERIOD headline)))))

(defun org-tj--get-priority (headline &optional _pd)
  (-some--> (org-element-property :priority headline)
            (- org-lowest-priority it)
            (* 1000 it)
            (/ it (- org-lowest-priority org-highest-priority))
            (max 1 it)))

(defun org-tj--get-inner (fun headline pd)
  (-some->>
   (org-tj--subheadlines headline)
   (--remove (member org-tj-ignore-tag (org-element-property :tags it)))
   (--map (funcall fun it pd))))

(defun org-tj--get-inner-tasks (headline pd)
  (org-tj--get-inner #'org-tj--build-task headline pd))

(defun org-tj--get-inner-report (kind headline pd)
  (-some->>
   (org-tj--subheadlines headline)
   (--remove (member org-tj-ignore-tag (org-element-property :tags it)))
   (--filter (equal kind (org-element-property :TJ3_REPORT_KIND it)))
   (--map (org-tj--build-report it pd))))

(defun org-tj--get-resourcereports (headline pd)
  (org-tj--get-inner-report "resource" headline pd))

(defun org-tj--get-taskreports (headline pd)
  (org-tj--get-inner-report "task" headline pd))
   
(defun org-tj--get-textreports (headline pd)
  (org-tj--get-inner-report "text" headline pd))

(defun org-tj--get-depends (headline pd)
  (let ((tree (org-tj--proc-data-tree pd))
        (ids (org-tj--proc-data-task-ids pd)))
    (-when-let (depends (org-tj--resolve-dependencies headline tree))
      (org-tj--format-dependencies depends headline ids))))

(defun org-tj--get-rich-text (attribute headline pd)
  (-some-->
   (org-element-contents headline)
   (assq 'section it)
   (org-element-contents it)
   (org-element-map it 'src-block #'identity)
   (--first (equal (org-element-property :name it) attribute) it)
   (org-tj--src-to-rich-text it)))

;;; Internal Functions

(defun org-tj--indent-string (s)
  "Indent string S by 2 spaces.
Return new string.  If S is the empty string, return it."
  (if (equal "" s) s (replace-regexp-in-string "^ *\\S-" "  \\&" s)))

(defun org-tj--keyword-to-attribute (keyword)
  "Return attribute KEYWORD as a formatted string.
KEYWORD is assumed to be like :TJ3_attribute."
  (->> keyword symbol-name (s-chop-prefix ":TJ3_") downcase))

(defun org-tj--build-attributes (attr-alist headline pd)
  "Return attributes string for HEADLINE.
ATTRIBUTES is a list of symbols representing valid attributes
for HEADLINE as keywords."
  ;; TODO add validation here for attributes in headline that start
  ;; with TJ3_ but are not valid
  (cl-flet
      ((eval-attr
        (attr-data)
        (let* ((name (car attr-data))
               (action (cdr attr-data))
               (value
                (cond
                 ((functionp action)
                  (funcall action headline pd))
                 ((keywordp action)
                  (org-element-property action headline))
                 (t (error "Unknown action: %s" action)))))
          (cond
           ((eq t value) (list name))
           (value (cons name value))))))
    (->> (--map (eval-attr it) attr-alist)
         (-non-nil))))

(defun org-tj--build-unique-id (item unique-ids)
  "Return a unique id for a given task or a resource.
ITEM is an `headline' type element representing the task or
resource.  Its id is derived from its name and made unique
against UNIQUE-IDS.  If the (downcased) first token of the
headline is not unique try to add more (downcased) tokens of the
headline or finally add more underscore characters (\"_\")."
  (let ((id (org-string-nw-p (or (org-element-property :TJ3_ID item)
                                 (org-element-property :CUSTOM_ID item)))))
    ;; If an id is specified, use it, as long as it's unique.
    (if (and id (not (member id unique-ids))) id
      (let* ((parts (split-string (org-element-property :raw-value item)))
	     (id (org-tj--clean-id (downcase (pop parts)))))
	;; Try to add more parts of the headline to make it unique.
	(while (and (car parts) (member id unique-ids))
	  (setq id (concat id "_"
			   (org-tj--clean-id (downcase (pop parts))))))
	;; If it's still not unique, add "_".
	(while (member id unique-ids)
	  (setq id (concat id "_")))
	id))))

(defun org-tj--clean-id (id)
  "Clean and return ID to make it acceptable for TaskJuggler.
ID is a string."
  ;; Replace non-ascii by "_".
  (replace-regexp-in-string
   "[^a-zA-Z0-9_]" "_"
   ;; Make sure id doesn't start with a number.
   (replace-regexp-in-string "^\\([0-9]\\)" "_\\1" id)))

(defun org-tj--file-keywords-list-format (keywords key &optional
                                                   alt-key bracket?)
  (-if-let (list (-some-->
                  (--filter (equal key (car it)) keywords)
                  (-map #'cdr it)
                  (if bracket? (format "{ %s }" (s-join " " it))
                    (s-join ", " it))))
      (format "%s %s\n" (or alt-key (downcase key)) list)
    ""))

(defun org-tj--file-keywords-first-format (keywords key &optional
                                                     alt-key
                                                     default-val)
  (-if-let (val (or (alist-get key keywords nil nil #'equal)
                    default-val))
      (format "%s %s\n" (or alt-key (downcase key)) val)
    ""))

(defun org-tj--file-keywords-all-format (keywords key &optional
                                                     alt-key)
  (or (-some->>
       (--filter (equal key (car it)) keywords)
       (-map #'cdr)
       (--map (format "%s %s\n" (or alt-key (downcase key)) it))
       (s-join ""))
      ""))

(defun org-tj--file-tj3-keywords (tree)
  "Return toplevel taskjuggler file keywords from buffer parse TREE."
  (->> tree
       org-element-contents
       (assoc 'section)
       org-element-contents
       (--filter (and (eq 'keyword (org-element-type it))
                      (-> (org-element-property :key it)
                          (substring 0 4)
                          (equal "TJ3_"))))
       (--map (cons (substring (org-element-property :key it) 4)
                    (org-element-property :value it)))))

(defun org-tj--file-project-keywords (keywords)
  (->> (--filter (s-starts-with? "PROJ_" (car it)) keywords)
       (--map (cons (s-chop-prefix "PROJ_" (car it))
                    (cdr it)))))

(defun org-tj--file-project-id (project-keywords info)
  (let ((alt-id (--> (plist-get info :input-buffer)
                     (f-no-ext it)
                     ;; TODO this is likely incomplete
                     (s-replace "-" "_" it))))
    (alist-get "ID" project-keywords alt-id nil #'equal)))

(defun org-tj--file-project-version (project-keywords)
  (alist-get "VERSION" project-keywords
             org-tj-default-project-version nil #'equal))

(defun org-tj--file-project-name (project-keywords)
  (alist-get "NAME" project-keywords "" nil #'equal))

(defun org-tj--file-project-start (project-keywords tasks)
  ;; TODO the first task may not have a start date
  ;; TODO convert org timestamps to tj3 format
  (let ((default (or (org-tj--get-start (car tasks))
                     (format-time-string "%Y-%m-%d"))))
    (alist-get "START" project-keywords default nil #'equal)))

(defun org-tj--file-project-end (project-keywords tasks)
  ;; TODO the first task may not have an date
  ;; TODO convert org timestamps to tj3 format
  (let ((default (or (-some->> (org-tj--get-end (car tasks))
                               (format "- %s"))
                     (format "+%sd" org-tj-default-project-duration))))
    (alist-get "END" project-keywords default nil #'equal)))

(defun org-tj--file-project-attributes (project-keywords)
  ;; TODO set the default for this
  (let* ((file-attrs
          (->> (--filter (equal "ATTRIBUTE" (car it)) project-keywords)
               (-map #'cdr)
               (--map (let ((s (s-split-up-to " " it 1 t)))
                        (cons (car s) (car (cdr s)))))))
         (default-attrs
           (--> org-tj-default-attributes
                (--remove (assoc-string (car it) file-attrs) it))))
    (->> (append file-attrs default-attrs)
         (--map (format "%s %s" (car it) (cdr it)))
         (s-join "\n"))))

(defun org-tj--file-copyright (keywords)
  ;; TODO add default
  (org-tj--file-keywords-first-format keywords "COPYRIGHT" nil ""))

(defun org-tj--file-balance (keywords)
  ;; TODO add default
  (org-tj--file-keywords-first-format keywords "BALANCE" nil ""))

(defun org-tj--file-rate (keywords)
  ;; TODO add default
  (org-tj--file-keywords-first-format keywords "RATE" nil ""))

(defun org-tj--file-navigator (keywords)
  (-if-let (nav (-some-->
                   (alist-get "NAVIGATOR" keywords "" nil #'equal)
                   (s-split " " it t)))
      (cl-case (length nav)
        (1 (format "navigator %s\n" (nth 0 nav)))
        (2 (format "navigator %s { %s }\n" (nth 0 nav) (nth 1 nav)))
        (t (error "Invalid navbar specification: %s" nav)))
    ""))

(defun org-tj--file-vacation-format (keywords)
  (org-tj--file-keywords-all-format keywords "VACATION"))

(defun org-tj--file-flags-format (keywords)
  (org-tj--file-keywords-list-format keywords "FLAG" "flags"))

;; TODO this actually needs to be in {}
(defun org-tj--file-limits-format (keywords)
  (org-tj--file-keywords-list-format keywords "LIMIT" "limits" t))


;;; Dependencies

;; TODO combine this with the formatter, no reason to keep them
;; separate
(defun org-tj--resolve-dependencies (task tree)
  "Return a list of all tasks on which TASK depends."
  ;; TODO add blocker eventually, baby steps
  (let* ((deps-ids (-some->>
                    (org-element-property :TJ3_DEPENDS task)
                    (s-replace-regexp "{.*}" "")
                    (s-split ",")
                    (-map #'s-trim)))
         (wants-prev-sibling?
          (and
           (member-ignore-case "previous-sibling" deps-ids)
           (not (org-export-first-sibling-p task nil))))
         (is-ordered?
          (and
           (org-element-property :ORDERED (org-export-get-parent task))
           (not (org-export-first-sibling-p task nil))))
         (prev-task
          (when (or wants-prev-sibling? is-ordered?)
            (list (org-export-get-previous-element task nil))))
         (depends
          (if (not deps-ids) prev-task
            (--> (org-tj--get-tasks tree)
                 (org-element-map it 'headline
                   (lambda (task)
                     (-when-let (task-id
                                 (or (org-element-property :TJ3_ID task)
                                     (org-element-property :ID task)))
                       (when (member task-id deps-ids) task))))
                 (append prev-task it)
                 (-non-nil it)
                 (-uniq it)))))
    depends))

(defun org-tj--format-dependencies (dependencies task task-ids)
  "Format DEPENDENCIES to match TaskJuggler syntax.
DEPENDENCIES is list of dependencies for TASK, as returned by
`org-tj-resolve-depedencies'.  TASK is a headline.
INFO is a plist used as a communication channel.  Return value
doesn't include leading \"depends\"."
  (let* ((dep-str (format "%s %s"
                          (org-element-property :BLOCKER task)
                          (org-element-property :TJ3_DEPENDS task)))
         (get-path
          (lambda (dep)
            ;; Return path to DEP relatively to TASK.
            (let ((parent (org-export-get-parent task))
                  (exclamations 1)
                  (option
                   (let ((id (org-element-property :TJ3_ID dep)))
                     (and id
                          (string-match (concat id " +\\({.*?}\\)") dep-str)
                          (match-string-no-properties 1 dep-str))))
                  path)
              ;; Compute number of exclamation marks by looking for the
              ;; common ancestor between TASK and DEP.
              (while (not (org-element-map parent 'headline
                            (lambda (hl) (eq hl dep))))
                (incf exclamations)
                (setq parent (org-export-get-parent parent)))
              ;; Build path from DEP to PARENT.
              (while (not (eq parent dep))
                (push (org-tj--get-id dep task-ids) path)
                (setq dep (org-export-get-parent dep)))
              ;; Return full path.  Add dependency options, if any.
              (concat (make-string exclamations ?!)
                      (mapconcat 'identity path ".")
                      (and option (concat " " option)))))))
    ;; Return dependencies string, without the leading "depends".
    (-some->> (--map (funcall get-path it) dependencies)
              (s-join ", "))))

;;; Translator Functions

(defun org-tj--subheadlines (headline)
  "Return subheadings under HEADLINE if any."
  (let ((contents (org-element-contents headline)))
    (if (assoc 'section contents) (cdr contents) contents)))

(defun org-tj--src-to-rich-text (src-block)
  "Convert org SRC-BLOCK to rich text. Return formatted string."
  (letrec
      ((buffer (with-temp-buffer
                 (insert (org-element-property :value src-block))
                 (org-element-parse-buffer)))
       (parse-elem
        (lambda (elem)
          (let ((type (org-element-type elem)))
            (cond
             ;; flank headlines with ===
             ((eq type 'headline) (funcall parse-hl elem))
             ;; convert links to either local links or generators
             ((eq type 'link) (funcall parse-link elem))
             ;; convert items to enumerated (#) or bulleted (*) lists
             ((eq type 'item) (funcall parse-item elem))
             ;; convert source blocks to literal text/html
             ((eq type 'src-block) (funcall parse-src-block elem))
             ;; wrap verbatim text in nowiki to make literal
             ((eq type 'verbatim) (funcall parse-verbatim elem))
             ;; apply formatting to all textual elements
             ((memq type '(plain-text italic bold underline
                                      superscript subscript))
              (funcall parse-text elem type))
             ;; format code as monospaced text
             ((eq type 'code) (funcall parse-code elem))
             ;; keep the text of timestamps as-is
             ((eq type 'timestamp)
              (funcall parse-value elem))
             ;; horizontal rules are just four dashes
             ((eq type 'horizontal-rule) "----\n")
             ;; these are containers, just iterate through them
             ((memq type '(section paragraph plain-list))
              (funcall parse-contents elem))
             ;; Ignore all other elements:
             ;; babel-call, center-block, clock, comment,
             ;; comment-block, diary-sexp, drawer, dynamic-block,
             ;; entity, example-block, export-block, export-snippet,
             ;; fixed-width, footnote-definition, footnote-reference,
             ;; inline-babel-call, inline-src-block, inlinetask,
             ;; keyword, latex-environment, latex-fragment, line-break
             ;; macro, planning, property-drawer, quote-block
             ;; radio-target, special-block,
             ;; statistics-cookie, table, table-cell, table-row, target,
             ;; verse-block
             (t "")))))
       (parse-src-block
        (lambda (src-block)
          (let ((src (org-element-property :value src-block)))
            (if (equal "html" (org-element-property :language src-block))
                (format "<html>\n%s\n</html>\n" src)
              (replace-regexp-in-string "^ *\\S-" " \\&" src)))))
       (parse-verbatim
        (lambda (verbatim)
          (->> (org-element-property :value verbatim)
               (format "<nowiki>%s</nowiki>"))))
       (parse-code
        (lambda (code)
          (->> (org-element-property :value code)
               (format "''''%s''''"))))
       (parse-contents
        (lambda (obj)
          (->> (org-element-contents obj)
               (--map (funcall parse-elem it))
               (apply #'concat))))
       (parse-hl
        (lambda (hl)
          (let ((txt (org-element-property :raw-value hl))
                (lvl (--> (org-element-property :level hl)
                          (if (> it 3) 3 it)
                          (s-repeat it "="))))
            (concat
             (format "%s %s %s\n" lvl txt lvl)
             (funcall parse-contents hl)))))
       (parse-link-id
        (lambda (link)
          (let ((path (org-element-property :path link))
                ;; find all siblings of this link that are not just
                ;; whitespace
                (non-ws-siblings
                 (->> (org-element-property :parent link)
                      org-element-contents
                      (-remove-item link)
                      (--remove
                       (and (eq 'plain-text (org-element-type it))
                            (equal (s-trim it) ""))))))
            ;; TODO resolve links by going to targets
            ;; TODO check if we are part of an item and use link?
            ;; use block generator when there are no siblings
            (if (= 0 (length non-ws-siblings))
                ;; TODO how to tell if we want a navbar?
                (format "<[report id=\"%s\"]>" path)
              (format "<-reportlink id=\"%s\"->" path)))))
       (parse-link-http
        (lambda (link)
          (let ((raw-link (org-element-property :raw-link link))
                (contents (org-element-contents link)))
            (if contents
                (format "[%s %s]" raw-link
                        (funcall parse-text link 'link))
              (format "[%s]" raw-link)))))
       (parse-link-file
        (lambda (link)
          (let ((path (org-element-property :path link))
                (contents (org-element-contents link)))
            (if contents
                (format "[[File:%s|%s]]" path
                        (funcall parse-text link 'link))
              (format "[[File:%s]]" path)))))
       (parse-link
        (lambda (link)
          (let ((type (org-element-property :type link)))
            (cond
             ((equal type "custom-id") (funcall parse-link-id link))
             ((equal type "http") (funcall parse-link-http link))
             ((equal type "file") (funcall parse-link-file link))
             (t type)))))
       (parse-item
        (lambda (item)
          (let* ((contents (funcall parse-contents item))
                 (begin (org-element-property :begin item))
                 ;; TODO this assumes that the parent is always a list
                 ;; if this is ever false we crash
                 (bullet (--> (org-element-property :parent item)
                              (org-element-property :type it)
                              (if (eq it 'ordered) "#" "*")))
                 (lvl (--> (org-element-property :structure item)
                           (alist-get begin it)
                           (nth 0 it)
                           (/ it 2)
                           (+ it 1)
                           (s-repeat it bullet)
                           (format "%s " it))))
            (concat lvl contents))))
       (parse-text
        (lambda (text type &optional is-italic is-bolded)
          (if (eq type 'plain-text)
              (let ((fmt (cond
                          ((and is-italic is-bolded) "'''''%s'''''")
                          (is-italic "''%s''")
                          (is-bolded "'''%s'''")
                          (t "%s"))))
                (->> text substring-no-properties (format fmt)))
            (let* ((blanks (org-element-property :post-blank text))
                   (fmt (->>
                         (cond
                          ((eq type 'superscript) "^%s")
                          ((eq type 'subscript) "_{%s}")
                          (t "%s"))
                         (s-append (s-repeat blanks " ")))))
              (->> (org-element-contents text)
                   (--map (let* ((inner-type (org-element-type it))
                                 (i (or is-italic (eq type 'italic)))
                                 (b (or is-bolded (eq type 'bold))))
                            (funcall parse-text it inner-type i b)))
                   (apply #'concat)
                   (format fmt))))))
       (parse-value
        (lambda (obj)
          (or (org-element-property :value obj)
              (org-element-property :raw-value obj))))
       (rich-text (s-trim (funcall parse-contents buffer))))
    (if (s-contains? "\n" rich-text)
        (format "-8<-\n%s\n->8-" (org-tj--indent-string rich-text))
      (format "'%s'" rich-text))))

(defun org-tj--get-tasks (tree)
  "Return list of headlines marked with `org-tj-project-tag'.
Only return the toplevel heading in a marked subtree. TREE is the
parse tree of the buffer."
  ;; TODO what if we have project tags in the subtree, we don't need
  ;; those
  (org-element-map tree 'headline
    (lambda (hl)
      (when (->> (org-element-property :tags hl)
                 (member org-tj-project-tag))
        hl))))

(defun org-tj--get-accounts (tree)
  "Return list of headlines marked with `org-tj-account-tag'.
Only return the toplevel heading in a marked subtree. TREE is the
parse tree of the buffer."
  ;; TODO what if we have shift tags in the subtree, we don't need
  ;; those
  (org-element-map tree 'headline
    (lambda (hl)
      (when (->> (org-element-property :tags hl)
                 (member org-tj-account-tag))
        hl))))

(defun org-tj--get-shifts (tree)
  "Return list of headlines marked with `org-tj-project-tag'.
Only return the toplevel heading in a marked subtree. TREE is the
parse tree of the buffer."
  ;; TODO what if we have shift tags in the subtree, we don't need
  ;; those
  (org-element-map tree 'headline
    (lambda (hl)
      (when (->> (org-element-property :tags hl)
                 (member org-tj-shift-tag))
        hl))))

(defun org-tj--get-resource-headlines (tree)
  "Return list of all org taskjuggler resource headlines."
  (--> (or tree (org-element-parse-buffer))
       (org-element-map it 'headline
         (lambda (hl)
           (when (member org-tj-resource-tag
                         (org-element-property :tags hl))
             hl))
         nil t)
       (org-element-map it 'headline #'identity)
       (--filter (org-element-property :TJ3_ID it) it)))

(defun org-tj--get-reports (tree)
  "Return reports tree from TREE."
  (org-element-map tree 'headline
    (lambda (hl)
      (when (member org-tj-report-tag (org-element-property :tags hl))
        hl))))

(defun org-tj--build-project (pd)
  "Return a project declaration.
PROJECT is a headline.  INFO is a plist used as a communication
channel.  If no start date is specified, start today.  If no end
date is specified, end `org-tj-default-project-duration'
days from now."
  (let* ((proj-kws (->> (org-tj--proc-data-keywords pd)
                        (org-tj--file-project-keywords)))
         (info (org-tj--proc-data-info pd))
         (tasks (org-tj--proc-data-tasks pd))
         (id (org-tj--file-project-id proj-kws info))
         (name (org-tj--file-project-name proj-kws))
         (version (org-tj--file-project-version proj-kws))
         (start (org-tj--file-project-start proj-kws tasks))
         (end (org-tj--file-project-end proj-kws tasks))
         (attrs (org-tj--indent-string
                 (org-tj--file-project-attributes proj-kws))))
    (format "project %s \"%s\" \"%s\" %s %s {\n%s\n}\n"
            id name version start end attrs)))

(defun org-tj--build-account (account pd)
  (org-tj--build-declaration 'account account pd))

(defun org-tj--build-accounts (pd)
  (->> (org-tj--proc-data-accounts pd)
          (--map (org-tj--build-account it pd))
          (--map (format (format "account %s" it)))
          (apply #'concat)))

(defun org-tj--build-shift (shift pd)
  (org-tj--build-declaration 'shift shift pd))

(defun org-tj--build-shifts (pd)
  (->> (org-tj--proc-data-shifts pd)
       (--map (org-tj--build-shift it pd))
       (--map (format (format "shift %s" it)))
       (apply #'concat)))

(defun org-tj--format-attributes (attribute-alist)
  (cl-flet
      ((format-attr
        (cell)
        (let ((key (symbol-name (car cell)))
              (val (cdr cell)))
          (cond
           ((null val) key)
           ((listp val) (->> val
                             (--map (format "%s %s" key it))
                             (apply #'concat)))
           (val (format "%s %s" key val))
           (t (error "Formatting error."))))))
    (->> attribute-alist
         (-map #'format-attr)
         (s-join "\n")
         (org-tj--indent-string))))

(defun org-tj--build-declaration (type headline pd)
  "Return a task declaration.

TASK is a headline.  INFO is a plist used as a communication
channel.

All valid attributes from TASK are inserted.  If TASK defines
a property \"task_id\" it will be used as the id for this task.
Otherwise it will use the ID property.  If neither is defined
a unique id will be associated to it."
  (let ((id (--> (symbol-name type)
                 (format "org-tj--proc-data-%s-ids" it)
                 (intern it)
                 (funcall it pd)
                 (org-tj--get-id headline it)))
        (name (org-tj--get-name headline))
        (attrs
         (--> (alist-get type org-tj--property-attributes)
              (org-tj--build-attributes it headline pd)
              ;; TODO how to generalize this?
              ;; (if (not (assoc 'allocate it)) it
              ;;   (cons '(purge . "allocate") it))
              (org-tj--format-attributes it))))
    (format "%s \"%s\" {\n%s\n}\n" id name attrs)))
  
(defun org-tj--build-task (task pd)
  (org-tj--build-declaration 'task task pd))

(defun org-tj--build-tasks (pd)
  (->> (org-tj--proc-data-tasks pd)
          (--map (org-tj--build-task it pd))
          (--map (format (format "task %s" it)))
          (apply #'concat)))

;; (defun org-tj--build-report (hl)
;;   "Create a task report definition."
;;   ;; assume the incoming headline is indeed a valid report
;;   (-if-let (type (org-element-property :REPORT_KIND hl))
;;       (let* ((name (org-element-property :raw-value hl))
;;              (id (org-element-property :CUSTOM_ID hl))
;;              (rich-text
;;               (-some-->
;;                (org-element-contents hl)
;;                (assq 'section it)
;;                (org-element-contents it)
;;                (org-element-map it 'src-block #'identity)
;;                (--filter
;;                 (member
;;                  (org-element-property :name it)
;;                  (-map #'symbol-name org-tj--report-attributes-rich-text))
;;                 it)
;;                (--map (cons (make-symbol (org-element-property :name it))
;;                             (org-tj--src-to-rich-text it))
;;                       it)))
;;              (props
;;               (-some-->
;;                (alist-get 'report org-tj--property-attributes)
;;                (org-tj--build-attributes it hl)
;;                (org-tj--indent-string it)))
;;                ;; (--map (cons it (org-element-property it hl)))
;;                ;; (--remove (not (cdr it)))))
;;              (attrs
;;               (-some->>
;;                (append rich-text)
;;                ;; TODO add validation here?
;;                (--map (format "%s %s\n" (symbol-name (car it)) (cdr it)))
;;                (-map #'org-tj--indent-string)
;;                (apply #'concat)))
;;              (inner-reports
;;               (->> (org-tj--subheadlines hl)
;;                    (--map (org-tj--build-report it))
;;                    (apply #'concat)
;;                    org-tj--indent-string)))
;;         ;; TODO validate the report type and scream if wrong?
;;         (if (not type) ""
;;           (format "%s %s \"%s\" {\n%s%s}\n" type id name (concat props attrs)
;;                   inner-reports)))
;;     (error "Type not specified for headline: %s"
;;            (org-element-property :raw-value hl))))

(defun org-tj--build-report (report pd)
  (org-tj--build-declaration 'report report pd))

(defun org-tj--get-report-kind (headline)
  (-if-let (kind (org-element-property :TJ3_REPORT_KIND headline))
    (if (member kind '("text" "task" "resource")) kind
      (error ("unknown kind: %s" kind)))
    (->> (org-element-property :raw-value)
         (error "kind not specified for headline \"%s\""))))

(defun org-tj--build-reports (pd)
  (-if-let (reports (org-tj--proc-data-reports pd))
      (->> reports
           (--map (-when-let (kind (org-tj--get-report-kind it))
                    (->> (org-tj--build-report it pd)
                         (format "%sreport %s" kind))))
           (-non-nil)
           (apply #'concat))
    "default reports\n"))
    ;; insert title in default reports
    ;; (let* ((title (org-export-data (plist-get info :title) info))
    ;;        (report-title (if (string= title "")
    ;;                          ;; TODO why are we getting this name?
    ;;                          (org-tj--get-name (car tasks))
    ;;                        title)))
    ;;   (mapconcat
    ;;    'org-element-normalize-string
    ;;    (--map
    ;;     (replace-regexp-in-string "%title" report-title it t t))))
    ;; org-tj-default-reports) "")

(defun org-tj--build-resource (resource pd)
  (org-tj--build-declaration 'resource resource pd))

;; (defun org-tj--build-resource (resource _info resource-ids)
;;   "Return a resource declaration.

;; RESOURCE is a headline.  INFO is a plist used as a communication
;; channel.

;; All valid attributes from RESOURCE are inserted.  If RESOURCE
;; defines a property \"resource_id\" it will be used as the id for
;; this resource.  Otherwise it will use the ID property.  If
;; neither is defined a unique id will be associated to it."
;;   (concat
;;    ;; Opening resource.
;;    (format "resource %s \"%s\" {\n"
;;            (org-tj--clean-id
;;             (or (org-element-property :RESOURCE_ID resource)
;;                 (org-element-property :ID resource)
;;                 (org-tj--get-id resource resource-ids)))
;;            (org-tj--get-name resource))
;;    ;; Add attributes.
;;    (->> (alist-get 'resource org-tj--property-attributes)
;;         (org-tj--build-attributes resource)
;;         (org-tj--indent-string))
;;    ;; Add inner resources.
;;    (->> (org-tj--subheadlines resource)
;;         (--map (org-tj--build-resource it nil resource-ids))
;;         (apply #'concat)
;;         org-tj--indent-string)
;;    ;; (org-tj--indent-string
;;    ;;  (mapconcat
;;    ;;   'identity
;;    ;;   (org-element-map (org-element-contents resource) 'headline
;;    ;;     (lambda (hl) (org-tj--build-resource hl info resource-ids))
;;    ;;     nil nil 'headline)
;;    ;;   ""))
;;    ;; Closing resource.
;;    "}\n"))

(defun org-tj--build-resources (pd)
  (-if-let (resources (org-tj--proc-data-resources pd))
      (->> resources
           (--map (org-tj--build-resource it pd))
           (--map (format (format "resource %s" it)))
           (apply #'concat))
    (format "resource %s \"%s\"\n" (user-login-name)
            user-full-name)))

(cl-defstruct (org-tj--proc-data
               (:copier nil))
  "A collection of processed data from the export tree."
  info
  tree
  keywords
  tasks
  task-ids
  accounts
  account-ids
  shifts
  shift-ids
  resources
  resource-ids
  reports
  report-ids)

(defun org-tj--add-allocates (tasks)
  (cl-flet ((add-allocates
             (task)
             (org-element-put-property
              task :TJ3_ALLOCATE (user-login-name))))
    (-map #'add-allocates tasks)))

(defun org-tj--make-proc-data (info)
  (let* ((tree (plist-get info :parse-tree))
         (tasks (->> (org-tj--get-tasks tree)
                     ))
         (accounts (org-tj--get-accounts tree))
         (shifts (org-tj--get-shifts tree))
         (resources (org-tj--get-resource-headlines tree))
         (reports (org-tj--get-reports tree)))
    (make-org-tj--proc-data
     :info info
     :tree tree
     :keywords (org-tj--file-tj3-keywords tree)
     :accounts accounts
     :account-ids (org-tj--assign-global-ids accounts)
     :tasks (if resources tasks (org-tj--add-allocates tasks))
     :task-ids (org-tj--assign-local-ids tasks)
     :shifts shifts
     :shift-ids (org-tj--assign-global-ids shifts)
     :resources resources
     :resource-ids (org-tj--assign-global-ids resources)
     :reports reports
     :report-ids (org-tj--assign-local-ids reports))))

(defun org-tj--build-tjp-file (_contents info)
  "Build full contents of a taskjuggler project file.
INFO is a plist holding export options. Return formatted string in 
taskjuggler syntax."
  (let ((pd (org-tj--make-proc-data info)))
    (concat
     ;; insert project
     (org-tj--build-project pd)
     ;; insert global properties
     ;; (org-element-normalize-string org-tj-default-global-properties)
     ;; (org-tj--file-navigator keywords)
     ;; (org-tj--file-copyright keywords)
     ;; (org-tj--file-rate keywords)
     ;; (org-tj--file-limits-format keywords)
     ;; (org-tj--file-vacation-format keywords)
     ;; (org-tj--file-flags-format keywords)
     ;; insert resources; provide a default one if none is specified
     (org-tj--build-resources pd)
     ;; (if resources
     ;;     (->> resources
     ;;          (--map (org-tj--build-resource it info resource-ids))
     ;;          (apply #'concat))
     ;;   (format "resource %s \"%s\" {\n}\n" (user-login-name)
     ;;           user-full-name))
     ;; insert accounts
     (org-tj--build-accounts pd)
     ;; (->> accounts
     ;;      (--map (org-tj--build-account it info account-ids tree))
     ;;      (apply #'concat))
     ;; insert shifts
     (org-tj--build-shifts pd)
     ;; (->> shifts
     ;;      (--map (org-tj--build-shift it info shift-ids tree))
     ;;      (apply #'concat))
     ;; 5. Insert tasks.
     (org-tj--build-tasks pd)
     ;; (->> (org-tj--proc-data-tasks pd)
     ;;      (--map (org-tj--build-task it pd))
     ;;      (--map (format (format "task %s" it)))
     ;;      (apply #'concat)))))
     ;; 6. Insert reports.  If no report is defined, insert default
     ;;    reports.
     (org-tj--build-reports pd))))
     ;; (if reports
     ;;     (mapconcat
     ;;      (lambda (report) (org-tj--build-report report))
     ;;      reports "")
     ;;   ;; insert title in default reports
     ;;   (let* ((title (org-export-data (plist-get info :title) info))
     ;;          (report-title (if (string= title "")
     ;;                            ;; TODO why are we getting this name?
     ;;                            (org-tj--get-name (car tasks))
     ;;                          title)))
     ;;     (mapconcat
     ;;      'org-element-normalize-string
     ;;      (--map
     ;;       (replace-regexp-in-string "%title" report-title it t t)
     ;;       org-tj-default-reports) ""))))))

;;; export functions

(org-export-define-backend 'taskjuggler
  '((template . org-tj--build-tjp-file))
  :menu-entry
  '(?J "Export to TaskJuggler"
       ((?j "As TJP file" (lambda (a s v b) (org-tj-export a s v)))
	(?p "As TJP file and process"
	    (lambda (a s v b)
	      (if a (org-tj-export a s v)
		(org-tj-export-and-process s v))))
	(?o "As TJP file, process and open"
	    (lambda (a s v b)
	      (if a (org-tj-export a s v)
		(org-tj-export-process-and-open s v)))))))

;;;###autoload
(defun org-tj-export (&optional async subtreep visible-only)
  "Export current buffer to a TaskJuggler file.

The exporter looks for a tree with tag that matches
`org-tj-project-tag' and takes this as the tasks for
this project.  The first node of this tree defines the project
properties such as project name and project period.

If there is a tree with tag that matches
`org-tj-resource-tag' this tree is taken as resources
for the project.  If no resources are specified, a default
resource is created and allocated to the project.

Also the TaskJuggler project will be created with default reports
as defined in `org-tj-default-reports'.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile
         (org-export-output-file-name org-tj-extension subtreep)))
    (org-export-to-file 'taskjuggler outfile
      async subtreep visible-only nil nil
      (lambda (file)
	(run-hook-with-args 'org-tj-final-hook file) nil))))

(defun org-tj-open-id-in-browser (id)
  "Open project ID in the default web browser."
  (browse-url
   (format
    ;; TODO make port a variable
    "http://localhost:8081/taskjuggler?project=%s;report=report" id)))

(defun org-tj-open-in-browser (file)
  "Open tj3 FILE in the default web browser."
  ;; TODO check if project is loaded and load if not loaded
  ;; if it is loaded throw a warning
  (--> (with-temp-buffer (insert-file-contents file) (buffer-string))
       (s-match "project \\([^[:space:]]+\\) " it)
       (nth 1 it)
       (org-tj-open-id-in-browser it)))

;;;###autoload
(defun org-tj-export-process-and-open (&optional subtreep visible-only)
  "Export current buffer to a TaskJuggler file, process and open it.

Export and process the file using
`org-tj-export-and-process' and open the generated
reports with a browser."
  (interactive)
  (let ((reports (org-tj-export-and-process subtreep visible-only)))
    (dolist (report reports)
      (org-tj-open-in-browser report))))

(defun org-tj--cmd (type &rest args)
  "Return formatted shell command string for TYPE.
ARGS are strings appended to the end of the command."
  (-->
   (cl-case type
     (client "tj3client")
     (deamon "tj3d")
     (web "tj3webd")
     (t (error "Unknown command type: %s" type)))
   (list it)
   (append it (list "--silent" "--no-color"))
   (append it args)
   (if (not org-tj-config-file) it
     (-insert-at 1 (concat "-c " org-tj-config-file) it))
   (s-join " " it)))

(defun org-tj--collect-errors (buffer)
  "Collect some kind of errors from \"tj3\" command output.

BUFFER is the buffer containing output.

Return collected error types as a string, or nil if there was
none."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t)
	    (errors ""))
	(while (re-search-forward "^.+:[0-9]+: \\(.*\\)$" nil t)
	  (setq errors (concat errors " " (match-string 1))))
	(and (org-string-nw-p errors) (org-trim errors))))))

(defun org-tj--load (file)
  "Load a TaskJuggler file.
FILE is the name of the file being loaded.  Processing is done
through the command given in `org-tj-process-command'.
Return a list of reports."
  (let* ((full-name (file-truename file))
         (out-dir
          (expand-file-name
           org-tj-reports-directory (file-name-directory file)))
         errors)
    (message (format "Processing TaskJuggler file %s..." file))
    (save-window-excursion
      (let ((outbuf (get-buffer-create "*Org Taskjuggler Output*")))
        (with-current-buffer outbuf (erase-buffer))
        (--> (org-tj--cmd 'client "add" "%f")
             (replace-regexp-in-string
              "%o" (shell-quote-argument out-dir)
              it t t)
             (replace-regexp-in-string
              "%f" (shell-quote-argument full-name)
              it t t)
             (shell-command it outbuf))
        ;; Collect standard errors from output buffer.
        (setq errors (org-tj--collect-errors outbuf)))
      (if (not errors)
          (message "Process completed.")
        (error (format "TaskJuggler failed with errors: %s" errors))))
    (file-expand-wildcards (format "%s/*.html" out-dir))))

;;;###autoload
(defun org-tj-export-and-process (&optional subtreep visible-only)
  "Export current buffer to a TaskJuggler file and process it.

The exporter looks for a tree with tag that matches
`org-tj-project-tag' and takes this as the tasks for
this project.  The first node of this tree defines the project
properties such as project name and project period.

If there is a tree with tag that matches
`org-tj-resource-tag' this tree is taken as resources
for the project.  If no resources are specified, a default
resource is created and allocated to the project.

Also the TaskJuggler project will be created with default reports
as defined in `org-tj-default-reports'.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return a list of reports."
  (interactive)
  (let ((file (org-tj-export nil subtreep visible-only)))
    (org-tj--load file)
    ;; TODO this is a silly hack
    ;; return file as list
    (list file)))

;; TODO use this function in a separate compile export path
;; (defun org-tj-compile (file)
;;   "Compile a TaskJuggler file.

;; FILE is the name of the file being compiled.  Processing is done
;; through the command given in `org-tj-process-command'.

;; Return a list of reports."
;;   (let* ((full-name (file-truename file))
;; 	 (out-dir
;; 	  (expand-file-name
;; 	   org-tj-reports-directory (file-name-directory file)))
;; 	 errors)
;;     (message (format "Processing TaskJuggler file %s..." file))
;;     (save-window-excursion
;;       (let ((outbuf (get-buffer-create "*Org Taskjuggler Output*")))
;; 	(unless (file-directory-p out-dir)
;; 	  (make-directory out-dir t))
;; 	(with-current-buffer outbuf (erase-buffer))
;; 	(shell-command
;; 	 (replace-regexp-in-string
;; 	  "%f" (shell-quote-argument full-name)
;; 	  (replace-regexp-in-string
;; 	   "%o" (shell-quote-argument out-dir)
;; 	   org-tj-process-command t t) t t) outbuf)
;; 	;; Collect standard errors from output buffer.
;; 	(setq errors (org-tj--collect-errors outbuf)))
;;       (if (not errors)
;; 	  (message "Process completed.")
;; 	(error (format "TaskJuggler failed with errors: %s" errors))))
;;     (file-expand-wildcards (format "%s/*.html" out-dir))))

(provide 'org-tj)
;;; org-tj.el ends here
