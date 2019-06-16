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

(defun org-tj--assign-names (headlines)
  (cl-labels
      ((filter-headlines
        (hs)
        (-when-let (filtered-hs (org-tj--filter-headlines hs))
          (->> (--map (filter-headlines (org-tj--subheadlines it))
                      filtered-hs)
               (apply #'append)
               (append filtered-hs)))))
    (let ((filtered-headlines (filter-headlines headlines)))
      (->> (-map #'org-tj--get-name filtered-headlines)
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

;;; headline attributes

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

(defun org-tj--format-attributes (attribute-alist)
  (cl-flet
      ((format-attr
        (cell)
        (let ((key (symbol-name (car cell)))
              (val (cdr cell)))
          (cond
           ((null val) key)
           ((listp val) (-some->> val
                                  (--map (format "%s %s" key it))
                                  (s-join "\n")))
           (val (format "%s %s" key val))
           (t (error "Formatting error."))))))
    ;; TODO this is redundant
    (-some->> attribute-alist
              (-map #'format-attr)
              (s-join "\n")
              (org-tj--indent-string)
              (format "{\n%s\n}"))))

(defun org-tj--build-declaration (attr-table headline pd)
  "Return a task declaration.

TASK is a headline.  INFO is a plist used as a communication
channel.

All valid attributes from TASK are inserted.  If TASK defines
a property \"task_id\" it will be used as the id for this task.
Otherwise it will use the ID property.  If neither is defined
a unique id will be associated to it."
  (let ((id (->> (org-tj--proc-data-ids pd)
                 (org-tj--get-id headline)))
        (name (format "\"%s\"" (org-tj--get-name headline)))
        (attrs
         (--> (org-tj--build-attributes attr-table headline pd)
              ;; TODO how to generalize this?
              ;; (if (not (assoc 'allocate it)) it
              ;;   (cons '(purge . "allocate") it))
              (org-tj--format-attributes it))))
    (s-join " " (list id name attrs))))

(defun org-tj--build-declarations (type pd)
  (let ((attr-table (alist-get type org-tj--attribute-alist))
        (headlines (--> (format "org-tj--proc-data-%ss" type)
                        (intern it)
                        (funcall it pd))))
    (--map (org-tj--build-declaration attr-table it pd) headlines)))

(defun org-tj--get-id (headline ids)
  "Return id for task or resource ITEM and list of IDS."
  (cdr (assq headline ids)))

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

(defun org-tj--get-inner-declaration (type headline pd)
  (let ((attr-table (alist-get type org-tj--attribute-alist)))
    (->>
     (org-tj--subheadlines headline)
     (--remove (member org-tj-ignore-tag (org-element-property :tags it)))
     (--map (org-tj--build-declaration attr-table it pd)))))

(defun org-tj--get-inner-reports (type headline pd)
  (let ((attr-table (alist-get type org-tj--attribute-alist))
        (kind (->> type (symbol-name) (s-chop-suffix "report"))))
    (->>
     (org-tj--subheadlines headline)
     (--remove (member org-tj-ignore-tag (org-element-property :tags it)))
     (--filter (equal kind (org-tj--get-report-kind it)))
     (--map (org-tj--build-declaration attr-table it pd)))))

;;; dependencies

(defun org-tj--resolve-dependencies (task tree pd)
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
            (--> (org-tj--proc-data-tasks pd)
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

(defun org-tj--format-dependencies (dependencies task ids)
  "Format DEPENDENCIES to match TaskJuggler syntax.
DEPENDENCIES is list of dependencies for TASK, as returned by
`org-tj-resolve-depedencies'.  TASK is a headline.
INFO is a plist used as a communication channel.  Return value
doesn't include leading \"depends\"."
  (let* ((dep-list (-some->> (org-element-property :TJ3_DEPENDS task)
                             (s-split ",")))
         (block-list (-some->> (org-element-property :BLOCKER task)
                               (s-split ",")))
         (dep-alist
          (->> (append dep-list block-list)
               (-non-nil)
               (--map (let* ((slice (s-split-up-to " " it 1))
                             (dep (nth 0 slice))
                             (attrs (nth 1 slice)))
                        (if attrs (cons dep attrs) dep))))))
    (cl-labels
        ((get-parent-ids
          (headline)
          (-when-let (parent (org-export-get-parent headline))
            (-when-let (id (org-tj--get-id parent ids))
              (cons id (get-parent-ids parent)))))
         (get-path
          (dep)
          (let* ((id (org-tj--get-id dep ids))
                 (parent-ids (reverse (get-parent-ids dep)))
                 (path (s-join "." (append parent-ids (list id)))))
            (-if-let (attributes (alist-get id dep-alist nil nil #'equal))
                (format "%s %s" path attributes)
              path))))
      (-some->> (-map #'get-path dependencies)
                (s-join ", ")))))

(defun org-tj--get-depends (headline pd)
  (let ((tree (org-tj--proc-data-tree pd))
        (ids (org-tj--proc-data-ids pd)))
    (-when-let (depends (org-tj--resolve-dependencies headline tree pd))
      (org-tj--format-dependencies depends headline ids))))

;;; rich text conversion

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

(defun org-tj--get-rich-text (attribute headline _pd)
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

;;; project declaration

(defun org-tj--get-kw-list (pd key &optional sep)
  (let ((sep (cond
              ((numberp sep) (concat ",\n" (s-repeat sep " ")))
              ((stringp sep) sep)
              (t ", "))))
    (-some--> (org-tj--proc-data-keywords pd)
              (--filter (equal key (car it)) it)
              (-map #'cdr it)
              (s-join sep it))))

(defun org-tj--get-kw (pd key &optional default)
  (-when-let (kws (org-tj--proc-data-keywords pd))
      (alist-get key kws default nil #'equal)))


(defun org-tj--get-project-id (pd)
  (let* ((info (org-tj--proc-data-info pd))
         (kws (org-tj--proc-data-keywords pd))
         (alt-id (--> (plist-get info :input-buffer)
                      (f-no-ext it)
                      ;; TODO this is likely incomplete
                      (s-replace "-" "_" it))))
    (alist-get "ID" kws alt-id nil #'equal)))

(defun org-tj--get-project-version (pd)
  (->> (org-tj--get-kw pd "VERSION" org-tj-default-project-version)
       (format "\"%s\"")))

(defun org-tj--get-project-name (pd)
  (->> (org-tj--get-kw pd "NAME")
       (format "\"%s\"")))

(defun org-tj--get-project-start (pd)
  ;; TODO the first task may not have a start date
  ;; TODO convert org timestamps to tj3 format
  (let ((default (or (-some->>
                      (org-tj--proc-data-tasks pd)
                      (car)
                      (org-tj--get-start))
                     (format-time-string "%Y-%m-%d"))))
    (org-tj--get-kw pd "START" default)))

(defun org-tj--get-project-end (pd)
  ;; TODO the first task may not have an date
  ;; TODO convert org timestamps to tj3 format
  (let ((default
          (or (-some->>
               (org-tj--proc-data-tasks pd)
               (car)
               (org-tj--get-end)
               (format "- %s"))
              (format "+%sd" org-tj-default-project-duration))))
    (org-tj--get-kw pd "END" default)))

(defun org-tj--format-attrs (attrs)
  (-some->> attrs
            (--map (s-join " " it))
            (s-join "\n")
            (org-tj--indent-string)
            (format "{\n%s\n}")))

(defun org-tj--get-project-attributes (pd)
  ;; TODO set the default for this
  (let* ((kw-attrs
          (->> (org-tj--proc-data-keywords pd)
               (--filter (equal "ATTRIBUTE" (car it)))
               (-map #'cdr)
               (--map (let ((s (s-split-up-to " " it 1 t)))
                        (cons (car s) (-drop 1 s))))))
         (default-attrs
           (--> org-tj-default-attributes
                (--remove (assoc-string (car it) kw-attrs) it))))
    (org-tj--format-attrs (append kw-attrs default-attrs))))

(defun org-tj--get-copyright (pd)
  ;; TODO add default
  (org-tj--get-kw pd "COPYRIGHT"))

(defun org-tj--get-balance (pd)
  ;; TODO add default
  (org-tj--get-kw pd "BALANCE"))

(defun org-tj--get-rate (pd)
  ;; TODO add default
  (org-tj--get-kw pd "RATE"))

(defun org-tj--get-navigator (pd)
  (-when-let (nav (-some-->
                   (org-tj--proc-data-keywords pd)
                   (alist-get "NAVIGATOR" it "" nil #'equal)
                   (s-split " " it t)))
    (cl-case (length nav)
      (1 (nth 0 nav))
      (2 (format "%s { %s }" (nth 0 nav) (nth 1 nav)))
      (t (error "Invalid navbar specification: %s" nav)))))

(defun org-tj--get-vacation (pd)
  (->> (org-tj--proc-data-keywords pd)
       (--filter (equal (car it) "VACATION"))
       (-map #'cdr)))

(defun org-tj--get-flags (pd)
  (org-tj--get-kw-list pd "FLAG" 6))

(defun org-tj--get-leaves (pd)
  (org-tj--get-kw-list pd "LEAVE" 7))

(defun org-tj--get-limit (pd)
  (-some->> (org-tj--get-kw-list pd "LIMIT" " ") (format "{ %s }")))

(defun org-tj--get-project (pd)
  (->>
   (list 
    (org-tj--get-project-id pd)
    (org-tj--get-project-name pd)
    (org-tj--get-project-version pd)
    (org-tj--get-project-start pd)
    (org-tj--get-project-end pd)
    (org-tj--get-project-attributes pd))
   (s-join " ")))

(defconst org-tj--attribute-alist
  `((account
     ((account . ,(-partial #'org-tj--get-inner-declaration 'account))
      (aggregate . :TJ3_AGGREGATE)
      (credits . :TJ3_CREDITS)
      (flags . :TJ3_FLAGS)))
    (shift
     ((leaves . :TJ3_LEAVES)
      (replace . :TJ3_REPLACE)
      (shift . ,(-partial #'org-tj--get-inner-declaration 'shift))
      (timezone . :TJ3_TIMEZONE)
      (vacation . :TJ3_VACATION)
      (workinghours . :TJ3_WORKINGHOURS)))
    (resource
     ;; leaveallowance and supplement not implemented
     ((booking . :TJ3_BOOKING)
      (chargeset . :TJ3_CHARGESET)
      (efficiency . :TJ3_EFFICIENCY)
      (email . :TJ3_EMAIL)
      (fail . :TJ3_FAIL)
      (flag . :TJ3_FLAGS)
      (journalentry . :TJ3_JOURNALENTRY)
      (leaves . :TJ3_LEAVES)
      (limits . :TJ3_LIMITS)
      (managers . :TJ3_MANAGERS)
      (purge . :TJ3_PURGE)
      (rate . :TJ3_RATE)
      (resource . ,(-partial #'org-tj--get-inner-declaration 'resource))
      (shifts . :TJ3_SHIFTS)
      (vacation . :TJ3_VACATION)
      (warn . :TJ3_WARN)
      (workinghours . :TJ3_WORKINGHOURS)))
    (task
     ;; adopt, effortdone, effortleft, and supplement not
     ;; implemented
     ((allocate . :TJ3_ALLOCATE)
      (booking . :TJ3_BOOKING)
      (charge . :TJ3_CHARGE)
      (chargeset . :TJ3_CHARGESET)
      (complete . org-tj--get-complete)
      (depends . org-tj--get-depends)
      (duration . :TJ3_DURATION)
      (effort . org-tj--get-effort)
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
      (shift . :TJ3_SHIFTS)
      (start . org-tj--get-start)
      (task . ,(-partial #'org-tj--get-inner-declaration 'task))
      (warn . :TJ3_WARN)))
    (report
     ;; accountreport, auxdir, export, opennodes, and tracereport
     ;; not implemented
     ((accountroot . :TJ3_ACCOUNTROOT)
      (balance . :TJ3_BALANCE)
      (caption . ,(-partial #'org-tj--get-rich-text "caption"))
      (center . ,(-partial #'org-tj--get-rich-text "center"))
      (columns . :TJ3_COLUMNS)
      (currencyformat . :TJ3_CURRENCYFORMAT)
      (end . ,(-partial #'org-tj--get-rich-text "end"))
      (epilog . ,(-partial #'org-tj--get-rich-text "epilog"))
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
      (period . :TJ3_PERIOD)
      (prolog . ,(-partial #'org-tj--get-rich-text "prolog"))
      (purge . :TJ3_PURGE)
      (rawhtmlhead . :TJ3_RAWHTMLHEAD)
      (resourcereport . ,(-partial #'org-tj--get-inner-reports 'resourcereport))
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
      (taskreport . ,(-partial #'org-tj--get-inner-reports 'taskreport))
      (taskroot . :TJ3_TASKROOT)
      (textreport . ,(-partial #'org-tj--get-inner-reports 'textreport))
      (timeformat . :TJ3_TIMEFORMAT)
      (timezone . :TJ3_TIMEZONE)
      (title . :TJ3_TITLE)
      (width . :TJ3_WIDTH)))))

(defconst org-tj--attribute-alist
      (let
          ((account
            `((account . ,(-partial #'org-tj--get-inner-declaration 'account))
              (aggregate . :TJ3_AGGREGATE)
              (credits . :TJ3_CREDITS)
              (flags . :TJ3_FLAGS)))
           (shift
            `((leaves . :TJ3_LEAVES)
              (replace . :TJ3_REPLACE)
              (shift . ,(-partial #'org-tj--get-inner-declaration 'shift))
              (timezone . :TJ3_TIMEZONE)
              (vacation . :TJ3_VACATION)
              (workinghours . :TJ3_WORKINGHOURS)))
           (resource
            ;; leaveallowance and supplement not implemented
            `((booking . :TJ3_BOOKING)
              (chargeset . :TJ3_CHARGESET)
              (efficiency . :TJ3_EFFICIENCY)
              (email . :TJ3_EMAIL)
              (fail . :TJ3_FAIL)
              (flag . :TJ3_FLAGS)
              (journalentry . :TJ3_JOURNALENTRY)
              (leaves . :TJ3_LEAVES)
              (limits . :TJ3_LIMITS)
              (managers . :TJ3_MANAGERS)
              (purge . :TJ3_PURGE)
              (rate . :TJ3_RATE)
              (resource . ,(-partial #'org-tj--get-inner-declaration 'resource))
              (shifts . :TJ3_SHIFTS)
              (vacation . :TJ3_VACATION)
              (warn . :TJ3_WARN)
              (workinghours . :TJ3_WORKINGHOURS)))
           (task
            ;; adopt, effortdone, effortleft, and supplement not
            ;; implemented
            `((allocate . :TJ3_ALLOCATE)
              (booking . :TJ3_BOOKING)
              (charge . :TJ3_CHARGE)
              (chargeset . :TJ3_CHARGESET)
              (complete . org-tj--get-complete)
              (depends . org-tj--get-depends)
              (duration . :TJ3_DURATION)
              (effort . org-tj--get-effort)
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
              (shift . :TJ3_SHIFTS)
              (start . org-tj--get-start)
              (task . ,(-partial #'org-tj--get-inner-declaration 'task))
              (warn . :TJ3_WARN)))
           (report
            ;; accountreport, auxdir, export, opennodes, and tracereport
            ;; not implemented
            `((accountroot . :TJ3_ACCOUNTROOT)
              (balance . :TJ3_BALANCE)
              (caption . ,(-partial #'org-tj--get-rich-text "caption"))
              (center . ,(-partial #'org-tj--get-rich-text "center"))
              (columns . :TJ3_COLUMNS)
              (currencyformat . :TJ3_CURRENCYFORMAT)
              (end . ,(-partial #'org-tj--get-rich-text "end"))
              (epilog . ,(-partial #'org-tj--get-rich-text "epilog"))
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
              (period . :TJ3_PERIOD)
              (prolog . ,(-partial #'org-tj--get-rich-text "prolog"))
              (purge . :TJ3_PURGE)
              (rawhtmlhead . :TJ3_RAWHTMLHEAD)
              (resourcereport . ,(-partial #'org-tj--get-inner-reports 'resourcereport))
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
              (taskreport . ,(-partial #'org-tj--get-inner-reports 'taskreport))
              (taskroot . :TJ3_TASKROOT)
              (textreport . ,(-partial #'org-tj--get-inner-reports 'textreport))
              (timeformat . :TJ3_TIMEFORMAT)
              (timezone . :TJ3_TIMEZONE)
              (title . :TJ3_TITLE)
              (width . :TJ3_WIDTH))))
        `((account . ,account)
          (shift . ,shift)
          (resource . ,resource)
          (task . ,task)
          (textreport . ,report)
          (taskreport . ,report)
          (resourcereport . ,report))))

(defconst org-tj--toplevel-alist
  `((project . org-tj--get-project)
    (copyright . org-tj--get-copyright)
    (vacation . org-tj--get-vacation)
    (flags . org-tj--get-flags)
    (account . ,(-partial #'org-tj--build-declarations 'account))
    (balance . org-tj--get-balance)
    (leaves . org-tj--get-leaves)
    (rate . org-tj--get-rate)
    (limits . org-tj--get-limit)
    (shift . ,(-partial #'org-tj--build-declarations 'shift))
    (resource . ,(-partial #'org-tj--build-declarations 'resource))
    (task . ,(-partial #'org-tj--build-declarations 'task))
    (navigator . org-tj--get-navigator)
    (textreport . ,(-partial #'org-tj--build-declarations 'textreport))
    (taskreport . ,(-partial #'org-tj--build-declarations 'taskreport))
    (resourcereport . ,(-partial #'org-tj--build-declarations 'resourcereport))))

(defun org-tj--format-kws (pd)
  (cl-flet
      ((process
        (cell)
        (let ((name (car cell))
              (fun (cdr cell)))
          (-when-let (value (funcall fun pd))
            (if (stringp value) (format "%s %s" name value)
              (->> value
                   (--map (format "%s %s" name it))
                   (s-join "\n")))))))
    (->> org-tj--toplevel-alist
         (-map #'process)
         (-non-nil))))

;;; Translator Functions

(defun org-tj--subheadlines (headline)
  "Return subheadings under HEADLINE if any."
  (let ((contents (org-element-contents headline)))
    (if (assoc 'section contents) (cdr contents) contents)))

(defun org-tj--get-report-kind (headline)
  (-if-let (kind (org-element-property :TJ3_REPORT_KIND headline))
    (if (member kind '("text" "task" "resource")) kind
      (error "unknown kind: %s" kind))
    (->> (org-element-property :raw-value)
         (error "kind not specified for headline \"%s\""))))

;;; process data structure

(cl-defstruct (org-tj--proc-data
               (:copier nil))
  "A collection of processed data from the export tree."
  info
  tree
  keywords
  ids
  accounts
  shifts
  resources
  tasks
  textreports
  taskreports
  resourcereports)

(defun org-tj--get-keywords (tree)
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

(defun org-tj--get-reports-kind (reports kind)
  (--filter (equal kind (org-tj--get-report-kind it)) reports))

(defun org-tj--get-tagged-headlines (tree tag)
  ;; TODO what if we have tags in the subtree, we don't need those
  (org-element-map tree 'headline
    (lambda (hl) (when (member tag (org-element-property :tags hl)) hl))))

(defun org-tj--get-task-headlines (tree)
  "Return list of headlines marked with `org-tj-project-tag'.
Only return the toplevel heading in a marked subtree. TREE is the
parse tree of the buffer."
  (org-tj--get-tagged-headlines tree org-tj-project-tag))

(defun org-tj--get-account-headlines (tree)
  "Return list of headlines marked with `org-tj-account-tag'.
Only return the toplevel heading in a marked subtree. TREE is the
parse tree of the buffer."
  (org-tj--get-tagged-headlines tree org-tj-account-tag))

(defun org-tj--get-shift-headlines (tree)
  "Return list of headlines marked with `org-tj-project-tag'.
Only return the toplevel heading in a marked subtree. TREE is the
parse tree of the buffer."
  (org-tj--get-tagged-headlines tree org-tj-shift-tag))

(defun org-tj--get-resource-headlines (tree)
  "Return list of all org taskjuggler resource headlines."
  (org-tj--get-tagged-headlines tree org-tj-resource-tag))

(defun org-tj--get-report-headlines (tree)
  "Return reports tree from TREE."
  (org-tj--get-tagged-headlines tree org-tj-report-tag))

(defun org-tj--add-allocates (tasks)
  (cl-flet ((add-allocates
             (task)
             (org-element-put-property
              task :TJ3_ALLOCATE (user-login-name))))
    (-map #'add-allocates tasks)))

(defun org-tj--make-proc-data (info)
  (let* ((tree (plist-get info :parse-tree))
         (tasks (org-tj--get-task-headlines tree))
         (accounts (org-tj--get-account-headlines tree))
         (shifts (org-tj--get-shift-headlines tree))
         (resources (org-tj--get-resource-headlines tree))
         (reports (org-tj--get-report-headlines tree))
         (textreports (org-tj--get-reports-kind reports "text"))
         (taskreports (org-tj--get-reports-kind reports "task"))
         (resourcereports (org-tj--get-reports-kind reports "resource")))
    (make-org-tj--proc-data
     :info info
     :tree tree
     :keywords (org-tj--get-keywords tree)
     :ids (append (org-tj--assign-global-ids accounts)
                  (org-tj--assign-global-ids shifts)
                  (org-tj--assign-global-ids resources)
                  (org-tj--assign-local-ids tasks)
                  (org-tj--assign-local-ids reports))
     :accounts accounts
     :shifts shifts
     :resources resources
     :tasks (if resources tasks (org-tj--add-allocates tasks))
     :textreports textreports
     :taskreports  taskreports
     :resourcereports resourcereports)))

(defun org-tj--build-tjp-file (_contents info)
  "Build full contents of a taskjuggler project file.
INFO is a plist holding export options. Return formatted string in 
taskjuggler syntax."
  (->> (org-tj--make-proc-data info)
       (org-tj--format-kws)
       (s-join "\n\n")))

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
