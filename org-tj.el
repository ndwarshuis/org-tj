;;; org-tj.el --- Org Mode TaskJuggler 3  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Nate Dwarshuis

;; Author: Nate Dwarshuis <natedwarshuis@gmail.com>
;; Keywords: outlines
;; Homepage: https://github.com/ndwarshuis/org-tj3
;; Package-Requires: ((emacs "25") (dash "2.15") (helm "3.2"))
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
(require 'dash)
(require 'org-element)
(require 'subr-x)
(require 'helm)
(require 'ox)
(eval-when-compile (require 'cl))

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

(defcustom org-tj-project-tag "taskjuggler_project"
  "Tag marking project's tasks.
This tag is used to find the tree containing all the tasks for
the project."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-tj-resource-tag "taskjuggler_resource"
  "Tag marking project's resources.
This tag is used to find the tree containing all the resources
for the project."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-tj-report-tag "taskjuggler_report"
  "Tag marking project's reports.
This tag is used to find the tree containing all the reports for
the project."
  :group 'org-export-taskjuggler
  :type 'string)

(defcustom org-tj-target-version 3.0
  "Which version of TaskJuggler the exporter is targeting.
By default a project plan is exported which conforms to version
3.x of TaskJuggler.  For a project plan that is compatible with
versions of TaskJuggler older than 3.0 set this to 2.4.

If you change this variable be sure to also change
`org-tj-default-reports' as the format of reports has
changed considerably between version 2.x and 3.x of TaskJuggler"
  :group 'org-export-taskjuggler
  :type 'number)

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

include reports.tji

These default are made to work with tj3.  If you are targeting
TaskJuggler 2.4 (see `org-tj-target-version') please
change these defaults to something like the following:

taskreport \"Gantt Chart\" {
  headline \"Project Gantt Chart\"
  columns hierarchindex, name, start, end, effort, duration, completed, chart
  timeformat \"%Y-%m-%d\"
  hideresource 1
  loadunit shortauto
}

resourcereport \"Resource Graph\" {
  headline \"Resource Allocation Graph\"
  columns no, name, utilization, freeload, chart
  loadunit shortauto
  sorttasks startup
  hidetask ~isleaf()
}"
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

(defcustom org-tj-valid-task-attributes
  '(account start note duration endbuffer endcredit end
	    flags journalentry length limits maxend maxstart minend
	    minstart period reference responsible scheduling
	    startbuffer startcredit statusnote chargeset charge)
  "Valid attributes for Taskjuggler tasks.
If one of these appears as a property for a headline, it will be
exported with the corresponding task.

Note that multiline properties are not supported, so attributes
like note or journalentry have to be on a single line."
  :group 'org-export-taskjuggler)

(defcustom org-tj-valid-project-attributes
  '(timingresolution timezone alertlevels currency currencyformat
  dailyworkinghours extend includejournalentry now numberformat
  outputdir scenario shorttimeformat timeformat trackingscenario
  weekstartsmonday weekstartssunday workinghours
  yearlyworkingdays)
  "Valid attributes for Taskjuggler project.
If one of these appears as a property for a headline that is a
project definition, it will be exported with the corresponding
task. Attribute 'timingresolution' should be the first in the
list."
  :group 'org-export-taskjuggler)

(defcustom org-tj-valid-resource-attributes
  '(limits vacation shift booking efficiency journalentry rate
	   workinghours flags)
  "Valid attributes for Taskjuggler resources.
If one of these appears as a property for a headline, it will be
exported with the corresponding resource."
  :group 'org-export-taskjuggler)

(defcustom org-tj-valid-report-attributes
  '(headline columns definitions timeformat hideresource hidetask
	     loadunit sorttasks formats period)
  "Valid attributes for Taskjuggler reports.
If one of these appears as a property for a headline, it will be
exported with the corresponding report."
  :group 'org-export-taskjuggler)

(defcustom org-tj-process-command
  "tj3 --silent --no-color --output-dir %o %f"
  "Command to process a Taskjuggler file.
The command will be given to the shell as a command to process a
Taskjuggler file.  \"%f\" in the command will be replaced by the
full file name, \"%o\" by the reports directory (see
`org-tj-reports-directory').

If you are targeting Taskjuggler 2.4 (see
`org-tj-target-version') this setting is ignored."
  :group 'org-export-taskjuggler)

(defcustom org-tj-reports-directory "reports"
  "Default directory to generate the Taskjuggler reports in.
The command `org-tj-process-command' generates the
reports and associated files such as CSS inside this directory.

If the directory is not an absolute path it is relative to the
directory of the exported file.  The directory is created if it
doesn't exist.

If you are targeting Taskjuggler 2.4 (see
`org-tj-target-version') this setting is ignored."
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


;;; Back-End Definition

(org-export-define-backend 'taskjuggler
  '((template . org-tj-project-plan))
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
		(org-tj-export-process-and-open s v))))))
  ;; TODO this seems silly to me...
  ;; This property will be used to store unique ids in communication
  ;; channel.  Ids will be retrieved with `org-tj-get-id'.
  :options-alist '((:taskjuggler-unique-ids nil nil nil)))


;;; Unique IDs

(defun org-tj-assign-task-ids (tasks info)
  "Assign a unique ID to each task in TASKS.
TASKS is a list of headlines.  INFO is a plist used as a
communication channel.  Return value is an alist between
headlines and their associated ID.  IDs are hierarchical, which
means they only need to be unique among the task siblings."
  ;; TODO this makes all ids globally unique, not bad but might be
  ;; a reason the original didn't do it seeing as this is way easier?
  (let* ((hls (->> tasks
                   (--map (org-element-map it 'headline #'identity))
                   (apply #'append)))
    (ids (->> hls
         (-reductions-from
          (lambda (a b)
            (let ((unique-id (org-tj--build-unique-id b a)))
              (append a (list unique-id))))
          nil)
         (-drop 1)
         (-map #'-last-item))))
    (--zip-with (cons it other) hls ids)))
  ;; this function doesn't work with lexical scoping
  ;; (let* (alist
  ;;    build-id			; For byte-compiler.
  ;;        (build-id
  ;;         (lambda (tasks local-ids)
  ;;           (org-element-map tasks 'headline
  ;;             (lambda (task)
  ;;               (let ((id (org-tj--build-unique-id task local-ids)))
  ;;                 (push id local-ids)
  ;;                 (push (cons task id) alist)
  ;;                 (funcall build-id (org-element-contents task) nil)))
  ;;             info nil 'headline))))
  ;;   (funcall build-id tasks nil)
  ;;   alist))

(defun org-tj-assign-resource-ids (resources info)
  "Assign a unique ID to each resource within RESOURCES.
RESOURCES is a list of headlines.  INFO is a plist used as a
communication channel.  Return value is an alist between
headlines and their associated ID."
  (let (ids)
    (org-element-map resources 'headline
      (lambda (resource)
        (let ((id (org-tj--build-unique-id resource ids)))
          (push id ids)
          (cons resource id)))
      info)))


;;; Accessors

(defun org-tj-get-project (info)
  "Return project in parse tree.
INFO is a plist used as a communication channel.  First headline
in buffer with `org-tj-project-tag' defines the project.
If no such task is defined, pick the first headline in buffer.
If there is no headline at all, return nil."
  (let ((tree (plist-get info :parse-tree)))
    (or (org-element-map tree 'headline
	  (lambda (hl)
	    (and (member org-tj-project-tag
			 (org-export-get-tags hl info))
		 hl))
	  info t)
	(org-element-map tree 'headline 'identity info t))))

(defun org-tj-get-id (item info)
  "Return id for task or resource ITEM.
ITEM is a headline.  INFO is a plist used as a communication
channel.  Return value is a string."
  (cdr (assq item (plist-get info :taskjuggler-unique-ids))))

(defun org-tj-get-name (item)
  "Return name for task or resource ITEM.
ITEM is a headline.  Return value is a string."
  ;; Quote double quotes in name.
  (replace-regexp-in-string
   "\"" "\\\"" (org-element-property :raw-value item) t t))

(defun org-tj-get-start (item)
  "Return start date for task or resource ITEM.
ITEM is a headline.  Return value is a string or nil if ITEM
doesn't have any start date defined."
  (let ((scheduled (org-element-property :scheduled item)))
    (or
     (and scheduled (org-timestamp-format scheduled "%Y-%02m-%02d"))
     (and (memq 'start org-tj-valid-task-attributes)
	  (org-element-property :START item)))))

(defun org-tj-get-end (item)
  "Return end date for task or resource ITEM.
ITEM is a headline.  Return value is a string or nil if ITEM
doesn't have any end date defined."
  (let ((deadline (org-element-property :deadline item)))
    (and deadline (org-timestamp-format deadline "%Y-%02m-%02d"))))


;;; Internal Functions

(defun org-tj--indent-string (s)
  "Indent string S by 2 spaces.
Return new string.  If S is the empty string, return it."
  (if (equal "" s) s (replace-regexp-in-string "^ *\\S-" "  \\&" s)))

(defun org-tj--build-attributes (item attributes)
  "Return attributes string for ITEM.
ITEM is a project, task, resource or report headline.  ATTRIBUTES
is a list of symbols representing valid attributes for ITEM."
  (mapconcat
   (lambda (attribute)
     (let ((value (org-element-property
                   (intern (upcase (format ":%s" attribute)))
                   item)))
       (and value (format "%s %s\n" attribute value))))
   (remq nil attributes) ""))

(defun org-tj--build-unique-id (item unique-ids)
  "Return a unique id for a given task or a resource.
ITEM is an `headline' type element representing the task or
resource.  Its id is derived from its name and made unique
against UNIQUE-IDS.  If the (downcased) first token of the
headline is not unique try to add more (downcased) tokens of the
headline or finally add more underscore characters (\"_\")."
  (let ((id (org-string-nw-p (org-element-property :TASK_ID item))))
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


;;; Dependencies

(defun org-tj-resolve-dependencies (task info)
  "Return a list of all tasks TASK depends on.
TASK is a headline.  INFO is a plist used as a communication
channel."
  (let ((deps-ids
         ;; Get all dependencies specified in BLOCKER and DEPENDS task
         ;; properties.  Clean options from them.
         (let ((deps (concat (org-element-property :BLOCKER task)
                             (org-element-property :DEPENDS task))))
           (and deps
                (split-string (replace-regexp-in-string "{.*?}" "" deps)
			      "[ ,]* +"))))
        depends)
    (when deps-ids
      ;; Find tasks with :task_id: property matching id in DEPS-IDS.
      ;; Add them to DEPENDS.
      (let* ((project (org-tj-get-project info))
             (tasks (if org-tj-keep-project-as-task project
                      (org-element-contents project))))
        (setq depends
              (org-element-map tasks 'headline
                (lambda (task)
                  (let ((task-id (or (org-element-property :TASK_ID task)
				     (org-element-property :ID task))))
                    (and task-id (member task-id deps-ids) task)))
                info)))
      ;; Check BLOCKER and DEPENDS properties.  If "previous-sibling"
      ;; belongs to DEPS-ID, add it to DEPENDS.
      (when (and (member-ignore-case "previous-sibling" deps-ids)
                 (not (org-export-first-sibling-p task info)))
        (let ((prev (org-export-get-previous-element task info)))
          (and (not (memq prev depends)) (push prev depends)))))
    ;; Check ORDERED status of parent.
    (let ((parent (org-export-get-parent task)))
      (when (and parent
                 (org-element-property :ORDERED parent)
                 (not (org-export-first-sibling-p task info)))
        (push (org-export-get-previous-element task info) depends)))
    ;; Return dependencies.
    depends))

(defun org-tj-format-dependencies (dependencies task info)
  "Format DEPENDENCIES to match TaskJuggler syntax.
DEPENDENCIES is list of dependencies for TASK, as returned by
`org-tj-resolve-depedencies'.  TASK is a headline.
INFO is a plist used as a communication channel.  Return value
doesn't include leading \"depends\"."
  (let* ((dep-str (concat (org-element-property :BLOCKER task)
			  " "
			  (org-element-property :DEPENDS task)))
	 (get-path
	  (lambda (dep)
	    ;; Return path to DEP relatively to TASK.
	    (let ((parent (org-export-get-parent task))
		  (exclamations 1)
		  (option
		   (let ((id (org-element-property :TASK_ID dep)))
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
		(push (org-tj-get-id dep info) path)
		(setq dep (org-export-get-parent dep)))
	      ;; Return full path.  Add dependency options, if any.
	      (concat (make-string exclamations ?!)
		      (mapconcat 'identity path ".")
		      (and option (concat " " option)))))))
    ;; Return dependencies string, without the leading "depends".
    (mapconcat (lambda (dep) (funcall get-path dep)) dependencies ", ")))


;;; Translator Functions

(defun org-tj-project-plan (contents info)
  "Build TaskJuggler project plan.
CONTENTS is ignored.  INFO is a plist holding export options.
Return complete project plan as a string in TaskJuggler syntax."
  (let* ((tree (plist-get info :parse-tree))
         (project (or (org-tj-get-project info)
                      (error "No project specified"))))
    (concat
     ;; 1. Insert header.
     (org-element-normalize-string org-tj-default-global-header)
     ;; 2. Insert project.
     (org-tj--build-project project info)
     ;; 3. Insert global properties.
     (org-element-normalize-string org-tj-default-global-properties)
     ;; 4. Insert resources.  Provide a default one if none is
     ;;    specified.
     (let ((main-resources
            ;; Collect contents from various trees marked with
            ;; `org-tj-resource-tag'.  Only gather top level
            ;; resources.
            (apply 'append
                   (org-element-map tree 'headline
                     (lambda (hl)
                       (and (member org-tj-resource-tag
                                    (org-export-get-tags hl info))
                            (org-element-map (org-element-contents hl) 'headline
                              'identity info nil 'headline)))
                     info nil 'headline))))
       ;; Assign a unique ID to each resource.  Store it under
       ;; `:taskjuggler-unique-ids' property in INFO.
       (setq info
             (plist-put info :taskjuggler-unique-ids
                        (org-tj-assign-resource-ids
                         main-resources info)))
       (concat
        (if main-resources
            (mapconcat
             (lambda (resource) (org-tj--build-resource resource info))
             main-resources "")
          (format "resource %s \"%s\" {\n}\n" (user-login-name) user-full-name))
        ;; 5. Insert tasks.
        (let ((main-tasks
               ;; If `org-tj-keep-project-as-task' is
               ;; non-nil, there is only one task.  Otherwise, every
               ;; direct children of PROJECT is a top level task.
               (if org-tj-keep-project-as-task (list project)
                 (or (org-element-map (org-element-contents project) 'headline
                       'identity info nil 'headline)
                     (error "No task specified")))))
          ;; Assign a unique ID to each task.  Add it to
          ;; `:taskjuggler-unique-ids' property in INFO.
          (setq info
                (plist-put info :taskjuggler-unique-ids
                           (append
                            (org-tj-assign-task-ids main-tasks info)
                            (plist-get info :taskjuggler-unique-ids))))
          ;; If no resource is allocated among tasks, allocate one to
          ;; the first task.
          (unless (org-element-map main-tasks 'headline
                    (lambda (task) (org-element-property :ALLOCATE task))
                    info t)
            (org-element-put-property
             (car main-tasks) :ALLOCATE
             (or (org-tj-get-id (car main-resources) info)
                 (user-login-name))))
          (mapconcat
           (lambda (task) (org-tj--build-task task info))
           main-tasks ""))
        ;; 6. Insert reports.  If no report is defined, insert default
        ;;    reports.
        (let ((main-reports
               ;; Collect contents from various trees marked with
               ;; `org-tj-report-tag'.  Only gather top level
               ;; reports.
               (apply 'append
                      (org-element-map tree 'headline
                        (lambda (hl)
                          (and (member org-tj-report-tag
                                       (org-export-get-tags hl info))
                               (org-element-map (org-element-contents hl)
                                   'headline 'identity info nil 'headline)))
                        info nil 'headline))))
          (if main-reports
              (mapconcat
               (lambda (report) (org-tj--build-report report info))
               main-reports "")
	    ;; insert title in default reports
	    (let* ((title (org-export-data (plist-get info :title) info))
		   (report-title (if (string= title "")
				     (org-tj-get-name project)
				   title)))
	      (mapconcat
	       'org-element-normalize-string
	       (mapcar
		(function
		 (lambda (report)
		   (replace-regexp-in-string "%title" report-title  report t t)))
		org-tj-default-reports) "")))))))))

(defun org-tj--build-project (project info)
  "Return a project declaration.
PROJECT is a headline.  INFO is a plist used as a communication
channel.  If no start date is specified, start today.  If no end
date is specified, end `org-tj-default-project-duration'
days from now."
  (let* ((first-line
          (format "project %s \"%s\" \"%s\" %s %s {\n"
                  (org-tj--get-project-id project info)
                  (org-tj-get-name project)
                  ;; Version is obtained through :TASKJUGGLER_VERSION:
                  ;; property or `org-tj-default-project-version'.
                  (or (org-element-property :VERSION project)
                      org-tj-default-project-version)
                  (or (org-tj-get-start project)
                      (format-time-string "%Y-%m-%d"))
                  (let ((end (org-tj-get-end project)))
                    (or (and end (format "- %s" end))
                        (format "+%sd"
                                org-tj-default-project-duration)))))
         (orig-attrs (--> (org-tj--build-attributes
                           project org-tj-valid-project-attributes)
                          (s-split "\n" it t)
                          (--map (s-split-up-to " " it 1 t) it)
                          (--map (cons (car it) (cadr it)) it)))
         (add-attrs
          (--> org-tj-default-attributes
               (--remove (assoc-string (car it) orig-attrs) it)))
         (attrs (--> orig-attrs
                     (append it add-attrs)
                     (--map (format "%s %s\n" (car it) (cdr it)) it)
                     (string-join it)
                     (org-tj--indent-string it))))
    ;; Closing project.
    (concat first-line attrs "}\n")))

(defun org-tj--build-resource (resource info)
  "Return a resource declaration.

RESOURCE is a headline.  INFO is a plist used as a communication
channel.

All valid attributes from RESOURCE are inserted.  If RESOURCE
defines a property \"resource_id\" it will be used as the id for
this resource.  Otherwise it will use the ID property.  If
neither is defined a unique id will be associated to it."
  (concat
   ;; Opening resource.
   (format "resource %s \"%s\" {\n"
           (org-tj--clean-id
            (or (org-element-property :RESOURCE_ID resource)
                (org-element-property :ID resource)
                (org-tj-get-id resource info)))
           (org-tj-get-name resource))
   ;; Add attributes.
   (org-tj--indent-string
    (org-tj--build-attributes
     resource org-tj-valid-resource-attributes))
   ;; Add inner resources.
   (org-tj--indent-string
    (mapconcat
     'identity
     (org-element-map (org-element-contents resource) 'headline
       (lambda (hl) (org-tj--build-resource hl info))
       info nil 'headline)
     ""))
   ;; Closing resource.
   "}\n"))

(defun org-tj--build-report (report info)
  "Return a report declaration.
REPORT is a headline.  INFO is a plist used as a communication
channel."
  (concat
   ;; Opening report.
   (format "%s \"%s\" {\n"
           (or (org-element-property :REPORT_KIND report) "taskreport")
           (org-tj-get-name report))
   ;; Add attributes.
   (org-tj--indent-string
    (org-tj--build-attributes
     report org-tj-valid-report-attributes))
   ;; Add inner reports.
   (org-tj--indent-string
    (mapconcat
     'identity
     (org-element-map (org-element-contents report) 'headline
       (lambda (hl) (org-tj--build-report hl info))
       info nil 'headline)
     ""))
   ;; Closing report.
   "}\n"))

(defun org-tj--build-task (task info)
  "Return a task declaration.

TASK is a headline.  INFO is a plist used as a communication
channel.

All valid attributes from TASK are inserted.  If TASK defines
a property \"task_id\" it will be used as the id for this task.
Otherwise it will use the ID property.  If neither is defined
a unique id will be associated to it."
  (let* ((allocate (org-element-property :ALLOCATE task))
         (complete
          (if (eq (org-element-property :todo-type task) 'done) "100"
            (org-element-property :COMPLETE task)))
         (depends (org-tj-resolve-dependencies task info))
         (effort (let ((property
                        (intern (concat ":" (upcase org-effort-property)))))
                   (org-element-property property task)))
         (milestone
          (or (org-element-property :MILESTONE task)
              (not (or (org-element-map (org-element-contents task) 'headline
                         'identity info t)  ; Has task any child?
                       effort
                       (org-element-property :LENGTH task)
                       (org-element-property :DURATION task)
                       (and (org-tj-get-start task)
                            (org-tj-get-end task))
                       (org-element-property :PERIOD task)))))
         (priority
          (let ((pri (org-element-property :priority task)))
            (and pri
                 (max 1 (/ (* 1000 (- org-lowest-priority pri))
                           (- org-lowest-priority org-highest-priority)))))))
    (concat
     ;; Opening task.
     (format "task %s \"%s\" {\n"
             (org-tj-get-id task info)
             (org-tj-get-name task))
     ;; Add default attributes.
     (and depends
          (format "  depends %s\n"
                  (org-tj-format-dependencies depends task info)))
     (and allocate
          (format "  purge %s\n  allocate %s\n"
                  ;; Compatibility for previous TaskJuggler versions.
                  (if (>= org-tj-target-version 3.0) "allocate"
                    "allocations")
                  allocate))
     (and complete (format "  complete %s\n" complete))
     (and effort
          (format "  effort %s\n"
                  (let* ((minutes (org-duration-to-minutes effort))
                         (hours (/ minutes 60.0)))
                    (format "%.1fh" hours))))
     (and priority (format "  priority %s\n" priority))
     (and milestone "  milestone\n")
     ;; Add other valid attributes.
     ;; TODO this can be cleaned up
     (org-tj--indent-string
      (let* ((orig-attributes (org-tj--build-attributes
                               task org-tj-valid-task-attributes))
             (start (-some->>
                     (org-tj-get-start task)
                     (format "start %s\n")))
             (end (-some->>
                   (org-tj-get-end task)
                   (format "end %s\n"))))
        (s-join "" (-non-nil (list orig-attributes start end)))))
      ;; Add inner tasks.
      (org-tj--indent-string
       (mapconcat 'identity
                  (org-element-map (org-element-contents task) 'headline
                    (lambda (hl) (org-tj--build-task hl info))
                    info nil 'headline)
                  ""))
      ;; Closing task.
      "}\n")))


;;; Interactive Functions

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

;;;###autoload
(defun org-tj-export-process-and-open (&optional subtreep visible-only)
  "Export current buffer to a TaskJuggler file, process and open it.

Export and process the file using
`org-tj-export-and-process' and open the generated
reports with a browser.

If you are targeting TaskJuggler 2.4 (see
`org-tj-target-version') the processing and display of
the reports is done using the TaskJuggler GUI."
  (interactive)
  ;; TODO remove tj2 codepath
  ;; (if (< org-tj-target-version 3.0)
  ;;     (let* ((process-name "TaskJugglerUI")
  ;;            (command
  ;;             (concat process-name " "
  ;;                     (org-tj-export nil subtreep visible-only))))
  ;;       (start-process-shell-command process-name nil command))
  (let ((reports (org-tj-export-and-process subtreep visible-only)))
    (print reports)
    (dolist (report reports)
      (org-tj-open-in-browser report))))

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


(provide 'ox-taskjuggler)

(defmacro org-tj--with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body))
           (indent 1))
  `(progn
     ,@(mapcar (lambda (adform)
                 (cons 'advice-add adform))
               adlist)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (adform)
                   `(advice-remove ,(car adform) ,(nth 2 adform)))
                 adlist))))

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

;; (defun org-tj--compile-no-report (orig-fn file)
;;   "Process FILE using ORIG-FN but don't compile to a report.
;; Instead override the `org-tj-process-command' such that FILE
;; added to the web server, and don't make the report directory as it is
;; not necessary."
;;   (let ((file (expand-file-name file)))
;;     (org-tj--with-advice
;;         ;; don't make a reports directory
;;         ((#'make-directory :override #'ignore))
;;       ;; override the original process command to load into server
;;       (let ((org-tj-process-command (org-tj--cmd 'client "add" "%f")))
;;         (funcall orig-fn file)))
;;     ;; just return the file path when done
;;     ;; the list is necessary for org-tj-export-process-and-open
;;     ;; since the code that opens the files is actually a loop that
;;     ;; expects a list
;;     (list file)))

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
       
;; (defunorg-tj--export-process-and-open-web (orig-fun &rest args)
;;   "Process ARGS using ORIG-FUN but open files in browser."
;;   (org-tj--with-advice
;;       ((#'org-open-file :override #'org-tj-open-in-browser))
;;     (apply orig-fun args)))

;; (defun org-tj--add-attributes (orig-fun &rest args)
;;   "Call ORIG-FUN with ARGS and add extra attributes to projects."
;;   ;; assume the original list is a newline-delimited string
;;   ;; break this string into cons cells of keyval pairs
;;   (let* ((orig-attributes
;;           (--> (apply orig-fun args)
;;                (s-split "\n" it t)
;;                (--map (s-split-up-to " " it 1 t) it)
;;                (--map (cons (car it) (cadr it)) it)))
;;          (add-attributes
;;           (--> org-tj-default-attributes
;;                (--remove (assoc-string (car it) orig-attributes) it))))
;;     (--> orig-attributes
;;          (append it add-attributes)
;;          (--map (format "%s %s\n" (car it) (cdr it)) it)
;;          (string-join it))))

(defun org-tj--get-project-id (project info)
  "Get the project id from PROJECT.
The id is just the 'TASK_ID' org property with 'prj_' appended.
INFO is a communication channel and is ignored"
  (let ((id (org-element-property :TASK_ID project)))
    (if id (format "prj_%s" id)
      (error "ERROR: No project id found"))))

;; (defun org-tj--add-project-attributes (orig-fun project info)
;;   "Call ORIG-FUN with PROJECT and INFO.
;; Add project attributes to PROJECT and also add the project id."
;;   (org-tj--with-advice
;;       ;; add default attributes
;;       ((#'org-tj--build-attributes
;;         :around #'org-tj--add-attributes)
;;        ;; add the project id
;;        ;; just use the toplevel id and add "prj_" to the front
;;        (#'org-tj-get-id
;;         :override #'org-tj--get-project-id))
;;     (funcall orig-fun project info)))

;; ;; TODO this can probably be consolidated
;; (defun org-tj--add-task-attributes* (orig-fun task attributes)
;;   (let* ((orig-attributes (funcall orig-fun task attributes))
;;          (start (-some->>
;;                  (org-tj-get-start task)
;;                  (format "start %s\n")))
;;          (end (-some->>
;;                (org-tj-get-end task)
;;                (format "end %s\n"))))
;;     (s-join "" (-non-nil (list orig-attributes start end)))))

;; (defun org-tj--add-task-attributes (orig-fun task info)
;;   (org-tj--with-advice
;;       ((#'org-tj--build-attributes
;;         :around #'org-tj--add-task-attributes*))
;;     (funcall orig-fun task info)))

(defun org-tj-add-to-server (file)
  "Add tj3 project FILE to web server."
  (call-process-shell-command
   (format "tj3client -c /home/ndwar/.config/tj3/taskjugglerrc add %s" file)))

(defun org-tj-load ()
  "Load tj3 files to the taskjuggler daemon."
  ;; TODO browse folders freely like dired
  (interactive)
  (let ((project-files
         (->> (directory-files nd/org-export-publishing-directory t)
              (--filter (equal (file-name-extension it t)
                               org-tj-extension)))))
    (helm
     :sources
     (list
      (helm-build-sync-source "Project Files"
        ;; TODO make these project ids
        :candidates project-files
        :action
        ;; TODO add load and view using function below
        '(("Load" . org-tj-add-to-server))))
     :buffer "*helm taskjuggler buffer*"
     :prompt "Project: ")))

(defun org-tj-loaded-ids ()
  "Get a list of loaded project id's from the taskjuggler daemon."
  (-->
   ;; assume --silent and /dev/null remove extraneous info
   (shell-command-to-string
    "tj3client -c /home/ndwar/.config/tj3/taskjugglerrc --silent status 2> /dev/null")
   (s-split "\n" it t)
   ;; assume the first two lines are headers
   (-drop 2 it)
   (--map (--> (s-split "|" it t) (nth 1 it) (s-trim it)) it)))

(defun org-tj-loaded-projects ()
  "Browse a loaded taskjuggler projects using helm."
  (interactive)
  (helm
   :sources
   (list
    (helm-build-sync-source "Loaded Projects"
      :candidates (org-tj-loaded-ids)
      :action
      '(("Open" . org-tj-open-id-in-browser)
        ("Remove" .
         (lambda (i)
           (call-process-shell-command
            (format
             "tj3client -c /home/ndwar/.config/tj3/taskjugglerrc remove %s" i)))))))
   :buffer "*helm taskjuggler buffer*"
   :prompt "Project ID: "))

(defun org-tj--get-project-headlines ()
  "Return list of all org taskjuggler project headlines."
  (--> (org-element-parse-buffer)
       (org-element-map it 'headline
         (lambda (hl)
           (when (member org-tj-project-tag
                         (org-element-property :tags hl))
             hl)))
       (org-element-map it 'headline #'identity)))

(defun org-tj--get-resource-headlines ()
  "Return list of all org taskjuggler resource headlines."
  (--> (org-element-parse-buffer)
       (org-element-contents it)
       ;; only look at top-level resource trees because this is
       ;; what the parser looks at
       (--filter (member org-tj-resource-tag
                         (org-element-property :tags it))
                 
                 it)
       (org-element-map it 'headline #'identity)))

(defun org-tj--get-task-ids (&optional headlines)
  "Return a list of all ID's for tasks in TJ projects.
Search through HEADLINES or all taskjuggler headlines in the buffer if
not given."
  (--> (or headlines (org-tj--get-project-headlines))
       (--map
        (org-element-property
         (if org-tj-use-id-property :ID :TASK_ID) it)
        it)
       (-non-nil it)
       (-uniq it)))

(defun org-tj--in-project-p ()
  "Check that the cursor is currently in a taskjuggler project."
  (member org-tj-project-tag (org-get-tags-at)))

(defun org-tj-set-id ()
  "Set the id for the current task.
Uniqueness is enforced within projects."
  (interactive)
  (unless (org-tj--in-project-p)
    (error "Not in taskjuggler project tree"))
  (let* ((unique-ids (org-tj--get-task-ids))
         (default-id (->
                      (save-excursion
                        (org-back-to-heading)
                        (org-element-context))
                      (org-tj--build-unique-id unique-ids)))
         (new-id (read-string
                  (format "Set ID (default: %s): " default-id)
                  nil nil default-id)))
    (cond
     ;; must be unique
     ((member new-id unique-ids)
      (error "ERROR: Non-unique ID given"))
     ;; must not have whitespace
     ((s-contains? " " new-id)
      (error "ERROR: ID has whitespace"))
     (t
      (org-set-property
       (if org-tj-use-id-property "ID" "TASK_ID")
       new-id)))))

(defun org-tj--get-dependencies ()
  "Return list of dependencies for the current headline."
  (-some->> (org-entry-get nil "DEPENDS") (s-split "[ ,]* +" )))

(defun org-tj--get-resources ()
  "Return list of dependencies for the current headline."
  (-some->> (org-entry-get nil "ALLOCATE") (s-split "[ ,]* +" )))

;; TODO this just deals with the DEPENDS property
;; make also want to add blocker
(defun org-tj-add-depends ()
  "Set the depends property for a taskjuggler task.
Will automatically create an ID to dependency if it does not exist."
  (interactive)
  (unless (org-tj--in-project-p)
    (error "Not in taskjuggler project tree"))
  (let* ((cur-deps (org-tj--get-dependencies))
         (id-prop (if org-tj-use-id-property "ID" "TASK_ID"))
         (tj-headlines
          (->>
           (org-tj--get-project-headlines)
           ;; take out the current headline
           (--remove (save-excursion
                       (org-back-to-heading)
                       (->> (org-element-context)
                            (org-element-property :begin)
                            (eq (org-element-property :begin it)))))
           ;; take out headlines that have subheadlines
           (--remove (< 0 (->
                           (org-element-contents it)
                           (org-element-map 'headline #'identity)
                           length)))))
         (unique-ids
          (->> tj-headlines
               (-reductions-from
                (lambda (a b)
                  (let ((unique-id (org-tj--build-unique-id b a)))
                    (append a (list unique-id))))
                nil)
               (-drop 1)
               (-map #'-last-item)))
         (mk-display
          (lambda (unique-id hl)
            (let* ((hl-id (org-element-property id-prop hl))
                   (text (org-element-property :raw-value hl)))
              (if (equal unique-id hl-id) text
                (concat text " (!)")))))
         (hl-pos (--map (org-element-property :begin it) tj-headlines))
         (disp (--zip-with (funcall mk-display it other)
                           unique-ids tj-headlines))
         (candidates (->> unique-ids (-zip-pair hl-pos) (-zip-pair disp))))
    (helm
     :sources
     (list
      (helm-build-sync-source "Link Targets"
        :candidates (--remove (member (cddr it) cur-deps) candidates)
        :action
        `(("Link" .
           (lambda (h)
             (let ((id (cdr h))
                   (begin (car h)))
               ;; add to DEPENDS if there are existing
               (cond
                ((not ',cur-deps)
                 (org-set-property "DEPENDS" id))
                ((not (member id ',cur-deps))
                 (->> id list (append ',cur-deps) (s-join " ")
                      (org-set-property "DEPENDS"))))
               ;; set the target id if not set already
               (save-excursion
                 (goto-char begin)
                 (unless (equal id (org-entry-get nil ,id-prop))
                   (org-set-property ,id-prop id))))))))
      (helm-build-sync-source "Existing links"
        :candidates (--filter (member (cddr it) cur-deps) candidates)
        :action
        `(("Unlink" .
           (lambda (h)
             (let ((new-deps (-remove-item (cdr h) ',cur-deps)))
               (if new-deps
                   (->> new-deps
                        (s-join " ")
                        (org-set-property "DEPENDS"))
                 (org-entry-delete nil "DEPENDS"))))))))
      :buffer "*helm taskjuggler buffer*"
      :prompt "Project ID: ")))

(defun org-tj-add-resource ()
  "Add or remove resource from the current entry."
  (interactive)
  (unless (org-tj--in-project-p)
    (error "Not in taskjuggler project tree"))
  (let* ((cur-res-ids (org-tj--get-resources))
         (id-prop (if org-tj-use-id-property "ID" "TASK_ID"))
         (res-ids
          (->>
           (org-tj--get-resource-headlines)
           ;; only need the headlines with resource_id's
           (--map (org-element-property :RESOURCE_ID it))
           -non-nil
           -uniq)))
    (helm
     :sources
     (list
      (helm-build-sync-source "Link Targets"
        :candidates (--remove (member it cur-res-ids) res-ids)
        :action
        `(("Link" .
           (lambda (id)
             (cond
              ((not ',cur-res-ids)
               (org-set-property "ALLOCATE" id))
              ((not (member id ',cur-res-ids))
               (->> id list (append ',cur-res-ids) (s-join " ")
                    (org-set-property "ALLOCATE"))))))))
     (helm-build-sync-source "Existing links"
       :candidates (--filter (member it cur-res-ids) res-ids)
       :action
       `(("Unlink" .
          (lambda (id)
            (let ((new-res-ids (-remove-item id ',cur-res-ids)))
              (if new-res-ids
                  (->> new-res-ids
                       (s-join " ")
                       (org-set-property "ALLOCATE"))
                (org-entry-delete nil "ALLOCATE"))))))))
    :buffer "*helm taskjuggler buffer*"
    :prompt "Project ID: ")))

(defconst org-tj--report-attributes-id
  '(accountroot resourceroot taskroot))

(defconst org-tj--report-attributes-string
  '(auxdir rawhtmlhead timeformat timezone title))

(defconst org-tj--report-attributes-int
  '(height width))

(defconst org-tj--report-attributes-discrete
  '(formats loadunit))

(defconst org-tj--report-attributes-rich-text
  '(caption center epilog footer header headline left prolog right))

;; TODO these are just stubs for the most part
(defun org-tj--parse-columns (columns)
  ;; TODO just assume we have a list of column names for now
  ;; add the column attributes later as a plist (I think?)
  (->> columns (--map (symbol-name it)) (s-join ", ")))

(defun org-tj--report-parse-string-attribute (val)
  "val is a symbol."
  (->> val symbol-name (format "\"%s\"")))

(defun org-tj--report-attributes-int (val)
  (if (not (numberp int)) (error "Not number: %s" val) val))

(defun org-tj--report-parse-discrete-attribute (val)
  ;; TODO add validation here
  val)

(defun org-tj--report-parse-rich-text-attribute (val)
  ;; TODO...this needs work :)
  val)

(defun org-tj--parse-attribute (attr)
  (let ((key (car attr))
        (val (cdr attr)))
    (->>
     (cond
      ;; return id attributes as-is
      ((memq key org-tj--report-attributes-id)
       val)
      ;; return strings with quotes around them
      ((memq key org-tj--report-attributes-string)
       (org-tj--report-parse-string-attribute val))
      ;; return intergers as-is
      ((memq key org-tj--report-attributes-int)
       (org-tj--report-parse-int-attribute val))
      ;; return discrete attributes as-is but check they are valid
      ;; TODO there are many more attributes that follow this pattern
      ((memq key org-tj--report-attributes-discrete)
       (org-tj--report-parse-discrete-attribute val))
      ;; TODO need to figure out exactly what rich text means
      ;; probably will end up making a separate parser to abstract
      ;; it because...rich text is weird
      ((memq key org-tj--report-attributes-rich-text)
       (org-tj--report-parse-rich-text-attribute val))
      ;; columns are parsed specially
      ((eq key 'columns)
       (org-tj--parse-columns val))
      ;; TODO this needs to be implemented
      ;; TODO there are other attributes like this that are effectively
      ;; logic equations
      ((eq key 'hideresource)
       val)
      (t (error "Invalid key: %s" key)))
     (format "%s %s" key))))
  
(defun org-tj-create-taskreport (name id attrs)
  "Create a task report definition."
  (->> attrs
       (-map #'org-tj--parse-attribute)
       (s-join "\n")
       ;; TODO do I care about indenting prettily?
       (format "taskreport %s %s {\n%s\n}" name id)))

;; (advice-add #'org-tj--build-project :around
;;             #'org-tj--add-project-attributes)
;; (advice-add #'org-tj-export-process-and-open :around
;;             #'org-tj--export-process-and-open-web)
;; (advice-add #'org-tj-compile :around
;;             #'org-tj--compile-no-report)
;; (advice-add #'org-tj--build-task :around
;;             #'org-tj--add-task-attributes)

(provide 'org-tj)
;;; org-tj.el ends here
