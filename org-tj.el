;;; org-tj.el --- Org Mode TaskJuggler 3 Integration  -*- lexical-binding: t; -*-

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
(require 'ox-taskjuggler)

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

(defun org-tj--compile-no-report (orig-fn file)
  "Process FILE using ORIG-FN but don't compile to a report.
Instead override the `org-taskjuggler-process-command' such that FILE
added to the web server, and don't make the report directory as it is
not necessary."
  (let ((file (expand-file-name file)))
    (org-tj--with-advice
        ;; don't make a reports directory
        ((#'make-directory :override #'ignore))
      ;; override the original process command to load into server
      (let ((org-taskjuggler-process-command (org-tj--cmd 'client "add" "%f")))
        (funcall orig-fn file)))
    ;; just return the file path when done
    ;; the list is necessary for org-taskjuggler-export-process-and-open
    ;; since the code that opens the files is actually a loop that
    ;; expects a list
    (list file)))

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
       
(defun org-tj--export-process-and-open-web (orig-fun &rest args)
  "Process ARGS using ORIG-FUN but open files in browser."
  (org-tj--with-advice
      ((#'org-open-file :override #'org-tj-open-in-browser))
    (apply orig-fun args)))

(defun org-tj--add-attributes (orig-fun &rest args)
  "Call ORIG-FUN with ARGS and add extra attributes to projects."
  ;; assume the original list is a newline-delimited string
  ;; break this string into cons cells of keyval pairs
  (let* ((orig-attributes
          (--> (apply orig-fun args)
               (s-split "\n" it t)
               (--map (s-split-up-to " " it 1 t) it)
               (--map (cons (car it) (cadr it)) it)))
         (add-attributes
          (--> org-tj-default-attributes
               (--remove (assoc-string (car it) orig-attributes) it))))
    (--> orig-attributes
         (append it add-attributes)
         (--map (format "%s %s\n" (car it) (cdr it)) it)
         (string-join it))))

(defun org-tj--get-project-id (project info)
  "Get the project id from PROJECT.
The id is just the 'TASK_ID' org property with 'prj_' appended.
INFO is a communication channel and is ignored"
  (let ((id (org-element-property :TASK_ID project)))
    (if id (format "prj_%s" id)
      (error "ERROR: No project id found"))))

(defun org-tj--add-project-attributes (orig-fun project info)
  "Call ORIG-FUN with PROJECT and INFO.
Add project attributes to PROJECT and also add the project id."
  (org-tj--with-advice
      ;; add default attributes
      ((#'org-taskjuggler--build-attributes
        :around #'org-tj--add-attributes)
       ;; add the project id
       ;; just use the toplevel id and add "prj_" to the front
       (#'org-taskjuggler-get-id
        :override #'org-tj--get-project-id))
    (funcall orig-fun project info)))

;; TODO this can probably be consolidated
(defun org-tj--add-task-attributes* (orig-fun task attributes)
  (let* ((orig-attributes (funcall orig-fun task attributes))
         (start (-some->>
                 (org-taskjuggler-get-start task)
                 (format "start %s\n")))
         (end (-some->>
               (org-taskjuggler-get-end task)
               (format "end %s\n"))))
    (s-join "" (-non-nil (list orig-attributes start end)))))

(defun org-tj--add-task-attributes (orig-fun task info)
  (org-tj--with-advice
      ((#'org-taskjuggler--build-attributes
        :around #'org-tj--add-task-attributes*))
    (funcall orig-fun task info)))

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
                               org-taskjuggler-extension)))))
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

(defun org-tj--get-headlines ()
  "Return list of all org taskjuggler headlines."
  (--> (org-element-parse-buffer)
       (org-element-map it 'headline
         (lambda (hl)
           (when (member org-taskjuggler-project-tag
                         (org-element-property :tags hl))
             hl)))
       (org-element-map it 'headline #'identity)))

(defun org-tj--get-ids (&optional headlines)
  "Return a list of all ID's for tasks in TJ projects.
Search through HEADLINES or all taskjuggler headlines in the buffer if
not given."
  (--> (or headlines (org-tj--get-headlines))
       (--map
        (org-element-property
         (if org-tj-use-id-property :ID :TASK_ID) it)
        it)
       (-non-nil it)
       (-uniq it)))

(defun org-tj--in-project-p ()
  "Check that the cursor is currently in a taskjuggler project"
  (member org-taskjuggler-project-tag (org-get-tags-at)))

(defun org-tj-set-id ()
  "Set the id for the current task.
Uniqueness is enforced within projects."
  (interactive)
  (unless (org-tj--in-project-p)
    (error "Not in taskjuggler project tree"))
  (let* ((unique-ids (org-tj--get-ids))
         (default-id (->
                      (save-excursion
                        (org-back-to-heading)
                        (org-element-context))
                      (org-taskjuggler--build-unique-id unique-ids)))
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

;; TODO this just deals with the DEPENDS property
;; make also want to add blocker
(defun org-tj-add-depends ()
  "Set the depends property for a taskjuggler task.
Will automatically create an ID to the dependency if it does not 
exist."
  (interactive)
  (unless (org-tj--in-project-p)
    (error "Not in taskjuggler project tree"))
  ;; TODO filter out headlines in current deps
  (let* ((cur-deps (org-tj--get-dependencies))
         (id-prop (if org-tj-use-id-property "ID" "TASK_ID"))
         (tj-headlines
          (->>
           (org-tj--get-headlines)
           ;; take out the toplevel hl (assume it is first)
           (-drop 1)
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
                  (let ((unique-id (org-taskjuggler--build-unique-id b a)))
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

(advice-add #'org-taskjuggler--build-project :around
            #'org-tj--add-project-attributes)
(advice-add #'org-taskjuggler-export-process-and-open :around
            #'org-tj--export-process-and-open-web)
(advice-add #'org-taskjuggler-compile :around
            #'org-tj--compile-no-report)
(advice-add #'org-taskjuggler--build-task :around
            #'org-tj--add-task-attributes)

(provide 'org-tj)
;;; org-tj.el ends here
