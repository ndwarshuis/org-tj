;;; org-tj3.el --- Org Mode TaskJuggler 3 Integration  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Nate Dwarshuis

;; Author: Nic Ferrier <natedwarshuis@gmail.com>
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

(defmacro org-tj3--with-advice (adlist &rest body)
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

(defcustom org-tj3-config-file (-> "XDG_CONFIG_HOME"
                                   getenv
                                   file-name-as-directory
                                   (concat "tj3")
                                   file-name-as-directory
                                   (concat "taskjugglerrc"))
  "The org-tj3 config file."
  :group 'org-tj3
  :type 'string)

(defcustom org-tj3-default-attributes
  '(("timingresolution" . "5 min"))
  "Default project attributes."
  :group 'org-tj3)

(defun org-tj3--cmd (type &rest args)
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
   (if (not org-tj3-config-file) it
     (-insert-at 1 (concat "-c " org-tj3-config-file) it))
   (s-join " " it)))

(defun org-tj3--compile-no-report (orig-fn file)
  "Process FILE using ORIG-FN but don't compile to a report.
Instead override the `org-taskjuggler-process-command' such that FILE
added to the web server, and don't make the report directory as it is
not necessary."
  (let ((file (expand-file-name file)))
    (org-tj3--with-advice
        ;; don't make a reports directory
        ((#'make-directory :override #'ignore))
      ;; override the original process command to load into server
      (let ((org-taskjuggler-process-command (org-tj3--cmd 'client "add" "%f")))
        (funcall orig-fn file)))
    ;; just return the file path when done
    ;; the list is necessary for org-taskjuggler-export-process-and-open
    ;; since the code that opens the files is actually a loop that
    ;; expects a list
    (list file)))

(defun org-tj3-open-id-in-browser (id)
  "Open project ID in the default web browser."
  (browse-url
   (format
    ;; TODO make port a variable
    "http://localhost:8081/taskjuggler?project=%s;report=report" id)))

(defun org-tj3-open-in-browser (file)
  "Open tj3 FILE in the default web browser."
  ;; TODO check if project is loaded and load if not loaded
  ;; if it is loaded throw a warning
  (--> (with-temp-buffer (insert-file-contents file) (buffer-string))
       (s-match "project \\([^[:space:]]+\\) " it)
       (nth 1 it)
       (org-tj3-open-id-in-browser it)))
       
(defun org-tj3--export-process-and-open-web (orig-fun &rest args)
  "Process ARGS using ORIG-FUN but open files in browser."
  (org-tj3--with-advice
      ((#'org-open-file :override #'org-tj3-open-in-browser))
    (apply orig-fun args)))

(defun org-tj3--add-attributes (orig-fun &rest args)
  "Call ORIG-FUN with ARGS and add extra attributes to projects."
  ;; assume the original list is a newline-delimited string
  ;; break this string into cons cells of keyval pairs
  (let* ((orig-attributes
          (--> (apply orig-fun args)
               (s-split "\n" it t)
               (--map (s-split-up-to " " it 1 t) it)
               (--map (cons (car it) (cadr it)) it)))
         (add-attributes
          (--> org-tj3-default-attributes
               (--remove (assoc-string (car it) orig-attributes) it))))
    (--> orig-attributes
         (append it add-attributes)
         (--map (format "%s %s\n" (car it) (cdr it)) it)
         (string-join it))))

(defun org-tj3--get-project-id (project info)
  "Get the project id from PROJECT.
The id is just the 'TASK_ID' org property with 'prj_' appended.
INFO is a communication channel and is ignored"
  (-some->> (org-element-property :TASK_ID project)
            (format "prj_%s")))

(defun org-tj3--add-project-attributes (orig-fun project info)
  "Call ORIG-FUN with PROJECT and INFO.
Add project attributes to PROJECT and also add the project id."
  (org-tj3--with-advice
      ;; add default attributes
      ((#'org-taskjuggler--build-attributes
        :around #'org-tj3--add-attributes)
       ;; add the project id
       ;; just use the toplevel id and add "prj_" to the front
       (#'org-taskjuggler-get-id
        :override #'org-tj3--get-project-id))
    (funcall orig-fun project info)))

(defun org-tj3-add-to-server (file)
  "Add tj3 project FILE to web server."
  (call-process-shell-command
   (format "tj3client -c /home/ndwar/.config/tj3/taskjugglerrc add %s" file)))

(defun org-tj3-load ()
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
        '(("Load" . org-tj3-add-to-server))))
     :buffer "*helm taskjuggler buffer*"
     :prompt "Project: ")))

(defun org-tj3-loaded-ids ()
  "Get a list of loaded project id's from the taskjuggler daemon."
  (-->
   ;; assume --silent and /dev/null remove extraneous info
   (shell-command-to-string
    "tj3client -c /home/ndwar/.config/tj3/taskjugglerrc --silent status 2> /dev/null")
   (s-split "\n" it t)
   ;; assume the first two lines are headers
   (-drop 2 it)
   (--map (--> (s-split "|" it t) (nth 1 it) (s-trim it)) it)))

(defun org-tj3-loaded-projects ()
  "Browse a loaded taskjuggler projects using helm."
  (interactive)
  (helm
   :sources
   (list
    (helm-build-sync-source "Loaded Projects"
      :candidates (org-tj3-loaded-ids)
      :action
      '(("Open" . org-tj3-open-id-in-browser)
        ("Remove" . 
         (lambda (i)
           (call-process-shell-command
            (format
             "tj3client -c /home/ndwar/.config/tj3/taskjugglerrc remove %s" i)))))))
   :buffer "*helm taskjuggler buffer*"
   :prompt "Project ID: "))

(advice-add #'org-taskjuggler--build-project :around
            #'org-tj3--add-project-attributes)
(advice-add #'org-taskjuggler-export-process-and-open :around
            #'org-tj3--export-process-and-open-web)
(advice-add #'org-taskjuggler-compile :around
            #'org-tj3--compile-no-report)

(provide 'org-tj3)
;;; org-tj3.el ends here
