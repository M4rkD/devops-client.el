;;; devops-client.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 mark
;;
;; Author: Mark Dawson <http://github/M4rkD>
;; Maintainer: Mark Dawson
;; Created: March 23, 2020
;; Modified: March 23, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/mark/devops-client.el
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'request)
(require 'ht)
(require 'dash)
(require 'cl-lib)

(defun azdev/new-store ()
  (ht-create))

(defvar azdev/base-url "https://dev.azure.com/swansea-university/_apis"
  "The base url of the organisation.")


(defvar azdev/default-project "Swansea%20Academy%20of%20Advanced%20Computing"
  "The default project.")

(defvar azdev/query-chunk-size 200
  "The maximum number of work items to fetch in one go.")

(defvar azdev/wi-store (azdev/new-store)
  "Default work item store.")

(defvar azdev/buffer "*devops*")

(defvar azdev/token-file-path "~/.azure-devops-token")

(defvar azdev/auth-token nil)

(defvar azdev/work-item-show-filter 'filter-open-or-recently-closed)
;; (defvar azdev/work-item-show-filter 'filter-nothing)

(defvar azdev/task-display-mapping
  `(("ID" id 10 ,#'azdev/id->printed-id)
    ("Title" title 40 ,#'identity)
    ("Status" state 10 ,#'identity)
    ("Assigned To" assigned-to 15 ,(lambda (name) (or name "---------------")))
    ("Type" work-item-type 15 ,#'identity)
    ("Updated" changed-date 11 ,(-partial #'format-time-string "%Y-%m-%d")))
  "List of mappings to obtain string for each column.
Each entry is of the form:
 (column-name field-in-data column-width transform-function)
Tranform is a function which takes in the value of key field-in-data of
work item data, and returns the string to display.
")

(defvar azdev/epic-feature-display-mapping
  `(("Title" title 40 ,#'identity)
    ("ID" id 10 ,#'azdev/id->printed-id)
    ("Status" state 10 ,#'identity))
  "List of mappings to obtain string for each column.
Each entry is of the form:
 (column-name field-in-data column-width transform-function)
Tranform is a function which takes in the value of key field-in-data of
work item data, and returns the string to display.
")

(defvar azdev/map:work-item->display-string
  (list "Development Task" 'azdev/task-display-mapping
        "Admin Task" 'azdev/task-display-mapping
        "Epic" 'azdev/epic-feature-display-mapping
        "Feature" 'azdev/epic-feature-display-mapping
        nil 'azdev/task-display-mapping)
  "Property list that specifies for each type of work item, the list of
columns to display.
Entry with key nil specifies the default entry.")

(defvar azdev/formatting-faces
  '("Development Task" (azdev-dev-task (nil nil azdev/format-status))
    "Admin Task" (azdev-admin-task (nil nil azdev/format-status))
    "Epic" (azdev-epic nil)
    "Feature" (azdev-feature nil)
    "Meeting" (azdev-meeting nil)
    "Meeting attendance" (azdev-meeting nil)
    )
  "Definitions of how to format rows/columns.
First element is the face to use for the row.
The second column in a list of functions to use to format each row.
Each function should take three arguments:
the work item data, the start position of the column, and the end position.")

(defvar azdev/state-colours '("Closed" "lime green"
                              "Active" "red")
  "Colours for colouring each state text.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tokens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/load-token ()
  "Return token from azdev/toekn-file-path."
  (with-temp-buffer
    (insert-file-contents azdev/token-file-path)
    (s-trim
    (buffer-string))))

(defun azdev/read-token ()
  (setq azdev/auth-token (azdev/load-token)))

(defun default-headers (content-type)
  "Return default headers, for a given CONTENT-TYPE."
  `(("Authorization" . ,(concat "Basic " azdev/auth-token))
    ("Content-Type" . ,content-type)))

(azdev/read-token)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Communications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/catch-request-errors (response)
  "Throw the appropriate error if RESPONSE does not have status 200.
Otherwise return the RESPONSE, unchanged."
  (if (= 200 (request-response-status-code response))
      response
    (let* ((is-json (cl-search "json" (request-response-header response "Content-Type")))
          (raw-data (request-response-data response)))

        (user-error "Error in response: %s" raw-data))))

(defun azdev/dispatch-get-request (uri)
  (azdev/--dispatch-request uri "GET" nil))

(defun azdev/dispatch-post-request (uri data)
  (azdev/--dispatch-request uri "POST" data))

(defun azdev/dispatch-patch-request (uri data)
  (azdev/--dispatch-request uri "PATCH" data))

(defun azdev/--dispatch-request (uri method data)
  "Dispatch request to endpoint URI (with json parsing) and METHOD.
Return parsed json data as an alist.
METHOD should be a string such as \"GET\" or \"POST\""
  (let* ((url (concat azdev/base-url uri))
         (content-type (if (string= method "PATCH")
                           "application/json-patch+json"
                         "application/json")))
    (message "Calling: [%S] %s" method url)
    (request-response-data
    (if data
        (request
          url
          :type method
          :parser 'json-read
          :headers (default-headers content-type)
          :sync t
          :data (json-encode data))
      (request
        url
        :type method
        :parser 'json-read
        :headers (default-headers content-type)
        :sync t)))))

(defun azdev/get-request (uri)
  "GET from URI of the current project."
  (azdev/dispatch-get-request uri))

(defun azdev/query (wiql)
  "Fetch the result of the the wiql query string given by WIQL."
  ;; wiql api used is documented at: https://docs.microsoft.com/en-us/rest/api/azure/devops/wit/wiql/query%20by%20wiql?view=azure-devops-rest-5.1
  (let ((wiql-uri "/wit/wiql?api-version=5.1"))
    (message "Wiql: %s" wiql)
    (azdev/dispatch-post-request wiql-uri `(("query" . ,wiql)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fetch/store work items by ID
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/--fetch-work-items--chunked-list->urls (chunked-list)
  "Return a list of work item urls, given a chunked list of ids.
Given a chunked list of ids (i.e. a list of lists of ids)
return a list of urls of size equal the length of CHUNKED-LIST,
of urls to fetch that list of IDs"
  (mapcar (lambda (ids-chunk)
            (concat
            "/wit/workItems?ids="
            (mapconcat 'number-to-string ids-chunk ",")
            "&$expand=All&errorPolicy=Omit"))
          chunked-list))

(defun azdev/filter-ids-not-in-store (store ids)
  "Given a STORE and a list of IDS, return the ids not in the store."
  (seq-filter
  (lambda (key)
    (not (ht-contains? store key)))
  ids))

(defun azdev/get-relation-matching-attributes-name (relations name)
  "Get list of IDs from RELATIONS where relation type matches NAME.

NAME is either \"CHILD\" or \"PARENT\" "
  (delq nil
        (mapcar (lambda (relation)
                  (if (string= name
                              (alist-get 'name
                                          (alist-get 'attributes relation)))
                      (azdev/relation-url>id (alist-get 'url relation))
                    ))
                relations)))

(defun azdev/identity-to-name (identity)
  "Given a devops identity, extract the name (displayName)"
  (alist-get 'displayName identity))

(defvar azdev/response-field-mappings
  '((id (id))
    (title (fields System\.Title))
    (children (relations)
              (azdev/get-relation-matching-attributes-name "Child"))
    (parent (relations)
            (azdev/get-relation-matching-attributes-name "Parent"))
    (relations-raw (relations))
    (area-path (fields System\.AreaPath))
    (team (fields System\.AreaPath) (azdev/area-path->team))
    (project (fields System\.TeamProject))
    (iteration-path (fields System\.IterationPath))
    (work-item-type (fields System\.WorkItemType))
    (state (fields System\.State))
    (reason (fields System\.Reason))
    (assigned-to (fields System\.AssignedTo) (azdev/identity-to-name))
    (created-date (fields System\.CreatedDate) (date-to-time))
    (created-by (fields System\.CreatedBy) (azdev/identity-to-name))
    (changed-date (fields System\.ChangedDate) (date-to-time) )
    (changed-by (fields System\.ChangedBy) (azdev/identity-to-name))
    (comment-count (fields System\.CommentCount))
    (board-column (fields System\.BoardColumn))
    (board-columnDone (fields System\.BoardColumnDone))
    (length (fields Custom\.Length))
    (completed-work (fields Microsoft\.VSTS\.Scheduling\.CompletedWork)))
  "Mappings from local field names to the field names path in the recieved JSON.
An additional function can be provided, which is used to map remote value to local value.
A list can be provided instead of the value mapping function, in which case the first entry of
the list is the function to call, and the remaining entries are the additional arguments to pass
(after the first).")

(defun azdev/area-path->team (area-path)
  "Convert an AREA-PATH into a team name"
  (car
   (last
    (s-split "\\\\" area-path))))

(defun azdev/assoc-recursive (keys alist)
  "Recursively find KEYs in ALIST."
  (while keys
    (setq alist (cdr (assoc (pop keys) alist))))
  alist)

(defun azdev/work-item-parse (work-item-response)
  "Transform the downloaded json into a work item"
  (mapcar (-lambda ((dest source-path func))
            (let* ((resp-value (azdev/assoc-recursive source-path work-item-response))
                   (value (if (and  func (listp func))
                              (apply (car func) resp-value (cdr func))
                             resp-value)))
              (cons dest value)))
          azdev/response-field-mappings))


(defun azdev/work-item-->store (store item)
  "Puts the work ITEM in the hash STORE by id, and return id."
  (let ((id (alist-get 'id item)))
    (ht-set! store id item)
    id))

(defun azdev/fetch-work-item-data-urls (ids)
  (azdev/--fetch-work-items--chunked-list->urls
   (-partition-all
    azdev/query-chunk-size
    ids)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query work items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/extract-ids-from-query-response(wiql-query-response)
  "Extract a list of ids from the WIQL-QUERY-RESPONSE response."
  (mapcar (apply-partially 'alist-get 'id)
          (alist-get 'workItems wiql-query-response)))

(defun azdev/query-work-item-ids (wiql)
  "Fetch the ids matching a given WIQL query."
  (azdev/extract-ids-from-query-response
              (azdev/query wiql)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specific requests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/get-projects ()
  "Fetch a list of all projects."
  (azdev/get-request "/projects"))

(defun azdev/get-teams ()
  "Fetch a list of all teams for the current project."
  (alist-get 'value
            (azdev/dispatch-get-request
              (concat "/projects/" azdev/default-project "/teams"))))

(defun azdev/get-all-work-items ()
  "Fetch a list of all teams for the current project."
  (alist-get 'value
            ((azdev/dispatch-get-request
              (concat "/projects/" azdev/default-project "/teams")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Queries as functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/query/all-work-items ()
  "Fetch all epics as a list of ids stored in STORE."
  (azdev/query-work-item-ids
   "SELECT [System.Id] FROM workitems WHERE [System.TeamProject] = 'Swansea Academy of Advanced Computing'"))

(defun azdev/query/all-epics ()
  "Fetch all epics as a list of ids stored in STORE."
  (azdev/query-work-item-ids
  "SELECT * FROM workitems WHERE [System.WorkItemType] = 'EPIC'"))

(defun azdev/query/epics-for-team (team)
  "Fetch all epics for a given TEAM into STORE.
Return ids of epics.
This function assumes that each team maps to an AreaPath."
  (azdev/query-work-item-ids
   (format "SELECT * FROM workitems WHERE [System.WorkItemType] = 'EPIC' AND \
[System.AreaPath] = 'Swansea Academy of Advanced Computing\\%s'" team)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extract relation information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/relation-url>id (relation-url)
  "Return the id (as a number) for a RELATION-URL.
The id is extracted as the last portion of the url."
  (string-to-number
  (car
    (last
    (split-string
      relation-url
      "/")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search and filter store
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Filtering works by functions azdev/pred/*, which create predicate functions (as closures) that can be combined with and/or.


(defun azdev/find/epics-for-given-team (store name)
  (azdev/find store (azdev/pred/and (azdev/pred/epic) (azdev/pred/team-name name))))

(defun azdev/find/team-names (store name)
  (azdev/unique-values-of-key store 'team))

(defun azdev/find/team-names-random-order (store)
  (require 'cookie1)

  (cookie-shuffle-vector
   (apply #'vector
          (azdev/unique-values-of-key store 'team))))

(defun azdev/find (store pred)
  "Return ids of entries for which PRED returns truthy.

PRED is a function which takes an item."
  (delete-dups
   (delq nil
         (ht-map (lambda (k v &rest rest)
                   (if (funcall pred v)
                       k
                     nil))
                 store))))

(cl-defun azdev/compute-days-since-time (work-item time &optional (key 'changed-date))
  "Utility function to compute the number of days before TIME that WORK ITEM was changed (or some other time KEY)."
  (/ (float-time
      (time-subtract
       time
       (alist-get key work-item)
       )) (* 60 60 24)))

(cl-defun azdev/pred/days-since-time (days &optional (key 'changed-date))
  "Computes the number of days since the predicate function was created."
  (let ((time-now (current-time)))
    (lambda (v)
      (> days
         (azdev/compute-days-since-time v time-now key)))))

(defun azdev/pred/string-value (key string)
  (lambda (v)
    (string= (alist-get key v) string)))

(defun azdev/pred/epic ()
  (azdev/pred/string-value 'work-item-type "Epic"))

(defun azdev/pred/team-name (team-name)
  (lambda (v)
    (string= (alist-get 'team v) team-name)))

(defun azdev/pred/or (pred1 pred2)
  (lambda (v)
    (or (funcall pred1 v) (funcall pred2 v))))

(defun azdev/pred/and (pred1 pred2)
  (lambda (v)
    (and (funcall pred1 v) (funcall pred2 v))))

(defun azdev/unique-values-of-key (store key)
  "Fetch all unique values of field KEY in STORE."
  (delete-dups
  (ht-map (lambda (k v)
            (alist-get key v))
          store)))


(azdev/unique-values-of-key azdev/wi-store 'team)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Walk the tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun azdev/walk-tree (store curr-node-id &optional (level 0) (acc nil))
  (let* ((data (ht-get store curr-node-id))
         (children (alist-get 'children data)))
    (if data
        (if children
          (-reduce-from (lambda (acc-child child-node-id)
                          (append acc-child (azdev/walk-tree store child-node-id (+ level 1))))
                        (append acc (list (cons level curr-node-id))) ;; inital value
                        children)
          (list (cons level curr-node-id)))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface azdev-epic
  '((default :foreground "white"
      :height 2.5
      :background "#FF7B00"
      :weight ultra-bold))
       "Basic face for highlighting."
       :group 'azdev-faces)

(defface azdev-feature
  '((default :foreground "white"
      :background "#773B93"
      :height 1.2
      :weight bold))
  " Basic face for highlighting. "
:group 'azdev-faces)

(defface azdev-dev-task
  '((default :foreground "black"
      :background "#FBD144"))
       "Basic face for highlighting."
       :group 'azdev-faces)

(defface azdev-admin-task
  '((default :foreground "white"
      :background "#0D60AB"))
       "Basic face for highlighting."
       :group 'azdev-faces)

(defface azdev-meeting
  '((default :foreground "black"
      :background "cornsilk"))
  "Basic face for highlighting. "
:group 'azdev-faces)

(defface azdev-heading
  '((default
      :background "default"
      :height 3.0))
       "Basic face for highlighting."
       :group 'azdev-faces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Line utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun azdev/line-start-to-next-line ()
  "Return a cons pair (start . end) with character position of this line
and the start of the next line (end of this line + 1)."
  (save-excursion
    (cons (progn (beginning-of-line)
                 (point))
          (min
           (progn (end-of-line)
                  (+ 1 (point)))
           (point-max)))))

(defun azdev/ewoc-id-current-line (ewoc)
  "Get the ID of the work item on the current line"
  (cdr
   (ewoc-data
    (ewoc-locate ewoc))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Formatting functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/overlay-face-props (start end props)
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face props)))

(defun azdev/format-status (data start end)
  "Function used to format the `status` column.
This uses the azdev/state-colours plist to determine the colour
to apply to the region."
  (let* ((state (alist-get 'state data))
         (color (lax-plist-get azdev/state-colours state)))
    (if color
        (azdev/overlay-face-props start end `((foreground-color . ,color)
                                     (weight . bold))))))

(defun azdev/get-column-widths-for-item (data)
  "For a work item DATA, return a list of the widths of each printed column.
Widths are determined by parsing azdev/get-display-mapping."
  (mapcar
   (-partial #'nth 2)
   (azdev/get-display-mapping data)))

(defun azdev/get-column-ranges-for-item (data)
  (azdev/col-widths-to-ranges
   (azdev/get-column-widths-for-item data)))

(defun azdev/col-widths-to-ranges (widths)
  (cdr
   (-reduce-from (lambda (acc it)
                   (let* ((start (cdr  (-last-item acc)))
                          (end (+ start it)))
                     (append acc (list (cons start end)))))
                 '((0 . 0))
                 widths)))

(defun azdev/lim-point-to-buffer (pos)
  (min pos
       (point-max)))


(defun azdev/format-range-from-work-item-type (data start end)
  (-let* ((wi-type (alist-get 'work-item-type data))
          ((default-face column-faces) (lax-plist-get azdev/formatting-faces wi-type))
          (item-column-ranges (azdev/get-column-ranges-for-item data))
          (indent (azdev/indent-at-point start)))
    (if default-face
        (add-text-properties start end `(face ,default-face)))
    (if column-faces
;;;  if column-faces is defined, assume that it's a formatting
;;;  function to apply to range to format
        (-zip-with (lambda (fmt-fnc range)
                     (let* ((start-col (+ indent start (car range)))
                           (end-col (+ indent start (cdr range))))
                       (if fmt-fnc
                           (funcall fmt-fnc
                                    data
                                    (azdev/lim-point-to-buffer start-col)
                                    (azdev/lim-point-to-buffer end-col)))))
                   column-faces
                   item-column-ranges))
    ))

(defun azdev/apply-format-to-current-line (ewoc store)
  "Call format-func passing start of line and start of next line as arguments"
  (-let* (((start . end) (azdev/line-start-to-next-line))
          (id (azdev/ewoc-id-current-line ewoc))
          (data (ht-get store id))
          (wi-type (alist-get 'work-item-type data)))
    (azdev/format-range-from-work-item-type data start end)))

(defun devops-format (ewoc store)
  (save-excursion
    (with-current-buffer azdev/buffer
      (ewoc-goto-node ewoc (ewoc-nth ewoc 0))
      (while (ewoc-next-line-or-nil ewoc)
        (azdev/apply-format-to-current-line ewoc store)))))

(defun ewoc-next-line-or-nil (ewoc)
  (let ((curr (point)))
    (ewoc-goto-next ewoc 1)
    (if (= curr (point))
        nil
      t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filtering functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun filter-nothing (data level)
  "Show all work items (based on work item DATA and LEVEL)."
  t)

(defun filter-only-not-closed (data level)
  "Show only items that are not closed (based on work item DATA and LEVEL)."
  (let* ((id (alist-get 'id data))
         (label (alist-get 'title data))
         (wi-type (alist-get 'work-item-type data))
         (changed-time (alist-get 'changed-date data))
         (wi-state (alist-get 'state data))
         (assigned-to (alist-get 'assigned-to data)))
    (not  (string= wi-state "Closed"))))


(defun filter-open-or-recently-closed (data level)
  "Show only items that are not closed (based on work item DATA and LEVEL)."
  (let* ((id (alist-get 'id data))
         (label (alist-get 'title data))
         (wi-type (alist-get 'work-item-type data))
         (changed-time (alist-get 'changed-date data))
         (wi-state (alist-get 'state data))
         (assigned-to (alist-get 'assigned-to data))
         (check-time (azdev/pred/days-since-time 7)))
    (or (not  (string= wi-state "Closed"))
        (funcall check-time data))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/indent-length (level)
  (+ 1 (* 4 level)))

(defun azdev/team-work-item-id+level (store team-name)
  "Get (level . id) cons pairs for items to show."
  (mapcan (lambda (epic-id)
            (azdev/walk-tree store epic-id))
          (azdev/find/epics-for-given-team store team-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Printing lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/id->printed-id (id)
  (concat ":" (number-to-string id) ": "))

(defun azdev/get-display-mapping (data)
  "Given a work item data, return specification of the columns of that work item."
  (let* ((wi-type (alist-get 'work-item-type data))
         (val (or (lax-plist-get azdev/map:work-item->display-string
                                 wi-type)
                  (lax-plist-get azdev/map:work-item->display-string
                                 nil))))
    (cond ((symbolp val) (eval val))
          val)))

(defun azdev/indent-at-point (pos)
  (or
   (plist-get (text-properties-at pos)
              'azdev-line-indent)
   0))

(defun azdev/add-text-props-to-string (props str)
  (let ((len (length str)))
    (add-text-properties 0 len props str)
    str))

(cl-defun azdev/string-for-work-item (data &optional (level 0))
  "For a given data entry, return the values of columns as a vector.
The way to obtain columns is defined in azdev/string-for-task-display-mapping."
  (let ((display-mapping (azdev/get-display-mapping data))
        (indent (azdev/indent-length level)))
    (azdev/add-text-props-to-string
     `(azdev-line-indent ,indent)
     (apply #'concat
            (s-repeat indent " ")
            (mapcar
             (-lambda ((col-name key length func))
               (s-truncate length
                           (s-pad-right length " "
                                        (funcall func
                                                 (alist-get key data)))))
             display-mapping)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Modifying the ids list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl-defun azdev/insert-blank-before-matching-line (store ids+level &optional (num-lines 1) (comp-values (list "Epic")))
  (mapcan
   (-lambda ((level . id))
     (let* ((data (ht-get store id))
            (wi-type (alist-get 'work-item-type data)))
       (if (-contains? comp-values wi-type)
           (append (make-list num-lines
                              '(blank-line))
                   (list
                    (cons level id)))
         (list
          (cons level id)))))
   ids+level))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fetching and storing work items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/fetch-work-items (ids)
  "Fetch data for each items from IDS in chunks."
  (mapcan
   (lambda (url)
     ;; loop over urls
     (let* ((response (azdev/get-request url))
            (values (alist-get 'value response)))
       (mapcar
        (lambda (value)
          (azdev/work-item-parse value))
        values)))
   (azdev/fetch-work-item-data-urls ids)))

(defun azdev/fetch-and-set-work-items (store ids)
  "Fetch all work items specified in IDS into STORE."
  (dolist (item (azdev/fetch-work-items ids))
        (azdev/store/set-item store item)))

(defun azdev/store/set-item (store item)
  "Add an ITEM into STORE."
  (ht-set! azdev/wi-store (alist-get 'id item) item))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Remote URLs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/construct-work-item-url (item-id)
  "Return the URL of a work item given the ITEM-ID."
  (concat
   "https://dev.azure.com/swansea-university/Swansea Academy of Advanced Computing/_workitems/edit/"
   (number-to-string item-id)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; High-level functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/fetch-and-set-all-items (store)
  "Fetch all work items and set them in STORE (e.g. when starting up)."
  (azdev/fetch-and-set-work-items
   store
   (azdev/query/all-work-items)))

(defun azdev/fetch-and-set-all-new-items (store)
  "Fetch all items that are not already in STORE, and set them in STORE."
(azdev/fetch-and-set-work-items
 store
 (azdev/filter-ids-not-in-store store
                                    (azdev/query/all-work-items))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating ewoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/ewoc-printer (ids+level)
  "TODO: the store is accessed directly here"
  (let ((level (car ids+level))
        (id (cdr ids+level)))
    (cond ((numberp level) (insert (azdev/string-for-work-item
                                    (ht-get azdev/wi-store id) level)))
          ;; When level is 'header
          ((equal level 'header) (insert id))
          ;; When level is 'blank-line
          ((equal level 'blank-line) (insert "\n")))))

(defun azdevops/add-team-items-to-ewoc (ewoc store teams)
  "Print the lines for all TEAMS using insert."
  (mapc
   (lambda (team-name)
     (ewoc-enter-last ewoc
                      `(header . ,team-name))
     (azdev/add-items-to-ewoc
      ewoc
      (azdev/insert-blank-before-matching-line
       store
       (azdev/team-work-item-id+level store team-name))))
   teams))

(cl-defun azdev/add-items-to-ewoc (ewoc ids+level)
  "Prints the provided item IDS from STORE."
  (mapc
   (lambda (id+level)
     (ewoc-enter-last ewoc id+level))
   ids+level))

(defun azdev/create-teams-ewoc! (store teams)
  (with-current-buffer azdev/buffer
    (erase-buffer)
    (let ((ewoc
           (ewoc-create
            #'azdev/ewoc-printer)))
      (azdevops/add-team-items-to-ewoc ewoc store teams)
      ewoc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Finding ewoc node and id
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/ewoc-current-id+node (ewoc)
  "Return a cons pair ( id . node ) for the ewoc node under point."
  (let* ((node (ewoc-locate ewoc))
         (id (cdr (ewoc-data node))))
    (cons id node )))

(defun azdev/ewoc-current-id (ewoc)
  (car (azdev/ewoc-current-id+node ewoc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Updating entries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/apply-updates-current-ewoc-item (ewoc store update-item-f)
  "Update the current item from EWOC in STORE and remotely using \
function UPDATE-ITEM-F.
UPDATE-ITEM-F take the item data, and return a list of changes."
  (-let* (((item-id . node) (azdev/ewoc-current-id+node ewoc))
          (changes (funcall update-item-f (ht-get store item-id))))
    (message (pp changes))
    (when changes
      (ht-set! store item-id
               (azdev/upload-changes-to-work-item item-id changes))
      (ewoc-invalidate ewoc node))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Update server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/source-path->devops-path (source-path)
  "Convert a source path specified as a SOURCE-PATH such as \
\(field1 field2 field3 ...\) to a string field1/field2/field3/.."
  (concat "/"
          (s-join "/"
                  (mapcar
                   #'symbol-name
                   source-path))))

(defun azdev/local-key->devops-path (local-key)
  (azdev/source-path->devops-path
   (car
    (alist-get local-key azdev/response-field-mappings))))

(defun azdev/spec-to-update-remote (field value operation)
  "Return DevOps API specification of an update to FIELD to VALUE with OPERATION.
Field is either a symbol specifying the local key  of the
field (e.g. title), or a string specifying the remote key
(e.g. fields/System.AssignedTo).
OPERATION is one of \"replace\", \"delete\" etc.
"
  `((op . ,operation)
    (path . ,(if (stringp field)
                 field
               (azdev/local-key->devops-path field)))
    (value . ,value)))

(defun azdev/multi-specs-to-update-remote (changes)
  "Returns a vector of multiple specs from list of CHANGES.
CHANGES is of the form:
  '((field . new-value)
    (another-field . new-value))
where fields are specified with either their local representation as a symbol
(e.g. title) or a full remote path as a string (e.g. \"fields/System.AssignedTo \" )."
  (apply #'vector
         (mapcar
          (-lambda ((field value operation))
            (azdev/spec-to-update-remote field value (or operation "replace")))
          changes)))

(defun azdev/upload-changes-to-work-item (work-item-id changes)
  "Apply CHANGES to work item with WORK-ITEM-ID.
Return the new parsed work item.
CHANGES is as specified in azdev/multi-spacs-to-update-remote"
  (azdev/work-item-parse
   (azdev/dispatch-patch-request (concat
                                  "/wit/workitems/"
                                  (number-to-string work-item-id)
                                  "?api-version=5.1&$expand=All&bypassRules=true")
                                 (azdev/multi-specs-to-update-remote changes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar team-order (vector
                  ;;"Computational Psychology Masters"
                  "CFD parallel preprocessor"
                  "SOMBRERO"
                  ;; "FASTSUM"
                  ;; "LLR Thirring model"
                  ;;"Professional development"
                  "Maxwell-Nefem Code"
                  "Marinos Manolesos"
                  ;;"Coastal"
                  "Richard ORorke"
                  ;;"Sp(2N) BSM"
                  ;; "Cluster prioritisation"
                  "Collaboration with Don Webber"
                  ;;"Swansea Academy of Advanced Computing"
                  ;;"Training"
                  ;; "Many-flavour QCD"
                  ;;"Support activities"
                  "AIMLAC CDT"
                  ;;"Supercomputing Wales administration"
                  "Mahsa Mokhtari"
                  "FEA for Multiphysics"
                  "Performance Reporting Tools"
                  "Monte Carlo spintronics"
                  "CellProfiler"
                  "HiRep"
                  ;; "AerOpt"
                  "DLMUSN"
                  "NLP Translation Toolkit"
                  "DWF Thirring model"
                  ;;"SA2C internal operations"
                  ;;"FSI wrapper"
                  ;;"Outreach"
                  ))

(defun devops-draw ()
  (interactive)

  (pop-to-buffer azdev/buffer)

  (setq azdev/wi-ewoc
        (azdev/create-teams-ewoc! azdev/wi-store team-order))

  (devops-format azdev/wi-ewoc azdev/wi-store)

  )

(defun devops-randomise-team-order ()
  (interactive)

  (setq team-order
        (azdev/find/team-names-random-order azdev/wi-store)))

(defun devops ()
  (interactive)

  ;; reset the store
  (setq azdev/wi-store (azdev/new-store))

  (azdev/fetch-and-set-all-items azdev/wi-store)

  (if (not team-order)
      (devops-randomise-team-order))

  (devops-draw))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mapping behaviour to updates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; For now just hardcode the behaviour.
; Later this should be a major mode.

(defun azdev/update-item/set-state-new (item)
  '((state "New")))

(defun azdev/update-item/set-state-closed (item)
  '((state "Closed")))

(defun azdev/update-item/set-state-active (item)
  '((state "Active")))

(defun azdev/update-item/set-title (item)
  `((title ,(read-from-minibuffer "Title: "
                                    (alist-get 'title item)))))

(defun azdev/set-current-item-state--new ()
  (interactive)
  (azdev/apply-updates-current-ewoc-item
   azdev/wi-ewoc
   azdev/wi-store
   #'azdev/update-item/set-state-new))

(defun azdev/set-current-item-state--closed ()
  (interactive)
  (azdev/apply-updates-current-ewoc-item
   azdev/wi-ewoc
   azdev/wi-store
   #'azdev/update-item/set-state-closed))


(defun azdev/set-current-item-state--active ()
  (interactive)
  (azdev/apply-updates-current-ewoc-item
   azdev/wi-ewoc
   azdev/wi-store
   #'azdev/update-item/set-state-active))

(defun azdev/set-current-item-title ()
  (interactive)
  (azdev/apply-updates-current-ewoc-item
   azdev/wi-ewoc
   azdev/wi-store
   #'azdev/update-item/set-title))

(defun azdev/visit-current-item-www ()
  (interactive)
  (let* ((item-id (azdev/ewoc-current-id azdev/wi-ewoc))
         (url (azdev/construct-work-item-url item-id)))
    (message (concat "Visiting: " url))
    (browse-url url)))

(defun azdev/fetch-current-id ()
  (interactive)
  (message (number-to-string (azdev/ewoc-current-id azdev/wi-ewoc))))

(defun azdev/print-to-pdf ()
  (interactive)
  (my/pdf-print-buffer-with-faces "~/Desktop/devops.devops"))

(map! :leader
      :desc "Set active state"
      :n "da" #'azdev/set-current-item-state--active)

(map! :leader
      :desc "Set new state"
      :n "dn" #'azdev/set-current-item-state--new)

(map! :leader
      :desc "Set closed state"
      :n "dc" #'azdev/set-current-item-state--closed)

(map! :leader
      :desc "Set title"
      :n "dt" #'azdev/set-current-item-title)

(map! :leader
      :desc "Print id"
      :n "di" #'azdev/fetch-current-id)

(map! :leader
      :desc "Visit"
      :n "dv" #'azdev/visit-current-item-www)

(map! :leader
      :desc "Print"
      :n "dp" #'azdev/print-to-pdf)

(defun add-doom-mapping ()
  "Add a keybinding in doom emacs for devops drawing"
  (interactive)
  (map! :leader
        :desc "Devops Draw"
        :n "dd" #'devops-draw))

(provide 'devops)
;;; devops.el ends here
