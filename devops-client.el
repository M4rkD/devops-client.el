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

(defun default-headers ()
  `(("Authorization" . ,(concat "Basic " azdev/auth-token))
    ("Content-Type" . "application/json")))

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

(defun azdev/--dispatch-request (uri method data)
  "Dispatch request to endpoint URI (with json parsing) and METHOD.
Return parsed json data as an alist.
METHOD should be a string such as \"GET\" or \"POST\""
  (let ((url (concat azdev/base-url uri)))
    (message "Calling: [%S] %s" method url)
    (request-response-data
    (if data
        (request
          url
          :type method
          :parser 'json-read
          :headers (default-headers)
          :sync t
          :data data)
      (request
        url
        :type method
        :parser 'json-read
        :headers (default-headers)
        :sync t)))))

(defun azdev/get-request (uri)
  "GET from URI of the current project."
  (azdev/dispatch-get-request uri))

(defun azdev/query (wiql)
  "Fetch the result of the the wiql query string given by WIQL."
  ;; wiql api used is documented at: https://docs.microsoft.com/en-us/rest/api/azure/devops/wit/wiql/query%20by%20wiql?view=azure-devops-rest-5.1
  (let ((wiql-uri "/wit/wiql?api-version=5.1"))
    (message "Wiql: %s" wiql)
    (azdev/dispatch-post-request wiql-uri (json-encode `(("query" . ,wiql))))))


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
            "&$expand=Relations&errorPolicy=Omit"))
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
    (assigned-to (fields System\.AssignedTo uniqueName))
    (created-date (fields System\.CreatedDate) (date-to-time))
    (created-by (fields System\.CreatedBy uniqueName))
    (changed-date (fields System\.ChangedDate) (date-to-time) )
    (changed-by (fields System\.ChangedBy uniqueName))
    (comment-count (fields System\.CommentCount))
    (board-column (fields System\.BoardColumn))
    (board-columnDone (fields System\.BoardColumnDone))
    (length (fields Custom\.Length)))
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

(defvar azdev-faces-alist '((epic . azdev-epic)
                            (feature . azdev-feature)
                            (dev-task . azdev-dev-task)
                            (admin-task . azdev-admin-task)
                            (heading . azdev-heading)))

(defface azdev-epic
  '((default :foreground "#FF7B00"
      :height 1.5
      :background "white"
      :weight ultra-bold))
       "Basic face for highlighting."
       :group 'azdev-faces)

(defface azdev-feature
  '((default :foreground "#773B93"
      :height 1.2
      :weight bold))
  "Basic face for highlighting."
       :group 'azdev-faces)

(defface azdev-dev-task
  '((default :foreground "black"  ;; "#FBD144"
      :background "default"))
       "Basic face for highlighting."
       :group 'azdev-faces)

(defface azdev-admin-task
  '((default :foreground "#0D60AB"
      :background "default"))
       "Basic face for highlighting."
       :group 'azdev-faces)

(defface azdev-heading
  '((default :foreground "white"
      :background "DimGray"))
       "Basic face for highlighting."
       :group 'azdev-faces)

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

(defun azdev/prefix-formatting-function (level)
  "Add start of depth LEVEL to the string start, to emulate org mode prefixes."
  (apply 'concat (make-list (+ level) "    ")))

(defun azdev/convert-string-to-face-symbol (id)
  (cond
   ((string= id "Epic") 'epic)
   ((string= id "Feature") 'feature)
   ((string= id "Development Task") 'dev-task)
   ((string= id "Admin Task") 'admin-task)
   (t id))
  )

(defun azdev/face (id &rest str)
  "Choose the font face by identifier ID and apply to STR."
  (let* ((ident (azdev/convert-string-to-face-symbol id))
         (str (apply #'concat str))
         (face (or (alist-get ident azdev-faces-alist)
                   'default)))
    (put-text-property 0 (length str) 'face face str)
    str))

(defun azdev/string-for-epic-or-feature (data level)
  "Return a display string for a heading (either an epic or a feature) given by DATA at LEVEL."
  (let* ((prefix (azdev/prefix-formatting-function level))
         (id (alist-get 'id data))
         (team (alist-get 'team data))
         (label (alist-get 'title data))
         (wi-type (alist-get 'work-item-type data)))
    (concat
     prefix
     (azdev/face wi-type
                 label
                 (if (string= wi-type "Epic")
                     (s-pad-left (- (window-width) (length label)) " " team))))))

(defun azdev/string-for-task (data level)
  (let* ((prefix (azdev/prefix-formatting-function level))
         (id (alist-get 'id data))
         (label (alist-get 'title data))
         (changed-date (format-time-string "%Y-%m-%d"
                                           (alist-get 'changed-date data)))
         (wi-type (alist-get 'work-item-type data))
         (wi-type (alist-get 'work-item-type data))
         (wi-state (concat "[" (s-pad-right 8 " " (alist-get 'state data)) "]"))
         (assigned-to (s-pad-right
                       20
                       " "
                       (s-downcase
                        (car
                         (s-split "@"
                                  (if-let ((name (alist-get 'assigned-to data)))
                                      name
                                    "---"))))))
         (pad-len (- 70 (length prefix))))
    (concat
     (azdev/face wi-type
                 (azdev/face 'prefix prefix)
                 " "
                 (azdev/face 'label (s-truncate pad-len (s-pad-right pad-len " " label)))
                 "  "
                 (azdev/face 'state wi-state)
                 "   "
                 (azdev/face 'assigned assigned-to)
                 "       ")

     (number-to-string id)

     "    "
     (azdev/face wi-type changed-date))))

(cl-defun azdev/string-for-work-item (data &optional (level 0))
  "Given work item DATA, print it using the PRINTER function.

Printer is a function such as #'format or #'message"
  (let* ((wi-type (alist-get 'work-item-type data)))
    (if (or (string= wi-type "Epic")
                (string= wi-type "Feature"))
            (azdev/string-for-epic-or-feature data level)
          (azdev/string-for-task data level))))

(defun azdev/heading-as-formatted-string (heading-string)
  "Format the HEADING-STRING as a header."
  (azdev/face 'heading (s-center (window-body-width) heading-string)))

(defun azdev/team-work-item-id+level (store team-name)
  "Get (level . id) cons pairs for items to show."
  (mapcan (lambda (epic-id)
            (azdev/walk-tree store epic-id))
          (azdev/find/epics-for-given-team store team-name)))


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
    (if level
        (insert
         (azdev/string-for-work-item (ht-get azdev/wi-store id) level))
      ;; a nil level means this is a heading
      (insert (azdev/heading-as-formatted-string id)))))


(defun azdevops/add-team-items-to-ewoc (ewoc store teams)
  "Print the lines for all TEAMS using insert."
  (mapc
   (lambda (team-name)
     (ewoc-enter-last ewoc
                      `(nil . ,team-name))

     (azdev/add-items-to-ewoc
      ewoc
      (azdev/team-work-item-id+level store team-name)))
   teams))

(cl-defun azdev/add-items-to-ewoc (ewoc ids+level)
  "Prints the provided item IDS from STORE."
  (mapc
   (lambda (id+level)
     (ewoc-enter-last ewoc id+level))
   ids+level))

(defun azdev/create-teams-ewoc! (store teams)
  (pop-to-buffer "*devops-teams*")
  (erase-buffer)
  (let ((ewoc
         (ewoc-create
          #'azdev/ewoc-printer)))
    (azdevops/add-team-items-to-ewoc ewoc store teams)
    ewoc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interacting with ewoc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive funcations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq team-order (vector
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

(defvar team-order nil)

(defun devops-draw ()
  (interactive)

  (setq azdev/wi-ewoc
        (azdev/create-teams-ewoc! azdev/wi-store team-order)))

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


(defun add-doom-mapping ()
  "Add a keybinding in doom emacs for devops drawing"
  (interactive)
  (map! :leader
        :desc "Devops Draw"
        :n "dd" #'devops-draw))

(provide 'devops)
;;; devops.el ends here
