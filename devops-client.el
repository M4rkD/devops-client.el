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

(defun az-devops/new-store ()
  (ht-create))

(defvar az-devops/base-url "https://dev.azure.com/swansea-university/_apis"
  "The base url of the organisation.")

(defvar az-devops/default-project "Swansea%20Academy%20of%20Advanced%20Computing"
  "The default project.")

(defvar az-devops/query-chunk-size 200
  "The maximum number of work items to fetch in one go.")

(defvar az-devops/wi-store (az-devops/new-store)
  "Default work item store.")

(defvar az-devops/buffer "*devops*")

(defvar az-devops/token-file-path "~/.azure-devops-token")

(defvar az-devops/auth-token nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tokens
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun az-devops/load-token ()
  "Return token from az-devops/toekn-file-path."
  (with-temp-buffer
    (insert-file-contents az-devops/token-file-path)
    (s-trim
     (buffer-string))))

(defun az-devops/read-token ()
  (setq az-devops/auth-token (az-devops/load-token)))

(defun default-headers ()
  `(("Authorization" . ,(concat "Basic " az-devops/auth-token))
    ("Content-Type" . "application/json")))

(az-devops/read-token)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Communications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun az-devops/dispatch-get-request (uri)
  "Dispatch GET request to endpoint URI (with json parsing)."
  (let ((url (concat az-devops/base-url uri)))
    (message "Calling: %s" url)
    (request
      url
      :headers (default-headers)
      :parser 'json-read
      :sync t)))

(defun az-devops/get-request (uri)
  "GET from URI of the current project."
  (request-response-data
   (az-devops/dispatch-get-request uri)))

(defun az-devops/query (wiql)
  "Fetch the result of the the wiql query string given by WIQL."
  ;; wiql api used is documented at: https://docs.microsoft.com/en-us/rest/api/azure/devops/wit/wiql/query%20by%20wiql?view=azure-devops-rest-5.1
  (let ((wiql-url (format "%s/wit/wiql?api-version=5.1" az-devops/base-url)))
    (message "Wiql: %s" wiql)
    (request-response-data
     (request
       wiql-url
       :type "POST"
       :headers (default-headers)
       :data (json-encode `(("query" . ,wiql)))
       :parser 'json-read
       :success (cl-function
                 (lambda (&key data &allow-other-keys)
                   (message "I sent: %S" (assoc-default 'args data))))
       :sync t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fetch/store work items by ID
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun az-devops/--fetch-work-items--chunked-list->urls (chunked-list)
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

(defun az-devops/filter-ids-not-in-store (store ids)
  "Given a STORE and a list of IDS, return the ids not in the store."
  (seq-filter
   (lambda (key)
     (not (ht-contains? store key)))
   ids))

(defun az-devops/work-item-parse (work-item)
  "Transform the downloaded json into a work item"
  (let ((fields (alist-get 'fields work-item)))
    `((title ,(alist-get 'System\.Title fields))
      (area-path ,(alist-get 'System\.AreaPath fields))
      (team-project ,(alist-get 'System\.TeamProject fields))
      (iteration-path ,(alist-get 'System\.IterationPath fields))
      (work-item-type ,(alist-get 'System\.WorkItemType fields))
      (state ,(alist-get 'System\.State fields))
      (reason ,(alist-get 'System\.Reason fields))
      (assigned-to ,(id->identity 'System\i.AssignedTo fields))
      (created-date ,(alist-get 'System\.CreatedDate fields))
      (created-by ,(id->identity 'System\.CreatedBy fields))
      (changed-date ,(alist-get 'System\.ChangedDate fields))
      (changed-by ,(id->identity 'System\.ChangedBy fields))
      (comment-count ,(alist-get 'System\.CommentCount fields))
      (board-column ,(alist-get 'System\.BoardColumn fields))
      (board-columnDone ,(alist-get 'System\.BoardColumnDone fields))
      (length ,(alist-get 'Custom\.Length fields))
      (parent ,(alist-get 'System\.Parent fields)))))

(defun az-devops/work-item-->store (store item)
  "Puts the work ITEM in the hash STORE by id, and return id."
  (let ((id (alist-get 'id item)))
    (ht-set! store id item)
    id))

(defun az-devops/fetch-work-items (store ids)
  "Fetch each work item in the list IDS and store in STORE.
If the entry already exists, don't download again.
Return the ids of the downloaded item (which may be smaller
than the ids requested).
Note: Each response is an association lists, which contains a value key.
The value key is a vector of values."
  (mapcan
   (lambda (url)
     (let* ((response (az-devops/get-request url))
            (values (alist-get 'value response)))
       (mapcar
        (lambda (item)
          ;; handle the item in the request
          (az-devops/work-item-->store store (az-devops/work-item-parse item)))
        values)))
   (az-devops/--fetch-work-items--chunked-list->urls
    (-partition-all
     az-devops/query-chunk-size
     (az-devops/filter-ids-not-in-store
      store
      ids)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Query work items
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun az-devops/extract-ids-from-query-response(wiql-query-response)
  "Extract a list of ids from the WIQL-QUERY-RESPONSE response."
  (mapcar (apply-partially 'alist-get 'id)
          (alist-get 'workItems wiql-query-response)))

(defun az-devops/query-work-items (store &rest wiql)
  "Fetch work items for a given WIQL query into STORE.
Return the ids of the query results.
WIQL can be a string or a format string, such as that passed to message or
the format function."
  (let ((ids (az-devops/extract-ids-from-query-response
              (az-devops/query
               (apply #'format wiql)))))
    (az-devops/fetch-work-items store ids)
    ids))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specific requests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun az-devops/get-projects ()
  "Fetch a list of all projects."
   (az-devops/GET-request "/projects"))

(defun az-devops/get-teams ()
  "Fetch a list of all teams for the current project."
  (alist-get 'value
             (request-response-data
              (az-devops/dispatch-get-request
               (concat "/projects/" az-devops/default-project "/teams")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specific queries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun az-devops/fetch-epics-by-team (store team)
  "Fetch all epics for a given TEAM into STORE.
Return ids of epics.
This function assumes that each team maps to an AreaPath."
  (az-devops/query-work-items
   store
    "SELECT * FROM workitems WHERE [System.WorkItemType] = 'EPIC' AND \
[System.AreaPath] = 'Swansea Academy of Advanced Computing\\%s'" team))


(defun az-devops/fetch-all-epics (store)
  "Fetch all epics as a list of ids stored in STORE."
  (az-devops/query-work-items
   store
   "SELECT * FROM workitems WHERE [System.WorkItemType] = 'EPIC'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun az-devops/print-it? (data level)
  (let* ((prefix (az-devops/prefix-printing-function level))
         (id (alist-get 'id data))
         (id (if (numberp id) (number-to-string id)))
         (fields (alist-get 'fields data))
         (label (alist-get 'System\.Title fields))
         (wi-type (alist-get 'System\.WorkItemType fields))
         (wi-state (alist-get 'System\.State fields))
         (assigned-to (alist-get 'displayName (alist-get 'System\.AssignedTo fields))))
    (not (string= wi-state "Closed"))))


(cl-defun az-devops/print-work-item (data printer &optional (level 0))
  "Given work item DATA, print it using the PRINTER function.

Printer is a function such as #'format or #'message"
  (if (az-devops/print-it? data level)
      (let* ((prefix (az-devops/prefix-printing-function level))
             (id (alist-get 'id data))
             (id (if (numberp id) (number-to-string id)))
             (fields (alist-get 'fields data))
             (label (alist-get 'System\.Title fields))
             (wi-type (alist-get 'System\.WorkItemType fields))
             (wi-state (alist-get 'System\.State fields))
             (assigned-to (alist-get 'displayName (alist-get 'System\.AssignedTo fields))))
        (funcall printer "%s %s  [%s]   <%s>        %s" prefix label id wi-state assigned-to))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extract relation information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun az-devops/relation->id (relation)
  "Return the id (as a number) for a RELATION.
The id is extracted as the last portion of the url to the relation"
  (string-to-number
   (car
    (last
     (split-string
      (alist-get 'url relation)
      "/")))))

(defun az-devops/work-item->relations-ids (work-item)
  "For a work item, extract ids of Hierarchy-Forward relations"
  (mapcan
   (lambda (item)
     (if (string= (alist-get 'rel item)
                  "System.LinkTypes.Hierarchy-Forward")
         (let ((id (az-devops/relation->id item)))
           (if (listp id)
               id
             (list id)))
       nil))
   (alist-get 'relations work-item)))

(defun az-devops/find-all-relations-ids (store)
  "Find every unique ids in a relation in STORE"
  (delete-dups
  (mapcan 'identity
   (ht-map
    (lambda (k v)
      (if-let ((relations (az-devops/work-item->relations-ids v)))
         relations))
    store))))

(defun az-devops/fetch-all-relations (store)
  "Fetches all missing relations in STORE.
This works by finding all the relations, and passing them all to
fetch-work-items. This works because the function fetch-work-items
does the filtering."
  (if-let ((ids (az-devops/find-all-relations-ids store)))
      (progn
        ;; Download work items
        (az-devops/fetch-work-items store ids)
        ;; recursive call until all relations downloaded
        (az-devops/fetch-all-relations store))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Walk the tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

 (defun az-devops/--listify (o)
   "Returns a list with a single element O, if O is not a list.
Otherwise return O unchanged."
   (if (listp o)
       o
     (list o)))

 (cl-defun az-devops/walk-tree (store curr-node func level)
   "Walk the tree, calling the function FUNC at each node.
Tree is walked by walking through parents, and mapping over each child for each parent. There is probably a more elegant way to return the result.
Return a list containing the results of each application of FUNC, in the order performed, in a flattened list."
   (let* ((data (ht-get store curr-node))
          (node-result (funcall func store curr-node level))
          (children (az-devops/work-item->relations-ids data))
          (child-result (mapcar
                         (lambda (node)
                           (az-devops/walk-tree store node func (1+ level)))
                         children)))

     (if children
         (-flatten
          (append
           (az-devops/--listify node-result)
           (az-devops/--listify child-result)))
       node-result)))

 (defun az-devops/prefix-printing-function (level)
   "Adds starts to the start, to emulate org mode prefixes"
   (concat (apply 'concat (make-list (+ level 2) "*")) " "))

 (defun az-devops/walk-tree-printing (store start-node)
   (az-devops/walk-tree store
                        start-node
                        (lambda (store node-id level)
                           (az-devops/print-work-item (ht-get store node-id) #'az-devops/buffer-insert-ln level))
                        0))

 (defun print-node-ids (store ids)
   "Printes the provided item IDS from STORE."
   (mapcar
    (lambda (item-id)
      (az-devops/print-work-item (ht-get store item-id) #'format))
    ids))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun az-devops/clear-buffer ()
  (with-current-buffer az-devops/buffer
    (erase-buffer)))

(defun az-devops/buffer-insert-ln (&rest str)
  (az-devops/buffer-insert (concat (apply #'format str) "\n")))

(defun az-devops/buffer-insert (str)
  (with-current-buffer az-devops/buffer
    (save-excursion
     (goto-char (point-max))
     (insert str)
     str)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fetching teams and epics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun teams-and-epics (store teams)
  (mapcar
   (lambda (team)
     `((name . ,(alist-get 'name team))
       (id . ,(alist-get 'id team))
       (epics . ,(az-devops/fetch-epics-by-team store (alist-get 'name team)))))
   teams))

(defun print-single-team-and-epics (store team-and-epics)
  (az-devops/buffer-insert-ln "* %s" (alist-get 'name team-and-epics))
  (let ((epics (alist-get 'epics team-and-epics)))
    (mapcar
     (lambda (epic)
       (az-devops/walk-tree-printing store epic))
     epics)))


(defun print-all-teams-and-epics (store teams-and-epics)
  (az-devops/clear-buffer)
  (mapcar
   (lambda (team+epic)
     (print-single-team-and-epics store team+epic)
     )
   teams-and-epics))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive funcations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun devops-fetch-all ()
  (interactive)
  ;; fetch info on teams
  (setq az-devops/teams (az-devops/get-teams))
  ;; seed the store with team epics
  (setq az-devops/teams-and-epics
        (teams-and-epics az-devops/wi-store az-devops/teams))
  ;; update the store with all relations
  (az-devops/fetch-all-relations az-devops/wi-store))

(defun devops-refresh ()
  (interactive)
  (setq az-devops/wi-store (az-devops/new-store))
  (devops-fetch-all))


(defun devops ()
  (interactive)
  (devops-refresh)
  (switch-to-buffer az-devops/buffer)
  (print-all-teams-and-epics az-devops/wi-store
                             az-devops/teams-and-epics)
  (org-mode))


(provide 'devops)
;;; devops.el ends here
