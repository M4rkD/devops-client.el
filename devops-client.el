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

(defun id->identity (key fields)
  "
  Returns a standardised string describing identity based KEY of fields
returned by DevOps for a given work item.

identities returned by DevOps are of the form:

(displayName . \"My Name\")
(url . \"...\")
(_links (avatar
          (href . \"...\")))
id . \"...)
(uniqueName . \"unique@web.com\")
(imageUrl . \"...\")
(descriptor . \"...\"))
"
  (let* ((identity-alist (alist-get key fields))
        (uniqueName (alist-get 'uniqueName identity-alist)))
    uniqueName))

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

(defun azdev/work-item-parse (work-item)
" Transform the downloaded json into a work item"
(let* ((fields (alist-get 'fields work-item))
       (relations (alist-get 'relations work-item))
       (area-path (alist-get 'System\.AreaPath fields))) ;; the work item is an alist
    `((id . ,(alist-get 'id work-item))
      (title . ,(alist-get 'System\.Title fields))
      (children . ,(azdev/get-relation-matching-attributes-name relations "Child"))
      (parent . ,(azdev/get-relation-matching-attributes-name relations "Parent"))
      (relations-raw . ,relations)
      (area-path . ,area-path)
      (team . ,(car
               (last
                (s-split "\\\\" area-path))))
      (project . ,(alist-get 'System\.TeamProject fields))
      (iteration-path . ,(alist-get 'System\.IterationPath fields))
      (work-item-type . ,(alist-get 'System\.WorkItemType fields))
      (state . ,(alist-get 'System\.State fields))
      (reason . ,(alist-get 'System\.Reason fields))
      (assigned-to . ,(id->identity 'System\.AssignedTo fields))
      (created-date . ,(alist-get 'System\.CreatedDate fields))
      (created-by . ,(id->identity 'System\.CreatedBy fields))
      (changed-date . ,(alist-get 'System\.ChangedDate fields))
      (changed-by . ,(id->identity 'System\.ChangedBy fields))
      (comment-count . ,(alist-get 'System\.CommentCount fields))
      (board-column . ,(alist-get 'System\.BoardColumn fields))
      (board-columnDone . ,(alist-get 'System\.BoardColumnDone fields))
      (length . ,(alist-get 'Custom\.Length fields))
      (parent . ,(alist-get 'System\.Parent fields)))))

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
;; Buffer management
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/clear-buffer ()
  (with-current-buffer azdev/buffer
    (erase-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search and filter store
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (azdev/find/epics-for-given-team azdev/wi-store "AerOpt")

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

(defun azdev/--listify (o)
  "Returns a list with a single element O, if O is not a list.
Otherwise return O unchanged."
  (if (listp o)
      o
    (list o)))

(cl-defun azdev/walk-tree (store curr-node func level)
  "Walk the tree, calling the function FUNC at each node.
Tree is walked by walking through parents, and mapping over each child for each parent. There is probably a more elegant way to return the result.
Return a list containing the results of each application of FUNC, in the order performed, in a flattened list."
  (let* ((data (ht-get store curr-node))
          (node-result (funcall func store curr-node level))
          (children (alist-get 'children data))
          (child-result (mapcar
                        (lambda (node)
                          (azdev/walk-tree store node func (1+ level)))
                        children)))

    (if children
        (-flatten
          (append
          (azdev/--listify node-result)
          (azdev/--listify child-result)))
      node-result)))

(defun azdev/walk-tree-printing (store start-node)
  (azdev/walk-tree store
                        start-node
                        (lambda (store node-id level)
                          (azdev/print-work-item (ht-get store node-id) level))
                        0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar azdev-faces-alist '((epic . 'azdev-epic)
                            (feature . 'azdev-feature)
                            (dev-task . 'azdev-dev-task)
                            (admin-task . 'azdev-admin-task)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Printing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun azdev/prefix-printing-function (level)
  "Adds starts of depth LEVEL to the string start, to emulate org mode prefixes"
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

(cl-defun azdev/print-work-item (data &optional (level 0))
  "Given work item DATA, print it using the PRINTER function.

Printer is a function such as #'format or #'message"
  (let* ((prefix (azdev/prefix-printing-function level))
         (id (alist-get 'id data))
         (label (alist-get 'title data))
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
    (if (or (string= wi-type "Epic")
            (string= wi-type "Feature"))
        (insert
         
         prefix
         (azdev/face wi-type
                     label
                     (if (string= wi-type "Epic")
                         (concat "[" (alist-get 'team data) "]"))
                     "\n"))
      (insert
       (azdev/face wi-type
                   (azdev/face 'prefix prefix)
                   " "
                   (azdev/face 'label (s-truncate pad-len (s-pad-right pad-len " " label)))
                   "  "
                   (azdev/face 'state wi-state)
                   "   "
                   (azdev/face 'assigned assigned-to)
                   "       "
                   (azdev/face 'type (number-to-string id))
                   "\n")))))

(cl-defun print-ids (store ids &optional (pri))
  "Prints the provided item IDS from STORE."
  (mapcar
    (lambda (item-id)
      (azdev/print-work-item (ht-get store item-id)))
    ids))

(defun azdev/print/tree-from-teams (teams)
  (mapcar
   (lambda (team-name)
     (azdev/print-team-header team-name)
     (mapcar (lambda (epic-id)
               (azdev/walk-tree-printing azdev/wi-store epic-id))
             (azdev/find/epics-for-given-team azdev/wi-store team-name)))
   teams))


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
;;; Interactive funcations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun devops-draw ()
  (interactive)

  (switch-to-buffer azdev/buffer)
  (azdev/clear-buffer)

  (azdev/print/tree-from-teams
   (azdev/find/team-names-random-order azdev/wi-store)))

(defun devops ()
  (interactive)

  ;; reset the store
  (setq azdev/wi-store (azdev/new-store))

  (azdev/fetch-and-set-all-items azdev/wi-store)

  (devops-draw))

(provide 'devops)
;;; devops.el ends here
