;;; devops-client-tests.el -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 mark
;;
;; Author: mark <http://github/mark>
;; Maintainer: mark <mark@mark-Surface-Book>
;; Created: April 08, 2020
;; Modified: April 08, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/mark/devops-client-tests
;; Package-Requires: ((emacs 26.3) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:


(defun azdev/populate-test-store--children ()
  (azdev/new-store))

(ert-deftest azdev/test-walk-tree--empty-store ()
  "Should return nil if passed an empty store."
  (should-not (azdev/walk-tree
            (azdev/new-store)
            999)))

(cl-defun azdev-test/create-store-children (levels &optional (store (azdev/new-store)))
  "Utility function to create store with given parent child relationship.
Items are given as cons pairs or either (parend-id . (child-1-id child-2-id ...))
or a single number to indicate a node with no children. "
  (mapcar
   (lambda (item-spec)
     (if (listp item-spec)
         (let ((node-id (car item-spec))
               (children (cadr item-spec)))
           (ht-set store node-id `((id . ,node-id) (children . ,children))))
       (ht-set store item-spec `((id . ,item-spec)))))
   levels)
  store)

(defun azdev-test/print-store (store)
  "Utility function to print out the content of the store"
  (pop-to-buffer "*devops-debug*")
  (erase-buffer)
  (ht-map (lambda (item arg)
            (insert (concat (number-to-string item) ":\n"))
            (insert (pp arg)))
          store))

(ert-deftest azdev/test-walk-tree--single-level-exl ()
  "Should return nil if id not in store."
  (should-not (azdev/walk-tree
               (azdev-test/create-store-children '((10 (1 2))
                                                   1
                                                   2))
               999)))

(ert-deftest azdev/test-walk-tree--single-level ()
  "Should return non-nil if id is in store."
  (should (azdev/walk-tree
               (azdev-test/create-store-children '(1
                                                   2
                                                   3))
               1)))

(ert-deftest azdev/test-walk-tree--two-level ()
  "Should return non-nil if id is in store."
  (should (equal (azdev/walk-tree
              (azdev-test/create-store-children '((10 (1 2))
                                                  1
                                                  2))
              10)
                 '((0 . 10)
                   (1 . 1)
                   (1 . 2)))))

(ert-deftest azdev/test-walk-tree--two-level-two-items ()
  "Should only follow path from parent."
  (should (equal (azdev/walk-tree
              (azdev-test/create-store-children '((10 (1 2))
                                                  1
                                                  2
                                                  (11 (3 4 5))
                                                  3 4
                                                  5))
              10)
                 '((0 . 10)
                   (1 . 1)
                   (1 . 2)))))

(ert-deftest azdev/test-walk-tree--two-level-two-items-connected ()
  "Should only follow path from parent."
  (should (equal (azdev/walk-tree
              (azdev-test/create-store-children '((10 (1 2 11))
                                                  1
                                                  2
                                                  (11 (3 4 5))
                                                  3
                                                  4
                                                  5
                                                  (12 (6 7 8))
                                                  6
                                                  7
                                                  8))
              10)
                 '((0 . 10)
                   (1 . 1 )
                   (1 . 2 )
                   (1 . 11)
                   (2 . 3 )
                   (2 . 4 )
                   (2 . 5)))))

(defun azdev-test/create-store-with-items (items)
  "Create store from list of item cons pairs (item-id . list-item-children-ids).
Use azdev-test/item! to create the items."
  (let ((store (azdev/new-store)))
    (dolist (item items)
      (let ((id (nth 0 item))
            (children (nth 1 item))
            (work-item-type (nth 2 item)))
        (ht-set store id (azdev-test/item! id children work-item-type))))
    store))

(cl-defun azdev-test/item! (item-id &optional (children nil) (work-item-type "Work Item"))
  "Crate a fake work item for testing"
  (let ((id-str (number-to-string item-id)))
    `((id . ,item-id)
      (title . ,(concat "Title#" id-str))
      (children . ,children)
      ;; (parent . ,parent)
      (area-path . ,(concat "Area Path #" id-str))
      (team . ,(concat "team #" id-str))
      (project . ,(concat "project" id-str))
      (iteration-path . ,(concat "iteration-path #" id-str))
      (work-item-type . ,work-item-type)
      (state . ,(concat "state #" id-str))
      (reason . ,(concat "reason #" id-str))
      (assigned-to . ,(concat "assigned-to #" id-str))
      (created-date . ,(date-to-time "Fri, 25 Mar 2016 16:24:56 +0100"))
      (created-by . ,(concat "created-by #" id-str))
      (changed-date . ,(date-to-time "Fri, 25 Mar 2016 16:24:56 +0100"))
      (changed-by . ,(concat "changed-by #" id-str))
      (comment-count . ,(concat "comment-count #" id-str))
      (parent . ,(concat "parent #" id-str)))))

(defun azdev-test/list-of-strsp (strs)
  "Returns true if list of strings, nil otherwise."
  (if (listp strs)
      (-all?
       'identity
       (mapcar #'stringp strs))
    nil))

(defun azdev-test/num+str-p (arg)
  "Check that ARGS is a cons pair of type (number . string)"
  (and (numberp (car arg)) (stringp (cdr arg))))

(defun azdev-test/list-of-id+str-p (strs)
  "Check that STRS is a list of cons pairs (number . string)."
  (if (listp strs)
      (-all?
       'identity
       (mapcar #'azdev-test/num+str-p strs))
    nil))

(defun azdev-test/mock-store ()
      (azdev-test/create-store-with-items '((100 (11 12) "Epic")
                                            (11 (1 2) "Feature")
                                            (12 (3 4) "Feature")
                                            (1 nil "Work Item")
                                            (2 nil "Work Item")
                                            (3 nil "Work Item")
                                            (4 nil "Work Item"))))

(setq azdev-test/mock-store--team-name
      (alist-get 'team
                 (ht-get (azdev-test/mock-store) 100)))

(ert-deftest azdev/test-find-epics-for-a-given-team ()
  "Should find one epic for the given team"
   (should (equal
            (azdev/find/epics-for-given-team
             (azdev-test/mock-store) azdev-test/mock-store--team-name)
            '(100))))


(ert-deftest azdev/test-get-work-items-in-order ()
  "Walk tree should return depth first list of epics, featuresghbn nand tasks."
   (should (equal
            (azdev/walk-tree (azdev-test/mock-store) 100)
            '((0 . 100)
              (1 . 11)
              (2 . 1)
              (2 . 2)
              (1 . 12)
              (2 . 3)
              (2 . 4)))))

(ert-deftest test-print-from-teams-list-of-strings ()
  "team-work-item-id+level should return list LEVEL . ID cons pairs"
  (should (equal (azdev/team-work-item-id+level
                  (azdev-test/mock-store)
                  azdev-test/mock-store--team-name)
                 '((0 . 100) (1 . 11) (2 . 1) (2 . 2) (1 . 12) (2 . 3) (2 . 4)))))

(ert-deftest test-insert-blank-line-before-epics ()
  "Inserts blank before Epic entries by default"
  (let* ((store (azdev-test/mock-store))
         (ids+level (azdev/team-work-item-id+level
                     store
                     azdev-test/mock-store--team-name)))
    (should (equal
             (azdev/insert-blank-before-matching-line store ids+level)
             '((blank-line) (blank-line) (0 . 100) (1 . 11) (2 . 1) (2 . 2) (1 . 12) (2 . 3) (2 . 4))))))

(ert-deftest test-insert-blank-line-before-feature ()
  "Inserts blank before Epic entries by default"
  (let* ((store (azdev-test/mock-store))
         (ids+level (azdev/team-work-item-id+level
                     store
                     azdev-test/mock-store--team-name)))
    (should (equal
             (azdev/insert-blank-before-matching-line
              store
              ids+level
              1
              '("Feature"))
             '((0 . 100) (blank-line) (1 . 11) (2 . 1) (2 . 2) (blank-line) (1 . 12) (2 . 3) (2 . 4))))))

(ert-deftest test-print-from-teams-list-of-strings () ;
  "tree-from-teams should return a list of strings."
  (should (equal (azdev/team-work-item-id+level
                  (azdev-test/mock-store)
                  azdev-test/mock-store--team-name)
                 '((0 . 100) (1 . 11) (2 . 1) (2 . 2) (1 . 12) (2 . 3) (2 . 4)))))

;; Fetch response from server
;; (setq azdev/resp (azdev/get-request
;;                   (car
;;                    (azdev/fetch-work-item-data-urls '(47729 50376)))))

(defvar azdev-test/resp--first-work-item
  '((id . 47729)
   (rev . 4)
   (fields
    (System\.AreaPath . "Swansea Academy of Advanced Computing\\Maxwell-Nefem Code")
    (System\.TeamProject . "Swansea Academy of Advanced Computing")
    (System\.IterationPath . "Swansea Academy of Advanced Computing")
    (System\.WorkItemType . "Development Task")
    (System\.State . "Closed")
    (System\.Reason . "Moved to state Closed")
    (System\.AssignedTo
     (displayName . "Mark Dawson")
     (url . "https://spsprodweu4.vssps.visualstudio.com/A657d9976-0138-44d2-a9bc-5fab3df7945b/_apis/Identities/dbc7955d-5d00-6f50-9c18-bb1507278122")
     (_links
      (avatar
       (href . "https://dev.azure.com/swansea-university/_apis/GraphProfile/MemberAvatars/aad.ZGJjNzk1NWQtNWQwMC03ZjUwLTljMTgtYmIxNTA3Mjc4MTIy")))
     (id . "dbc7955d-5d00-6f50-9c18-bb1507278122")
     (uniqueName . "mark.dawson@Swansea.ac.uk")
     (imageUrl . "https://dev.azure.com/swansea-university/_apis/GraphProfile/MemberAvatars/aad.ZGJjNzk1NWQtNWQwMC03ZjUwLTljMTgtYmIxNTA3Mjc4MTIy")
     (descriptor . "aad.ZGJjNzk1NWQtNWQwMC03ZjUwLTljMTgtYmIxNTA3Mjc4MTIy"))
    (System\.CreatedDate . "2019-05-03T07:12:23.373Z")
    (System\.CreatedBy
     (displayName . "Mark Dawson")
     (url . "https://spsprodweu4.vssps.visualstudio.com/A657d9976-0138-44d2-a9bc-5fab3df7945b/_apis/Identities/dbc7955d-5d00-6f50-9c18-bb1507278122")
     (_links
      (avatar
       (href . "https://dev.azure.com/swansea-university/_apis/GraphProfile/MemberAvatars/aad.ZGJjNzk1NWQtNWQwMC03ZjUwLTljMTgtYmIxNTA3Mjc4MTIy")))
     (id . "dbc7955d-5d00-6f50-9c18-bb1507278122")
     (uniqueName . "mark.dawson@Swansea.ac.uk")
     (imageUrl . "https://dev.azure.com/swansea-university/_apis/GraphProfile/MemberAvatars/aad.ZGJjNzk1NWQtNWQwMC03ZjUwLTljMTgtYmIxNTA3Mjc4MTIy")
     (descriptor . "aad.ZGJjNzk1NWQtNWQwMC03ZjUwLTljMTgtYmIxNTA3Mjc4MTIy"))
    (System\.ChangedDate . "2020-04-11T19:15:41.25Z")
    (System\.ChangedBy
     (displayName . "Mark Dawson")
     (url . "https://spsprodweu4.vssps.visualstudio.com/A657d9976-0138-44d2-a9bc-5fab3df7945b/_apis/Identities/dbc7955d-5d00-6f50-9c18-bb1507278122")
     (_links
      (avatar
       (href . "https://dev.azure.com/swansea-university/_apis/GraphProfile/MemberAvatars/aad.ZGJjNzk1NWQtNWQwMC03ZjUwLTljMTgtYmIxNTA3Mjc4MTIy")))
     (id . "dbc7955d-5d00-6f50-9c18-bb1507278122")
     (uniqueName . "mark.dawson@Swansea.ac.uk")
     (imageUrl . "https://dev.azure.com/swansea-university/_apis/GraphProfile/MemberAvatars/aad.ZGJjNzk1NWQtNWQwMC03ZjUwLTljMTgtYmIxNTA3Mjc4MTIy")
     (descriptor . "aad.ZGJjNzk1NWQtNWQwMC03ZjUwLTljMTgtYmIxNTA3Mjc4MTIy"))
    (System\.CommentCount . 0)
    (System\.Title . "debug partitioning error - edit")
    (System\.BoardColumn . "Closed")
    (System\.BoardColumnDone . :json-false)
    (Microsoft\.VSTS\.Common\.StateChangeDate . "2020-03-18T09:53:03.637Z")
    (Microsoft\.VSTS\.Common\.ClosedDate . "2020-03-18T09:53:03.637Z")
    (Microsoft\.VSTS\.Common\.ClosedBy
     (displayName . "Mark Dawson")
     (url . "https://spsprodweu4.vssps.visualstudio.com/A657d9976-0138-44d2-a9bc-5fab3df7945b/_apis/Identities/dbc7955d-5d00-6f50-9c18-bb1507278122")
     (_links
      (avatar
       (href . "https://dev.azure.com/swansea-university/_apis/GraphProfile/MemberAvatars/aad.ZGJjNzk1NWQtNWQwMC03ZjUwLTljMTgtYmIxNTA3Mjc4MTIy")))
     (id . "dbc7955d-5d00-6f50-9c18-bb1507278122")
     (uniqueName . "mark.dawson@Swansea.ac.uk")
     (imageUrl . "https://dev.azure.com/swansea-university/_apis/GraphProfile/MemberAvatars/aad.ZGJjNzk1NWQtNWQwMC03ZjUwLTljMTgtYmIxNTA3Mjc4MTIy")
     (descriptor . "aad.ZGJjNzk1NWQtNWQwMC03ZjUwLTljMTgtYmIxNTA3Mjc4MTIy"))
    (WEF_BFBBEE48123140CD87AB35A89B1BF281_Kanban\.Column . "Closed")
    (WEF_BFBBEE48123140CD87AB35A89B1BF281_Kanban\.Column\.Done . :json-false)
    (WEF_F9561DF77C8E45709C75F92B2507A4E1_Kanban\.Column . "Closed")
    (WEF_F9561DF77C8E45709C75F92B2507A4E1_Kanban\.Column\.Done . :json-false)
    (System\.Parent . 47632))
   (relations .
              [((rel . "System.LinkTypes.Hierarchy-Reverse")
                (url . "https://dev.azure.com/swansea-university/3edae9a7-678f-4dd7-9da1-7708ccc5e63a/_apis/wit/workItems/47632")
                (attributes
                 (isLocked . :json-false)
                 (name . "Parent")))])
   (url . "https://dev.azure.com/swansea-university/3edae9a7-678f-4dd7-9da1-7708ccc5e63a/_apis/wit/workItems/47729")))

(defvar azdev-test/resp-parsed--first-work-item
  '((id . 47729)
    (title . "debug partitioning error - edit")
    (children)
    (parent 47632)
    (relations-raw .
                   [((rel . "System.LinkTypes.Hierarchy-Reverse")
                     (url . "https://dev.azure.com/swansea-university/3edae9a7-678f-4dd7-9da1-7708ccc5e63a/_apis/wit/workItems/47632")
                     (attributes
                      (isLocked . :json-false)
                      (name . "Parent")))])
    (area-path . "Swansea Academy of Advanced Computing\\Maxwell-Nefem Code")
    (team . "Maxwell-Nefem Code")
    (project . "Swansea Academy of Advanced Computing")
    (iteration-path . "Swansea Academy of Advanced Computing")
    (work-item-type . "Development Task")
    (state . "Closed")
    (reason . "Moved to state Closed")
    (assigned-to . "Mark Dawson")
    (created-date 23755 59863)
    (created-by . "Mark Dawson")
    (changed-date 24210 5981)
    (changed-by . "Mark Dawson")
    (comment-count . 0)
    (board-column . "Closed")
    (board-columnDone . :json-false)
    (length)
    (completed-work))
  "Expected value of work item after having been parsed")


(ert-deftest azdev-test/test-parse-individual-response-work-item ()
  "Check that parsed work item is equal to expected value."
  (should
   (equal
    (azdev/work-item-parse azdev-test/resp--first-work-item)
    azdev-test/resp-parsed--first-work-item)))

(ert-deftest azdev-test/test-convert-source-path ()
  (should (string=
           (azdev/source-path->devops-path '(fields System\.assignedTo uniqueName))
           "/fields/System.assignedTo/uniqueName")))

(ert-deftest azdev-test/test-convert-source-path ()
  (should (string=
           (azdev/local-key->devops-path 'title)
           "/fields/System.Title")))

(ert-deftest azdev-test/spec-to-update-remote ()
  "spec-to-update-remote should return the space that DevOps requires"
  (should (equal
           (azdev/spec-to-update-remote 'title "new title" "operation")
           '((op . "operation")
             (path . "/fields/System.Title")
             (value . "new title")))))

(ert-deftest azdev-test/spec-to-update-remote--explicit-path ()
  "spec-to-update-remote should return the spac that DevOps requires,
with an explicit path when provided as a string."
  (should (equal
           (azdev/spec-to-update-remote "/explicit/path" "new title" "some-op")
           '((op . "some-op")
             (path . "/explicit/path")
             (value . "new title")))))

(ert-deftest azdev-test/multi-specs-to-update-remote ()
  "Should combine return values of spec-to-update-remote"
  (should (equal
           (azdev/multi-specs-to-update-remote '((title "Some Title")
                                                 (work-item-type "Epic")))
           '[((op . "replace")
              (path . "/fields/System.Title")
              (value . "Some Title"))
             ((op . "replace")
              (path . "/fields/System.WorkItemType")
              (value . "Epic"))])))

(defun azdev/extract-dirty-and-values (work-item)
  (mapcar
   (lambda (key)
     (cons key
           (alist-get key work-item)))
   (car (alist-get 'dirty work-item))))

(ert-deftest azdev-test/extract-dirty-and-values ()
  "Extract dirty values, but not non-dirty"
  (should (equal
           (azdev/extract-dirty-and-values
            '((a "a")
              (b "b")
              (c "c")
              (d "d")
              (e "e")
              (dirty (a b c))))
           '((a "a")
             (b "b")
             (c "c")))))

(ert-deftest azdev-test/extract-dirty-and-values--dups ()
  "Ignore duplicate values."
  (should (equal
           (azdev/extract-dirty-and-values
            '((a "a")
              (b "b")
              (a "not me!")
              (c "c")
              (b "not me either!")
              (d "d")
              (e "e")
              (dirty (a b c))))
           '((a "a")
             (b "b")
             (c "c")))))

(ert-deftest azdev-test/extract-dirty-and-values--dups-dirty ()
  "Ignore duplicate values."
  (should (equal
           (azdev/extract-dirty-and-values
            '((a "a")
              (b "b")
              (c "c")
              (d "d")
              (e "e")
              (dirty (a b c b))))
           '((a "a")
             (b "b")
             (c "c")
             (b "b")))))

(ert-deftest azdev-test/extract-dirty-and-values--missing ()
  "No value when missing - use this to signify deletion."
  (should (equal
           (azdev/extract-dirty-and-values
            '((a "a")
              (b "b")
              (d "d")
              (e "e")
              (dirty (a b c))))
           '((a "a")
             (b "b")
             (c)))))


(ert-deftest azdev/test--string-list-for-task ()
  (should
   (equal
    (azdev/--string-list-for-task
     (azdev-test/item! 100 nil "Work Item"))
    '(":100:     " "Title#100                               " "state #100" "assigned-to ..." "Work Item      " "2016-03-25 "))))


(ert-deftest azdev/test-string-list-for-task ()
  (should
   (equal
    (azdev/string-for-task
     (azdev-test/item! 100 nil "Work Item")
     1)
    (apply #'concat
           '(":100:     " "Title#100                               " "state #100" "assigned-to ..." "Work Item      " "2016-03-25 " "\n")))))

(provide 'devops-client-tests)
;;; devops-client-tests.el ends here
