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

(setq azdev-test/mock-store--team-name
      (alist-get 'team
                 (ht-get azdev-test/mock-store 100)))

(setq azdev-test/mock-store
      (azdev-test/create-store-with-items '((100 (11 12) "Epic")
                                            (11 (1 2) "Feature")
                                            (12 (3 4) "Feature")
                                            (1 nil "Work Item")
                                            (2 nil "Work Item")
                                            (3 nil "Work Item")
                                            (4 nil "Work Item"))))

(ert-deftest azdev/test-fetch-formatted-strings-returns-list-of-strings ()
  "print-formatted-strings should return list of strings for printing."
  (should (azdev-test/list-of-id+str-p
           (azdev/fetch-formatted-strings
            azdev-test/mock-store
            '((0 . 100)
              (1 . 11)
              (2 . 1 )
              (2 . 2 )
              (1 . 12)
              (2 . 3 )
              (2 . 4))))))

(ert-deftest azdev/test-find-epics-for-a-given-team ()
  "Should find one epic for the given team"
   (should (equal
            (azdev/find/epics-for-given-team
             azdev-test/mock-store azdev-test/mock-store--team-name)
            '(100))))


(ert-deftest azdev/test-get-work-items-in-order ()
  "Walk tree should return depth first list of epics, features and tasks."
   (should (equal
            (azdev/walk-tree azdev-test/mock-store 100)
            '((0 . 100)
              (1 . 11)
              (2 . 1)
              (2 . 2)
              (1 . 12)
              (2 . 3)
              (2 . 4)))))

(ert-deftest test-print-from-teams-list-of-strings ()
  "tree-from-teams should return a list of strings."
  (should (azdev/team-work-item-lines
            azdev-test/mock-store
            azdev-test/mock-store--team-name)))

(provide 'devops-client-tests)
;;; devops-client-tests.el ends here
