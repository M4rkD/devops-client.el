



(az-devops/find-all-relations-ids wi-store)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq t+e
(teams-and-epics)
      )

(car t+e)

(print-teams-and-epics wi-store (car t+e))

(alist-get 'relations (ht-get wi-store 50365))



 (print-node-ids wi-store
(az-devops/fetch-epics-by-team wi-store "AerOpt"))

 (az-devops/walk-tree-printing wi-store 46171)

 ;;;;;;;;;;;;;;;;;;;;;;;;;
 (setq data (ht-get wi-store 46171))
 (setq children (az-devops/work-item->relations-ids data))
(setq child-result (mapcar
                         (lambda (node-id)

                           )
                         children))


 (az-devops/print-work-item  #'format)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Playground
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(az-devops/fetch-all-relations wi-store)

(az-devops/find-all-relations-ids wi-store)

(az-devops/fetch-work-items wi-store (list 45550 45554 45555 45556 45557 45558 45559 45560))

(setq all-epics
      (az-devops/fetch-all-epics wi-store))

(setq aeropt-epics (az-devops/fetch-epics-by-team
      wi-store
      "AerOpt"))

(az-devops/work-item->relations-ids (ht-get wi-store 45554 ))




(length wi-store)


(IDS-work-item (gethash id wi-store) #'format)


(gethash 45555 wi-store)


;; walk the tree
;;

(defun az-devops/work-items->work-items-alist (work-items)
  "Convert a list of WORK-ITEMS to alist with lookup by id."
  (mapcar
   (lambda (item)
     `(,(alist-get 'id item) . ,(list item)))
   work-items))

(defun az-devops/populate-unfetched-ids (items work-item-alist)
  "Populate missing entries in ITEMS from alist WORK-ITEM-ALIST.
WORK-ITEM-ALIST is an association list of ids, like: (id . (work-item))"
  (mapcan
   (lambda (item)
     (let ((children (alist-get 'children item)))
       ;; list of either child ids, or ids at this level
       (if children
           ;; iterate into children if they exist
           (az-devops/populate-unfetched-ids children work-item-alist)
         ;; if they don't exist, set ids a this level
         (...!)
         )))
   items))

def add_unfetched_children(...):
...
(setq ids (az-devops/find-unfetched-ids all-epics))
(setq wis (az-devops/fetch-work-items ids))
(setq wisa (az-devops/work-items->work-items-alist wis))

wis




...



(alist-get 'rel (elt (alist-get 'relations (elt all-epics 1)) 0))

(work-item->relations-ids (elt all-epics 3))



(mapcan
 (lambda (i)
        (append nil '(6)))
 (list 3))


(length (find-missing-ids all-epics))

(length (delete-dups (find-missing-ids all-epics)))

(setq item
(elt all-epics 1)
      )

(let ((item (elt (alist-get 'relations item) 0)))

(if (string= (alist-get 'rel item)
                       "System.LinkTypes.Hierarchy-Forward")
                    (relation->id item)
                  nil))

(setq
 a
)

(alist-get 'he '((hi . 6)))

                (if (string= (alist-get 'rel a)
                       "System.LinkTypes.Hierarchy-Forward")
                    (relation->id a)
                  nil)

(listp )

(work-item->relations-ids item)


(mapcar 'cdr (mapcar 'car all-epics))



(setq all-epics (az-devops/all-epics))

(setq item
(elt all-epics 0)
      )

(alist-get 'id item)
(setq fields (alist-get 'fields item))
(setq relation (elt (alist-get 'relations item) 0))


(relation->id relation)

(mapcar 'car fields)


(setq ids (az-devops/extract-ids-from-query-response all-epics))

(setq chunked-list (az-devops/--fetch-work-items--chunk-list ids az-devops/query-chunk-size))

(elt vals 3)

(append [1 2 3] [1 2 3] nil)



(setq item (car request-response)
      )


(reduce
 'nconc
 )

(-reduce-from
 (lambda (result item)
   (append result (alist-get 'value item))
   ;;(nconc result item)
   )
 '()

 )
(append
(alist-get 'value item)
(alist-get 'value item)
 )

ddd

;; testing API
;; (comment
;;  (setq all-teams (az-devops/get-teams))
;;
;;  ;; AerOpt
;;  (setq wiql (format "SELECT * FROM workitems WHERE [System.WorkItemType] = 'EPIC' AND [System.AreaPath] = 'Swansea Academy of Advanced Computing\\%s'" team))
;;
;;  (setq q-resp (query wiql))
;;
;;
;;  (setq ids (az-devops/extract-ids-from-query-response all-epics))
;;
;;  )

;; testing the api
;; (az-devops/get-projects)
;;
;; (az-devops/get-teams "Swansea%20Academy%20of%20Advanced%20Computing")

setq a (make-hash-table :test eql))


(puthash 123 1 a)

(defun az-devops/find-unfetched-ids (items)
  "Find the ids of unfetched children a tree"
  (mapcan
   (lambda (item)
     (let ((children (alist-get 'children item)))
       ;; list of either child ids, or ids at this level
       (if children
           ;; iterate into children if they exist
           (az-devops/find-unfetched-ids children)
         ;; if they don't exist, fetch ids a this level
         (work-item->relations-ids item)
         )))
   items))
(setq team "AerOpt")

(setq ids
(az-devops/extract-ids-from-query-response
(az-devops/query
(apply #'format '("SELECT * FROM workitems WHERE [System.WorkItemType] = 'EPIC' AND \
[System.AreaPath] = 'Swansea Academy of Advanced Computing\\AerOpt'")))))

ids


(az-devops/query-work-items wi-store
"SELECT * FROM workitems WHERE [System.WorkItemType] = 'EPIC' AND \
[System.AreaPath] = 'Swansea Academy of Advanced Computing\\AerOpt'")
