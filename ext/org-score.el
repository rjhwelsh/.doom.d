;;; ext/org-score.el -*- lexical-binding: t; -*-

;; Org-score provides functions / methods to score the priority of org-agenda items

;; TODO  Obtain a numerical representation of the priority value
;; TODO  org-agenda-score-entry-at-point function, see org-agenda sorting function below

(require 'org)
(defvar org-score-vector nil
  "A list of functions and polynomial score coefficients used to calculate the score of an org entry at-point.

Each list element consists of a FUNCTION SYMBOL and a series of FLOATs like '('org-score-get-todo-state-number 1.0 2.0).
If the function returns nil at POM, the score returned will be 0, disregarding any coefficients for that element.
Each function must take a single POM (point-marker) argument.

For a single element, n:
Let '(F k0 k1); the score, Sn is calculated as follows:
Sn = k0 + k1*F + k2*F^2 ...

The scores for all the elements in org-score-vector are added together to form the final score.
Stotal = S0 + S1 + S2 + S3 + ....
")

(defun org-score-score-at-marker (pom)
  "Returns the score of org-entry at marker POM based on the `org-score-vector' values."
  (if org-score-vector
      (apply '+
             (mapcar
              (lambda (score-vec)
                (let ((value-n
                       (funcall (car score-vec) pom)) ;; Each function should be able to be called with pom only
                      (klist (cdr score-vec))         ;; List of coeffs, k0, k1, k2 ...
                      (n 0)
                      (result 0))
                  (if value-n
                      (dolist (k klist result)
                        (setq result (+ result (* k (expt value-n n))))
                        (setq n (1+ n)))
                    0)))
              org-score-vector))
    0))

(defun org-score-user-defined-sort (a b)
  "Sorting strategy for agenda items."
  (let* ((ma (or (get-text-property 1 'org-marker a)
                 (get-text-property 1 'org-hd-marker a)))
         (mb (or (get-text-property 1 'org-marker b)
                 (get-text-property 1 'org-hd-marker b)))
         (sa (org-score-score-at-marker ma))
         (sb (org-score-score-at-marker mb)))
    (cond ((< sa sb) -1)
          ((< sb sa) +1)
          (t nil))))

(defun org-score-todo-keywords-extract (&optional todo-keywords)
  "Extract todo keywords from the `org-todo-keywords' variable."
  (mapcar
   (lambda (sequence)
     (let* ((keywords (cdr sequence)))
       (org-remove-keyword-keys keywords)
       ))
   (or todo-keywords org-todo-keywords)))

(defun org-score-get-todo-state-number (pom)
  "Gets the current todo state as a float.
The float represents 1.0 as complete and 0.0 as incomplete/unstarted"
  (let ((todostate (org-entry-get pom "TODO"))) ;; Get todo keyword at point
    (apply
     ;; Returns nil if there is no todo keyword
     (lambda (&rest number-or-nil-list)
       (let ((number-list (remove nil number-or-nil-list)))
         (if number-list
             (apply 'min (remove nil number-list)))))
     (mapcar
      (lambda (keywords)
        (let* ((done-keywords (member "|" keywords))
               (todo-keywords (cdr (member "|" (reverse keywords))))
               (todo-stage (member todostate todo-keywords))
               (done-stage (member todostate done-keywords)))
          (cond
           (todo-stage
            ;; Divide result by total length
            (/
             (float (length todo-stage)) ;; Get distance from end of workflow
             (float (-
                     (1+ (length keywords))                       ;; Total length
                     (if done-keywords (length done-keywords) 1)  ;; Length of done keywords incl "|"
                     ))))
           (done-stage 1.0)
           (t nil))))
      (org-score-todo-keywords-extract org-todo-keywords)))))

(defun org-score-get-seconds-to-now (pom)
  "Returns the difference between closest timestamp item to now in seconds.
Returns nil if there are no available timestamps to score."
  (let* ((now (float-time))
         (time-props '("DEADLINE" "SCHEDULED" "TIMESTAMP"))
         (time-sec-or-nil
          (mapcar
           (lambda (prop)
             (let* ((ts (org-entry-get pom prop))
                    (secs (when ts (org-time-string-to-seconds ts)))
                    (dt (when secs (- now secs)))
                    (dta (when dt (abs dt))))
               dta))
           time-props))
         (time-sec (remove nil time-sec-or-nil))
         )
    (when time-sec
      (apply 'min time-sec)
      )))

(defun org-score-get-parent-marker (pom)
  "Gets the character marker `pom' of the parent of the current entry at the current `pom'."
  (if pom
      (org-with-point-at pom
        ;; (when (and (org-current-level) (> (org-current-level) 1)))
        (org-up-heading-or-point-min)
        (point-marker))
    pom))
