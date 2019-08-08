;;; -*- lexical-binding: t; -*-
(require 'promise)
(require 'dom)

(defun howdoyou--google-to-links (dom)
  (let* ((my-divs (dom-by-class dom "jfp3ef"))
         (my-a-tags (mapcar (lambda (a-div)
                              (dom-attr (dom-child-by-tag a-div 'a) 'href))
                            my-divs)))
    (mapcar (lambda (it) (substring it 7))
            (seq-filter (lambda (it) (if it t nil)) my-a-tags))))

(defun howdoyou--promise-dom (url)
  "promise a cons (url . dom)"
  (promise-new
   (lambda (resolve reject)
     (url-retrieve url
                   (lambda (status)
                     ;; All errors are reliably captured and rejected with appropriate values.
                     (if (plist-get status :error)
                         (funcall reject (plist-get status :error))
                       (condition-case ex
                           (with-current-buffer (current-buffer)
                             (if (not (url-http-parse-headers))
                                 (funcall reject (buffer-string))
                               ;; (message "%s" (buffer-string))
                               ;; (message "got it")
                               (funcall resolve (cons url (libxml-parse-html-region (point-min) (point-max))))))
                         (error (funcall reject ex)))))))))
(defvar howdoyou--links nil
  "list of so links from google search")

(defvar howdoyou--current-link-index 0
  "current index of link")

(defvar howdoyou--current-lang nil
  "guested language")

(defvar howdoyou--number-of-answers 3
  "number of maximal answers to show")

(defun howdoyou-promise-answer (query)
  "query and print answer"
  (let ((url "https://www.google.com/search")
        (args (concat "?q="
                      (url-hexify-string query)
                      (url-hexify-string " ")
                      (url-hexify-string "site:stackoverflow.com OR ")
                      (url-hexify-string "site:stackexchange.com OR ")
                      (url-hexify-string "site:superuser.com OR ")
                      (url-hexify-string "site:serverfault.com OR ")
                      (url-hexify-string "site:askubunu.com"))))
    (promise-chain (howdoyou--promise-dom (concat url args))
      (then (lambda (result)
              (howdoyou--google-to-links (cdr result))))
      (then (lambda (links)
              ;; (message "%s" links)
              ;; (setq thanh links)
              (setq howdoyou--links links)
              (setq howdoyou--current-link-index 0)
              (howdoyou--promise-dom (car links))))
      (then #'howdoyou--promise-so-answer)
      (then #'howdoyou--print-answer))
    (concat url args)))

(defun howdoyou--get-so-tags (dom)
  (let ((tag-doms (dom-by-class (dom-by-class dom "^post-taglist")
                                "^post-tag$")))
    (mapcar #'dom-text tag-doms)))

(defun howdoyou--promise-so-answer (result)
  "Get the first child in class answers and question from
`result' which is a `(url . dom)' return `(url question answer tags)'."
  (setq thanh-so (cdr result))
  (let* ((answer-nodes (dom-by-class (cdr result) "answercell"))
         (question-dom (car (dom-by-id (cdr result) "^question$")))
         (number-of-answers (if (> (length answer-nodes) howdoyou--number-of-answers)
                                howdoyou--number-of-answers
                              (length answer-nodes)))
         (tags (howdoyou--get-so-tags (cdr result)))
         (acc nil))
    (list (car result)
          (dom-by-class question-dom "post-text")
          (dotimes (i number-of-answers acc)
            (setq acc (append acc (dom-by-class (nth i answer-nodes) "post-text"))))
          tags)))

(defun howdoyou--print-answer (answer-list)
  "Print ANSWER-LIST to buffer."
  (let* ((howdoi-buffer (get-buffer-create "*How Do You*"))
         (url (car answer-list))
         (question (nth 1 answer-list) )
         (answers (nth 2 answer-list))
         (tags (nth 3 answer-list))
         (lang (car (nth 3 answer-list)))) ;; first tag is usually the language
    (setq thanh answers)
    (setq thanh2 question)
    (setq howdoyou--current-lang lang)
    (save-selected-window
      (with-current-buffer howdoi-buffer
        (read-only-mode -1)
        (erase-buffer)
        (insert "#+STARTUP: showall indent\n")
        (insert "* Question\n")
        (insert (replace-regexp-in-string "&.*$" "" url)) ;; url
        (howdoyou--print-dom question)
        (insert "tags: ")
        (dolist (tag tags)
          (insert tag)
          (insert " "))
        (dolist (answer answers)
          (insert "\n* Answer")
          (howdoyou--print-dom answer))
        (org-mode)
        (goto-char (point-min)))
      (pop-to-buffer howdoi-buffer))))

(defun howdoyou--print-node (node)
  (let ((shr-bullet "- ")) ;; insead of *
    (shr-insert-document node)))

(defun howdoyou--it-to-it (it)
  (cond
   ((and (listp it)
         (listp (cdr it))) ;; check for list but not cons
    (cond
     ((equal (car it) 'a)
      (concat "[["
              (dom-attr it 'href)
              "]["
              (dom-texts it)
              "]]"))
     ((equal (car it) 'pre)
      (append `(pre nil "#+begin_example " ,howdoyou--current-lang "\n") (nthcdr 2 it) '("#+end_example")))
     (t (mapcar #'howdoyou--it-to-it it))))
   (t it)))

(defun howdoyou--print-dom (dom)
  (howdoyou--print-node (mapcar #'howdoyou--it-to-it dom)))

(defun howdoyou-query (query)
  (interactive "sQuery: ")
  (howdoyou-promise-answer query))

(defun howdoyou-n-link (n)
  (setq howdoyou--current-link-index
        (if (and (<= (+ n howdoyou--current-link-index) (length howdoyou--links))
                 (>= (+ n howdoyou--current-link-index) 0))
            (+ n howdoyou--current-link-index)
          howdoyou--current-link-index))
  (promise-chain
      (howdoyou--promise-dom (nth howdoyou--current-link-index
                                  howdoyou--links))
    (then #'howdoyou--promise-so-answer)
    (then #'howdoyou--print-answer)))

(defun howdoyou-next-link ()
  "go to next link"
  (interactive)
  (howdoyou-n-link 1))

(defun howdoyou-previous-link ()
  "go to previous link"
  (interactive)
  (howdoyou-n-link -1))
