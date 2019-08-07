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
`result' which is a `(url . dom)' return `(url question answer
tags)'."
  ;; (message "got the (cdr result)")
  ;; (setq thanh (cdr result))
  (let ((answer-dom (car (dom-by-class (cdr result) "^answer\s?")))
        (question-dom (car (dom-by-id (cdr result) "^question$")))
        (tags (howdoyou--get-so-tags (cdr result))))
    ;; (tags '("sh" "yay")))
    (list (car result)
          (dom-by-class question-dom "post-text")
          (dom-by-class answer-dom "post-text")
          tags)))
;; (dom-texts (dom-by-class answer-dom "post-text"))))

(defun howdoyou--print-answer (answer-list)
  "Print ANSWER-LIST to buffer."
  (let ((howdoi-buffer (get-buffer-create "*How Do You*"))
        (url (car answer-list))
        (question (nth 1 answer-list) )
        (answer (nth 2 answer-list))
        (tags (nth 3 answer-list))
        (lang (car (nth 3 answer-list)))) ;; first tag is usually the language
    (setq thanh answer)
    (save-selected-window
      (with-current-buffer howdoi-buffer
        (read-only-mode -1)
        (erase-buffer)
        (insert "#+STARTUP: showall indent\n")
        (insert "* Question\n")
        (insert (replace-regexp-in-string "&.*$" "" url)) ;; url
        ;; (shr-insert-document (nth 1 answer))
        (howdoyou--print-dom question lang)
        ;; (setq thanh (nth 1 answer))
        ;; (shr-insert "==================Answer==================")
        ;; (insert "\n==================Answer==================\n")
        ;; (insert (nth 3 answer))
        (insert "tags: ")
        (dolist (tag tags)
          (insert tag)
          (insert " "))
        (insert "\n* Answer")
        (howdoyou--print-dom answer lang)
        ;; (shr-insert-document (nth 2 answer))
        ;; (eww-mode)
        (org-mode)
        (goto-char (point-min)))
      (pop-to-buffer howdoi-buffer))))

(defun howdoyou--print-node (node lang)
  (let ((tag (dom-tag node))
        (shr-bullet "- ")) ;; insead of *
    (cond
     ((equal tag 'pre)
      (insert "\n#+begin_example " lang "\n")
      (shr-insert-document node)
      (insert "#+end_example\n"))
     ;; ((equal tag 'a)
     ;;  (insert "[[")
     ;;  (insert (dom-attr node 'href))
     ;;  (insert "]")
     ;;  (insert "[")
     ;;  (insert (dom-text node))
     ;;  (insert "]]"))
     ;; ((equal tag 'img)
     ;;  (shr-insert-document node))
     (t (shr-insert-document node)))))

(defun howdoyou--it-to-it (it)
  (cond
   ((and (listp it)
         (listp (cdr it)))
    (if (equal (car it) 'a)
        (concat "[["
                (dom-attr it 'href)
                "]["
                (dom-text it)
                "]]")
      (mapcar #'howdoyou--it-to-it it)))
   (t it)))

(defun howdoyou--print-dom (dom lang)
  (let* ((my-dom (mapcar #'howdoyou--it-to-it dom))
        (children (dom-non-text-children my-dom)))
    (dolist (child children)
      (howdoyou--print-node child lang))))

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
