;;; howdoyou.el --- A stackoverflow and its sisters' sites reader   -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Thanh Vuong

;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg/howdoyou/
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Put a description of the package here

;;; Code:
(require 'promise)
(require 'dom)
(require 'cl-lib)
(require 'request)

;; (require 'org)

(defun howdoyou--extract-links-from-l-class (dom)
  "Extract links in l class from DOM."
  (let ((my-nodes (dom-by-class dom "^l$")))
    (mapcar (lambda (a-node)
              (dom-attr a-node 'href))
            my-nodes)))

(defun howdoyou--extract-links-from-r-class (dom)
  "Extract links inside r class from DOM."
  (let ((my-nodes (dom-by-class dom "^r$")))
    (mapcar (lambda (a-node)
              (dom-attr (dom-child-by-tag a-node 'a) 'href))
            my-nodes)))

(defun howdoyou--extract-links-from-google (dom)
  "Produce links from google search dom.
DOM is a dom object of the google search, returns a list of links"
  (if-let ((links (howdoyou--extract-links-from-l-class dom)))
      links
    (howdoyou--extract-links-from-r-class dom)))

(defun howdoyou--curl-promise-dom (url)
  (promise-new
   (lambda (resolve reject)
     ;; shadow reject-curl-options to have user agent
     (let ((request-curl-options `(,(format "-A %s" (howdoyou--get-user-agent)))))
       (request url
               :parser (lambda () (libxml-parse-html-region (point-min) (point-max)))
               :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                     (funcall reject  error-thrown)))
               :success (cl-function (lambda (&key data &allow-other-keys)
                                       (funcall resolve (cons url data)))))))))

(defvar howdoyou--use-curl t)

(defun howdoyou--url-promise-dom (url)
  "Promise a cons (URL . dom).
URL is a link string. Download the url and parse it to a DOM object"
  ;; (message "%s" url)
  (promise-new
   (lambda (resolve reject)
     (let ((url-user-agent (howdoyou--get-user-agent)))
       (url-retrieve url
                     (lambda (status)
                       (if (plist-get status :error)
                           (funcall reject (plist-get status :error))
                         (condition-case ex
                             (with-current-buffer (current-buffer)
                               (if (not (url-http-parse-headers))
                                   (funcall reject (buffer-string))
                                 ;; (setq thanh-web (buffer-string))
                                 (funcall resolve (cons url (libxml-parse-html-region (point-min) (point-max))))))
                           (error (funcall reject ex))))))))))

(defun howdoyou--promise-dom (url)
  "Promise a cons (URL . dom).
URL is a link string. Download the url and parse it to a DOM object"
  (if howdoyou--use-curl (howdoyou--curl-promise-dom url)
    (howdoyou--url-promise-dom url)))

(defvar howdoyou--current-user-agent 0)

(defvar howdoyou--user-agents
  '("Mozilla/5.0 (Macintosh; Intel Mac OS X 10.7; rv:11.0) Gecko/20100101 Firefox/11.0"
    "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:22.0) Gecko/20100 101 Firefox/22.0"
    "Mozilla/5.0 (Windows NT 6.1; rv:11.0) Gecko/20100101 Firefox/11.0"
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_7_4) AppleWebKit/536.5 (KHTML, like Gecko) Chrome/19.0.1084.46 Safari/536.5"
    "Mozilla/5.0 (Windows; Windows NT 6.1) AppleWebKit/536.5 (KHTML, like Gecko) Chrome/19.0.1084.46  Safari/536.5"))

(defun howdoyou--get-user-agent ()
  "Roate user agent from `howdoyou--user-agents'."
  (let ((user-agent (nth howdoyou--current-user-agent howdoyou--user-agents)))
    (setq howdoyou--current-user-agent (if (>= howdoyou--current-user-agent
                                               (length howdoyou--user-agents))
                                           0
                                         (1+ howdoyou--current-user-agent)))
    user-agent))

(defvar howdoyou--links nil
  "List of so links from google search.")

(defvar howdoyou--current-link-index 0
  "Current index of link.")

(defvar howdoyou--current-lang nil
  "Guested language.")

(defvar howdoyou--number-of-answers 3
  "Number of maximal answers to show.")

(defun howdoyou-promise-answer (query)
  "Process QUERY and print answers to *How Do You* buffer."
  (let ((url "https://www.google.com/search")
        (args (concat "?q="
                      (url-hexify-string query)
                      (url-hexify-string " ")
                      (url-hexify-string "site:stackoverflow.com OR ")
                      (url-hexify-string "site:stackexchange.com OR ")
                      (url-hexify-string "site:superuser.com OR ")
                      (url-hexify-string "site:serverfault.com OR ")
                      (url-hexify-string "site:askubunu.com")
                      "&hl=en")))
    (promise-chain (howdoyou--promise-dom (concat url args))
      (then (lambda (result)
              ;; (setq thanh-dom (cdr result))
              (howdoyou--extract-links-from-google (cdr result))))
      (then (lambda (links)
              ;; (message "%s" links)
              ;; (setq thanh links)
              (setq howdoyou--links links)
              (setq howdoyou--current-link-index 0)
              (howdoyou--promise-dom (car links))))
      (then #'howdoyou--promise-so-answer)
      (then #'howdoyou--print-answer)
      (promise-catch (lambda (reason)
                       (message "catch the error: %s" reason))))))

(defun howdoyou--get-so-tags (dom)
  "Extract list of tags from stackoverflow DOM."
  (let ((tag-doms (dom-by-class (dom-by-class dom "^post-taglist")
                                "^post-tag$")))
    (mapcar #'dom-text tag-doms)))

(defun howdoyou--promise-so-answer (result)
  "Produce answer-list  from stackoverflow response.
RESULT is a (url . dom).
Return (url title question answers scores tags)"
  ;; (setq thanh-so (cdr result))
  (let* ((answer-nodes (dom-by-class (cdr result) "answercell"))
         (question-dom (car (dom-by-id (cdr result) "^question$")))
         (title (car (dom-by-class (cdr result) "question-hyperlink")))
         (number-of-answers (if (> (length answer-nodes) howdoyou--number-of-answers)
                                howdoyou--number-of-answers
                              (length answer-nodes)))
         (tags (howdoyou--get-so-tags (cdr result)))
         (score-nodes (dom-by-class (cdr result) "js-vote-count"))
         (acc nil)
         (scores nil))
    (list (car result)
          (dom-text title)
          (dom-by-class question-dom "post-text")
          (dotimes (i number-of-answers acc)
            (setq acc (append acc (dom-by-class (nth i answer-nodes) "post-text"))))
          (dotimes (i (1+ number-of-answers) scores)
            (setq scores (append scores `(,(dom-text (nth i score-nodes))))))
          tags)))

(defun howdoyou--print-answer (answer-list)
  "Print ANSWER-LIST to *How Do You* buffer."
  (let* ((howdoi-buffer (get-buffer-create "*How Do You*"))
         (url (car answer-list))
         (title (nth 1 answer-list))
         (question (nth 2 answer-list))
         (answers (nth 3 answer-list))
         (scores (nth 4 answer-list))
         (question-score (car scores))
         (answer-scores (cdr scores))
         (tags (nth 5 answer-list))
         (first-run t) ;; flag for special treatment of first answer
         (lang (car tags))) ;; first tag is usually the language
    ;; (setq thanh answers)
    ;; (setq thanh-scores scores)
    (setq howdoyou--current-lang lang)
    (save-selected-window
      (with-current-buffer howdoi-buffer
        (read-only-mode -1)
        (erase-buffer)
        (insert "#+STARTUP: overview indent\n#+TITLE: " title "\n")
        (insert url) ;; url
        (insert (format "\n* Question (%s)" question-score))
        (howdoyou--print-dom question)
        (insert "\nTags: ")
        (dolist (tag tags)
          (insert tag)
          (insert " "))
        (cl-mapcar (lambda (a s)
                     (insert (format "\n* Answer (%s)" s))
                     (when first-run
                       (insert "\n:PROPERTIES:\n:VISIBILITY: all\n:END:\n")
                       (setq first-run nil))
                     (howdoyou--print-dom a))
                   answers
                   answer-scores)
        (org-mode)
        (goto-char (point-min)))
      (pop-to-buffer howdoi-buffer))))

(defun howdoyou--print-node (dom)
  "Print the DOM."
  (let ((shr-bullet "- ")) ;; insead of *
    (shr-insert-document dom)))

(defun howdoyou--it-to-it (it)
  "Map node to node.
IT is an element in the DOM tree. Map to different IT when it is
a, img or pre. Othewise just copy"
  (cond
   ((and (listp it)
         (listp (cdr it))) ;; check for list but not cons
    (cond
     ((and (equal (car it) 'a)
           (not (dom-by-tag it 'img))) ;; bail out if img
      (concat "[["
              (dom-attr it 'href)
              "]["
              (dom-texts it)
              "]]"))
     ((equal (car it) 'pre)
      `(pre nil "#+begin_example " ,howdoyou--current-lang "\n" ,(nthcdr 2 it) "\n#+end_example"))
     ;; `(div nil (p nil "#+begin_example " ,howdoyou--current-lang "\n")
     ;;       ,it
     ;;       "#+end_example"))
     (t (mapcar #'howdoyou--it-to-it it))))
   (t it)))

(defun howdoyou--print-dom (dom)
  "Map new dom from DOM and print it."
  (howdoyou--print-node (mapcar #'howdoyou--it-to-it dom)))

(defun howdoyou-query (query)
  "Prompt for QUERY and search for answer.
Pop up *How Do You* buffer to show the answer."
  (interactive "sQuery: ")
  (howdoyou-promise-answer query))

(defun howdoyou-n-link (n)
  "Jump N steps in `howdoyou--links' and request and print the answer."
  (setq howdoyou--current-link-index
        (if (and (<= (+ n howdoyou--current-link-index) (length howdoyou--links))
                 (>= (+ n howdoyou--current-link-index) 0))
            (+ n howdoyou--current-link-index)
          howdoyou--current-link-index))
  (promise-chain
      (howdoyou--promise-dom (nth howdoyou--current-link-index
                                  howdoyou--links))
    (then #'howdoyou--promise-so-answer)
    (then #'howdoyou--print-answer)
    (promise-catch (lambda (reason)
                     (message "catch the error: %s" reason)))))

(defun howdoyou-read-so-link (link)
  "Read stackoverflow LINK in buffer."
  (promise-chain (howdoyou--promise-dom link)
    (then #'howdoyou--promise-so-answer)
    (then #'howdoyou--print-answer)
    (promise-catch (lambda (reason)
                     (message "catch the error: %s" reason)))))

(defun howdoyou-next-link ()
  "Go to next link stored in google search."
  (interactive)
  (howdoyou-n-link 1))

(defun howdoyou-previous-link ()
  "Go to previous link stored in google search."
  (interactive)
  (howdoyou-n-link -1))

(provide 'howdoyou)
;;; howdoyou.el ends here
