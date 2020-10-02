;;; howdoyou-test.el --- Tests for howdoyou
(ert-deftest test/links-from-google-search ()
  "Should be able to show links"
  (dolist (google-file '("google.0.html" "google.1.html" "google.2.html"  "google.3.html"  "google.4.html"))
    (let* ((dom (make-dom-from-file google-file))
           (result (howdoyou--extract-links-from-google dom)))
      (message "%s" result)
      (should (listp result)))))

;; (ert-deftest test/links-from-google-bot-search ()
;;   "Should be able to show links"
;;   (let* ((dom (make-dom-from-file "google2.html"))
;;          (result (howdoyou--extract-links-from-google dom)) )
;;     (message "%s" result)
;;     (should (listp result))))

(ert-deftest-async test/promise-dom (done)
                   (promise-done
                    (promise-chain
                        (howdoyou--promise-dom "https://www.google.com")
                      (then (lambda (result)
                              (should (listp result))
                              ;; (message "%s" result)
                              ;; (error "error test")
                              (funcall done)))
                      (promise-catch done))))
;; (promise-catch (lambda (e) (funcall done e))))))

(ert-deftest-async test/promise-curl-dom (done)
                   (promise-done
                    (promise-chain
                        (howdoyou--curl-promise-dom "https://www.google.com")
                      (then (lambda (result)
                              (should (listp result))
                              ;; (message "%s" result)
                              (funcall done)))
                      (promise-catch done))))

(ert-deftest-async test/howdoyou-read-so-link (done)
                   (promise-done
                    (promise-chain
                        (howdoyou-read-so-link "https://stackoverflow.com/questions/8425102/how-do-i-load-my-script-into-the-node-js-repl")
                      (then (lambda (result)
                              (should (buffer-live-p (get-buffer "*How Do You*")))
                              (funcall done)))
                      (promise-catch done))))

(ert-deftest-async test/promise-parsing (done)
                   (promise-done
                    (promise-chain
                        (howdoyou--promise-dom "https://stackoverflow.com/questions/586735/how-can-i-check-if-a-current-buffer-exists-in-emacs")
                      (then #'howdoyou--promise-so-answer)
                      (then (lambda (result)
                              (should (listp result))
                              (funcall done)))
                      (promise-catch done))))

;;; howdoyou-test.el ends here
