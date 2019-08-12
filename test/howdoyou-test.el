;;; howdoyou-test.el --- Tests for howdoyou
(ert-deftest test/links-from-google-search ()
  "Should be able to show links"
  (let* ((dom (make-dom-from-file "google.html"))
         (result (howdoyou--extract-links-from-google dom)) )
    (message "%s" result)
    (should (listp result))))

(ert-deftest test/links-from-google-bot-search ()
  "Should be able to show links"
  (let* ((dom (make-dom-from-file "google2.html"))
         (result (howdoyou--extract-links-from-google dom)) )
    (message "%s" result)
    (should (listp result))))

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

;;; howdoyou-test.el ends here
