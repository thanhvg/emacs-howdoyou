;;; howdoyou-test.el --- Tests for howdoyou
(ert-deftest test/links-from-google-search ()
  "Should be able to show links"
  (let* ((dom (make-dom-from-file "google.html"))
         (result (howdoyou--extract-links-from-google dom)) )
    (message "%s" result)
    (should (listp result))))
;;; howdoyou-test.el ends here
