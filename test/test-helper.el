;;; test-helper.el --- Helpers for howdoyou-test.el
(require 'f)
;; (require 'ert-async)

(defvar root-test-path
  (f-dirname (f-this-file)))

(defvar root-code-path
  (f-parent root-test-path))

(add-to-list 'load-path root-code-path)

(require 'howdoyou)

(defun make-dom-from-file (file)
  (with-temp-buffer
    (insert-file-contents (concat root-test-path "/" file))
    (libxml-parse-html-region (point-min) (point-max))))
;;; test-helper.el ends here
