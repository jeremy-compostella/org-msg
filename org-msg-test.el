(ert-deftest org-msg-test-css-parsing ()
  "Test css list to string and back is consistant."
  (let* ((css-list   (org-msg-load-css))
	 (css-string  (org-msg-css-to-string))
	 (css-string-list (with-temp-buffer
			   (insert css-string)
			   (org-msg-css-to-list))))
  (should (equal (car css-list) (car css-string-list)))
  (should (equal css-list css-string-list))))

(ert-deftest org-msg-test-css-1 ()
  "Simple css prop to string test."
  (should (equal (org-msg-props-to-style
		  '((color . "red")
		    (background . "blue")))
		 "color:red;background:blue;")))
