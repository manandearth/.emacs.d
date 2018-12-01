;;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'gauche-mode)
(require 'gauche-paredit)

;; suppress `Matching: ...' output
(setq blink-matching-paren nil)
(setq-default indent-tabs-mode nil)

(defmacro gauche-with-temp-buffer (contents &rest body)
  (declare (indent 1))
  `(save-window-excursion
     (with-temp-buffer
       (insert ,contents)
       (pop-to-buffer (current-buffer))
       (gauche-mode)
       (font-lock-fontify-buffer)
       ,@body)))

(defun gauche-paredit-with-input-to-temp-buffer (macro)
  (gauche-with-temp-buffer ""
    (enable-gauche-paredit-mode)
    (execute-kbd-macro macro)
    (buffer-string)))

(defun gauche-paredit-test-key-sequence (expected input)
  (should (equal expected
                 (gauche-paredit-with-input-to-temp-buffer input))))

(defun gauche-test-string (str)
  (replace-regexp-in-string (rx bol (* space) "|")
                            ""
                            str))

(defun gauche-test-indent (input expected)
  (gauche-with-temp-buffer
      (gauche-test-string input)
    (goto-char (point-min))
    (indent-sexp)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   (gauche-test-string expected)))))

(ert-deftest gauche-mode-indent-define-record-type-test/r6rs ()
  (gauche-with-temp-buffer
      (gauche-test-string
       "(define-record-type pare
       |(fields kar kdr))
       |")
    (setq gauche-mode-define-record-type-syntax 'r6rs)
    (goto-char (point-min))
    (indent-sexp)
    (should (equal (buffer-string)
                   (gauche-test-string
                    "(define-record-type pare
                    |  (fields kar kdr))
                    |")))
    (setq gauche-mode-define-record-type-syntax 'srfi)
    (goto-char (point-min))
    (indent-sexp)
    (should (equal (buffer-string)
                   (gauche-test-string
                    "(define-record-type pare
                    |    (fields kar kdr))
                    |")))))

(ert-deftest gauche-mode-indent-define-record-type-test/srfi ()
  (gauche-with-temp-buffer
      (gauche-test-string
       "(define-record-type :pare
       |(kons x y)
       |pare?
       |(x kar set-kar!)
       |(y kdr))
       |")
    (setq gauche-mode-define-record-type-syntax 'r6rs)
    (goto-char (point-min))
    (indent-sexp)
    (should (equal (buffer-string)
                   (gauche-test-string
                    "(define-record-type :pare
                    |  (kons x y)
                    |  pare?
                    |  (x kar set-kar!)
                    |  (y kdr))
                    |")))
    (setq gauche-mode-define-record-type-syntax 'srfi)
    (goto-char (point-min))
    (indent-sexp)
    (should (equal (buffer-string)
                   (gauche-test-string
                    "(define-record-type :pare
                    |    (kons x y)
                    |    pare?
                    |  (x kar set-kar!)
                    |  (y kdr))
                    |")))))

(ert-deftest gauche-mode-indent-define-condition-type-test ()
  (gauche-test-indent
   "(define-condition-type &c &condition
   |c?
   |(x c-x))
   |"
   "(define-condition-type &c &condition
   |                       c?
   |  (x c-x))
   |")
  (gauche-test-indent
   "(define-condition-type &c
   |&condition
   |c?
   |(x c-x))
   |"
   "(define-condition-type &c
   |    &condition
   |    c?
   |  (x c-x))
   |")
  (gauche-test-indent
   "(define-condition-type &c &condition
   |make-c c?
   |(x c-x))
   |"
   "(define-condition-type &c &condition
   |                       make-c c?
   |  (x c-x))
   |")
  (gauche-test-indent
   "(define-condition-type &c
   |&condition
   |make-c
   |c?
   |(x c-x))
   |"
   "(define-condition-type &c
   |    &condition
   |    make-c
   |    c?
   |  (x c-x))
   |")
  )

(ert-deftest gauche-mode-indent-while/until-test ()
  (gauche-test-indent
   "(while expr
   |body)
   |"
   "(while expr
   |  body)
   |")
  (gauche-test-indent
   "(while expr
   |)
   |"
   "(while expr
   |  )
   |")
  (gauche-test-indent
   "(while
   |expr
   |)
   |"
   "(while
   |    expr
   |  )
   |")
  (gauche-test-indent
   "(while expr
   |=> var
   |body)
   |"
   "(while expr
   |    => var
   |  body)
   |")
  (gauche-test-indent
   "(while expr
   |guard
   |=> var
   |body)
   |"
   "(while expr
   |    guard
   |    => var
   |  body)
   |")
  (gauche-test-indent
   "(while expr guard
   |=> var
   |body)
   |"
   "(while expr guard
   |       => var
   |  body)
   |")
  (gauche-test-indent
   "(while =>
   |body)
   |"
   "(while =>
   |  body)
   |")
  (gauche-test-indent
   "(while expr
   |e1
   |e2
   |=>
   |body)
   |"
   "(while expr
   |  e1
   |  e2
   |  =>
   |  body)
   |")
  )

(ert-deftest gauche-mode-indent-define-cfn/define-cproc-test ()
  (gauche-test-indent
   "(define-cproc foo () ::<int> :constant
   |1)
   |"
   "(define-cproc foo () ::<int> :constant
   |  1)
   |")
  (gauche-test-indent
   "(define-cproc foo ()
   |::<int>
   |:constant
   |1)
   |"
   "(define-cproc foo ()
   |              ::<int>
   |              :constant
   |  1)
   |")
  (gauche-test-indent
   "(define-cfn foo () ::<int> :constant
   |1)
   |"
   "(define-cfn foo () ::<int> :constant
   |  1)
   |")
  (gauche-test-indent
   "(define-cfn foo ()
   |::<int>
   |:constant
   |1)
   |"
   "(define-cfn foo ()
   |            ::<int>
   |            :constant
   |  1)
   |")
  )

(ert-deftest gauche-mode-toggle-paren-type-test ()
  (should
   (equal "[a (b c) d]"
          (gauche-with-temp-buffer "(a (b c) d)"
            (goto-char (point-min))
            (gauche-mode-toggle-paren-type)
            (buffer-string))))
  (should
   (equal "(a (b c) d)"
          (gauche-with-temp-buffer "[a (b c) d]"
            (goto-char (point-min))
            (gauche-mode-toggle-paren-type)
            (buffer-string))))
  (should
   (equal "(a [b c] d)"
          (gauche-with-temp-buffer "(a (b c) d)"
            (goto-char 4)
            (gauche-mode-toggle-paren-type)
            (buffer-string))))
  (should
   (equal "(a [b c] d)"
          (gauche-with-temp-buffer "(a (b c) d)"
            (goto-char 8)
            (gauche-mode-toggle-paren-type)
            (buffer-string))))
  (should
   (equal "(a (b c) d)"
          (gauche-with-temp-buffer "[a (b c) d]"
            (goto-char (point-min))
            (gauche-mode-toggle-paren-type)
            (buffer-string))))
  (should
   (equal "[a (b c) d]"
          (gauche-with-temp-buffer "(a (b c) d)"
            (goto-char (point-max))
            (backward-char 1)
            (gauche-mode-toggle-paren-type)
            (buffer-string))))
  (should
   (equal "[a (b c) d"
          (gauche-with-temp-buffer "(a (b c) d"
            (goto-char (point-min))
            (gauche-mode-toggle-paren-type)
            (buffer-string))))
  (should
   (equal "(a [b c] d"
          (gauche-with-temp-buffer "(a (b c) d"
            (goto-char 4)
            (gauche-mode-toggle-paren-type)
            (buffer-string))))(should
   (equal "(a [b c d]"
          (gauche-with-temp-buffer "(a (b c d)"
            (goto-char 4)
            (gauche-mode-toggle-paren-type)
            (buffer-string))))
  )

(ert-deftest gauche-paredit-slash-test ()
  (gauche-with-temp-buffer ""
    (enable-gauche-paredit-mode)
    (execute-kbd-macro (kbd "#/"))
    (should (equal (buffer-string) "#//"))
    (font-lock-fontify-buffer)
    (execute-kbd-macro (kbd "DEL"))
    (should (equal (buffer-string) ""))
    (execute-kbd-macro (kbd "#/a"))
    (should (equal (buffer-string) "#/a/"))
    (font-lock-fontify-buffer)
    (backward-char)
    (execute-kbd-macro (kbd "DEL"))
    (should (equal (buffer-string) "#/a/"))))

(ert-deftest gauche-paredit-datum-label-prefix-test ()
  (dotimes (_ 100)
    (let ((n (random 1000)))
      (gauche-paredit-test-key-sequence
       (format "#%s=()" n)
       (kbd (format "# %s = (" n))))))

(ert-deftest gauche-paredit-debug-print-prefix-test ()
  (gauche-paredit-test-key-sequence
   "#?=()"
   (kbd "# ? = (")))

(ert-deftest gauche-paredit-vector-prefix-test ()
  (gauche-paredit-test-key-sequence
   "(f #vu8())"
   (kbd "(f SPC #vu8("))
  (cl-loop for n in '(8 16 32 64)
           for p in '(s u)
           do (gauche-paredit-test-key-sequence
               (format "(f #%s%s())" p n)
               (kbd (format "(f SPC #%s%s(" p n))))
  (gauche-paredit-test-key-sequence
   "(f #s48 ())"
   (kbd "(f SPC #s48("))
  (cl-loop for n in '(16 32 64)
           do (gauche-paredit-test-key-sequence
               (format "(f #f%s())" n)
               (kbd (format "(f SPC #f%s(" n))))
  (gauche-paredit-test-key-sequence
   "(f #f8 ())"
   (kbd "(f SPC #f8("))
  )

(ert-deftest gauche-paredit-short-lambda-prefix-test ()
  (gauche-paredit-test-key-sequence
   "(^(x) x)"
   (kbd "(^(x) SPC x"))
  (gauche-paredit-test-key-sequence
   "(f (g) h)"
   (kbd "(f(g) SPC h"))
  )
