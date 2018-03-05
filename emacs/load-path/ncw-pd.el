;; ;; A copy of "make bazel-test" but skip PDP tests
;; (defvar cmd-test-nopdp
;;   (concat
;;    "bazel test --test_verbose_timeout_warnings $("
;;    "bazel query 'tests(//... - //test/journey/... - //workstation/... - //pdp/...)'"
;;    " | xargs)"))

(defvar cmd-test "make bazel-test")

(defvar cmd-buildcli "make -C workstation build")

(defun ncw/pd-test ()
  (interactive)
  (async-shell-command (concat "cd $PUPPETLABS/cloud-discovery && " cmd-test)))

(defun ncw/pd-buildcli ()
  (interactive)
  (async-shell-command (concat "cd $PUPPETLABS/cloud-discovery && " cmd-buildcli)))

(provide 'ncw-pd)
