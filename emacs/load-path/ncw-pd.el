(defvar test-cmd (concat
                  "bazel test --test_verbose_timeout_warnings $("
                  "bazel query 'tests(//... - //test/journey/... - //pdp/...)'"
                  " | xargs)"))

(defvar buildcli-cmd "bazel build //workstation:build-cli")

(defun ncw/pd-test ()
  (interactive)
  (async-shell-command (concat "cd $PUPPETLABS/cloud-discovery && " test-cmd)))

(defun ncw/pd-buildcli ()
  (interactive)
  (async-shell-command (concat "cd $PUPPETLABS/cloud-discovery && " buildcli-cmd)))

(provide 'ncw-pd)
