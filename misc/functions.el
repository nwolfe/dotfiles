
(defun ncw/jjb-do-test (arg)
  (async-shell-command
   (concat "cd $PUPPETLABS/ci-job-configs && "
           "source local/bin/activate && "
           "jenkins-jobs --conf enterprise/builder.conf test "
           "resources/:enterprise/projects "
           "\"" arg "_*\"")))

(defun ncw/jjb-test-prompt (arg)
  (interactive "sName regex: ")
  (ncw/jjb-do-test arg))

(defun ncw/jjb-do-deploy (arg)
  (async-shell-command
   (concat "cd $PUPPETLABS/ci-job-configs && "
           "source local/bin/activate && "
           "cjc-manager deploy enterprise \"experimental_" arg "_*\"")))

(defun ncw/jjb-deploy-prompt (arg)
  (interactive "sName regex: ")
  (ncw/jjb-do-deploy arg))
