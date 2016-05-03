
(defun ncw/jjb-do-test (jenkins job)
  (async-shell-command
   (concat "cd $PUPPETLABS/ci-job-configs && "
           "source local/bin/activate && "
           "jenkins-jobs --conf jenkii/" jenkins "/builder.conf test "
           "resources/:jenkii/" jenkins "/projects "
           "\"" job "_*\"")))

(defun ncw/jjb-test-prompt (jenkins job)
  (interactive "sJenkins: \nsName regex: ")
  (ncw/jjb-do-test jenkins job))

(defun ncw/jjb-do-deploy (arg)
  (async-shell-command
   (concat "cd $PUPPETLABS/ci-job-configs && "
           "source local/bin/activate && "
           "cjc-manager deploy enterprise \"experimental_" arg "_*\"")))

(defun ncw/jjb-deploy-prompt (arg)
  (interactive "sName regex: ")
  (ncw/jjb-do-deploy arg))
