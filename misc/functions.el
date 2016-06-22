
(defun ncw/jjb-do-test (jenkins job)
  (async-shell-command
   (concat "cd $PUPPETLABS/ci-job-configs && "
           "source local/bin/activate && "
           "jenkins-jobs --conf jenkii/" jenkins "/builder.conf test "
           "resources:jenkii/" jenkins "/resources:jenkii/" jenkins "/projects "
           "\"" job "_*\"")))

(defun ncw/jjb-test-prompt (jenkins job)
  (interactive "sJenkins: \nsName regex: ")
  (ncw/jjb-do-test jenkins job))

(defun ncw/jjb-do-deploy (jenkins job)
  (async-shell-command
   (concat "cd $PUPPETLABS/ci-job-configs && "
           "source local/bin/activate && "
           "cjc-manager deploy " jenkins " \"" job "_*\"")))

(defun ncw/jjb-deploy-prompt (jenkins job)
  (interactive "sJenkins: \nsName regex: ")
  (ncw/jjb-do-deploy jenkins job))

(defun ncw/jjb-compare (jenkins before-sha after-sha)
  (interactive "sJenkins: \nsBefore: \nsAfter (optional): ")
  (async-shell-command
   (concat "cd $PUPPETLABS/ci-job-configs && "
           "source local/bin/activate && "
           "./utils/compare.sh " jenkins " " before-sha " " after-sha)))
