(defvar cjc-manager-docker-image "cinext-registry.delivery.puppetlabs.net/debian/jessie/jenkins/python-3.4.5/cjc-manager:2")

(defun ncw/jjb-do-test (jenkins job)
  (async-shell-command
   (concat "cd $PUPPETLABS/ci-job-configs && "
           "rm -rf $PUPPETLABS/ci-job-configs/tmp && "
           "docker run --rm -v $PUPPETLABS/ci-job-configs:/cjc -w /cjc "
           cjc-manager-docker-image
           " cjc-manager test " jenkins " \"" job "\" && "
           "cat $PUPPETLABS/ci-job-configs/tmp/*/" job)))

(defun ncw/jjb-test-prompt (jenkins job)
  (interactive "sJenkins: \nsName regex: ")
  (ncw/jjb-do-test jenkins job))

(defun ncw/jjb-do-deploy (jenkins job)
  (async-shell-command
   (concat "cd $PUPPETLABS/ci-job-configs && "
           "docker run --rm -v $PUPPETLABS/ci-job-configs:/cjc -w /cjc "
           cjc-manager-docker-image
           " cjc-manager deploy " jenkins " \"" job "\"")))

(defun ncw/jjb-deploy-prompt (jenkins job)
  (interactive "sJenkins: \nsName regex: ")
  (ncw/jjb-do-deploy jenkins job))

(defun ncw/jjb-compare (jenkins before-sha after-sha)
  (interactive "sJenkins: \nsBefore: \nsAfter (optional): ")
  (async-shell-command
   (concat "cd $PUPPETLABS/ci-job-configs && "
           "docker run --rm -v $PUPPETLABS/ci-job-configs:/cjc -w /cjc "
           cjc-manager-docker-image
           " cjc-manager compare " jenkins " " before-sha " " after-sha)))

(provide 'ncw-jjb)
