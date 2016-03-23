
(defun ncw/jjb-do-test (arg)
  (async-shell-command
   (concat "cd $PUPPETLABS/ci-job-configs && "
           "source local/bin/activate && "
           "jenkins-jobs --conf enterprise/builder.conf test "
           "resources/:enterprise/projects "
           "\"experimental_" arg "_*\"")))

(defun ncw/jjb-test-prompt (arg)
  (interactive "sName regex: ")
  (ncw/jjb-do-test arg))

(defun ncw/jjb-test-WIP-pe-r10k-vanagon ()
  (interactive)
  (ncw/jjb-do-test "WIP-pe-r10k-vanagon"))

(defun ncw/jjb-test-WIP-r10k ()
  (interactive)
  (ncw/jjb-do-test "WIP-r10k"))



(defun ncw/jjb-do-deploy (arg)
  (async-shell-command
   (concat "cd $PUPPETLABS/ci-job-configs && "
           "source local/bin/activate && "
           "cjc-manager deploy enterprise \"experimental_" arg "_*\"")))

(defun ncw/jjb-deploy-prompt (arg)
  (interactive "sName regex: ")
  (ncw/jjb-do-deploy arg))

(defun ncw/jjb-deploy-WIP-pe-r10k-vanagon ()
  (interactive)
  (ncw/jjb-do-deploy "WIP-pe-r10k-vanagon"))

(defun ncw/jjb-deploy-WIP-r10k ()
  (interactive)
  (ncw/jjb-do-deploy "WIP-r10k"))
