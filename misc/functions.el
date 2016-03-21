
(defun ncw/test-jjb (arg)
  (interactive "P")
  (async-shell-command
   (concat "cd $PUPPETLABS/ci-job-configs && "
           "source local/bin/activate && "
           "jenkins-jobs --conf enterprise/builder.conf test " (if arg "-o output " "")
           "resources/:enterprise/projects \"experimental_WIP-pe-r10k-vanagon_*\"")))

(defun ncw/deploy-jjb ()
  (interactive)
  (async-shell-command
   (concat "cd $PUPPETLABS/ci-job-configs && "
           "source local/bin/activate && "
           "cjc-manager deploy enterprise \"experimental_WIP-pe-r10k-vanagon_*\"")))
