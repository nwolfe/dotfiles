(defun ncw/pd-test ()
  (interactive)
  (async-shell-command (concat "cd $PUPPETLABS/cloud-discovery && "
                               "make test")))

(defun ncw/pd-test-cli ()
  (interactive)
  (async-shell-command (concat "cd $PUPPETLABS/cloud-discovery && "
                               "make -C workstation unit-tests")))

(defun ncw/pd-buildcli ()
  (interactive)
  (async-shell-command (concat "cd $PUPPETLABS/cloud-discovery && "
                               "make -C workstation build")))

(provide 'ncw-pd)
