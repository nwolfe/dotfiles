
(defun ncw/sync-game ()
  (interactive)
  (async-shell-command "scp -r /users/nwolfe/workspace/tmp/py-roguelike 192.168.1.125:~/workspace/gaming"))

(provide 'ncw-gamedev)
