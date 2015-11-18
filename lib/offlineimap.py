#!/usr/bin/python

## Shells out to the gpg binary to parse the contents of .authinfo.gpg
## then extracts the password for the specified machine/login.
##
## Assumes the lines of the .authinfo.gpg are in the form:
##
##   machine X login Y password Z
##
## For example:
##
##   machine imap.gmail.com login user@gmail.com password opensesame
##
## See: http://quotenil.com/OfflineIMAP-with-Encrypted-Authinfo.html

import re, os

def get_authinfo_pass(machine, login):
    authinfo = os.popen("gpg -q --no-tty -d ~/.authinfo.gpg").read()
    s = "machine %s login %s password ([^ ]*)" % (machine, login)
    return re.compile(s).search(authinfo).group(1)
