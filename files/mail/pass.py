#!/usr/bin/env python3
import re
import os


def get_password_emacs(machine, login, port):
    s = "machine %s login %s password ([^ ]*) port %s\n" % (
        machine, login, port)
    p = re.compile(s)
    authinfo = os.popen("cat ~/.authinfo").read()
    return p.search(authinfo).group(1)
