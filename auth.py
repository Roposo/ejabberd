#!/usr/bin/python

import urllib2
import sys
from struct import *

def from_ejabberd():
    input_length = sys.stdin.read(2)
    (size,) = unpack('>h', input_length)
    return sys.stdin.read(size).split(':')

def to_ejabberd(bool):
    answer = 0
    if bool:
        answer = 1
    token = pack('>hh', 2, answer)
    sys.stdout.write(token)
    sys.stdout.flush()

def auth(username, server, password):
    url = "http://192.168.10.200:9000/auth/authenticate_user?user=" + username + "&server=" + server + "&pass=" + password
    url_handle = urllib2.urlopen(url)
    auth_res = url_handle.read()
    url_handle.close()
    if auth_res == 'true':
        return True
    return False

def isuser(username, server):
    return True

def setpass(username, server, password):
    return True

while True:
    data = from_ejabberd()
    success = False
    if data[0] == "auth":
        success = auth(data[1], data[2], data[3])
    elif data[0] == "isuser":
        success = isuser(data[1], data[2])
    elif data[0] == "setpass":
        success = setpass(data[1], data[2], data[3])
    to_ejabberd(success)
