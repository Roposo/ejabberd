#!/usr/bin/python

import requests
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
    url = "http://localhost:9015/auth/authenticate_user"
    payload = {'user': username, 'server': server, 'pass': password}
    auth_res = requests.post(url, data = payload)
    if auth_res.text == 'true':
        return True
    return False

def isuser(username, server):
    url = "http://localhost:9015/auth/user_exists"
    payload = {'user': username, 'server': server}
    auth_res = requests.post(url, data = payload)
    if auth_res.text == 'true':
        return True
    return False

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
