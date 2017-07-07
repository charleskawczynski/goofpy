import os
import sys
import shutil
import glob
from collections import OrderedDict
import re

def flatten(foo):
    l = []
    for x in foo:
        if hasattr(x, '__iter__') and not isinstance(x, str):
            for y in flatten(x):
                l.append(y)
        else:
            l.append(x)
    return l

def read_file_contents(file_name):
    try:
        f = open(file_name,'r+')
        c = f.read().split('\n')
    except:
        pass
    return c

def write_string_to_file(file_name,s):
    f = open(file_name,'w+')
    f.write(s)
    f.close()
    return

def make_path(new_path):
    if not os.path.exists(new_path):
        os.makedirs(new_path)

def delete_entire_tree_safe(d):
    if os.path.exists(d):
        shutil.rmtree(d)

