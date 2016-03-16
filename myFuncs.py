import os
import sys
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
            
def file_len(fname):
    with open(fname) as f:
        for i, l in enumerate(f):
            pass
    return i + 1
    
def printFilesInDir(dir_):
    tot_dir = os.getcwd() + '/' + dir_
    print '**** CWD: ' + str(os.getcwd()) + ' ****'
    print '**** FILES IN FOLDER: ' + str(tot_dir) + ' ****'
    for files in glob.glob(tot_dir+'*'):
        print files
    print '**************************************'

def writeListToFile(path,name,ext,l):
    pne = path + name + ext
    try:
        f = open(pne,'w+')
    except IOError as e: 
        print '---e in writeContentsToFile---'
        printFilesInDir(path)
        print e
        print '------'
    else:
        f.write('\n'.join(l))
        
def getContentsByPath(path,name,ext):
    pne = path + name + ext
    try: 
        f = open(pne,'r+')
    except IOError as e:
        print '---e in setContentsByPath---'                    # remove all values from d:

        printFilesInDir(path)
        print e
        print '------'
    else:
        lines = f.read().split('\n')
        return lines
#            print 'File contents set: ' + self.getFilePne()

def getContentsByFullPath(pne):
    path = pne.split(os.path.basename(pne))[0]
    try: 
        f = open(pne,'r+')
    except IOError as e:
        print '---e in setContentsByPath---'
        printFilesInDir(path)
        print e
        print '------'
    else:
        lines = f.read().split('\n')
        return lines
#            print 'File contents set: ' + self.getFilePne()

def getClasses(path,name,ext):
    c = getContentsByPath(path,name,ext)
    classes = []
    for k in c:
        j = k.strip(' ')
        if not j.startswith('#') and j.startswith('def '):
            s = k.split('(')
            s_new = s[0].split('def ')
            classes.append(s_new[-1].strip(' '))
    classes.pop(0)
    return classes
    
def getClassesByFullPath(pne):
    c = getContentsByFullPath(pne)
    classes = []
    for k in c:
        j = k.strip(' ')
        if not j.startswith('#') and j.startswith('def '):
            s = k.split('(')
            s_new = s[0].split('def ')
            classes.append(s_new[-1].strip(' '))
    classes.pop(0)
    return classes
    
# MAKE BATCH FILE***********************************
def makeDotBat(path):
    FL = getFileList(path)
    FN = getFuncNames(path,FL)
    t = zip(FL,FN)
    progFiles = getProgFiles(path,FL)
    FL = [];FN = []
    for k,v in t:
        if not k in progFiles:
            FL.append(k);FN.append(v)
    tnew = zip(FL,FN)
    SL = sortFileList(path,tnew,FL,FN)
    for progFile in progFiles:
        FL = []
        FL.insert(0,'df')
        FL.append(SL)
        FL.append(progFile + ' /exe:' + progFile)
        FL = flatten(FL)
        FL = [' '.join(FL)]
        writeListToFile(path,progFile+'_compile','.bat',FL)
    return
    
def getFileList(path):
<<<<<<< HEAD
    fext = '.f90'
    FL = []
    for f in os.listdir(path):
        if f.endswith(fext):
            FL.append(f[:-len(fext)])
=======
    FL = []
    for f in os.listdir(path):
        if f.endswith(".f"):
            FL.append(f[:-2])
>>>>>>> cc80e570353cdab2994bc065dadc0d821d478d12
    return FL
    
def isPastFirstLineOfScopingUnit(s):
    TF = [False]*6
    TF[1] = s.lower().rstrip().lstrip().startswith('implicit none')
    TF[2] = s.lower().rstrip().lstrip().startswith('use ')
    TF[3] = s.lower().rstrip().lstrip().startswith('type ')
    TF[4] = s.lower().rstrip().lstrip().startswith('contains')
    TF[5] = s.lower().rstrip().lstrip().startswith('call ')
    return any(TF)

def isPastUseStatements(s):
    TF = [False]*5
    TF[1] = s.lower().rstrip().lstrip().startswith('implicit none')
    TF[2] = s.lower().rstrip().lstrip().startswith('type ')
    TF[3] = s.lower().rstrip().lstrip().startswith('contains')
    TF[4] = s.lower().rstrip().lstrip().startswith('call ')
    return any(TF)
    
def getFuncNames(path,FL):
<<<<<<< HEAD
    fext = '.f90'
=======
>>>>>>> cc80e570353cdab2994bc065dadc0d821d478d12
    r1 = re.compile(r'(?<=program\s)[A-Za-z_0-9]+')
    r2 = re.compile(r'(?<=module\s)[A-Za-z_0-9]+')
    r3 = re.compile(r'(?<=function\s)[A-Za-z_0-9]+')
    r4 = re.compile(r'(?<=subroutine\s)[A-Za-z_0-9]+')
    FN = []
    d = OrderedDict()
    for f in FL:
<<<<<<< HEAD
        s = getContentsByFullPath(path + f + fext)
=======
        s = getContentsByFullPath(path + f + '.f')
>>>>>>> cc80e570353cdab2994bc065dadc0d821d478d12
        d[f] = []
        for k in s:
            if not k.startswith('!'):
                if isPastFirstLineOfScopingUnit(k): break
                if k.lower().rstrip().lstrip().startswith('program'):
                    d[f].append(r1.findall(k))
                if k.lower().rstrip().lstrip().startswith('module'):
                    d[f].append(r2.findall(k))
                if k.lower().rstrip().lstrip().startswith('function'):
                    d[f].append(r3.findall(k))
                if k.lower().rstrip().lstrip().startswith('subroutine'):
                    d[f].append(r4.findall(k))
        d[f] = flatten(d[f])
    for k,v in d.iteritems():
        FN.append(v)
    FN = flatten(FN)
    return FN

def getProgFiles(path,FL):
<<<<<<< HEAD
    fext = '.f90'
    progFiles = []
    for f in FL:
        s = getContentsByFullPath(path + f + fext)
=======
    progFiles = []
    for f in FL:
        s = getContentsByFullPath(path + f + '.f')
>>>>>>> cc80e570353cdab2994bc065dadc0d821d478d12
        for k in s:
            if not k.startswith('!'):
                if k.lower().rstrip().lstrip().startswith('program'):
                    progFiles.append(f)
    return progFiles

def sortFileList(path,t,FL,FN):
<<<<<<< HEAD
    fext = '.f90'
=======
>>>>>>> cc80e570353cdab2994bc065dadc0d821d478d12
    d = OrderedDict()
    r = re.compile(r'(?<=use\s)[A-Za-z_0-9]+')
    SL = []
    for fl,fn in t:
<<<<<<< HEAD
        s = getContentsByFullPath(path + fl + fext)
=======
        s = getContentsByFullPath(path + fl + '.f')
>>>>>>> cc80e570353cdab2994bc065dadc0d821d478d12
        d[(fl,fn)] = []
        for k in s:
            if not k.startswith('!'):
                if isPastUseStatements(k): break
                if k.lower().rstrip().lstrip().startswith('use '):
                    w = r.findall(k)
                    d[(fl,fn)].append(w)
        d[(fl,fn)] = flatten(d[(fl,fn)])
    remainingFiles = FL
    for i in range(0,len(remainingFiles)):
        dNew = d
        for tNew,dependentModsNew in dNew.iteritems():
            if len(dependentModsNew) == 0:
                fl = tNew[0]
                fn = tNew[1]
                try:remainingFiles.remove(fl)
                except:pass
                SL.append(fl)
                # Remove key:
                d = removekey(d,tNew)
                # remove all values from d:
                d = removeValues(d,fn)
                
    SL = flatten(SL)
    return SL

def removekey(d, key):
    r = OrderedDict()
    r = d
    del r[key]
    return r

def removeValues(d, val):
    r = OrderedDict()
    for k,arr in d.iteritems():
        r[k] = [ x for x in arr if not x == val ]
    return r
    
    
    
