# This is a Fortran 90 Object Oriented Generator,
# Created by Charlie Kawczynski, 2014
import os
import sys
if os.name == 'posix':
    pass # this is the operating system name
elif os.name == 'nt':
    clear = lambda: os.system('cls')
    clear()
targetPath = os.getcwd()
sys.path.append(targetPath)
from userInput import userInput as UI
import myFuncs as func
import inspect

# For windows machine, the following message:
# UMD has deleted: module_name, module_name, etc.
# is a poor name for a message, it should read "reloaded", not "deleted"

<<<<<<< HEAD
fext = '.f90'
=======
>>>>>>> cc80e570353cdab2994bc065dadc0d821d478d12
all_functions = inspect.getmembers(UI,inspect.isfunction)

abspath = os.path.abspath(__file__)
generatorPath = os.path.dirname(abspath)
os.chdir(targetPath)

path = targetPath + '/../'
name = 'userInput'
ext = '.py'

classList = func.getClassesByFullPath(targetPath + '/userInput.py')
func.makeDotBat(targetPath + '/classes/')
userIn = UI()
fun = 'userIn' + '.' + classList[0] + '()'
fileName = eval(fun)

basePath = fileName.split(os.path.basename(fileName))[0]
basePath = targetPath # added on windows machine...
sys.path.append(basePath)

N_calc = 0
N_class = 0

# AllClassFuncs_mod
fname = 'allClassFuncs_mod'
<<<<<<< HEAD
l = func.getContentsByFullPath(generatorPath + '/' + fname + fext)
func.writeListToFile(basePath + '/classes/',fname,fext,l)

for f in classList[1:]:
    fun = 'userIn' + '.' + f + '()'
    print fun
    fclass,props = eval(fun)
=======
l = func.getContentsByFullPath(generatorPath + '/' + fname + '.f')
func.writeListToFile(basePath + '/classes/',fname,'.f',l)

for f in classList[1:]:
    fun = 'userIn' + '.' + f + '()'
    fclass,props = eval(fun)
    
>>>>>>> cc80e570353cdab2994bc065dadc0d821d478d12
    # Fortran Class
    lofl = fclass.getClassFile(props,classList)
    l = func.flatten(lofl)
    path = basePath + '/classes/'
    fname = f + '_mod'
<<<<<<< HEAD
    func.writeListToFile(path,fname,fext,l)
    N_class += func.file_len(path+fname+fext)
=======
    func.writeListToFile(path,fname,'.f',l)
    N_class += func.file_len(path+fname+'.f')
>>>>>>> cc80e570353cdab2994bc065dadc0d821d478d12
    
    # Fortran Calc Files
    for name, object_ in props.iteritems():
        if (object_.getCalc()):
            lofl = fclass.getCalcFile(object_,classList)
            l = func.flatten(lofl)
            path = basePath + '/calc/'
            fname = 'calc_' + name
            try:
<<<<<<< HEAD
                with open(path+fname+fext) as fh:
                    pass
            except:
                func.writeListToFile(path,fname,fext,l)
                N_calc += func.file_len(path+fname+fext)
=======
                with open(path+fname+'.f') as fh:
                    pass
            except:
                func.writeListToFile(path,fname,'.f',l)
                N_calc += func.file_len(path+fname+'.f')
>>>>>>> cc80e570353cdab2994bc065dadc0d821d478d12
    
    # Fortran Helper files
    if fclass.getHelper():
        path = basePath + '/helper/'
        fname = f + '_helper'
        try:
<<<<<<< HEAD
            with open(path+fname+fext) as fh:
                pass
        except:
            func.writeListToFile(path,fname,fext,[])
=======
            with open(path+fname+'.f') as fh:
                pass
        except:
            func.writeListToFile(path,fname,'.f',[])
>>>>>>> cc80e570353cdab2994bc065dadc0d821d478d12

N_tot = N_class + N_calc
print 'Number of lines generated (Class files): ' + str(N_class)
print 'Number of lines generated (Calc files): ' + str(N_calc)
print 'Number of lines generated (Total): ' + str(N_tot)
print ''
print 'Code generation complete.'




