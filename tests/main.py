import os
if os.name == 'posix':
    pass # this is the operating system name
elif os.name == 'nt':
    clear = lambda: os.system('cls')
    clear()
import inspect; import copy
from collections import OrderedDict
import sys
PS = '\\' # Linux Machine
PS = '/' # Windows Machine
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'+PS+'source'
sys.path.append(generatorPath)
import generator as g
import fortran_property as FP

# add_prop parameters:
#     1) name (string)
#     2) data type (integer,real etc.) (string)
#     3) Object / allocatable / Primitive / Parameter (string)
#     4) dimension (int)

g = g.generator()
g.set_directories(os.path.abspath(__file__))
g.print()

# module_name = 'array'
# g.add_module(module_name)
# g.module[module_name].set_used_modules(['IO_tools_mod'])
# g.module[module_name].set_privacy('private')
# g.module[module_name].add_prop(FP.init_all('a','real(cp)','private','primitive',True,1,6,'0.0_cp'))
# g.module[module_name].add_prop(FP.init_all('N','integer','private','primitive',False,0,0,'0'))

# module_name = 'coordinates'
# g.add_module(module_name)
# g.module[module_name].set_used_modules([''])
# g.module[module_name].set_privacy('private')
# g.module[module_name].add_prop(FP.init_all('hn','array','private','object',False,1,1,'0'))
# g.module[module_name].add_prop(FP.init_all('hc','array','private','object',False,1,1,'0'))

# module_name = 'grid'
# g.add_module(module_name)
# g.module[module_name].set_used_modules([''])
# g.module[module_name].set_privacy('private')
# g.module[module_name].add_prop(FP.init_all('c','coordinates','private','object',False,1,3,'0'))

# module_name = 'TMP'
# g.add_module(module_name)
# g.module[module_name].set_used_modules(['used_modB1_mod','used_modB2_mod'])
# g.module[module_name].set_privacy('private')
# g.module[module_name].add_prop(FP.init_all('g','grid','private','object',False,1,1,'0'))
# g.module[module_name].add_prop(FP.init_all('b','grid','private','object',True,1,2,'0'))
# g.module[module_name].add_prop(FP.init_all('i','integer','private','primitive',False,1,1,'0'))
# g.module[module_name].add_prop(FP.init_all('L','logical','private','primitive',False,1,1,'.false.'))

# g.generate_code()


module_name = 'array'
g.add_module(module_name)
g.module[module_name].set_used_modules(['module_needed_for_array_mod'])
g.module[module_name].set_privacy('private')
g.module[module_name].add_prop(FP.init_all('N','integer','private','primitive',False,0,0,'0'))
g.module[module_name].add_prop(FP.init_all('a','real(8)','private','primitive',True,1,6,'0.0'))

module_name = 'arrays'
g.add_module(module_name)
g.module[module_name].set_used_modules(['module_needed_for_array_mod'])
g.module[module_name].set_privacy('private')
g.module[module_name].add_prop(FP.init_all('N','integer','private','primitive',False,0,0,'0'))
g.module[module_name].add_prop(FP.init_all('a','array','private','object',True,1,6,'0.0'))

g.generate_code()
