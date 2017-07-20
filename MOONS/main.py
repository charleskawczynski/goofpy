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
g.set_directories(os.path.abspath(__file__),PS)
g.add_base_files(['precision'+PS+'current_precision.f90'])
g.add_base_files(['IO'+PS+'inquire_funcs.f90'])
g.add_base_files(['IO'+PS+'IO_check.f90'])
g.add_base_files(['IO'+PS+'IO_tools.f90'])
g.add_base_files(['string'+PS+'string.f90'])
g.add_base_files(['string'+PS+'string_aux.f90'])
# g.print()
prim = 'primitive'
obj = 'object'
priv = 'public'
log = 'logical'
# cp = 'real(selected_real_kind(32))' # ! Quad precision
# cp = 'real(selected_real_kind(8))'  # ! Single precision
real = 'real(cp)' # ! Double precision (default)

m_name = 'array'
g.add_module(m_name)
g.module[m_name].set_used_modules(['IO_tools_mod'])
g.module[m_name].set_privacy('private')
g.module[m_name].add_prop(FP.init_all('f',real,priv,prim,True,1,6,'0.0_cp'))
g.module[m_name].add_prop(FP.init_all('N','integer',priv,prim,False,0,0,'0'))

m_name = 'sparse'
g.add_module(m_name)
g.module[m_name].set_used_modules(['IO_tools_mod'])
g.module[m_name].set_privacy('private')
g.module[m_name].add_prop(FP.init_all('L','array',priv,obj,False,1,1,'0'))
g.module[m_name].add_prop(FP.init_all('D','array',priv,obj,False,1,1,'0'))
g.module[m_name].add_prop(FP.init_all('U','array',priv,obj,False,1,1,'0'))
g.module[m_name].add_prop(FP.init_all('staggered','logical',priv,prim,False,0,0,'0'))

m_name = 'coordinates'
g.add_module(m_name)
g.module[m_name].set_used_modules(['IO_tools_mod'])
g.module[m_name].set_privacy('private')
g.module[m_name].add_prop(FP.init_all('hn','array',priv,obj,False,1,1,'0'))
g.module[m_name].add_prop(FP.init_all('hc','array',priv,obj,False,1,1,'0'))
g.module[m_name].add_prop(FP.init_all('col','sparse',priv,obj,False,1,2,'0'))
g.module[m_name].add_prop(FP.init_all('stag','sparse',priv,obj,False,1,2,'0'))

m_name = 'grid'
g.add_module(m_name)
g.module[m_name].set_used_modules(['IO_tools_mod'])
g.module[m_name].set_privacy('private')
g.module[m_name].add_prop(FP.init_all('c','coordinates',priv,obj,False,1,3,'0'))

m_name = 'grid_field'
g.add_module(m_name)
g.module[m_name].set_used_modules(['IO_tools_mod'])
g.module[m_name].set_privacy('private')
g.module[m_name].add_prop(FP.init_all('f',real,priv,prim,True,3,3,'0'))
g.module[m_name].add_prop(FP.init_all('s','integer',priv,prim,False,1,3,'0'))
g.module[m_name].add_prop(FP.init_all('s_1D','integer',priv,prim,False,1,1,'0'))

m_name = 'block'
g.add_module(m_name)
g.module[m_name].set_used_modules(['IO_tools_mod'])
g.module[m_name].set_privacy('private')
g.module[m_name].add_prop(FP.init_all('g','grid',priv,obj,False,1,3,'0'))

m_name = 'mesh'
g.add_module(m_name)
g.module[m_name].set_used_modules(['IO_tools_mod'])
g.module[m_name].set_privacy('private')
g.module[m_name].add_prop(FP.init_all('B','block',priv,obj,True,1,3,'0'))

# m_name = 'simple_int_tensor'
# g.add_module(m_name)
# g.module[m_name].set_used_modules(['IO_tools_mod'])
# g.module[m_name].set_privacy('private')
# g.module[m_name].add_prop(FP.init_all('eye','integer',priv,prim,False,1,3,'0'))

# m_name = 'export_logicals'
# g.add_module(m_name)
# g.module[m_name].set_used_modules(['IO_tools_mod'])
# g.module[m_name].set_privacy('private')
# L=['export_analytic','export_meshes','export_vort_SF','export_mat_props','export_cell_volume','export_ICs','export_planar','export_symmetric','export_mesh_block','export_soln_only']
# for k in L: g.module[m_name].add_prop(FP.init_all(k,'logical',priv,prim,False,1,1,'0'))

# m_name = 'TMP'
# g.add_module(m_name)
# g.module[m_name].set_used_modules(['IO_tools_mod'])
# g.module[m_name].set_privacy('private'
# g.module[m_name].add_prop(FP.init_all('g','grid',priv,obj,False,1,1,'0'))
# g.module[m_name].add_prop(FP.init_all('b','grid',priv,obj,True,1,2,'0'))
# g.module[m_name].add_prop(FP.init_all('i','integer',priv,prim,False,1,1,'0'))
# g.module[m_name].add_prop(FP.init_all('L','logical',priv,prim,False,1,1,'.false.'))

g.generate_code()
