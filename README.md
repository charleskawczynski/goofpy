goofpy
======
-------------------------------- ABOUT ------------------------------------
GoofPy is a Generator of Object-Oriented Fortran code via Python.
The main.py file in the test folder is a sample.
------------------------------- main.py (example) -----------------------------------
(header) ...

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
---------------------------------------------------------------------------

This test code generates the following subroutines:
 - init
 - delete
 - display
 - print
 - export
 - import
for all of the class properties, specified in the main.py file.

------------------------------ HOW TO USE ---------------------------------

  Then run python main.py in a PROJECT_NAME folder. A folder, named
  'generated_code' and .f90 files will be generated.

------------------------------- OPTIONS -----------------------------------

- Two types of data structure may be chosen:
    1) 'primitive' - any primitive type (integer, character, real etc.)
    2) 'object' - a derived data type (which are basically objects in fortran 90)
