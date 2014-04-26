goofpy
======

GoofPy is a Generator of Object-Oriented Fortran code via Python. The userInput.py file is a sample.

        goofPy (Generator of Object Oriented Fortran via Python)

-------------------------------- ABOUT ------------------------------------

This code generates functions to
 - set, setAll
 - get, getAll
 - print, printAll
 - writeToFile, writeToFileAll
for all properties.

------------------------------ HOW TO USE ---------------------------------

- Run this file with the following folder structure:
    /PROJECT_NAME
        userInput.py
        /classes/
        /calc/
        /helper/
  then check the 'class', 'helper' and 'calc' folders

------------------------------- OPTIONS -----------------------------------

- The Fortran class name is specified by the Python function name.

- Three types of properties may be chosen:
    1) Object - an abstract object (for abstract data structures)
    2) Pointer - a pointer object (for dynamic capabilities)
    3) Primitive - any primitive type (integer, character, real etc.)
    4) Parameter - a once-assignable primitive, often used for array sizes (integer, character, real etc.)

- Values of Parameters may be set with: prop.setValue()

- Modules can be added to a class specified by a list of strings with:
f.setUsedModules(['mod1','mod2',...,'modn'])

- To add functionality to a class, include a 'helper' file in 
the 'helper' folder with: f.setHelper(True)
Note: does not overwrite files

- To specify class privacy, use: f.setPrivacy('') prublic/private
Note: default = private
- To specify a parameter's privacy, use: prop.setPrivacy('') prublic/private
Note: default = private

- To auto-calculate a property when an object is made, use:
prop.setAutoCalc(True) and  
prop.setDependentProps([props['x'],props['y']])
to set the dependent properties of the calculated property.
This property may then be defined in the 'calc' folder.
Note: autocalc functionality does not overwrite files
