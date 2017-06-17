import inspect; import copy
from collections import OrderedDict
import sys
# Linux Machine
PS = '\\'
# Windows Machine
PS = '/'
generatorPath = 'C:'+PS+'Users'+PS+'Charlie'+PS+'Documents'+PS+'GitHub'+PS+'goofPy'
sys.path.append(generatorPath)
import propertyClass as pc
import fortranClass as fc
if __name__ == "__main__":
    import generator as g

class userInput:
    """
            goofPy (Generator of Object Oriented Fortran via Python)
    -------------------------------- ABOUT ------------------------------------
    This code generates functions to
     - set, setAll
     - get, getAll
     - print, printAll
    for all properties.
    ------------------------------ HOW TO USE ---------------------------------
    - Run this file with the following folder structure:
        /PROJECT_NAME
            userInput.py
            /classes/
            /calc/
            /helper/
            /generator/
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
    """

    # Sample input:
    def __init__(self):
        self.sp = 'real(kind=sp_num)';
        self.dp = 'real(kind=dp_num)';
        self.qp = 'real(kind=qp_num';
    def defineFile(self):
        fileName = inspect.getfile(inspect.currentframe())
        return fileName

    def counters(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])

        f.setUsedModules([''])
        f.setHelper(False)
        f.setPrivacy('public')

        prop.setName('i')
        prop.setClass('integer')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('j')
        prop.setClass('integer')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('k')
        prop.setClass('integer')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('i_geometry')
        prop.setClass('integer')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        return f,props

    def geomParams(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])

        f.setUsedModules(['counters_mod','constants_mod','simParams_mod','debugger_mod'])
        f.setHelper(True)

        prop.setName('Length1')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('Length2')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('Height')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('Width')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('Diameter_in')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('Diameter_out')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('a_out')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('b_out')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('a_in')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        prop.setAutoCalc(True)
        prop.setDependentProps([props['a_out'],props['Diameter_out']])
        props[prop.getName()] = copy.copy(prop)

        prop.setName('b_in')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        prop.setAutoCalc(True)
        prop.setDependentProps([props['b_out'],props['Diameter_in']])
        props[prop.getName()] = copy.copy(prop)

        prop.setName('h_ellipse')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        prop.setAutoCalc(True)
        prop.setDependentProps([props['Length1']])
        props[prop.getName()] = copy.copy(prop)

        prop.setName('k_ellipse')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        prop.setAutoCalc(True)
        prop.setDependentProps([props['b_out']])
        props[prop.getName()] = copy.copy(prop)

        return f,props

    def combinedNormTau(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])

        f.setUsedModules(['constants_mod','counters_mod','exportData_mod'])
        f.setHelper(True)

        prop.setName('CNT')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        return f,props

    def vector(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])

        f.setUsedModules(['constants_mod','counters_mod','simParams_mod','debugger_mod'])
        f.setHelper(True)

        prop.setName('x')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('y')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('z')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('mag')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        prop.setAutoCalc(True)
        prop.setDependentProps([props['x'],props['y'],props['z']])
        props[prop.getName()] = copy.copy(prop)

        prop.setName('slope')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        prop.setAutoCalc(True)
        prop.setDependentProps([props['x'],props['y']])
        props[prop.getName()] = copy.copy(prop)

        return f,props

    def position(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])

        f.setUsedModules(['constants_mod'])
        f.setHelper(True)

        prop.setName('initial')
        prop.setClass('vector')
        prop.setObjectType('Object') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('instant')
        prop.setClass('vector')
        prop.setObjectType('Object') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('final')
        prop.setClass('vector')
        prop.setObjectType('Object') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        return f,props

    def direction(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])

        f.setUsedModules(['constants_mod'])
        f.setHelper(True)

        prop.setName('initial')
        prop.setClass('vector')
        prop.setObjectType('Object') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('instant')
        prop.setClass('vector')
        prop.setObjectType('Object') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('final')
        prop.setClass('vector')
        prop.setObjectType('Object') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('theta_i')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('theta')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('theta_f')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('phi_i')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        return f,props

    def energy(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])

        f.setUsedModules(['constants_mod','simParams_mod','debugger_mod'])
        f.setHelper(True)

        prop.setName('irradiated')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('instant')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('transmitted')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('absorbed')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('escaped')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        return f,props

    def BCs(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])

        f.setUsedModules(['constants_mod','simParams_mod','debugger_mod','vector_mod'])
        f.setHelper(True)

        prop.setName('x_min')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('x_max')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('y_min')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('y_max')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('xmin_IO')
        prop.setClass('logical')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('xmax_IO')
        prop.setClass('logical')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('ymin_IO')
        prop.setClass('logical')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('ymax_IO')
        prop.setClass('logical')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        return f,props

    def vLine(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])

        f.setUsedModules(['constants_mod','simParams_mod','debugger_mod','vector_mod'])
        f.setHelper(True)

        prop.setName('id')
        prop.setClass('integer')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('BC')
        prop.setClass('BCs')
        prop.setObjectType('Object') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('x')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('normalRight')
        prop.setClass('logical')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        return f,props

    def hLine(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])

        f.setUsedModules(['constants_mod','simParams_mod','debugger_mod','vector_mod'])
        f.setHelper(True)

        prop.setName('id')
        prop.setClass('integer')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('BC')
        prop.setClass('BCs')
        prop.setObjectType('Object') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('y')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('normalUp')
        prop.setClass('logical')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        return f,props

    def eLine(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])

        f.setUsedModules(['constants_mod','simParams_mod','debugger_mod','vector_mod'])
        f.setHelper(True)

        prop.setName('id')
        prop.setClass('integer')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('BC')
        prop.setClass('BCs')
        prop.setObjectType('Object') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('a')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('b')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('h')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('k')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('normalOutward')
        prop.setClass('logical')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        return f,props

    def line(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])

        f.setUsedModules(['constants_mod','simParams_mod','debugger_mod','vector_mod','BCs_mod'])
        f.setHelper(True)

        prop.setName('id')
        prop.setClass('integer')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('h')
        prop.setClass('hLine')
        prop.setObjectType('Pointer') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('v')
        prop.setClass('vLine')
        prop.setObjectType('Pointer') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('e')
        prop.setClass('eLine')
        prop.setObjectType('Pointer') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        return f,props

    def surface(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])

        f.setUsedModules(['constants_mod','simParams_mod','geomParams_mod','debugger_mod','counters_mod','exportData_mod'])
        f.setHelper(True)

        prop.setName('id')
        prop.setClass('integer')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('l')
        prop.setClass('line')
        prop.setObjectType('Object') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('entrance')
        prop.setClass('logical')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('exit')
        prop.setClass('logical')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        return f,props

    def distribution(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])

        f.setUsedModules(['constants_mod','simParams_mod'])
        f.setHelper(True)

        prop.setName('min')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('max')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        return f,props

    def photon(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])

        f.setUsedModules(['constants_mod','simParams_mod','debugger_mod','distribution_mod','exportData_mod','vector_mod','line_mod','surface_mod'])
        f.setHelper(True)

        prop.setName('p')
        prop.setClass('position')
        prop.setObjectType('Object') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('d')
        prop.setClass('direction')
        prop.setObjectType('Object') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('e')
        prop.setClass('energy')
        prop.setObjectType('Object') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('id')
        prop.setClass('integer')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('S_id')
        prop.setClass('integer')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('endPursuit')
        prop.setClass('logical')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        return f,props

    def photonResults(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])

        f.setUsedModules(['constants_mod','simParams_mod'])
        f.setHelper(True)

        prop.setName('p')
        prop.setClass('photon')
        prop.setObjectType('Object') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        return f,props

    def simResults(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])

        f.setUsedModules(['constants_mod','simParams_mod','exportData_mod','photon_mod','photonResults_mod','combinedNormTau_mod'])
        f.setHelper(True)

        prop.setName('irradiated')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('tau')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('normTau')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('theta')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('phi')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('time')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        return f,props

    def time(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])

        f.setUsedModules(['constants_mod','counters_mod','simResults_mod'])
        f.setHelper(True)

        prop.setName('elapsed')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('cumulative')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('average')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('remaining')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        prop.setName('N_Photons')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)

        return f,props

