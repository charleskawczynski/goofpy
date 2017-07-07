import inspect; import copy
from collections import OrderedDict
import sys
# Linux Machine
generatorPath = '/home/charlie/Dropbox/UCLA/FUSION SHARED/CHARLIES RESEARCH/pyProjects/goofPy/default'
# Windows Machine
generatorPath = 'C:\Users\Charlie\Dropbox\UCLA\FUSION SHARED\CHARLIES RESEARCH\pyProjects\goofPy\default'
sys.path.append(generatorPath)
import propertyClass as pc
import fortranClass as fc
if __name__ == "__main__":
    import generator as g

class userInput:
    """ THIS IS A SAMPLE USER INPUT FILE """
    def __init__(self):
        self.sp = 'real(kind=spn)';
        self.dp = 'real(kind=dpn)';
        self.qp = 'real(kind=qpn';
    def defineFile(self):
        fileName = inspect.getfile(inspect.currentframe())
        return fileName
        
    def griddata(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])
        
        f.setUsedModules(['constants'])
        f.setHelper(False)
        f.setPrivacy('public')
        
        prop.setName('M')
        prop.setClass('integer')
        prop.setObjectType('Parameter') # Object / Pointer / Primitive / Parameter
        prop.setValue('3')
        prop.setPrivacy('public')
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('N')
        prop.setClass('integer')
        prop.setObjectType('Parameter') # Object / Pointer / Primitive / Parameter
        prop.setValue('3')
        prop.setPrivacy('public')
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('x_0')
        prop.setClass(self.dp)
        prop.setObjectType('Parameter') # Object / Pointer / Primitive / Parameter
        prop.setValue('dble(0)')
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('y_0')
        prop.setClass(self.dp)
        prop.setObjectType('Parameter') # Object / Pointer / Primitive / Parameter
        prop.setValue('dble(0)')
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('x_f')
        prop.setClass(self.dp)
        prop.setObjectType('Parameter') # Object / Pointer / Primitive / Parameter
        prop.setValue('dble(1)')
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('y_f')
        prop.setClass(self.dp)
        prop.setObjectType('Parameter') # Object / Pointer / Primitive / Parameter
        prop.setValue('dble(1)')
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('dx')
        prop.setClass(self.dp)
        prop.setObjectType('Parameter') # Object / Pointer / Primitive / Parameter
        prop.setValue('(x_f - x_0)/dble(M)')
        props[prop.getName()] = copy.copy(prop)

        prop.setName('dy')
        prop.setClass(self.dp)
        prop.setObjectType('Parameter') # Object / Pointer / Primitive / Parameter
        prop.setValue('(y_f - y_0)/dble(N)')
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('xn')
        prop.setClass(self.dp+',dimension(0:M)')
        prop.setObjectType('Parameter') # Object / Pointer / Primitive / Parameter
        prop.setValue('(/(x_0+dble(i)*dx,i=0,M)/)')
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('yn')
        prop.setClass(self.dp+',dimension(0:N)')
        prop.setObjectType('Parameter') # Object / Pointer / Primitive / Parameter
        prop.setValue('(/(y_0+dble(i)*dy,i=0,N)/)')
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('xc')
        prop.setClass(self.dp+',dimension(0:M-1)')
        prop.setObjectType('Parameter') # Object / Pointer / Primitive / Parameter
        prop.setValue('(/(x_0+dble(i+0.5)*dx,i=0,M-1)/)')
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('yc')
        prop.setClass(self.dp+',dimension(0:N-1)')
        prop.setObjectType('Parameter') # Object / Pointer / Primitive / Parameter
        prop.setValue('(/(y_0+dble(i+0.5)*dy,i=0,N-1)/)')
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('xe')
        prop.setClass(self.dp+',dimension(0:M+1)')
        prop.setObjectType('Parameter') # Object / Pointer / Primitive / Parameter
        prop.setValue('(/x_0,(x_0+dble(i+0.5)*dx,i=0,M-1),x_f/)')
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('ye')
        prop.setClass(self.dp+',dimension(0:N+1)')
        prop.setObjectType('Parameter') # Object / Pointer / Primitive / Parameter
        prop.setValue('(/y_0,(y_0+dble(i+0.5)*dy,i=0,N-1),y_f/)')
        props[prop.getName()] = copy.copy(prop)
        
        return f,props

    def ref(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])
        
        f.setUsedModules(['constants'])
        f.setHelper(False)
        
        prop.setName('rho')
        prop.setClass(self.dp)
        prop.setObjectType('Parameter') # Object / Pointer / Primitive / Parameter
        prop.setValue('dble(1.0)')
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('p')
        prop.setClass(self.dp)
        prop.setObjectType('Parameter') # Object / Pointer / Primitive / Parameter
        prop.setValue('dble(1.0)')
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('t_c') # total convective time units
        prop.setClass(self.dp)
        prop.setObjectType('Parameter') # Object / Pointer / Primitive / Parameter
        prop.setValue('dble(4.0)')
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('L') # characteristic length
        prop.setClass(self.dp)
        prop.setObjectType('Parameter') # Object / Pointer / Primitive / Parameter
        prop.setValue('dble(1.0)')
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('Re')
        prop.setClass(self.dp)
        prop.setObjectType('Parameter') # Object / Pointer / Primitive / Parameter
        prop.setValue('dble(1000.0)')
        props[prop.getName()] = copy.copy(prop)
        
        return f,props

    def uField(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])
        
        f.setUsedModules(['constants','griddata_mod'])
        f.setHelper(False)
        
        prop.setName('x')
        prop.setClass(self.dp+',dimension(0:M+1,0:N)')
        prop.setValue('(/ ((/ (dble(0.0), i=0,M+1) /), j=0,N) /)')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('y')
        prop.setClass(self.dp+',dimension(0:M,0:N+1)')
        prop.setValue('(/ ((/ (dble(0.0), i=0,M) /), j=0,N+1) /)')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        return f,props

    def pField(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])
        
        f.setUsedModules(['constants','griddata_mod'])
        f.setHelper(False)
        
        prop.setName('x')
        prop.setClass(self.dp+',dimension(0:M-1,0:N-1)')
        prop.setValue('(/ ((/ (dble(0.0), i=0,M-1) /), j=0,N-1) /)')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('y')
        prop.setClass(self.dp+',dimension(0:M-1,0:N-1)')
        prop.setValue('(/ ((/ (dble(0.0), i=0,M-1) /), j=0,N-1) /)')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        return f,props

    def schemes(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])
        
        f.setUsedModules(['constants'])
        f.setHelper(True)
        
        prop.setName('CD2')
        prop.setClass('logical')
        prop.setObjectType('Parameter') # Object / Pointer / Primitive / Parameter
        prop.setValue('.true.')
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('trapezoidal')
        prop.setClass('logical')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('explicitEuler')
        prop.setClass('logical')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('implicitEuler')
        prop.setClass('logical')
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        return f,props

    def runData(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])
        
        f.setUsedModules(['constants'])
        f.setHelper(True)
        
        prop.setName('t_tot')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('dt')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('nsteps')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('t_f')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('Fo')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('Co')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('s') # time discretization
        prop.setClass('schemes')
        prop.setObjectType('Object') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        return f,props
        
    def solution(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])
        
        f.setUsedModules(['constants'])
        f.setHelper(True)
        
        prop.setName('un')
        prop.setClass('uField')
        prop.setObjectType('Object') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('unm1')
        prop.setClass('uField')
        prop.setObjectType('Object') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('unp1')
        prop.setClass('uField')
        prop.setObjectType('Object') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('pn')
        prop.setClass('pField')
        prop.setObjectType('Object') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('pnm1')
        prop.setClass('pField')
        prop.setObjectType('Object') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        return f,props

    def stats(self):
        f = fc.fortranClass();
        props = OrderedDict()
        prop = pc.propertyClass()
        f.setName(inspect.stack()[0][3])
        
        f.setUsedModules(['constants'])
        f.setHelper(True)
        
        prop.setName('L1')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('L2')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('Linf')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('CPUtime')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('N_steps')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('CPU_t_per_step')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        prop.setName('CPU_t_per_L2')
        prop.setClass(self.dp)
        prop.setObjectType('Primitive') # Object / Pointer / Primitive / Parameter
        props[prop.getName()] = copy.copy(prop)
        
        return f,props
