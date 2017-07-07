import os
import myFuncs as func
import fortran_property as FP
import fortran_module as FM
from collections import OrderedDict

# DOUBLE CHECK WHEREVER CALL SET FUNCTIONS ARE USED THAT THE ISSET(SETSET) OR SETSET == True
# AND DO THE SAME WITH PRINT FUNCTIONS SO THAT THESE CAN BE TURNED OFF IF DESIRED+

class generate_fortran_module:
    nFiles = 0 # number of input files

    def __init__(self):
        fortran_module.nFiles += 1
        # From ref: http://en.wikibooks.org/wiki/Fortran/Beginning_Fortran
        # The maximum number of characters per line is 72. So, conservatively:
        self.prop = OrderedDict()
        self.maxLineLength = 71
        self.implicitNone = 'implicit none'
        self.baseSpaces = ' '*7
        self.spaces = ['' for x in range(10)]
        for i in range(len(self.spaces)):
            self.spaces[i] = ' '*i
        self.stars = ['' for x in range(30)]
        for i in range(len(self.stars)):
            self.stars[i] = '*'*i

        self.ptr = 'ptr'


    def contruct_fortran_module(self):
        c = []
        c.append(self.baseSpaces + 'module ' + self.get_name() + '_mod')
        c.append(self.write_used_modules())

        c.append(self.define_values_given_props())
        c.append('')
        c.append(self.open_class_definition())
        c.append(self.write_class_definition())
        c.append(self.close_class_definition())
        c.append('')
        c.append(self.baseSpaces + 'contains')
        c.append(self.write_all_functions())
        c.append(self.baseSpaces + 'end module ' + self.get_name() + '_mod')

        # Break lines to maximum number of characters
        l = func.flatten(c)
        s = []
        for k in l:
            s.append(self.breakLine(k,[]))
        return s

    ###CONFIG####################################################################*/

    def write_used_modules(self):
        dependent_names = OrderedDict()
        c = [self.baseSpaces + 'use allClassFuncs_mod']
        for i in range(len(self.usedModules)):
            if not self.usedModules[i] == '':
                c.append( self.baseSpaces + 'use ' + self.usedModules[i])
        for key in self.prop:
            if self.prop[key].get_class() in self.allClasses:
                if not any(self.prop[key].get_class() in v for v in dependent_names.values()):
                    c.append(self.baseSpaces + 'use ' + self.prop[key].get_class() + '_mod')
                    dependent_names[key] = self.prop[key].get_class()
        c.append('')
        return c

    def define_values_given_props(self):
        c = []
        for key in self.prop:
            key = self.prop[key].get_name()
            if self.prop[key].get_object_type().lower() == 'Parameter'.lower():
                value = self.prop[key].getValue()
                try:
                    if self.prop[key].get_privacy().lower() == 'public'.lower():
                        c.append(self.baseSpaces + self.prop[key].get_class() + ',parameter :: ' + key + ' = ' + value)
                    else:
                        c.append(self.baseSpaces + self.prop[key].get_class() + ',private,parameter :: ' + key + ' = ' + value)
                except:
                    c.append(self.baseSpaces + self.prop[key].get_class() + ',private,parameter :: ' + key + ' = ' + value)
            elif self.prop[key].get_object_type().lower() == 'Primitive'.lower():
                try:
                    value = self.prop[key].getValue()
                    try:
                        if self.prop[key].get_privacy().lower() == 'public'.lower():
                            c.append(self.baseSpaces + self.prop[key].get_class() + ' :: ' + key + ' = ' + value)
                        else:
                            c.append(self.baseSpaces + self.prop[key].get_class() + ',private :: ' + key + ' = ' + value)
                    except:
                            c.append(self.baseSpaces + self.prop[key].get_class() + ',private :: ' + key + ' = ' + value)

                except:
                    pass
        return c

    def open_class_definition(self):
        c = [self.baseSpaces + 'type ' + self.name]
        try:
            if not self.get_privacy().lower() == 'public'.lower():
                c.append(self.baseSpaces + self.spaces[2] + 'private')
        except:
            c.append(self.baseSpaces + self.spaces[2] + 'private')
        return c


    def init_prop(self,prop,intent):
        try:
            if (prop.get_object_type().lower() == 'Primitive'.lower()):
                return self.baseSpaces + self.spaces[2] + prop.get_class() + ', intent(' + intent + ') :: ' + prop.get_name()
            elif (prop.get_object_type().lower() == 'Parameter'.lower()):
                return self.baseSpaces + self.spaces[2] + prop.get_class() + ', intent(' + intent + ') :: ' + prop.get_name()
            elif(prop.get_object_type().lower() == 'Object'.lower()):
                return self.baseSpaces + self.spaces[2] + 'type(' + prop.get_class() + '), intent(' + intent + ') :: ' + prop.get_name()
            elif(prop.get_object_type().lower() == 'Pointer'.lower()):
                return self.baseSpaces + self.spaces[2] + 'type(' + prop.get_class() + '), intent(' + intent + '),optional,target :: ' + prop.get_name()
        except:
            print('No correct property types were chosen in Property')




    def initializeDummy(self,prop):
        try:
            if (prop.get_object_type().lower() == 'Primitive'.lower()):
                return self.baseSpaces + self.spaces[2] + prop.get_class() + ' :: ' + prop.get_name()
            elif (prop.get_object_type().lower() == 'Parameter'.lower()):
                return self.baseSpaces + self.spaces[2] + prop.get_class() + ' :: ' + prop.get_name()
            elif (prop.get_object_type().lower() == 'Pointer'.lower()):
                return self.baseSpaces + self.spaces[2] + 'type(' + prop.get_class() + '),target :: ' + prop.get_name()
            elif (prop.get_object_type().lower() == 'Object'.lower()):
                return self.baseSpaces + self.spaces[2] + 'type(' + prop.get_class() + ') :: ' + prop.get_name()
        except:
            print('No correct property types were chosen in Dummy')



    def initializeThis(self,intent):
        return self.baseSpaces + self.spaces[2] + 'type(' + self.name + '),intent(' + intent + ') :: this'


    def funcImplicitNone(self):
        return self.baseSpaces + self.spaces[2] + self.implicitNone

    def implicitNone(self):
        return self.baseSpaces + self.implicitNone

    def breakLine(self,stringList,result = []):
        spaces = self.baseSpaces[:-2]
        if (len(stringList) >= self.maxLineLength):
            strMax = stringList[0:self.maxLineLength]
            # If commas exist in the function signature, then break the line down
            # If this is still a problem then reduce the size of the function name or property names
            if ')' in strMax:
                cutoff = strMax.rfind(')')+1
            elif ',' in strMax:
                cutoff = strMax.rfind(',')+1
            elif '(' in strMax:
                cutoff = strMax.rfind('(')+1
            else:
                result = stringList

            if any(x in strMax for x in [',','(',')']):
                strCut = strMax[0:cutoff]
                strRemain = stringList[cutoff:]
                if not (len(strRemain.replace(' ','')) <= len('')):
                    result.append(strCut)
                    strRemain = spaces + '&   ' + strRemain
                    self.breakLine(strRemain,result)
        else:
            result.append(stringList)

        return result

    ###CLASS DEFINITION####################################################################*/
    def write_class_definition(self):
        c = []
        for key in self.prop:
            key = self.prop[key].get_name()
            func = 'write' + self.prop[key].get_object_type() + 'ClassDefinition'
            c.append(getattr(self, func)(key))
        return c




    def writeObjectClassDefinition(self,name):
        return self.baseSpaces + self.spaces[2] + 'type(' + self.prop[name].get_class() + ') :: ' + self.prop[name].get_name()


    def writePointerClassDefinition(self,name):
        return self.baseSpaces + self.spaces[2] + 'type(' + self.prop[name].get_class() + '), pointer :: ptr_' + self.prop[name].get_name()


    def writePrimitiveClassDefinition(self,name):
        return self.baseSpaces + self.spaces[2] + self.prop[name].get_class() + ' :: ' + self.prop[name].get_name()

    def writeParameterClassDefinition(self,name):
        return self.baseSpaces + self.spaces[2] + self.prop[name].get_class() + ' :: ' + self.prop[name].get_name()


    def close_class_definition(self):
        return self.baseSpaces + 'endtype'


    ###WRITE ALL FUNCTIONS############################################################*/
    def write_all_functions(self):
        c = []
        c.append('')
        c.append(self.setAll())
        c.append('')
        c.append(self.writeSetFunction())

        c.append(self.writeGetFunction())

        c.append(self.getAll())
        c.append('')
        c.append(self.writePrintFunction())

        c.append(self.printAll())
        c.append('')

        c.append(self.makeWriteFunction())

        c.append(self.makeWriteAll())
        c.append('')
        return c


    def full_func_signature(self,sig,args,result): return 'function ' + sig + '(' + args + ') result(' + result + ')'
    def full_sub_signature(self,sig,args): return 'subroutine ' + sig + '(' + args + ')'
    def end_function(self,function = False): return 'end function'
    def end_sub(self,function = False): return 'end subroutine'




    ###SET####################################################################*/
    def writeSetFunction(self):
        c = []
        for key in self.prop:
            if not self.prop[key].get_object_type().lower() == 'Parameter'.lower():
                if (self.prop[key].getSet()):
                    func = 'writeSet' + self.prop[key].get_object_type() + 'Function'
                c.append(getattr(self, func)(key))
                c.append('')
        return c


    def writeSetObjectFunction(self,name):
        sig = 'set' + self.name.capitalize() + name.capitalize()
        totalSig = self.baseSpaces + self.full_sub_signature(sig,'this,'+ name)
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('inout'))
        c.append(self.init_prop(self.prop[name],'in') )
        c.append(self.baseSpaces + self.spaces[2] + 'this%' + name + ' = ' + name)
        c.append(self.baseSpaces + self.end_sub())
        return c


    def writeSetPointerFunction(self,name):

        sig = 'setPoly' + self.prop[name].get_class().capitalize()
        totalSig = self.baseSpaces + self.full_sub_signature(sig,'this,'+ name)
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('inout') )
        c.append(self.init_prop(self.prop[name],'in') )
        c.append(self.baseSpaces + self.spaces[2] + 'this%' + self.ptr + '_' + name + ' => ' + name)
        c.append(self.nullifyOthers(self.prop,name))
        c.append(self.baseSpaces + self.end_sub())
        return c


    def writeSetPrimitiveFunction(self,name):
        sig = 'set' + self.name.capitalize() + name.capitalize()
        totalSig = self.baseSpaces +self.full_sub_signature(sig,'this,'+ name)
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('inout') )
        c.append(self.init_prop(self.prop[name],'in') )
        c.append(self.baseSpaces + self.spaces[2] + 'this%' + name + ' = ' + name)
        c.append(self.baseSpaces + self.end_sub())
        return c


    def setAll(self):
        sig = 'set' + self.name.capitalize()
        c = []
        self.setArgObjects()
        self.setArgList()
        argList = ','.join(self.argList)
        totalSig = self.baseSpaces + self.full_func_signature(sig,argList,'this')
        c.append(totalSig)
        c.append(self.baseSpaces + self.spaces[2] + 'implicit none' )
        c.append(self.baseSpaces + self.spaces[2] + 'type(' + self.name + ') :: this' )

        for key in self.argObjects:
            try:
                self.argObjects[key].getValue()
            except:
                c.append(self.init_prop(self.argObjects[key],'in'))

        for key in self.argObjects:
            try:
                self.argObjects[key].getValue()
                c.append(self.baseSpaces + self.spaces[2] + 'this%' + key + ' = ' + key)
            except: pass

        for key in self.argObjects:
            try:
                self.argObjects[key].getValue()
            except:
                func = 'callSet' + self.argObjects[key].get_object_type()
                c.append(getattr(self, func)(self.argObjects[key]))

        c.append(self.baseSpaces + self.end_function())

        return c



    def callSetObject(self,object_):
        return self.baseSpaces + self.spaces[2] + 'call Set' + self.name.capitalize() + object_.get_name().capitalize() + '(this,' + object_.get_name() + ')'


    def callSetPointer(self,object_):
        c = []
        c.append(self.baseSpaces + self.spaces[2] + 'if (present(' + object_.get_name() + ')) then' )
        c.append(self.baseSpaces + self.spaces[4] + 'call SetPoly' + object_.get_class().capitalize() + '(this,' + object_.get_name() + ')' )
        c.append(self.nullifyOthers(self.prop,object_.get_name()))
        c.append(self.baseSpaces + self.spaces[2] + 'endif' )
        return c


    def callSetPrimitive(self,object_):
        return self.baseSpaces + self.spaces[2] + 'call Set' + self.name.capitalize() + object_.get_name().capitalize() + '(this,' + object_.get_name() + ')'


    def setArgObjects(self):
        self.argObjects = OrderedDict()
        for key in self.prop:
            # if not self.prop[key].getCalc():
            try: self.prop[key].getValue()
            except: self.argObjects[key] = self.prop[key]


    def getArgObjects(self):
        return self.argObjects


    def setArgList(self):
        argList = []
        for key in self.prop:
            object_ = self.prop[key]
            name = key
            # if not object_.getCalc():
            try: object_.getValue()
            except: argList.append(name)
        self.argList = argList


    def getArgList(self):
        return self.argList


    def nullifyOthers(self,objectArray,name):
        c = []
        for key in objectArray:
            if not objectArray[key].get_name() == name:
                if (objectArray[key].get_object_type().lower() == 'Pointer'.lower()):
                    c.append(self.baseSpaces + self.spaces[2] + 'nullify(this%' + self.ptr + '_' + objectArray[key].get_name() + ')' )
        return c


    ###GET####################################################################*/

    def writeGetFunction(self):
        c = []
        for key in self.prop:
            if self.prop[key].getGet():
                func = 'writeGet' + self.prop[key].get_object_type() + 'Function'
            c.append(getattr(self, func)(key))
            c.append('')
        return c


    def writeGetObjectFunction(self,name):
        sig = 'get' + self.name.capitalize() + name.capitalize()
        totalSig = self.baseSpaces + self.full_func_signature(sig,'this',name)
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('in'))
        c.append(self.initializeDummy(self.prop[name]))
        c.append(self.baseSpaces + self.spaces[2] + name + ' = this%' + name)
        c.append(self.baseSpaces + self.end_function())
        return c

    def writeGetPointerFunction(self,name):
        sig = 'getPoly' + self.prop[name].get_class().capitalize()
        totalSig = self.baseSpaces + self.full_func_signature(sig,'this',name)
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('in'))
        c.append(self.initializeDummy(self.prop[name]))
        c.append(self.baseSpaces + self.spaces[2] + name + ' = this%' + self.ptr + '_' + name)
        c.append(self.baseSpaces + self.end_function() )
        return c


    def writeGetPrimitiveFunction(self,name):
        sig = 'get' + self.name.capitalize() + name.capitalize()
        totalSig = self.baseSpaces + self.full_func_signature(sig,'this',name)
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('in'))
        c.append(self.initializeDummy(self.prop[name]))
        c.append(self.baseSpaces + self.spaces[2] + name + ' = this%' + name)
        c.append(self.baseSpaces + self.end_function() )
        return c

    def writeGetParameterFunction(self,name):
        c = []
        c = self.writeGetPrimitiveFunction(name)
        return c


    def callGetObject(self,object_):
        return self.baseSpaces + self.spaces[2] + object_.get_name() + ' = get' + self.name.capitalize() + object_.get_name().capitalize() + '(this)'


    def callGetPointer(self,object_):
        c = []
        c.append(self.baseSpaces + self.spaces[2] + "if (associated(this%ptr_" + object_.get_name() + ")) then")
        c.append(self.baseSpaces + self.spaces[4] + object_.get_name() + ' = getPoly' + object_.get_class().capitalize() + '(this)' )
#            self.baseSpaces + self.spaces[2] + "else" +
#            self.baseSpaces + self.spaces[4] + 'call SetPoly' + ucfirst(object.get_class()) + '(this,' + object.get_name() + ')'  +
#            self.baseSpaces + self.spaces[4] + object.get_name() + ' = setPoly' + ucfirst(object.get_class()) + '(this)'  +
        c.append(self.baseSpaces + self.spaces[2] + "endif")
        return c


    def callGetPrimitive(self,object_):
        return self.baseSpaces + self.spaces[2] + object_.get_name() + ' = get' + self.name.capitalize() + object_.get_name().capitalize() + '(this)'

    def callGetParameter(self,object_):
        return self.baseSpaces + self.spaces[2] + object_.get_name() + ' = get' + self.name.capitalize() + object_.get_name().capitalize() + '(this)'



    def getAll(self):
        sig = 'get' + self.name.capitalize()
        c = []
        self.setTotalArgObjects()
        self.setTotalArgList()
        argList = 'this,' + ','.join(self.argList)
        # removing this requirement allows creating an object with no inputs!
        # if (!(count(self.argList) < 1)):
        totalSig = self.baseSpaces + self.full_sub_signature(sig,argList)
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.baseSpaces + self.spaces[2] + 'implicit none' )
        c.append(self.baseSpaces + self.spaces[2] + 'type(' + self.name + '),intent(in) :: this' )

        # If calculated objects need to be initialized, then this line below must be changed to:
        #             foreach (self.prop as name => object):
        for key in self.argObjects:
            if not self.argObjects[key].get_object_type().lower() == 'Parameter'.lower():
                c.append(self.init_prop(self.argObjects[key],'out') )
            else:
                c.append(self.initializeDummy(self.argObjects[key]) )


        for key in self.argObjects:
            if self.argObjects[key].get_object_type().lower() == 'Parameter'.lower():
                func = 'callGetPrimitive'
            else:
                func = 'callGet' + self.argObjects[key].get_object_type()
            c.append(getattr(self, func)(self.argObjects[key]))
        c.append(self.baseSpaces + self.end_sub())
        return c

    def setTotalArgObjects(self):
        for key in self.prop:
            self.argObjects[key] = self.prop[key]

    def getTotalArgObjects(self):
        return self.argObjects

    def setTotalArgList(self):
        argList = []
        for key in self.prop:
            argList.append(key)
        self.argList = argList

    def getTotalArgList(self):
        return self.argList

    ###PRINT####################################################################*/
    def writePrintFunction(self):
        c = []
        for key in self.prop:
            if (self.prop[key].getPrint()):
                if self.prop[key].get_object_type().lower() == 'Parameter'.lower():
                    func = 'writePrint' + 'Primitive' + 'Function'
                else:
                    func = 'writePrint' + self.prop[key].get_object_type() + 'Function'
                c.append(getattr(self, func)(key))
                c.append('')
        return c


    def writePrintObjectFunction(self,name):
        sig = 'print' + self.name.capitalize() + name.capitalize()
        totalSig = self.baseSpaces + self.full_sub_signature(sig,'this')
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('in'))
        c.append(self.baseSpaces + self.spaces[2] + 'type(' + self.prop[name].get_class() + ') :: ' + name)
        c.append(self.baseSpaces + self.spaces[2] + 'call print' + self.prop[name].get_class().capitalize() + '(this%' + name + ')')
        c.append(self.baseSpaces + self.end_sub() )
        return c


    def writePrintPointerFunction(self,name):
        sig = 'print' + self.name.capitalize() + name.capitalize()
        totalSig = self.baseSpaces + self.full_sub_signature(sig,'this')

        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('in'))
        c.append(self.baseSpaces + self.spaces[2] + 'call print' + self.prop[name].get_class().capitalize() + '(this%ptr_' + name + ')')
        c.append(self.baseSpaces + self.end_sub() )
        return c


    def writePrintPrimitiveFunction(self,name):
        sig = 'print' + self.name.capitalize() + name.capitalize()
        totalSig = self.baseSpaces + self.full_sub_signature(sig,'this')

        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('in'))
        c.append(self.baseSpaces + self.spaces[2] + self.prop[name].get_class() +  ' :: ' + name)
        c.append(self.baseSpaces + self.spaces[2] + 'write(*,*) "' + name + ': ", this%' + name)
        c.append(self.baseSpaces + self.end_sub() )
        return c


    def printAll(self):
        sig = 'print' + self.name.capitalize()
        totalSig = self.baseSpaces + self.full_sub_signature(sig,'this')
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append( self.baseSpaces + self.spaces[2] + 'implicit none')
        c.append( self.baseSpaces + self.spaces[2] + 'type(' + self.name + '), intent(in) :: this' )

        # add an "avoid this line" if no properties exist to be printed
        c.append(self.baseSpaces + self.spaces[2] + "write(*,*) 'Printed data for " + self.name + "'")
        c.append(self.baseSpaces + self.spaces[2] + "write(*,*) '" + self.stars[15] +"'")
        #  *******ONE OF TYPES**************'"
        for key in self.prop:
            if self.prop[key].get_object_type().lower() == 'Parameter'.lower():
                func = 'writeCallPrint' + 'Primitive'
            else:
                func = 'writeCallPrint' + self.prop[key].get_object_type()
            c.append(getattr(self, func)(self.prop[key]))

        c.append(self.baseSpaces + self.spaces[2] + "write(*,*) '" + self.stars[15] + "'")
        c.append(self.baseSpaces + self.spaces[2] + "write(*,*) ''")
        c.append(self.baseSpaces + self.end_sub() )
        return c


    def writeCallPrintObject(self,object_):
        return self.baseSpaces + self.spaces[2] + "call print" +  object_.get_class().capitalize() + "(this%" + object_.get_name() + ")"
    def writeCallPrintPointer(self,object_):
        c = []
        c.append(self.baseSpaces + self.spaces[2] + "if (associated(this%ptr_" + object_.get_name() + ")) then")
        c.append(self.baseSpaces + self.spaces[4] + "call print" +  object_.get_class().capitalize() + object_.get_name() + "(this%ptr_" + object_.get_name() + ")")
        c.append(self.baseSpaces + self.spaces[2] + "endif")
        return c
    def writeCallPrintPrimitive(self,object_):
        return self.baseSpaces + self.spaces[2] + "call print" + self.get_name().capitalize() + object_.get_name().capitalize() + "(this)"


    def initializeObject(self,object_):
        return self.baseSpaces + self.spaces[2] + "type(" + object_.get_class() + ") :: " + object_.get_name()


    def initializePointer(self,object_):
        return self.baseSpaces + self.spaces[2] + "type(" + object_.get_class() + ") :: " + object_.get_name()


    def initializePrimitive(self,object_):
        return self.baseSpaces + self.spaces[2] + object_.get_class() +  " :: " + object_.get_name()


    ### WRITE TO FILE ####################################################################*/
    def makeWriteFunction(self):
        c = []
        for key in self.prop:
            if (self.prop[key].getPrint()):
                if self.prop[key].get_object_type().lower() == 'Parameter'.lower():
                    func = 'makeWrite' + 'Primitive' + 'Function'
                else:
                    func = 'makeWrite' + self.prop[key].get_object_type() + 'Function'
                c.append(getattr(self, func)(key))
                c.append('')
        return c


    def makeWriteObjectFunction(self,name):
        sig = 'write' + self.name.capitalize() + name.capitalize()
        totalSig = self.baseSpaces + self.full_sub_signature(sig,'this,dir')
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('in'))
        c.append(self.baseSpaces + self.spaces[2] + 'character(len=*),intent(in) :: dir')
        c.append(self.baseSpaces + self.spaces[2] + 'type(' + self.prop[name].get_class() + ') :: ' + name)
        c.append(self.baseSpaces + self.spaces[2] + 'call write' + self.prop[name].get_class().capitalize() + '(this%' + name + ',dir)')
        c.append(self.baseSpaces + self.end_sub() )
        return c


    def makeWritePointerFunction(self,name):
        sig = 'write' + self.name.capitalize() + name.capitalize()
        totalSig = self.baseSpaces + self.full_sub_signature(sig,'this,dir')

        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('in'))
        c.append(self.baseSpaces + self.spaces[2] + 'character(len=*),intent(in) :: dir')
        c.append(self.baseSpaces + self.spaces[2] + 'call write' + self.prop[name].get_class().capitalize() + '(this%ptr_' + name + ',dir)')
        c.append(self.baseSpaces + self.end_sub() )
        return c


    def makeWritePrimitiveFunction(self,name):
        sig = 'write' + self.name.capitalize() + name.capitalize()
        totalSig = self.baseSpaces + self.full_sub_signature(sig,'this,dir,parentUnit')

        c = []
        c.append(totalSig)
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('in'))
        c.append(self.baseSpaces + self.spaces[2] + self.prop[name].get_class() +  ' :: ' + name)
        c.append(self.baseSpaces + self.spaces[2] + 'character(len=*),intent(in) :: dir')
        c.append(self.baseSpaces + self.spaces[2] + 'integer,optional :: parentUnit')
        c.append(self.baseSpaces + self.spaces[2] + 'integer :: NewU')

        c.append(self.baseSpaces + self.spaces[2] + 'if (present(parentUnit)) then')
        c.append(self.baseSpaces + self.spaces[4] + 'NewU = parentUnit')
        c.append(self.baseSpaces + self.spaces[4] + 'write(NewU,*) "' + name + ': ", this%' + name)
        c.append(self.baseSpaces + self.spaces[2] + 'else')
        c.append(self.baseSpaces + self.spaces[4] + 'call newAndOpen(dir,"' + name + '")')
        c.append(self.baseSpaces + self.spaces[4] + 'write(NewU,*) "' + name + ': ", this%' + name)
        c.append(self.baseSpaces + self.spaces[4] + 'call closeAndMessage(NewU,"' + name + '")')
        c.append(self.baseSpaces + self.spaces[2] + 'endif')

        c.append(self.baseSpaces + self.end_sub() )
        return c


    def makeWriteAll(self):
        sig = 'write' + self.name.capitalize()
        totalSig = self.baseSpaces + self.full_sub_signature(sig,'this,dir,parentUnit')
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.baseSpaces + self.spaces[2] + 'implicit none')
        c.append(self.baseSpaces + self.spaces[2] + 'type(' + self.name + '), intent(in) :: this' )
        c.append(self.baseSpaces + self.spaces[2] + 'character(len=*),intent(in) :: dir')

        c.append(self.baseSpaces + self.spaces[2] + 'integer,optional :: parentUnit')
        c.append(self.baseSpaces + self.spaces[2] + 'integer :: NewU')
        c.append(self.baseSpaces + self.spaces[2] + 'if (present(parentUnit)) then')
        c.append(self.baseSpaces + self.spaces[4] + 'NewU = parentUnit')
        c.append(self.baseSpaces + self.spaces[2] + 'else')
        c.append(self.baseSpaces + self.spaces[4] + 'NewU = newUnit()')
        c.append(self.baseSpaces + self.spaces[4] + 'open(NewU,file=trim(dir) //"' + self.name.capitalize() + '.txt")')
        c.append(self.baseSpaces + self.spaces[2] + 'endif')

        # add an "avoid this line" if no properties exist to be printed
        #  *******ONE OF TYPES**************'"
        for key in self.prop:
            if self.prop[key].get_object_type().lower() == 'Parameter'.lower():
                func = 'makeCallToWrite' + 'Primitive'
            else:
                func = 'makeCallToWrite' + self.prop[key].get_object_type()
            c.append(getattr(self, func)(self.prop[key]))

        c.append(self.baseSpaces + self.spaces[2] + 'if (.not. present(parentUnit)) then')
        c.append(self.baseSpaces + self.spaces[4] + 'close(NewU)')
        c.append(self.baseSpaces + self.spaces[4] + "write(*,*) '+++ Data for " + self.name + " written to file +++'")
        c.append(self.baseSpaces + self.spaces[2] + 'endif')

        c.append(self.baseSpaces + self.end_sub() )
        return c


    def makeCallToWriteObject(self,object_):
        return self.baseSpaces + self.spaces[2] + "call write" +  object_.get_class().capitalize() + "(this%" + object_.get_name() + ",dir,NewU)"


    def makeCallToWritePointer(self,object_):
        c = []
        c.append(self.baseSpaces + self.spaces[2] + "if (associated(this%ptr_" + object_.get_name() + ")) then")
        c.append(self.baseSpaces + self.spaces[4] + "call write" +  object_.get_class().capitalize() + "(this%ptr_" + object_.get_name() + ",dir,NewU)")
        c.append(self.baseSpaces + self.spaces[2] + "endif")
        return c


    def makeCallToWritePrimitive(self,object_):
        return self.baseSpaces + self.spaces[2] + "call write" + self.get_name().capitalize() + object_.get_name().capitalize() + "(this,dir,NewU)"

    def conditionalWrite(self):
        totalSig = self.baseSpaces + 'subroutine conditionalWrite' + self.name.capitalize() + '(u,var)'

        c = []
        c.append(totalSig)
        c.append(self.funcImplicitNone())
        c.append(self.baseSpaces + self.spaces[2] + 'if (present(parentUnit)) then')
        c.append(self.baseSpaces + self.spaces[4] + 'NewU = parentUnit')
        c.append(self.baseSpaces + self.spaces[4] + 'write(NewU,*) "' + name + ': ", this%' + name)
        c.append(self.baseSpaces + self.spaces[2] + 'else')
        c.append(self.baseSpaces + self.spaces[4] + 'call newAndOpen(dir,"' + name + '"")')
        c.append(self.baseSpaces + self.spaces[4] + 'write(NewU,*) "' + name + ': ", this%' + name)
        c.append(self.baseSpaces + self.spaces[4] + 'call closeAndMessage(NewU,"' + name + '"")')
        c.append(self.baseSpaces + self.spaces[2] + 'endif')
        c.append(self.baseSpaces + self.end_function() )
        return c

