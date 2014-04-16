import os
import myFuncs as func
from collections import OrderedDict

# DOUBLE CHECK WHEREVER CALL SET FUNCTIONS ARE USED THAT THE ISSET(SETSET) OR SETSET == True
# AND DO THE SAME WITH PRINT FUNCTIONS SO THAT THESE CAN BE TURNED OFF IF DESIRED+

class fortranClass: 
    nFiles = 0 # number of input files

    def __init__(self): 
        fortranClass.nFiles += 1
        # From ref: http://en.wikibooks.org/wiki/Fortran/Beginning_Fortran
        # The maximum number of characters per line is 72. So, conservatively:
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
    
    
    def getClassFile(self,props,classes): 
        c = []
        self.setProp(props)
        self.setListOfAllClasses(classes)
        c.append(self.writeOpenModule(self.getName() + '_mod'))
        c.append(self.writeUseModule())

        c.append(self.defineValueGivenProps())
        c.append('')
        c.append(self.openClassDefinition())
        c.append(self.writeClassDefinition())
        c.append(self.closeClassDefinition())
        c.append('')
        c.append(self.writeContains())
        c.append(self.writeAllFunctions())
        c.append(self.writeCloseModule(self.getName() + '_mod'))
        
        # Break lines to maximum number of characters
        l = func.flatten(c)
        s = []
        for k in l:
            s.append(self.breakLine(k,[]))
        return s
    
    
    def getCalcFile(self,prop,classes): 
        c = []
        self.setProp(prop)
        self.setListOfAllClasses(classes)
        c.append(self.writeCalcFunction(prop))
        return c
    

    def getParamFile(self): 
        c = []
        c.append(self.writeOpenModule(self.getName()))
        c.append(self.writeCloseModule(self.getName()))
        return c
    

    ################################################################################*/
    
    def getContents(self): 
        return self.contents
    
    def getCalcContents(self):  return self.calcContents
    def clearContents(self): 
        del (self.contents)
    
    def clearClass(self): 
        del (self.prop)
        del (self.argObjects)
        del (self.argList)
    
    def getName(self): 
        return self.name
    
    def setName(self,name): 
        self.name = name
        self.setHelper(False)
    
    def setProp(self,prop): 
        self.prop = prop
    
    def setPrivacy(self,privacy): 
        self.privacy = privacy
    
    def getPrivacy(self): 
        return self.privacy

    def setListOfAllClasses(self,AllClasses): 
        self.allClasses = AllClasses
    
    
    def setUsedModules(self,usedModules): 
        self.usedModules = usedModules
    

    def setHelper(self,helper): 
        self.helper = helper
    

    def getHelper(self): return self.helper

    ###CONFIG####################################################################*/
    
    def writeOpenModule(self,name): 
        return self.baseSpaces + 'module ' + name 
    
    def writeUseModule(self): 
        dependentNames = OrderedDict()
        c = []
        for i in range(len(self.usedModules)): 
            if not self.usedModules[i] == '':     
                c.append( self.baseSpaces + 'use ' + self.usedModules[i])
        
        for name, object_ in self.prop.iteritems():
            if object_.getClass() in self.allClasses:
                if not any(object_.getClass() in v for v in dependentNames.values()):
                    c.append(self.baseSpaces + 'use ' + object_.getClass() + '_mod')
                    dependentNames[name] = object_.getClass()
        
        c.append('')
        return c
    
    def writeCloseModule(self,name): 
        return  self.baseSpaces + 'end module ' + name
    
    def writeContains(self): 
        return self.baseSpaces + 'contains' 
    
    
    def initializeProp(self,prop,intent):
        try:
            if (prop.getObjectType().lower() == 'Primitive'.lower()): 
                return self.baseSpaces + self.spaces[2] + prop.getClass() + ', intent(' + intent + ') :: ' + prop.getName()
            elif (prop.getObjectType().lower() == 'Parameter'.lower()): 
                return self.baseSpaces + self.spaces[2] + prop.getClass() + ', intent(' + intent + ') :: ' + prop.getName()
            elif(prop.getObjectType().lower() == 'Object'.lower()): 
                return self.baseSpaces + self.spaces[2] + 'type(' + prop.getClass() + '), intent(' + intent + ') :: ' + prop.getName()
            elif(prop.getObjectType().lower() == 'Pointer'.lower()): 
                return self.baseSpaces + self.spaces[2] + 'type(' + prop.getClass() + '), intent(' + intent + '),optional,target :: ' + prop.getName()
        except:
            print 'No correct property types were chosen in Property'
        
    
    

    def initializeDummy(self,prop): 
        try:
            if (prop.getObjectType().lower() == 'Primitive'.lower()): 
                return self.baseSpaces + self.spaces[2] + prop.getClass() + ' :: ' + prop.getName()
            elif (prop.getObjectType().lower() == 'Parameter'.lower()): 
                return self.baseSpaces + self.spaces[2] + prop.getClass() + ' :: ' + prop.getName()
            elif (prop.getObjectType().lower() == 'Pointer'.lower()): 
                return self.baseSpaces + self.spaces[2] + 'type(' + prop.getClass() + '),target :: ' + prop.getName()
            elif (prop.getObjectType().lower() == 'Object'.lower()): 
                return self.baseSpaces + self.spaces[2] + 'type(' + prop.getClass() + ') :: ' + prop.getName()
        except:
            print 'No correct property types were chosen in Dummy'
        
    
    
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
    def writeClassDefinition(self): 
        c = []
        for name, object_ in self.prop.iteritems():
            name = object_.getName()
            func = 'write' + object_.getObjectType() + 'ClassDefinition'
            c.append(getattr(self, func)(name))
        return c
    

    def defineValueGivenProps(self): 
        c = []
        for name, object_ in self.prop.iteritems():
            name = object_.getName()
            if object_.getObjectType().lower() == 'Parameter'.lower():
                value = object_.getValue()
                try:
                    if object_.getPrivacy().lower() == 'public'.lower():
                        c.append(self.baseSpaces + object_.getClass() + ',parameter :: ' + name + ' = ' + value)
                    else:
                        c.append(self.baseSpaces + object_.getClass() + ',private,parameter :: ' + name + ' = ' + value)
                except:
                    c.append(self.baseSpaces + object_.getClass() + ',private,parameter :: ' + name + ' = ' + value)
            elif object_.getObjectType().lower() == 'Primitive'.lower():
                try:
                    value = object_.getValue()
                    try:
                        if object_.getPrivacy().lower() == 'public'.lower():
                            c.append(self.baseSpaces + object_.getClass() + ' :: ' + name + ' = ' + value)
                        else:
                            c.append(self.baseSpaces + object_.getClass() + ',private :: ' + name + ' = ' + value)
                    except:
                            c.append(self.baseSpaces + object_.getClass() + ',private :: ' + name + ' = ' + value)

                except:
                    pass
        return c

    def openClassDefinition(self): 
        c = []
        c.append(self.baseSpaces + 'type ' + self.name)
        try:
            if not self.getPrivacy().lower() == 'public'.lower():
                c.append(self.baseSpaces + self.spaces[2] + 'private')
        except:
            c.append(self.baseSpaces + self.spaces[2] + 'private')
        return c
    

    def writeObjectClassDefinition(self,name): 
        return self.baseSpaces + self.spaces[2] + 'type(' + self.prop[name].getClass() + ') :: ' + self.prop[name].getName()
    

    def writePointerClassDefinition(self,name): 
        return self.baseSpaces + self.spaces[2] + 'type(' + self.prop[name].getClass() + '), pointer :: ptr_' + self.prop[name].getName()
    

    def writePrimitiveClassDefinition(self,name): 
        return self.baseSpaces + self.spaces[2] + self.prop[name].getClass() + ' :: ' + self.prop[name].getName()

    def writeParameterClassDefinition(self,name): 
        return self.baseSpaces + self.spaces[2] + self.prop[name].getClass() + ' :: ' + self.prop[name].getName()
    

    def closeClassDefinition(self): 
        return self.baseSpaces + 'endtype'+self.baseSpaces
    

    ###WRITE ALL FUNCTIONS############################################################*/
    def writeAllFunctions(self): 
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
        c.append(self.writeHelperFunction())
        c.append('')
        return c
    

    def fullFunctionSignature(self,sig,args,result): 
        return 'function ' + sig + '(' + args + ') result(' + result + ')'

    def endFunction(self,function = False): 
            return 'end function'
    

    def fullSubroutineSignature(self,sig,args): 
        return 'subroutine ' + sig + '(' + args + ')'
    

    def endSubroutine(self,function = False): 
            return 'end subroutine'
    

    ###SET####################################################################*/
    def writeSetFunction(self): 
        c = []
        for name, object_ in self.prop.iteritems():
            if not object_.getObjectType().lower() == 'Parameter'.lower():
                if (object_.getSet()):
                    func = 'writeSet' + object_.getObjectType() + 'Function'
                c.append(getattr(self, func)(name))
                c.append('')
        return c
    

    def writeSetObjectFunction(self,name): 
        sig = 'set' + self.name.capitalize() + name.capitalize()
        totalSig = self.baseSpaces + self.fullSubroutineSignature(sig,'this,'+ name)
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('inout'))
        c.append(self.initializeProp(self.prop[name],'in') )
        c.append(self.baseSpaces + self.spaces[2] + 'this%' + name + ' = ' + name)
        c.append(self.baseSpaces + self.endSubroutine())
        return c
    

    def writeSetPointerFunction(self,name): 

        sig = 'setPoly' + self.prop[name].getClass().capitalize()
        totalSig = self.baseSpaces + self.fullSubroutineSignature(sig,'this,'+ name)
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('inout') )
        c.append(self.initializeProp(self.prop[name],'in') )
        c.append(self.baseSpaces + self.spaces[2] + 'this%' + self.ptr + '_' + name + ' => ' + name)
        c.append(self.nullifyOthers(self.prop,name))
        c.append(self.baseSpaces + self.endSubroutine())
        return c
    

    def writeSetPrimitiveFunction(self,name): 
        sig = 'set' + self.name.capitalize() + name.capitalize()
        totalSig = self.baseSpaces +self.fullSubroutineSignature(sig,'this,'+ name)
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('inout') )
        c.append(self.initializeProp(self.prop[name],'in') )
        c.append(self.baseSpaces + self.spaces[2] + 'this%' + name + ' = ' + name)
        c.append(self.baseSpaces + self.endSubroutine())
        return c
    

    def setAll(self): 
        sig = 'set' + self.name.capitalize()
        c = []
        self.setArgObjects()
        self.setArgList()
        argList = ','.join(self.argList)
        totalSig = self.baseSpaces + self.fullFunctionSignature(sig,argList,'this')
        c.append(totalSig)
        c.append(self.baseSpaces + self.spaces[2] + 'implicit none' )
        c.append(self.baseSpaces + self.spaces[2] + 'type(' + self.name + ') :: this' )
                
        for name, object_ in self.argObjects.iteritems():
            try:
                object_.getValue()
            except: 
                c.append(self.initializeProp(object_,'in'))
            
        for name, object_ in self.prop.iteritems():
            try:
                object_.getValue()
                c.append(self.baseSpaces + self.spaces[2] + 'this%' + name + ' = ' + name)
            except: pass

        for name, object_ in self.argObjects.iteritems():
            try:
                object_.getValue()
            except: 
                func = 'callSet' + object_.getObjectType()
                c.append(getattr(self, func)(object_))
        
                                            
        for name, object_ in self.prop.iteritems():
            if (object_.getAutoCalc()): 
                c.append(self.callCalcFunction(object_))
            
        c.append(self.baseSpaces + self.endFunction())
        
        return c
    


    def callSetObject(self,object_): 
        return self.baseSpaces + self.spaces[2] + 'call Set' + self.name.capitalize() + object_.getName().capitalize() + '(this,' + object_.getName() + ')' 
    

    def callSetPointer(self,object_): 
        c = []
        c.append(self.baseSpaces + self.spaces[2] + 'if (present(' + object_.getName() + ')) then' )
        c.append(self.baseSpaces + self.spaces[4] + 'call SetPoly' + object_.getClass().capitalize() + '(this,' + object_.getName() + ')' )
        c.append(self.nullifyOthers(self.prop,object_.getName()))
        c.append(self.baseSpaces + self.spaces[2] + 'endif' )
        return c
    
    
    def callSetPrimitive(self,object_): 
        return self.baseSpaces + self.spaces[2] + 'call Set' + self.name.capitalize() + object_.getName().capitalize() + '(this,' + object_.getName() + ')' 
    

    def setArgObjects(self):
        self.argObjects = OrderedDict()
        for name, object_ in self.prop.iteritems():
            if not object_.getCalc(): 
                try: object_.getValue()
                except: self.argObjects[name] = object_
    

    def getArgObjects(self): 
        return self.argObjects
    

    def setArgList(self): 
        argList = []
        for name, object_ in self.prop.iteritems():
            if not object_.getCalc():
                try: object_.getValue()
                except: argList.append(name)
            
        
        self.argList = argList
    

    def getArgList(self): 
        return self.argList
    

    def nullifyOthers(self,objectArray,name): 
        c = []
        for (nameAllOther, objectAllOther) in objectArray.iteritems():
            if not objectAllOther.getName() == name:
                if (objectAllOther.getObjectType().lower() == 'Pointer'.lower()): 
                    c.append(self.baseSpaces + self.spaces[2] + 'nullify(this%' + self.ptr + '_' + objectAllOther.getName() + ')' )
                
        return c
    

    ###GET####################################################################*/

    def writeGetFunction(self): 
        c = []
        for (name, object_) in self.prop.iteritems():
            if object_.getGet():
                func = 'writeGet' + object_.getObjectType() + 'Function'
            c.append(getattr(self, func)(name))
            c.append('')
        return c
    

    def writeGetObjectFunction(self,name): 
        sig = 'get' + self.name.capitalize() + name.capitalize()
        totalSig = self.baseSpaces + self.fullFunctionSignature(sig,'this',name)
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('in'))
        c.append(self.initializeDummy(self.prop[name]))
        c.append(self.baseSpaces + self.spaces[2] + name + ' = this%' + name)
        c.append(self.baseSpaces + self.endFunction())
        return c

    def writeGetPointerFunction(self,name): 
        sig = 'getPoly' + self.prop[name].getClass().capitalize()
        totalSig = self.baseSpaces + self.fullFunctionSignature(sig,'this',name)
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('in'))
        c.append(self.initializeDummy(self.prop[name]))
        c.append(self.baseSpaces + self.spaces[2] + name + ' = this%' + self.ptr + '_' + name)
        c.append(self.baseSpaces + self.endFunction() )
        return c
    

    def writeGetPrimitiveFunction(self,name): 
        sig = 'get' + self.name.capitalize() + name.capitalize()
        totalSig = self.baseSpaces + self.fullFunctionSignature(sig,'this',name)
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('in'))
        c.append(self.initializeDummy(self.prop[name]))
        c.append(self.baseSpaces + self.spaces[2] + name + ' = this%' + name)
        c.append(self.baseSpaces + self.endFunction() )
        return c

    def writeGetParameterFunction(self,name):
        c = []
        c = self.writeGetPrimitiveFunction(name)
        return c
    

    def callGetObject(self,object_): 
        return self.baseSpaces + self.spaces[2] + object_.getName() + ' = get' + self.name.capitalize() + object_.getName().capitalize() + '(this)' 
    

    def callGetPointer(self,object_):
        c = []
        c.append(self.baseSpaces + self.spaces[2] + "if (associated(this%ptr_" + object_.getName() + ")) then")
        c.append(self.baseSpaces + self.spaces[4] + object_.getName() + ' = getPoly' + object_.getClass().capitalize() + '(this)' )
#            self.baseSpaces + self.spaces[2] + "else" +
#            self.baseSpaces + self.spaces[4] + 'call SetPoly' + ucfirst(object.getClass()) + '(this,' + object.getName() + ')'  + 
#            self.baseSpaces + self.spaces[4] + object.getName() + ' = setPoly' + ucfirst(object.getClass()) + '(this)'  +
        c.append(self.baseSpaces + self.spaces[2] + "endif")
        return c
    

    def callGetPrimitive(self,object_): 
        return self.baseSpaces + self.spaces[2] + object_.getName() + ' = get' + self.name.capitalize() + object_.getName().capitalize() + '(this)' 
    
    def callGetParameter(self,object_): 
        return self.baseSpaces + self.spaces[2] + object_.getName() + ' = get' + self.name.capitalize() + object_.getName().capitalize() + '(this)' 
    
    

    def getAll(self): 
        sig = 'get' + self.name.capitalize()
        c = []
        self.setTotalArgObjects()
        self.setTotalArgList()
        argList = 'this,' + ','.join(self.argList)
        # removing this requirement allows creating an object with no inputs!
        # if (!(count(self.argList) < 1)):  
        totalSig = self.baseSpaces + self.fullSubroutineSignature(sig,argList)
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.baseSpaces + self.spaces[2] + 'implicit none' )
        c.append(self.baseSpaces + self.spaces[2] + 'type(' + self.name + '),intent(in) :: this' )
                
        # If calculated objects need to be initialized, then this line below must be changed to:
        #             foreach (self.prop as name => object): 
        for (name, object_) in self.argObjects.iteritems():
            if not object_.getObjectType().lower() == 'Parameter'.lower():
                c.append(self.initializeProp(object_,'out') )
            else:
                c.append(self.initializeDummy(object_) )
            

        for (name, object_) in self.argObjects.iteritems():
            if object_.getObjectType().lower() == 'Parameter'.lower():
                func = 'callGetPrimitive'
            else:
                func = 'callGet' + object_.getObjectType()
            c.append(getattr(self, func)(object_))
        c.append(self.baseSpaces + self.endSubroutine())
        return c
    


    def setTotalArgObjects(self): 
        for (name, object_) in self.prop.iteritems():
            self.argObjects[name] = object_
        
    

    def getTotalArgObjects(self): 
        return self.argObjects
    

    def setTotalArgList(self): 
        argList = []
        for (name, object_) in self.prop.iteritems():
            argList.append(name)
        
        self.argList = argList
    

    def getTotalArgList(self): 
        return self.argList
    


    ###CALC####################################################################*/

    def writeCalcFunction(self,prop): 
        sig = 'Calc' + self.name.capitalize() + prop.getName().capitalize()
        if (prop.isDependentPropsSet()): 
            self.dependentProps = self.getDependentProps(prop)
            self.dependentArgs = self.getDependentArgs(prop)
        else: 
            self.dependentArgs = []
        
        self.argList = ','.join(self.dependentArgs)
        totalSig = self.baseSpaces + self.fullSubroutineSignature(sig,'this')
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('inout'))
        
        if (prop.isDependentPropsSet()): 
            for (dependentPropName, object_) in self.dependentProps.iteritems():
                # func = 'initialize' + object.getObjectType()
                # c.append(self.func(object)
                c.append(self.initializeDummy(object_) )
            
        
        if (prop.isDependentPropsSet()): 
            for (dependentPropName, object_) in self.dependentProps.iteritems():
                # func = 'initialize' + object.getObjectType()
                # c.append(self.func(object)
                func = 'callGet' + object_.getObjectType()
                c.append(getattr(self, func)(object_))
            
        c.append(self.baseSpaces + self.spaces[2] + 'this%' + prop.getName() + ' = ')
        c.append(self.baseSpaces + self.endSubroutine())
        c.append('')
        return c
    
    
    def getDependentProps(self,prop): 
        tempDependentProps = prop.getDependentProps()
        dependentProps = OrderedDict()
        for i in range(len(prop.getDependentProps())):
            dependentProps[tempDependentProps[i].getName()] = tempDependentProps[i]
   
        return dependentProps
    
    
    def getDependentArgs(self,prop): 
        dependentArgs = []
        dependentProps = self.getDependentProps(prop)
        for (dependentPropName, object_) in dependentProps.iteritems():
            dependentArgs.append(dependentPropName)
        
        return dependentArgs
    
    
    def callCalcFunction(self,prop): 
        c = []
        if (prop.isDependentPropsSet()):
            self.dependentProps = self.getDependentProps(prop)
            self.dependentArgs = self.getDependentArgs(prop)
        else: 
            self.dependentArgs = []
        
        self.argList = ',this%'.join(self.dependentArgs)
        c.append(self.baseSpaces + self.spaces[2] + 'call Calc' + self.name.capitalize() + prop.getName().capitalize() + '(this)')
        return c
    
    ###PRINT####################################################################*/
    def writePrintFunction(self): 
        c = []
        for (name, object_) in self.prop.iteritems():
            if (object_.getPrint()): 
                if object_.getObjectType().lower() == 'Parameter'.lower():
                    func = 'writePrint' + 'Primitive' + 'Function'
                else:
                    func = 'writePrint' + object_.getObjectType() + 'Function'
                c.append(getattr(self, func)(name))
                c.append('')
        return c
    

    def writePrintObjectFunction(self,name): 
        sig = 'print' + self.name.capitalize() + name.capitalize()
        totalSig = self.baseSpaces + self.fullSubroutineSignature(sig,'this')
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('in'))
        c.append(self.baseSpaces + self.spaces[2] + 'type(' + self.prop[name].getClass() + ') :: ' + name)
        c.append(self.baseSpaces + self.spaces[2] + 'call print' + self.prop[name].getClass().capitalize() + '(this%' + name + ')')
        c.append(self.baseSpaces + self.endSubroutine() )
        return c
    

    def writePrintPointerFunction(self,name): 
        sig = 'print' + self.name.capitalize() + name.capitalize()
        totalSig = self.baseSpaces + self.fullSubroutineSignature(sig,'this')

        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('in'))
        c.append(self.baseSpaces + self.spaces[2] + 'call print' + self.prop[name].getClass().capitalize() + '(this%ptr_' + name + ')')
        c.append(self.baseSpaces + self.endSubroutine() )
        return c
    

    def writePrintPrimitiveFunction(self,name): 
        sig = 'print' + self.name.capitalize() + name.capitalize()
        totalSig = self.baseSpaces + self.fullSubroutineSignature(sig,'this')

        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append(self.funcImplicitNone())
        c.append(self.initializeThis('in'))
        c.append(self.baseSpaces + self.spaces[2] + self.prop[name].getClass() +  ' :: ' + name)
        c.append(self.baseSpaces + self.spaces[2] + 'write(*,*) "' + name + ': ", this%' + name)
        c.append(self.baseSpaces + self.endSubroutine() )
        return c
    

    def printAll(self): 
        sig = 'print' + self.name.capitalize()
        totalSig = self.baseSpaces + self.fullSubroutineSignature(sig,'this')
        c = []
        c.append(self.breakLine(totalSig,[]))
        c.append( self.baseSpaces + self.spaces[2] + 'implicit none')
        c.append( self.baseSpaces + self.spaces[2] + 'type(' + self.name + '), intent(in) :: this' )

        # add an "avoid this line" if no properties exist to be printed
        c.append(self.baseSpaces + self.spaces[2] + "write(*,*) 'Printed data for " + self.name + "'")
        c.append(self.baseSpaces + self.spaces[2] + "write(*,*) '" + self.stars[15] +"'")
        #  *******ONE OF TYPES**************'"
        for (name, object_) in self.prop.iteritems():
            if object_.getObjectType().lower() == 'Parameter'.lower():
                func = 'writeCallTo' + 'Primitive'
            else:
                func = 'writeCallTo' + object_.getObjectType()
            c.append(getattr(self, func)(object_))
        
        c.append(self.baseSpaces + self.spaces[2] + "write(*,*) '" + self.stars[15] + "'")
        c.append(self.baseSpaces + self.spaces[2] + "write(*,*) ''")
        c.append(self.baseSpaces + self.endSubroutine() )
        return c
    
    
    def writeCallToObject(self,object_): 
        return self.baseSpaces + self.spaces[2] + "call print" +  object_.getClass().capitalize() + "(this%" + object_.getName() + ")"
    
    
    def writeCallToPointer(self,object_): 
        c = []
        c.append(self.baseSpaces + self.spaces[2] + "if (associated(this%ptr_" + object_.getName() + ")) then")
        c.append(self.baseSpaces + self.spaces[4] + "call print" +  object_.getClass().capitalize() + "(this%ptr_" + object_.getName() + ")")
        c.append(self.baseSpaces + self.spaces[2] + "endif")
        return c
    
    
    def writeCallToPrimitive(self,object_): 
        return self.baseSpaces + self.spaces[2] + "call print" + self.getName().capitalize() + object_.getName().capitalize() + "(this)"
    

    def initializeObject(self,object_): 
        return self.baseSpaces + self.spaces[2] + "type(" + object_.getClass() + ") :: " + object_.getName()
    

    def initializePointer(self,object_): 
        return self.baseSpaces + self.spaces[2] + "type(" + object_.getClass() + ") :: " + object_.getName()
    

    def initializePrimitive(self,object_): 
        return self.baseSpaces + self.spaces[2] + object_.getClass() +  " :: " + object_.getName()
    

    ###INCLUDE HELPER####################################################################*/
    # Is this needed for each poperty of a given class? or just the whole class?
    def writeHelperFunction(self): 
        c = []

        if (self.getHelper()): 
            c.append( self.baseSpaces + self.spaces[2] + 'include \'../helper/' + self.name + '_helper.f\'' )
        
        
        for (name, object_) in self.prop.iteritems():
            if (object_.getCalc()): 
                func = 'writeIncludeCalculate' + object_.getObjectType() + 'Function'
                c.append(getattr(self, func)(name))
        
        return c
    
    
    ###INCLUDE CALCULATE####################################################################*/
    
    def writeIncludeCalculateObjectFunction(self,name): 
        return  self.baseSpaces + self.spaces[2] + 'include \'../calc/calc_' + name + '.f\''
    
    
    def writeIncludeCalculatePointerFunction(self,name): 
        return  self.baseSpaces + self.spaces[2] + 'include \'../calc/calc_' + name + '.f\''
    
    
    def writeIncludeCalculatePrimitiveFunction(self,name): 
        return  self.baseSpaces + self.spaces[2] + 'include \'../calc/calc_' + name + '.f\''
    
    
    
