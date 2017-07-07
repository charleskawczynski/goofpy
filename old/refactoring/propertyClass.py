class propertyClass:
  def __init__(self):
    # Default Settings
    self.setDefault();

  def setDefault(self):
    self.unsetAll();
    self.setSet(True);
    self.setGet(True);
    self.setPrint(True);
    self.setCalc(False);
    self.setHelper(False);
    self.setAutoCalc(False);
    self.setParam(False);

  def unsetAll(self):
    try:
        self.dependentProps
        del (self.dependentProps);
    except: pass
    try:
        self.name
        del (self.name);
    except: pass
    try:
        self.value
        del (self.value);
    except: pass
    try:
        self.class_
        del (self.class_);
    except: pass
    try:
        self.objectType
        del (self.objectType);
    except: pass

  def setALLFalse(self):
    self.setSet(False);
    self.setGet(False);
    self.setPrint(False);
    self.setCalc(False);
    self.setHelper(False);
    self.setAutoCalc(False);

  def setName(self,name): self.setDefault();self.name = name;
  def setPrivacy(self,privacy): self.privacy = privacy;
  def setValue(self,value): self.value = value;
  def setClass(self,class_): self.class_ = class_;
  def setObjectType(self,objectType): self.objectType = objectType;
  def setDependentProps(self,dependentProps): self.dependentProps = dependentProps;

  def setSet(self,set_): self.set_ = set_
  def setGet(self,get): self.get = get;
  def setPrint(self,print_): self.print_ = print_;
  def setCalc(self,calc): self.calc = calc;
  def setHelper(self,helper): self.helper = helper;
  def setAutoCalc(self,autoCalc):
    self.autoCalc = autoCalc;
    if (autoCalc):
      self.setCalc(True);

  def setParam(self,param):
    self.param = param;
    if (param):
      self.setALLFalse();


  def getName(self): return self.name;
  def getPrivacy(self): return self.privacy;
  def getValue(self): return self.value;
  def getClass(self): return self.class_;
  def getObjectType(self): return self.objectType;
  def getDependentProps(self):
    try:
        return self.dependentProps
    except:
        return False


  def getSet(self): return self.set_;
  def getGet(self): return self.get;
  def getPrint(self): return self.print_;
  def getCalc(self): return self.calc;
  def getHelper(self): return self.helper;
  def getAutoCalc(self): return self.autoCalc;

  def getParam(self): return self.param;

  def isDependentPropsSet(self):
    try:
        self.dependentProps
        return True
    except:
        return False


  def isAutoCalcSet(self): return self.autoCalc is not None;

  def checkVariables(self):
            print('checking variables?')
