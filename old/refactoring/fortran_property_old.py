class fortran_property:
  def __init__(self):
    self.setDefault();

  def setDefault(self):
    self.delete();
    self.set_init_copy(True);
    self.set_getter(True);
    self.set_print(True);
    self.setParam(False);

  def delete(self):
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
        self.object_type
        del (self.object_type);
    except: pass

  def set_all_false(self):
    self.set_init_copy(False);
    self.set_getter(False);
    self.set_print(False);

  def set_name(self,name): self.setDefault();self.name = name;
  def set_privacy(self,privacy): self.privacy = privacy;
  def set_class(self,class_): self.class_ = class_;
  def set_object_type(self,object_type): self.object_type = object_type;
  def set_dependent_props(self,dependentProps): self.dependentProps = dependentProps;

  def set_init_copy(self,set_): self.set_ = set_
  def set_getter(self,get): self.get = get;
  def set_print(self,print_): self.print_ = print_;

  def setParam(self,param):
    self.param = param;
    if (param):
      self.set_all_false();


  def get_name(self): return self.name;
  def get_privacy(self): return self.privacy;
  def get_class(self): return self.class_;
  def get_object_type(self): return self.object_type;
  def get_dependent_props(self):
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
