class fortran_property:
  # Additional routines to consider:
  # compare
  # "" many routines for variable size
  # WARNING: insists copy-able. if (size(obj%a).lt.1) stop 'Error: object allocated but size<1 in init_copy in object.f90'
  # Add available preprocessor directives: ! Pre-processor directives: (_DEBUG_MESH_)

  def __init__(self):
    self.name = 'default_name'
    self.class_ = 'default_class'
    self.privacy = 'default_privacy'
    self.object_type = 'default_object_type'
    self.default_value = '0'
    self.spaces = []

  def set_do_loop_iter(self,do_loop_iter):
    self.do_loop_iter = do_loop_iter;
    self.do_loop_start = ['do '+self.do_loop_iter+'=1,size(this%'+self.name+ ')']

  def set_spaces(self,spaces): self.spaces = spaces;

  def set_name(self,name): self.name = name.lower();
  def get_name(self): return self.name;
  def set_default_value(self,default_value): self.default_value = default_value;
  def get_default_value(self): return self.default_value;
  def set_class(self,class_): self.class_ = class_.lower();
  def get_class(self): return self.class_;
  def set_privacy(self,privacy): self.privacy = privacy.lower();
  def get_privacy(self): return self.privacy;
  def set_object_type(self,object_type): self.object_type = object_type;
  def get_object_type(self): return self.object_type;

  def write_class_definition(self):
    if self.object_type=='primitive':
      return [self.class_+self.sig+' :: '+self.name + self.assign_default_value]
    elif self.object_type=='object':
      return ['type(' + self.class_ + ')'+self.sig+' :: '+self.name]

  def write_display(self):
    if self.object_type=='primitive':
      return ["write(un,*) '" +self.name+ " = ',this%" + self.name]
    elif self.object_type=='object':
      return ['call display' +  '(this%' + self.name + ',un)']

  def write_export(self):
    if self.object_type=='primitive':
      return ['write(un,*) this%'  +self.name]
    elif self.object_type=='object':
      return ['call export(this%' + self.name + ',un)']

  def write_import(self):
    if self.object_type=='primitive':
      return ['read(un,*) this%'  +self.name]
    elif self.object_type=='object':
      return ['call import(this%' + self.name + ',un)']

  def write_init_copy(self):
    if self.allocatable:
      L =     ['if (allocated(that%'+self.name+')) then']
      L = L + [self.spaces[2]+'allocate(this%'+self.name+'(size(that%'+self.name+')))']
    if self.allocatable and not self.object_type=='primitive':
      L = L + [self.spaces[2]+x for x in self.do_loop_start]
    if self.object_type=='primitive' and self.allocatable:
      L = L + [self.spaces[2]+'this%'+self.name+' = that%' + self.name]
    elif self.object_type=='primitive' and not self.allocatable:
      L = ['this%'+self.name+' = that%' + self.name]
    elif self.object_type=='object' and self.allocatable:
      L = L + [self.spaces[4]+'call init(this%'+self.name+'('+self.do_loop_iter+'),that%' + self.name + '('+self.do_loop_iter+'))']
    elif self.object_type=='object' and not self.allocatable:
      L = ['call init(this%'+self.name+',that%' + self.name + ')']
    if self.allocatable and not self.object_type=='primitive':
      L = L + [self.spaces[2]+'enddo']
    if self.allocatable:
      L = L + ['endif']
    return L

  def write_delete(self):
    if self.allocatable:
      L = ['if (allocated(this%'+self.name+')) then']
    if self.allocatable and not self.object_type=='primitive':
      L = L+[self.spaces[2]+x for x in self.do_loop_start]
    if self.object_type=='primitive' and self.allocatable:
      L = L + [self.spaces[2]+'this%'+self.name+' = ' + self.default_value]
    elif self.object_type=='primitive' and not self.allocatable:
      L = ['this%'+self.name+' = ' + self.default_value]
    elif self.object_type=='object' and self.allocatable:
      L = L+ [self.spaces[4]+'call delete(this%'+self.name+'('+self.do_loop_iter+'))']
    elif self.object_type=='object' and not self.allocatable:
      L = ['call delete(this%'+self.name+')']
    if self.allocatable and not self.object_type=='primitive':
      L = L + [self.spaces[2]+'enddo']
    if self.allocatable:
      L = L+[self.spaces[2]+'deallocate(this%'+self.name+')']
      L = L+['endif']
    return L

  def set_default_primitives(self):
    if self.object_type=='primitive' and self.class_=='integer':
      self.default_value = '0'
    if self.object_type=='primitive' and self.class_=='logical':
      self.default_value = '.false.'

  def print(self):
    print('--------------------------------- property')
    print('name        = '+str(self.name))
    print('class_      = '+str(self.class_))
    print('privacy     = '+str(self.privacy))
    print('object_type = '+str(self.object_type))
    print('---------------------------------')
    return

def init_all(name,class_,privacy,object_type,allocatable,rank,dimension,default_value):
  prop = fortran_property()
  prop.name = name.lower()
  prop.class_ = class_.lower()
  prop.privacy = privacy.lower()
  prop.object_type = object_type.lower()
  prop.dimension = dimension
  prop.dimension_s = dimension
  prop.rank = rank
  prop.allocatable = allocatable
  prop.default_value = default_value
  prop.set_default_primitives()

  if rank>1 and dimension<=1 and not allocatable: raise ValueError('rank>1 and dimension<=1')
  if allocatable and dimension<=1: raise ValueError('allocatable and dimension<=1')

  prop.rank_s = (rank*':,')[:-1]

  if allocatable and dimension>1: prop.dimension_s = prop.rank_s
  else: prop.dimension_s = str(dimension)

  if dimension>1 and allocatable:
    prop.sig = ',dimension('+prop.rank_s+'),allocatable'
    prop.assign_default_value = ''
  elif dimension>1 and not allocatable:
    prop.sig = ',dimension('+str(dimension)+')'
    prop.assign_default_value = ' = '+prop.default_value
  else:
    prop.assign_default_value = ' = '+prop.default_value
    prop.sig = ''

  return prop

