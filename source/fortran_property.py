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
    self.default_real = '0'
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

  def write_class_definition(self,all_private):
    if all_private: privacy_temp = ''
    else: privacy_temp = ','+self.privacy

    if self.object_type=='primitive':
      L = [self.class_+self.sig+privacy_temp+' :: '+self.name + self.assign_default_value]
    elif self.object_type=='object':
      L = ['type(' + self.class_ + ')'+self.sig+privacy_temp+' :: '+self.name]
    elif self.object_type=='procedure':
      L = ['procedure(' + self.class_ + '),pointer,nopass'+self.sig+privacy_temp+' :: '+self.name]
    return L

  def set_display_spaces(self,display_spaces): self.display_spaces = display_spaces

  def write_display(self):
    L = []
    if self.object_type=='primitive':
      L = ["write(un,*) '" +self.name+ self.display_spaces+ " = ',this%" + self.name]
    elif self.object_type=='procedure': pass
    elif self.object_type=='object':
      L = ['call display' +  '(this%' + self.name + ',un)']
    return L

  def write_export(self):
    L = []
    if self.object_type=='primitive':
      L = ['write(un,*) this%'  +self.name]
    elif self.object_type=='procedure': pass
    elif self.object_type=='object':
      L = ['call export(this%' + self.name + ',un)']
    return L

  def write_import(self):
    L = []
    if self.object_type=='primitive':
      L = ['read(un,*) this%'  +self.name]
    elif self.object_type=='procedure': pass
    elif self.object_type=='object':
      L = ['call import(this%' + self.name + ',un)']
    return L

  def write_init_copy(self):
    L = []
    if self.allocatable and self.rank>1:
      L = L + ['if (allocated(that%'+self.name+')) then']
      L = L + [self.spaces[2]+self.int_rank_shape]
      L = L + [self.spaces[2]+'allocate(this%'+self.name+'('+self.int_rank_list+'))']
    elif self.allocatable and not self.rank>1:
      L = L + ['if (allocated(that%'+self.name+')) then']
      L = L + [self.spaces[2]+'allocate(this%'+self.name+'(size(that%'+self.name+')))']
    if self.allocatable and not (self.object_type=='primitive' or self.object_type=='procedure'):
      L = L + [self.spaces[2]+x for x in self.do_loop_start]
    if (self.object_type=='primitive' or self.object_type=='procedure') and self.allocatable:
      L = L + [self.spaces[2]+'this%'+self.name+' = that%' + self.name]
    elif self.object_type=='primitive' and not self.allocatable:
      L = L + ['this%'+self.name+' = that%' + self.name]
    elif self.object_type=='procedure' and not self.allocatable:
      L = L + ['this%'+self.name+' => that%' + self.name]
    elif self.object_type=='object' and self.allocatable:
      L = L + [self.spaces[4]+'call init(this%'+self.name+'('+self.do_loop_iter+'),that%' + self.name + '('+self.do_loop_iter+'))']
    elif self.object_type=='object' and not self.allocatable:
      L = L + ['call init(this%'+self.name+',that%' + self.name + ')']
    if self.allocatable and not self.object_type=='primitive':
      L = L + [self.spaces[2]+'enddo']
    if self.allocatable:
      L = L + ['endif']
    return L

  def write_delete(self):
    L = []
    if self.allocatable:
      L = L + ['if (allocated(this%'+self.name+')) then']
    if self.allocatable and not (self.object_type=='primitive' or self.object_type=='procedure'):
      L = L + [self.spaces[2]+x for x in self.do_loop_start]
    if self.object_type=='primitive' and self.allocatable:
      L = L + [self.spaces[2]+'this%'+self.name+' = ' + self.default_value]
    if self.object_type=='procedure' and self.allocatable:
      L = L + [self.spaces[2]+'nullify(this%'+self.name+')']
    elif self.object_type=='primitive' and not self.allocatable:
      L = L + ['this%'+self.name+' = ' + self.default_value]
    elif self.object_type=='procedure' and not self.allocatable:
      L = L + [self.spaces[2]+'nullify(this%'+self.name+')']
    elif self.object_type=='object' and self.allocatable:
      L = L + [self.spaces[4]+'call delete(this%'+self.name+'('+self.do_loop_iter+'))']
    elif self.object_type=='object' and not self.allocatable:
      L = L + ['call delete(this%'+self.name+')']
    if self.allocatable and not self.object_type=='primitive':
      L = L + [self.spaces[2]+'enddo']
    if self.allocatable:
      L = L + [self.spaces[2]+'deallocate(this%'+self.name+')']
      L = L + ['endif']
    return L

  def set_default_primitives(self):
    primitive_list = ['integer','logical','character','real']
    if self.object_type=='primitive' and 'integer' in self.class_.lower():
      self.default_value = '0'
    if self.object_type=='primitive' and 'logical' in self.class_.lower():
      self.default_value = '.false.'
    if self.object_type=='primitive' and 'character' in self.class_.lower():
      self.default_value = "' '"
    if self.object_type=='primitive' and 'real' in self.class_.lower():
      self.default_value = self.default_real

  def set_default_real(self,default_real): self.default_real = default_real

  def print(self):
    print('--------------------------------- property')
    print('name        = '+str(self.name))
    print('class_      = '+str(self.class_))
    print('privacy     = '+str(self.privacy))
    print('object_type = '+str(self.object_type))
    print('---------------------------------')
    return

  def init_remaining(self,name,class_,privacy,allocatable = False,rank = 1,dimension = 1,procedure = False):
    self.name = name.lower()
    self.class_ = class_.lower()
    self.privacy = privacy.lower()

    self.dimension = dimension
    self.procedure = procedure
    self.dimension_s = dimension
    self.rank = rank
    self.name_length = len(name)
    self.allocatable = allocatable

    primitive_list = ['integer','logical','real','character']
    if any([x in class_  for x in primitive_list if not '_' in class_]):
      self.object_type = 'primitive'
    else:
      if procedure:
        self.object_type = 'procedure'
      else:
        self.object_type = 'object'
    self.set_default_primitives()

    if rank>1 and dimension<=1 and not allocatable: raise ValueError('rank>1 and dimension<=1')
    if allocatable and dimension<=1: raise ValueError('allocatable and dimension<=1')

    self.rank_deffered = (rank*':,')[:-1]
    if rank>1:
      i_rank_s = 's_'+name
      self.int_rank_def = 'integer,dimension('+str(rank)+') :: '+i_rank_s
      self.int_rank_shape = i_rank_s+' = shape(that%'+name+')'
      self.int_rank_list = ''.join([i_rank_s+'('+str(x+1)+'),' for x in range(rank)])[:-1]
    else:
      self.int_rank_def = ''
      self.int_rank_shape = ''
      self.int_rank_list = ''

    if allocatable and dimension>1: self.dimension_s = self.rank_deffered
    else: self.dimension_s = str(dimension)

    if self.object_type=='primitive' and 'character' in class_.lower():
      if dimension>1 and allocatable:
        self.sig = '(len='+str(dimension)+')'+',dimension('+self.rank_deffered+'),allocatable'
        self.assign_default_value = ''
      elif dimension>1 and not allocatable:
        self.sig = '(len='+str(dimension)+')'
        self.assign_default_value = ' = '+self.default_value
      else:
        self.assign_default_value = ' = '+self.default_value
        self.sig = ''
    else:
      if dimension>1 and allocatable:
        self.sig = ',dimension('+self.rank_deffered+'),allocatable'
        self.assign_default_value = ''
      elif dimension>1 and not allocatable:
        self.sig = ',dimension('+str(dimension)+')'
        self.assign_default_value = ' = '+self.default_value
      else:
        self.assign_default_value = ' = '+self.default_value
        self.sig = ''

    return self
