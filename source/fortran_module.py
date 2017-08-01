import os
import funcs as func
import fortran_property as FP
from collections import OrderedDict

class fortran_module:

    def __init__(self):
        self.prop = OrderedDict()
        self.maxLineLength = 71
        self.do_loop_iter = 'i_iter'
        self.implicitNone = 'implicit none'
        self.base_spaces = ' '*7
        self.raw_lines = []
        self.raw_lines_used = False
        self.any_allocatables = False
        # self.base_spaces = ''
        self.spaces = ['' for x in range(50)]
        for i in range(len(self.spaces)):
            self.spaces[i] = ' '*i
        self.stars = ['' for x in range(30)]
        for i in range(len(self.stars)):
            self.stars[i] = '*'*i

    def set_base_spaces(self,base_spaces): self.base_spaces = base_spaces

    def set_name(self,name): self.name = name.lower()
    def set_folder_name(self,folder_name): self.folder_name = folder_name.lower()
    def set_default_real(self,default_real): self.default_real = default_real
    def get_name(self): return self.name

    def get_props(self): return self.prop

    def add_prop(self,name,class_,privacy,allocatable = False,rank = 1,dimension = 1,procedure = False):
        prop = FP.fortran_property()
        prop.set_default_real(self.default_real)
        prop.init_remaining(name,class_,privacy,allocatable,rank,dimension,procedure)
        self.prop[prop.name] = prop
        allocatables = [self.prop[k].allocatable and not self.prop[k].object_type=='primitive' for k in self.prop]
        self.any_allocatables = any(allocatables)

    def print_props(self):
        for key in self.prop:
            self.prop[key].print()

    def set_list_of_classes(self,list_of_classes): self.list_of_classes = list_of_classes
    def get_list_of_classes(self): return self.list_of_classes

    def set_used_modules(self,used_modules): self.used_modules = used_modules
    def get_used_modules(self): return self.used_modules

    def add_raw_lines(self,raw_lines):
        self.raw_lines = self.raw_lines+raw_lines
        self.raw_lines_used = True

    ################################################################################*/

    def contruct_fortran_module(self,class_list,base_modules):
        c = []
        for key in self.prop:
            self.prop[key].set_do_loop_iter(self.do_loop_iter)
            self.prop[key].set_spaces(self.spaces)

        self.class_list = class_list
        c.append('module ' + self.get_name().upper() + '_mod')
        c.append(self.write_used_modules(base_modules))
        c.append([self.implicitNone]+[''])
        c.append(self.write_selected_kinds())
        c.append(['private'])
        c.append(['public :: '+self.name.upper()])
        c.append(['public :: init,delete,display,print,export,import']+[''])
        c.append(self.write_interfaces()+[''])
        c.append(self.class_definition())
        c.append(['end type']+[''])
        c.append(['contains'])
        c.append(self.write_all_functions())
        c.append('end module')

        # # Break lines to maximum number of characters
        l = func.flatten(c)
        l = [x for x in l if not x==None]
        if self.raw_lines_used: l = self.raw_lines
        l = [self.base_spaces+x if not (x=='' or x.startswith('#')) else x for x in l]
        # l = [self.base_spaces+x for x in l]
        s = [self.breakLine(k,[]) for k in l]
        s = l
        return s

    def write_used_modules(self,base_modules):
        dependent=[]
        c = [self.used_modules]
        c = [item for sublist in c for item in sublist]
        for key in self.prop:
            dependent.append([x for x in self.class_list if self.prop[key].get_class().lower()==x.lower()])
            dependent.append([x for x in base_modules if self.prop[key].get_class().lower()==x.lower()])
        dependent = list(set([item for sublist in dependent for item in sublist if item]))
        c = c+[x+'_mod' for x in dependent]
        return ['use '+x if x else None for x in c]

    def write_selected_kinds(self):
        L = []
        L = L+['integer,parameter :: li = selected_int_kind(16)']
        L = L+['#ifdef _QUAD_PRECISION_']
        L = L+['integer,parameter :: cp = selected_real_kind(32) ! Quad precision']
        L = L+['#else']
        L = L+['#ifdef _SINGLE_PRECISION_']
        L = L+['integer,parameter :: cp = selected_real_kind(8)  ! Single precision']
        L = L+['#else']
        L = L+['integer,parameter :: cp = selected_real_kind(14) ! Double precision (default)']
        L = L+['#endif']
        L = L+['#endif']
        return L

    def write_interfaces(self):
        alias = []
        sub_name = []
        alias = alias+['init'];    sub_name = sub_name+['init'];
        alias = alias+['init'];    sub_name = sub_name+['init_many'];
        # alias = alias+['init'];    sub_name = sub_name+['init_many_alloc'];
        alias = alias+['delete'];  sub_name = sub_name+['delete'];
        alias = alias+['delete'];  sub_name = sub_name+['delete_many'];
        # alias = alias+['delete'];  sub_name = sub_name+['delete_many_alloc'];
        alias = alias+['display']; sub_name = sub_name+['display'];
        alias = alias+['display']; sub_name = sub_name+['display_many'];
        # alias = alias+['display']; sub_name = sub_name+['display_many_alloc'];
        alias = alias+['print'];   sub_name = sub_name+['print'];
        alias = alias+['print'];   sub_name = sub_name+['print_many'];
        # alias = alias+['print'];   sub_name = sub_name+['print_many_alloc'];
        alias = alias+['export'];  sub_name = sub_name+['export'];
        alias = alias+['export'];  sub_name = sub_name+['export_many'];
        # alias = alias+['export'];  sub_name = sub_name+['export_many_alloc'];
        alias = alias+['import'];  sub_name = sub_name+['import'];
        alias = alias+['import'];  sub_name = sub_name+['import_many'];
        # alias = alias+['import'];  sub_name = sub_name+['import_many_alloc'];
        alias = alias+['export'];  sub_name = sub_name+['export_wrapper'];
        alias = alias+['import'];  sub_name = sub_name+['import_wrapper'];
        st_al = [len(x) for x in alias]
        st_sn = [len(x) for x in sub_name]
        sp_al = [self.spaces[max(st_al)-x] for x in st_al]
        sp_sn = [self.spaces[max(st_sn)-x] for x in st_sn]
        c = ['interface '+x+';'+s for x,s in zip(alias,sp_al)]
        c = [x+'module procedure '+sn+'_'+self.name+';' for x,sn in zip(c,sub_name)]
        c = [x+s+'end interface' for x,s in zip(c,sp_sn)]
        return c

    ################################################################################*/

    def write_all_functions(self):
        c = []
        c.append('')
        c.append(self.init_copy()+[''])
        c.append(self.init_copy_many()+[''])
        # c.append(self.init_copy_many_alloc()+[''])
        c.append(self.init_delete()+[''])
        c.append(self.init_delete_many()+[''])
        # c.append(self.init_delete_many_alloc()+[''])
        c.append(self.display_module()+[''])
        c.append(self.display_module_many()+[''])
        # c.append(self.display_module_many_alloc()+[''])
        c.append(self.print_module()+[''])
        c.append(self.print_module_many()+[''])
        # c.append(self.print_module_many_alloc()+[''])
        c.append(self.export_module()+[''])
        c.append(self.export_module_many()+[''])
        # c.append(self.export_module_many_alloc()+[''])
        c.append(self.import_module()+[''])
        c.append(self.import_module_many()+[''])
        # c.append(self.import_module_many_alloc()+[''])
        c.append(self.export_wrapper_module()+[''])
        c.append(self.import_wrapper_module()+[''])
        return c

    def class_definition(self):
        c = ['type ' + self.name.upper()]
        p = [self.prop[k].get_privacy() for k in self.prop]
        self.all_private = all([x=='private' for x in p])
        if self.all_private:
            c=c+[self.spaces[2]+'private']
        for key in self.prop:
            c.append([self.spaces[2]+x for x in self.prop[key].write_class_definition(self.all_private)])
        return c

    def init_copy(self):
        sig = 'init_' + self.name.upper()
        self.set_arg_objects()
        self.set_arg_list()
        c = [self.full_sub_signature(sig,'this,that')]
        c.append(self.spaces[2] + self.implicitNone )
        c.append(self.spaces[2] + 'type(' + self.name + '),intent(inout) :: this' )
        c.append(self.spaces[2] + 'type(' + self.name + '),intent(in) :: that' )
        if self.any_allocatables:
            c.append(self.spaces[2] + 'integer :: '+self.do_loop_iter)
        for key in self.arg_objects:
            if self.arg_objects[key].rank>1:
                c.append([self.spaces[2]+self.arg_objects[key].int_rank_def])
        c.append(self.spaces[2] + 'call delete(this)')
        for key in self.arg_objects:
            c.append([self.spaces[2]+x for x in self.arg_objects[key].write_init_copy()])
        c.append(self.end_sub())
        return c

    def init_delete(self):
        sig = 'delete_' + self.name.upper()
        self.set_arg_objects()
        self.set_arg_list()
        c = [self.full_sub_signature(sig,'this')]
        c.append(self.spaces[2] + self.implicitNone )
        c.append(self.spaces[2] + 'type(' + self.name + '),intent(inout) :: this' )
        if self.any_allocatables:
            c.append(self.spaces[2] + 'integer :: '+self.do_loop_iter)
        for key in self.arg_objects:
            c.append([self.spaces[2]+x for x in self.arg_objects[key].write_delete()])
        c.append(self.end_sub())
        return c

    def display_module(self):
        c = [self.full_sub_signature('display_' + self.name.upper(),'this,un')]
        c.append(self.spaces[2] + self.implicitNone)
        c.append(self.spaces[2] + 'type(' + self.name + '),intent(in) :: this' )
        c.append(self.spaces[2] + 'integer,intent(in) :: un' )

        st_n = [len(self.prop[key].name) for key in self.prop]
        sp_n = [self.spaces[max(st_n)-x] for x in st_n]
        for key,s in zip(self.prop,sp_n):
            self.prop[key].set_display_spaces(s)

        for key in self.prop:
            c.append([self.spaces[2]+x for x in self.prop[key].write_display()])
        c.append(self.end_sub())
        return c

    def print_module(self):
        c = [self.full_sub_signature('print_' + self.name.upper(),'this')]
        c.append(self.spaces[2] + self.implicitNone)
        c.append(self.spaces[2] + 'type(' + self.name + '),intent(in) :: this' )
        c.append(self.spaces[2] + "call display(this,6)")
        c.append(self.end_sub() )
        return c

    def export_module(self):
        c = [self.full_sub_signature('export_' + self.name.upper(),'this,un')]
        c.append(self.spaces[2] + self.implicitNone)
        c.append(self.spaces[2] + 'type(' + self.name + '),intent(in) :: this' )
        c.append(self.spaces[2] + 'integer,intent(in) :: un' )
        for key in self.prop:
            c.append([self.spaces[2]+x for x in self.prop[key].write_export()])
        c.append(self.end_sub())
        return c

    def import_module(self):
        c = [self.full_sub_signature('import_' + self.name.upper(),'this,un')]
        c.append(self.spaces[2] + self.implicitNone)
        c.append(self.spaces[2] + 'type(' + self.name + '),intent(inout) :: this' )
        c.append(self.spaces[2] + 'integer,intent(in) :: un' )
        for key in self.prop:
            c.append([self.spaces[2]+x for x in self.prop[key].write_import()])
        c.append(self.end_sub())
        return c

    ################################################################################*/

    def init_copy_many(self):
        sig = 'init_many_' + self.name.upper()
        L = [self.full_sub_signature(sig,'this,that')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),dimension(:),intent(inout) :: this']
        L=L+[self.spaces[2]+'type('+self.name+'),dimension(:),intent(in) :: that']
        L=L+[self.spaces[2]+'integer :: '+self.do_loop_iter]
        L=L+[self.spaces[2]+'if (size(that).gt.0) then']
        L=L+[self.spaces[4]+'do '+self.do_loop_iter+'=1,size(this)']
        L=L+[self.spaces[6]+'call init(this('+self.do_loop_iter+'),that('+self.do_loop_iter+'))']
        L=L+[self.spaces[4]+'enddo']
        L=L+[self.spaces[2]+'endif']
        L=L+[self.end_sub()]
        return L

    def init_copy_many_alloc(self):
        sig = 'init_many_alloc_' + self.name.upper()
        L = [self.full_sub_signature(sig,'this,that')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),dimension(:),intent(inout),allocatable :: this']
        L=L+[self.spaces[2]+'type('+self.name+'),dimension(:),intent(in),allocatable :: that']
        L=L+[self.spaces[2]+'integer :: '+self.do_loop_iter]
        L=L+[self.spaces[2]+'if (allocated(that)) then']
        L=L+[self.spaces[4]+'allocate(this(size(that)))']
        L=L+[self.spaces[4]+'do '+self.do_loop_iter+'=1,size(this)']
        L=L+[self.spaces[6]+'call init(this('+self.do_loop_iter+'),that('+self.do_loop_iter+'))']
        L=L+[self.spaces[4]+'enddo']
        L=L+[self.spaces[2]+'endif']
        L=L+[self.end_sub()]
        return L

    def init_delete_many(self):
        sig = 'delete_many_' + self.name.upper()
        L = [self.full_sub_signature(sig,'this')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),dimension(:),intent(inout) :: this']
        L=L+[self.spaces[2]+'integer :: '+self.do_loop_iter]
        L=L+[self.spaces[2]+'if (size(this).gt.0) then']
        L=L+[self.spaces[4]+'do '+self.do_loop_iter+'=1,size(this)']
        L=L+[self.spaces[6]+'call delete(this('+self.do_loop_iter+'))']
        L=L+[self.spaces[4]+'enddo']
        L=L+[self.spaces[2]+'endif']
        L=L+[self.end_sub()]
        return L

    def init_delete_many_alloc(self):
        sig = 'delete_many_alloc_' + self.name.upper()
        L = [self.full_sub_signature(sig,'this')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),dimension(:),intent(inout),allocatable :: this']
        L=L+[self.spaces[2]+'integer :: '+self.do_loop_iter]
        L=L+[self.spaces[2]+'if (allocated(this)) then']
        L=L+[self.spaces[4]+'do '+self.do_loop_iter+'=1,size(this)']
        L=L+[self.spaces[6]+'call delete(this('+self.do_loop_iter+'))']
        L=L+[self.spaces[4]+'enddo']
        L=L+[self.spaces[4]+'deallocate(this)']
        L=L+[self.spaces[2]+'endif']
        L=L+[self.end_sub()]
        return L

    def display_module_many(self):
        sig = 'display_many_' + self.name.upper()
        L = [self.full_sub_signature(sig,'this,un')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),dimension(:),intent(in) :: this']
        L=L+[self.spaces[2]+'integer,intent(in) :: un']
        L=L+[self.spaces[2]+'integer :: '+self.do_loop_iter]
        L=L+[self.spaces[2]+'if (size(this).gt.0) then']
        L=L+[self.spaces[4]+'do '+self.do_loop_iter+'=1,size(this)']
        L=L+[self.spaces[6]+'call display(this('+self.do_loop_iter+'),un)']
        L=L+[self.spaces[4]+'enddo']
        L=L+[self.spaces[2]+'endif']
        L=L+[self.end_sub()]
        return L

    def display_module_many_alloc(self):
        sig = 'display_many_alloc_' + self.name.upper()
        L = [self.full_sub_signature(sig,'this,un')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),dimension(:),intent(in),allocatable :: this']
        L=L+[self.spaces[2]+'integer,intent(in) :: un']
        L=L+[self.spaces[2]+'integer :: '+self.do_loop_iter]
        L=L+[self.spaces[2]+'if (allocated(this)) then']
        L=L+[self.spaces[4]+'do '+self.do_loop_iter+'=1,size(this)']
        L=L+[self.spaces[6]+'call display(this('+self.do_loop_iter+'),un)']
        L=L+[self.spaces[4]+'enddo']
        L=L+[self.spaces[2]+'endif']
        L=L+[self.end_sub()]
        return L

    def print_module_many(self):
        sig = 'print_many_' + self.name.upper()
        L = [self.full_sub_signature(sig,'this')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),dimension(:),intent(in),allocatable :: this']
        L=L+[self.spaces[2]+'call display(this,6)']
        L=L+[self.end_sub()]
        return L

    def print_module_many_alloc(self):
        sig = 'print_many_alloc_' + self.name.upper()
        L = [self.full_sub_signature(sig,'this')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),dimension(:),intent(in) :: this']
        L=L+[self.spaces[2]+'call display(this,6)']
        L=L+[self.end_sub()]
        return L

    def export_module_many(self):
        sig = 'export_many_' + self.name.upper()
        L = [self.full_sub_signature(sig,'this,un')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),dimension(:),intent(in) :: this']
        L=L+[self.spaces[2]+'integer,intent(in) :: un']
        L=L+[self.spaces[2]+'integer :: '+self.do_loop_iter]
        L=L+[self.spaces[2]+'if (size(this).gt.0) then']
        L=L+[self.spaces[4]+'do '+self.do_loop_iter+'=1,size(this)']
        L=L+[self.spaces[6]+'call export(this('+self.do_loop_iter+'),un)']
        L=L+[self.spaces[4]+'enddo']
        L=L+[self.spaces[2]+'endif']
        L=L+[self.end_sub()]
        return L

    def export_module_many_alloc(self):
        sig = 'export_many_alloc_' + self.name.upper()
        L = [self.full_sub_signature(sig,'this,un')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),dimension(:),intent(in),allocatable :: this']
        L=L+[self.spaces[2]+'integer,intent(in) :: un']
        L=L+[self.spaces[2]+'integer :: '+self.do_loop_iter]
        L=L+[self.spaces[2]+'if (allocated(this)) then']
        L=L+[self.spaces[4]+'do '+self.do_loop_iter+'=1,size(this)']
        L=L+[self.spaces[6]+'call export(this('+self.do_loop_iter+'),un)']
        L=L+[self.spaces[4]+'enddo']
        L=L+[self.spaces[2]+'endif']
        L=L+[self.end_sub()]
        return L

    def import_module_many(self):
        sig = 'import_many_' + self.name.upper()
        L = [self.full_sub_signature(sig,'this,un')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),dimension(:),intent(inout) :: this']
        L=L+[self.spaces[2]+'integer,intent(in) :: un']
        L=L+[self.spaces[2]+'integer :: '+self.do_loop_iter]
        L=L+[self.spaces[2]+'if (size(this).gt.0) then']
        L=L+[self.spaces[4]+'do '+self.do_loop_iter+'=1,size(this)']
        L=L+[self.spaces[6]+'call import(this('+self.do_loop_iter+'),un)']
        L=L+[self.spaces[4]+'enddo']
        L=L+[self.spaces[2]+'endif']
        L=L+[self.end_sub()]
        return L

    def import_module_many_alloc(self):
        sig = 'import_many_alloc_' + self.name.upper()
        L = [self.full_sub_signature(sig,'this,un')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),dimension(:),intent(inout),allocatable :: this']
        L=L+[self.spaces[2]+'integer,intent(in) :: un']
        L=L+[self.spaces[2]+'integer :: '+self.do_loop_iter]
        L=L+[self.spaces[2]+'if (allocated(this)) then']
        L=L+[self.spaces[4]+'do '+self.do_loop_iter+'=1,size(this)']
        L=L+[self.spaces[6]+'call import(this('+self.do_loop_iter+'),un)']
        L=L+[self.spaces[4]+'enddo']
        L=L+[self.spaces[2]+'endif']
        L=L+[self.end_sub()]
        return L

    def export_wrapper_module(self):
        sig = 'export_wrapper_' + self.name.upper()
        L = [self.full_sub_signature(sig,'this,dir,name')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),intent(in) :: this']
        L=L+[self.spaces[2]+'character(len=*),intent(in) :: dir,name']
        L=L+[self.spaces[2]+'integer :: un']
        L=L+[self.spaces[2]+'un = new_and_open(dir,name)']
        L=L+[self.spaces[2]+'call export(this,un)']
        L=L+[self.spaces[2]+'close(un)']
        L=L+[self.end_sub()]
        return L

    def import_wrapper_module(self):
        sig = 'import_wrapper_' + self.name.upper()
        L = [self.full_sub_signature(sig,'this,dir,name')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),intent(inout) :: this']
        L=L+[self.spaces[2]+'character(len=*),intent(in) :: dir,name']
        L=L+[self.spaces[2]+'integer :: un']
        L=L+[self.spaces[2]+'un = new_and_open(dir,name)']
        L=L+[self.spaces[2]+'call import(this,un)']
        L=L+[self.spaces[2]+'close(un)']
        L=L+[self.end_sub()]
        return L

    ################################################################################*/

    def full_func_signature(self,sig,args,result): return 'function ' + sig + '(' + args + ') result(' + result + ')'
    def full_sub_signature(self,sig,args): return 'subroutine ' + sig + '(' + args + ')'
    def end_function(self,function = False): return 'end function'
    def end_sub(self,function = False): return 'end subroutine'
    def set_arg_objects(self): self.arg_objects = self.prop
    def set_arg_list(self): self.arg_list = [key for key in self.prop]

    ################################################################################*/

    def breakLine(self,stringList,result = []):
        spaces = self.base_spaces[:-2]
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

    ################################################################################*/
