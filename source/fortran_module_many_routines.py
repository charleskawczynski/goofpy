    def init_copy_many(self):
        sig = 'init_many_' + self.name.lower()
        L = [self.full_sub_signature(sig,'this,that')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),dimension(:),intent(inout) :: this']
        L=L+[self.spaces[2]+'type('+self.name+'),dimension(:),intent(in) :: that']
        for key in self.arg_objects:
            L = self.arg_objects[key].get_list_of_local_iterators()
            if L: c.append(L)
        L=L+[self.spaces[2]+'if (size(that).gt.0) then']
        L=L+[self.spaces[4]+'do '+self.do_loop_iter+'=1,size(this)']
        L=L+[self.spaces[6]+'call init(this('+self.do_loop_iter+'),that('+self.do_loop_iter+'))']
        L=L+[self.spaces[4]+'enddo']
        L=L+[self.spaces[2]+'endif']
        L=L+[self.end_sub()]
        return L

    def init_copy_many_alloc(self):
        sig = 'init_many_alloc_' + self.name.lower()
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
        sig = 'delete_many_' + self.name.lower()
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
        sig = 'delete_many_alloc_' + self.name.lower()
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
        sig = 'display_many_' + self.name.lower()
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
        sig = 'display_many_alloc_' + self.name.lower()
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
        sig = 'print_many_' + self.name.lower()
        L = [self.full_sub_signature(sig,'this')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),dimension(:),intent(in),allocatable :: this']
        L=L+[self.spaces[2]+'call display(this,6)']
        L=L+[self.end_sub()]
        return L

    def print_module_many_alloc(self):
        sig = 'print_many_alloc_' + self.name.lower()
        L = [self.full_sub_signature(sig,'this')]
        L=L+[self.spaces[2]+self.implicitNone]
        L=L+[self.spaces[2]+'type('+self.name+'),dimension(:),intent(in) :: this']
        L=L+[self.spaces[2]+'call display(this,6)']
        L=L+[self.end_sub()]
        return L

    def export_module_many(self):
        sig = 'export_many_' + self.name.lower()
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
        sig = 'export_many_alloc_' + self.name.lower()
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
        sig = 'import_many_' + self.name.lower()
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
        sig = 'import_many_alloc_' + self.name.lower()
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
