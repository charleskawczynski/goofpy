       module BCTYPE_mod
       use IO_tools_mod
       implicit none

       integer,parameter :: li = selected_int_kind(16)
#ifdef _QUAD_PRECISION_
       integer,parameter :: cp = selected_real_kind(32) ! Quad precision
#else
#ifdef _SINGLE_PRECISION_
       integer,parameter :: cp = selected_real_kind(8)  ! Single precision
#else
       integer,parameter :: cp = selected_real_kind(14) ! Double precision (default)
#endif
#endif
       private
       public :: BCTYPE
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_bctype;          end interface
       interface init;   module procedure init_many_bctype;     end interface
       interface delete; module procedure delete_bctype;        end interface
       interface delete; module procedure delete_many_bctype;   end interface
       interface display;module procedure display_bctype;       end interface
       interface display;module procedure display_many_bctype;  end interface
       interface print;  module procedure print_bctype;         end interface
       interface print;  module procedure print_many_bctype;    end interface
       interface export; module procedure export_bctype;        end interface
       interface export; module procedure export_many_bctype;   end interface
       interface import; module procedure import_bctype;        end interface
       interface import; module procedure import_many_bctype;   end interface
       interface export; module procedure export_wrapper_bctype;end interface
       interface import; module procedure import_wrapper_bctype;end interface

       type BCTYPE
         private
         logical :: dirichlet = .false.
         logical :: neumann = .false.
         logical :: robin = .false.
         logical :: periodic = .false.
         logical :: symmetric = .false.
         logical :: antisymmetric = .false.
         logical :: prescribed = .false.
         logical :: defined = .false.
         real(cp) :: meanval = 0.0_cp
         character(len=1) :: bct = ' '
       end type

       contains

       subroutine init_BCTYPE(this,that)
         implicit none
         type(bctype),intent(inout) :: this
         type(bctype),intent(in) :: that
         call delete(this)
         this%dirichlet = that%dirichlet
         this%neumann = that%neumann
         this%robin = that%robin
         this%periodic = that%periodic
         this%symmetric = that%symmetric
         this%antisymmetric = that%antisymmetric
         this%prescribed = that%prescribed
         this%defined = that%defined
         this%meanval = that%meanval
         this%bct = that%bct
       end subroutine

       subroutine init_many_BCTYPE(this,that)
         implicit none
         type(bctype),dimension(:),intent(inout) :: this
         type(bctype),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_BCTYPE(this)
         implicit none
         type(bctype),intent(inout) :: this
         this%dirichlet = .false.
         this%neumann = .false.
         this%robin = .false.
         this%periodic = .false.
         this%symmetric = .false.
         this%antisymmetric = .false.
         this%prescribed = .false.
         this%defined = .false.
         this%meanval = 0.0_cp
         this%bct = ' '
       end subroutine

       subroutine delete_many_BCTYPE(this)
         implicit none
         type(bctype),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_BCTYPE(this,un)
         implicit none
         type(bctype),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'dirichlet     = ',this%dirichlet
         write(un,*) 'neumann       = ',this%neumann
         write(un,*) 'robin         = ',this%robin
         write(un,*) 'periodic      = ',this%periodic
         write(un,*) 'symmetric     = ',this%symmetric
         write(un,*) 'antisymmetric = ',this%antisymmetric
         write(un,*) 'prescribed    = ',this%prescribed
         write(un,*) 'defined       = ',this%defined
         write(un,*) 'meanval       = ',this%meanval
         write(un,*) 'bct           = ',this%bct
       end subroutine

       subroutine display_many_BCTYPE(this,un)
         implicit none
         type(bctype),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_BCTYPE(this)
         implicit none
         type(bctype),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_BCTYPE(this)
         implicit none
         type(bctype),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_BCTYPE(this,un)
         implicit none
         type(bctype),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%dirichlet
         write(un,*) this%neumann
         write(un,*) this%robin
         write(un,*) this%periodic
         write(un,*) this%symmetric
         write(un,*) this%antisymmetric
         write(un,*) this%prescribed
         write(un,*) this%defined
         write(un,*) this%meanval
         write(un,*) this%bct
       end subroutine

       subroutine export_many_BCTYPE(this,un)
         implicit none
         type(bctype),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_BCTYPE(this,un)
         implicit none
         type(bctype),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%dirichlet
         read(un,*) this%neumann
         read(un,*) this%robin
         read(un,*) this%periodic
         read(un,*) this%symmetric
         read(un,*) this%antisymmetric
         read(un,*) this%prescribed
         read(un,*) this%defined
         read(un,*) this%meanval
         read(un,*) this%bct
       end subroutine

       subroutine import_many_BCTYPE(this,un)
         implicit none
         type(bctype),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_BCTYPE(this,dir,name)
         implicit none
         type(bctype),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_BCTYPE(this,dir,name)
         implicit none
         type(bctype),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module