       module MESH_mod
       use IO_tools_mod
       use block_mod
       use mesh_props_mod
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
       public :: MESH
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_mesh;          end interface
       interface init;   module procedure init_many_mesh;     end interface
       interface delete; module procedure delete_mesh;        end interface
       interface delete; module procedure delete_many_mesh;   end interface
       interface display;module procedure display_mesh;       end interface
       interface display;module procedure display_many_mesh;  end interface
       interface print;  module procedure print_mesh;         end interface
       interface print;  module procedure print_many_mesh;    end interface
       interface export; module procedure export_mesh;        end interface
       interface export; module procedure export_many_mesh;   end interface
       interface import; module procedure import_mesh;        end interface
       interface import; module procedure import_many_mesh;   end interface
       interface export; module procedure export_wrapper_mesh;end interface
       interface import; module procedure import_wrapper_mesh;end interface

       type MESH
         private
         type(block),dimension(:),allocatable :: b
         type(mesh_props) :: mp
         logical :: defined = .false.
       end type

       contains

       subroutine init_MESH(this,that)
         implicit none
         type(mesh),intent(inout) :: this
         type(mesh),intent(in) :: that
         integer :: i_iter
         call delete(this)
         if (allocated(that%b)) then
           allocate(this%b(size(that%b)))
           do i_iter=1,size(this%b)
             call init(this%b(i_iter),that%b(i_iter))
           enddo
         endif
         call init(this%mp,that%mp)
         this%defined = that%defined
       end subroutine

       subroutine init_many_MESH(this,that)
         implicit none
         type(mesh),dimension(:),intent(inout) :: this
         type(mesh),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_MESH(this)
         implicit none
         type(mesh),intent(inout) :: this
         integer :: i_iter
         if (allocated(this%b)) then
           do i_iter=1,size(this%b)
             call delete(this%b(i_iter))
           enddo
           deallocate(this%b)
         endif
         call delete(this%mp)
         this%defined = .false.
       end subroutine

       subroutine delete_many_MESH(this)
         implicit none
         type(mesh),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_MESH(this,un)
         implicit none
         type(mesh),intent(in) :: this
         integer,intent(in) :: un
         call display(this%b,un)
         call display(this%mp,un)
         write(un,*) 'defined = ',this%defined
       end subroutine

       subroutine display_many_MESH(this,un)
         implicit none
         type(mesh),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_MESH(this)
         implicit none
         type(mesh),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_MESH(this)
         implicit none
         type(mesh),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_MESH(this,un)
         implicit none
         type(mesh),intent(in) :: this
         integer,intent(in) :: un
         call export(this%b,un)
         call export(this%mp,un)
         write(un,*) this%defined
       end subroutine

       subroutine export_many_MESH(this,un)
         implicit none
         type(mesh),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_MESH(this,un)
         implicit none
         type(mesh),intent(inout) :: this
         integer,intent(in) :: un
         call import(this%b,un)
         call import(this%mp,un)
         read(un,*) this%defined
       end subroutine

       subroutine import_many_MESH(this,un)
         implicit none
         type(mesh),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_MESH(this,dir,name)
         implicit none
         type(mesh),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_MESH(this,dir,name)
         implicit none
         type(mesh),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module