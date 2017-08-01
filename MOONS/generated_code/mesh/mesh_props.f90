       module MESH_PROPS_mod
       use IO_tools_mod
       use simple_int_tensor_mod
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
       public :: MESH_PROPS
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_mesh_props;          end interface
       interface init;   module procedure init_many_mesh_props;     end interface
       interface delete; module procedure delete_mesh_props;        end interface
       interface delete; module procedure delete_many_mesh_props;   end interface
       interface display;module procedure display_mesh_props;       end interface
       interface display;module procedure display_many_mesh_props;  end interface
       interface print;  module procedure print_mesh_props;         end interface
       interface print;  module procedure print_many_mesh_props;    end interface
       interface export; module procedure export_mesh_props;        end interface
       interface export; module procedure export_many_mesh_props;   end interface
       interface import; module procedure import_mesh_props;        end interface
       interface import; module procedure import_many_mesh_props;   end interface
       interface export; module procedure export_wrapper_mesh_props;end interface
       interface import; module procedure import_wrapper_mesh_props;end interface

       type MESH_PROPS
         private
         type(simple_int_tensor),dimension(3) :: int_tensor
         logical,dimension(:),allocatable :: plane
         integer,dimension(:),allocatable :: n_cells
         logical :: plane_any = .false.
         integer :: n_cells_tot = 0
         real(cp) :: volume = 0.0_cp
         real(cp),dimension(3) :: hmax = 0.0_cp
         real(cp),dimension(3) :: hmin = 0.0_cp
         real(cp),dimension(3) :: dhmax = 0.0_cp
         real(cp),dimension(3) :: dhmin = 0.0_cp
         real(cp) :: dhmax_max = 0.0_cp
         real(cp) :: dhmin_min = 0.0_cp
       end type

       contains

       subroutine init_MESH_PROPS(this,that)
         implicit none
         type(mesh_props),intent(inout) :: this
         type(mesh_props),intent(in) :: that
         call delete(this)
         call init(this%int_tensor,that%int_tensor)
         if (allocated(that%plane)) then
           allocate(this%plane(size(that%plane)))
           this%plane = that%plane
         endif
         if (allocated(that%n_cells)) then
           allocate(this%n_cells(size(that%n_cells)))
           this%n_cells = that%n_cells
         endif
         this%plane_any = that%plane_any
         this%n_cells_tot = that%n_cells_tot
         this%volume = that%volume
         this%hmax = that%hmax
         this%hmin = that%hmin
         this%dhmax = that%dhmax
         this%dhmin = that%dhmin
         this%dhmax_max = that%dhmax_max
         this%dhmin_min = that%dhmin_min
       end subroutine

       subroutine init_many_MESH_PROPS(this,that)
         implicit none
         type(mesh_props),dimension(:),intent(inout) :: this
         type(mesh_props),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_MESH_PROPS(this)
         implicit none
         type(mesh_props),intent(inout) :: this
         call delete(this%int_tensor)
         if (allocated(this%plane)) then
           this%plane = .false.
           deallocate(this%plane)
         endif
         if (allocated(this%n_cells)) then
           this%n_cells = 0
           deallocate(this%n_cells)
         endif
         this%plane_any = .false.
         this%n_cells_tot = 0
         this%volume = 0.0_cp
         this%hmax = 0.0_cp
         this%hmin = 0.0_cp
         this%dhmax = 0.0_cp
         this%dhmin = 0.0_cp
         this%dhmax_max = 0.0_cp
         this%dhmin_min = 0.0_cp
       end subroutine

       subroutine delete_many_MESH_PROPS(this)
         implicit none
         type(mesh_props),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_MESH_PROPS(this,un)
         implicit none
         type(mesh_props),intent(in) :: this
         integer,intent(in) :: un
         call display(this%int_tensor,un)
         write(un,*) 'plane       = ',this%plane
         write(un,*) 'n_cells     = ',this%n_cells
         write(un,*) 'plane_any   = ',this%plane_any
         write(un,*) 'n_cells_tot = ',this%n_cells_tot
         write(un,*) 'volume      = ',this%volume
         write(un,*) 'hmax        = ',this%hmax
         write(un,*) 'hmin        = ',this%hmin
         write(un,*) 'dhmax       = ',this%dhmax
         write(un,*) 'dhmin       = ',this%dhmin
         write(un,*) 'dhmax_max   = ',this%dhmax_max
         write(un,*) 'dhmin_min   = ',this%dhmin_min
       end subroutine

       subroutine display_many_MESH_PROPS(this,un)
         implicit none
         type(mesh_props),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_MESH_PROPS(this)
         implicit none
         type(mesh_props),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_MESH_PROPS(this)
         implicit none
         type(mesh_props),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_MESH_PROPS(this,un)
         implicit none
         type(mesh_props),intent(in) :: this
         integer,intent(in) :: un
         call export(this%int_tensor,un)
         write(un,*) this%plane
         write(un,*) this%n_cells
         write(un,*) this%plane_any
         write(un,*) this%n_cells_tot
         write(un,*) this%volume
         write(un,*) this%hmax
         write(un,*) this%hmin
         write(un,*) this%dhmax
         write(un,*) this%dhmin
         write(un,*) this%dhmax_max
         write(un,*) this%dhmin_min
       end subroutine

       subroutine export_many_MESH_PROPS(this,un)
         implicit none
         type(mesh_props),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_MESH_PROPS(this,un)
         implicit none
         type(mesh_props),intent(inout) :: this
         integer,intent(in) :: un
         call import(this%int_tensor,un)
         read(un,*) this%plane
         read(un,*) this%n_cells
         read(un,*) this%plane_any
         read(un,*) this%n_cells_tot
         read(un,*) this%volume
         read(un,*) this%hmax
         read(un,*) this%hmin
         read(un,*) this%dhmax
         read(un,*) this%dhmin
         read(un,*) this%dhmax_max
         read(un,*) this%dhmin_min
       end subroutine

       subroutine import_many_MESH_PROPS(this,un)
         implicit none
         type(mesh_props),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_MESH_PROPS(this,dir,name)
         implicit none
         type(mesh_props),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_MESH_PROPS(this,dir,name)
         implicit none
         type(mesh_props),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module