       module GEOMETRY_PROPS_mod
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
       public :: GEOMETRY_PROPS
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_geometry_props;          end interface
       interface init;   module procedure init_many_geometry_props;     end interface
       interface delete; module procedure delete_geometry_props;        end interface
       interface delete; module procedure delete_many_geometry_props;   end interface
       interface display;module procedure display_geometry_props;       end interface
       interface display;module procedure display_many_geometry_props;  end interface
       interface print;  module procedure print_geometry_props;         end interface
       interface print;  module procedure print_many_geometry_props;    end interface
       interface export; module procedure export_geometry_props;        end interface
       interface export; module procedure export_many_geometry_props;   end interface
       interface import; module procedure import_geometry_props;        end interface
       interface import; module procedure import_many_geometry_props;   end interface
       interface export; module procedure export_wrapper_geometry_props;end interface
       interface import; module procedure import_wrapper_geometry_props;end interface

       type GEOMETRY_PROPS
         private
         integer :: geometry = 0
         real(cp) :: tw = 0.0_cp
         integer,dimension(3) :: periodic_dir = 0
         integer,dimension(6) :: apply_bc_order = 0
       end type

       contains

       subroutine init_GEOMETRY_PROPS(this,that)
         implicit none
         type(geometry_props),intent(inout) :: this
         type(geometry_props),intent(in) :: that
         call delete(this)
         this%geometry = that%geometry
         this%tw = that%tw
         this%periodic_dir = that%periodic_dir
         this%apply_bc_order = that%apply_bc_order
       end subroutine

       subroutine init_many_GEOMETRY_PROPS(this,that)
         implicit none
         type(geometry_props),dimension(:),intent(inout) :: this
         type(geometry_props),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_GEOMETRY_PROPS(this)
         implicit none
         type(geometry_props),intent(inout) :: this
         this%geometry = 0
         this%tw = 0.0_cp
         this%periodic_dir = 0
         this%apply_bc_order = 0
       end subroutine

       subroutine delete_many_GEOMETRY_PROPS(this)
         implicit none
         type(geometry_props),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_GEOMETRY_PROPS(this,un)
         implicit none
         type(geometry_props),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'geometry       = ',this%geometry
         write(un,*) 'tw             = ',this%tw
         write(un,*) 'periodic_dir   = ',this%periodic_dir
         write(un,*) 'apply_bc_order = ',this%apply_bc_order
       end subroutine

       subroutine display_many_GEOMETRY_PROPS(this,un)
         implicit none
         type(geometry_props),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_GEOMETRY_PROPS(this)
         implicit none
         type(geometry_props),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_GEOMETRY_PROPS(this)
         implicit none
         type(geometry_props),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_GEOMETRY_PROPS(this,un)
         implicit none
         type(geometry_props),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%geometry
         write(un,*) this%tw
         write(un,*) this%periodic_dir
         write(un,*) this%apply_bc_order
       end subroutine

       subroutine export_many_GEOMETRY_PROPS(this,un)
         implicit none
         type(geometry_props),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_GEOMETRY_PROPS(this,un)
         implicit none
         type(geometry_props),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%geometry
         read(un,*) this%tw
         read(un,*) this%periodic_dir
         read(un,*) this%apply_bc_order
       end subroutine

       subroutine import_many_GEOMETRY_PROPS(this,un)
         implicit none
         type(geometry_props),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_GEOMETRY_PROPS(this,dir,name)
         implicit none
         type(geometry_props),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_GEOMETRY_PROPS(this,dir,name)
         implicit none
         type(geometry_props),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module