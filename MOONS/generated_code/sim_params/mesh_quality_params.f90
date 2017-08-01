       module MESH_QUALITY_PARAMS_mod
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
       public :: MESH_QUALITY_PARAMS
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_mesh_quality_params;          end interface
       interface init;   module procedure init_many_mesh_quality_params;     end interface
       interface delete; module procedure delete_mesh_quality_params;        end interface
       interface delete; module procedure delete_many_mesh_quality_params;   end interface
       interface display;module procedure display_mesh_quality_params;       end interface
       interface display;module procedure display_many_mesh_quality_params;  end interface
       interface print;  module procedure print_mesh_quality_params;         end interface
       interface print;  module procedure print_many_mesh_quality_params;    end interface
       interface export; module procedure export_mesh_quality_params;        end interface
       interface export; module procedure export_many_mesh_quality_params;   end interface
       interface import; module procedure import_mesh_quality_params;        end interface
       interface import; module procedure import_many_mesh_quality_params;   end interface
       interface export; module procedure export_wrapper_mesh_quality_params;end interface
       interface import; module procedure import_wrapper_mesh_quality_params;end interface

       type MESH_QUALITY_PARAMS
         private
         real(cp) :: max_mesh_stretch_ratio = 0.0_cp
         integer :: n_max_points_add = 0
         integer :: n_iter = 0
         logical :: auto_find_n = .false.
       end type

       contains

       subroutine init_MESH_QUALITY_PARAMS(this,that)
         implicit none
         type(mesh_quality_params),intent(inout) :: this
         type(mesh_quality_params),intent(in) :: that
         call delete(this)
         this%max_mesh_stretch_ratio = that%max_mesh_stretch_ratio
         this%n_max_points_add = that%n_max_points_add
         this%n_iter = that%n_iter
         this%auto_find_n = that%auto_find_n
       end subroutine

       subroutine init_many_MESH_QUALITY_PARAMS(this,that)
         implicit none
         type(mesh_quality_params),dimension(:),intent(inout) :: this
         type(mesh_quality_params),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_MESH_QUALITY_PARAMS(this)
         implicit none
         type(mesh_quality_params),intent(inout) :: this
         this%max_mesh_stretch_ratio = 0.0_cp
         this%n_max_points_add = 0
         this%n_iter = 0
         this%auto_find_n = .false.
       end subroutine

       subroutine delete_many_MESH_QUALITY_PARAMS(this)
         implicit none
         type(mesh_quality_params),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_MESH_QUALITY_PARAMS(this,un)
         implicit none
         type(mesh_quality_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'max_mesh_stretch_ratio = ',this%max_mesh_stretch_ratio
         write(un,*) 'n_max_points_add       = ',this%n_max_points_add
         write(un,*) 'n_iter                 = ',this%n_iter
         write(un,*) 'auto_find_n            = ',this%auto_find_n
       end subroutine

       subroutine display_many_MESH_QUALITY_PARAMS(this,un)
         implicit none
         type(mesh_quality_params),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_MESH_QUALITY_PARAMS(this)
         implicit none
         type(mesh_quality_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_MESH_QUALITY_PARAMS(this)
         implicit none
         type(mesh_quality_params),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_MESH_QUALITY_PARAMS(this,un)
         implicit none
         type(mesh_quality_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%max_mesh_stretch_ratio
         write(un,*) this%n_max_points_add
         write(un,*) this%n_iter
         write(un,*) this%auto_find_n
       end subroutine

       subroutine export_many_MESH_QUALITY_PARAMS(this,un)
         implicit none
         type(mesh_quality_params),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_MESH_QUALITY_PARAMS(this,un)
         implicit none
         type(mesh_quality_params),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%max_mesh_stretch_ratio
         read(un,*) this%n_max_points_add
         read(un,*) this%n_iter
         read(un,*) this%auto_find_n
       end subroutine

       subroutine import_many_MESH_QUALITY_PARAMS(this,un)
         implicit none
         type(mesh_quality_params),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_MESH_QUALITY_PARAMS(this,dir,name)
         implicit none
         type(mesh_quality_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_MESH_QUALITY_PARAMS(this,dir,name)
         implicit none
         type(mesh_quality_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module