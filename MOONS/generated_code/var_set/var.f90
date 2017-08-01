       module VAR_mod
       use IO_tools_mod
       use solver_settings_mod
       use matrix_free_params_mod
       use export_lines_mod
       use export_planes_mod
       use export_field_mod
       use time_marching_params_mod
       use iter_solver_params_mod
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
       public :: VAR
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_var;          end interface
       interface init;   module procedure init_many_var;     end interface
       interface delete; module procedure delete_var;        end interface
       interface delete; module procedure delete_many_var;   end interface
       interface display;module procedure display_var;       end interface
       interface display;module procedure display_many_var;  end interface
       interface print;  module procedure print_var;         end interface
       interface print;  module procedure print_many_var;    end interface
       interface export; module procedure export_var;        end interface
       interface export; module procedure export_many_var;   end interface
       interface import; module procedure import_var;        end interface
       interface import; module procedure import_many_var;   end interface
       interface export; module procedure export_wrapper_var;end interface
       interface import; module procedure import_wrapper_var;end interface

       type VAR
         private
         integer :: ic = 0
         integer :: bc = 0
         type(solver_settings) :: ss
         type(matrix_free_params) :: mfp
         type(time_marching_params) :: tmp
         type(iter_solver_params) :: isp
         type(export_lines) :: unsteady_lines
         type(export_planes) :: unsteady_planes
         type(export_field) :: unsteady_field
       end type

       contains

       subroutine init_VAR(this,that)
         implicit none
         type(var),intent(inout) :: this
         type(var),intent(in) :: that
         call delete(this)
         this%ic = that%ic
         this%bc = that%bc
         call init(this%ss,that%ss)
         call init(this%mfp,that%mfp)
         call init(this%tmp,that%tmp)
         call init(this%isp,that%isp)
         call init(this%unsteady_lines,that%unsteady_lines)
         call init(this%unsteady_planes,that%unsteady_planes)
         call init(this%unsteady_field,that%unsteady_field)
       end subroutine

       subroutine init_many_VAR(this,that)
         implicit none
         type(var),dimension(:),intent(inout) :: this
         type(var),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_VAR(this)
         implicit none
         type(var),intent(inout) :: this
         this%ic = 0
         this%bc = 0
         call delete(this%ss)
         call delete(this%mfp)
         call delete(this%tmp)
         call delete(this%isp)
         call delete(this%unsteady_lines)
         call delete(this%unsteady_planes)
         call delete(this%unsteady_field)
       end subroutine

       subroutine delete_many_VAR(this)
         implicit none
         type(var),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_VAR(this,un)
         implicit none
         type(var),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'ic              = ',this%ic
         write(un,*) 'bc              = ',this%bc
         call display(this%ss,un)
         call display(this%mfp,un)
         call display(this%tmp,un)
         call display(this%isp,un)
         call display(this%unsteady_lines,un)
         call display(this%unsteady_planes,un)
         call display(this%unsteady_field,un)
       end subroutine

       subroutine display_many_VAR(this,un)
         implicit none
         type(var),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_VAR(this)
         implicit none
         type(var),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_VAR(this)
         implicit none
         type(var),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_VAR(this,un)
         implicit none
         type(var),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%ic
         write(un,*) this%bc
         call export(this%ss,un)
         call export(this%mfp,un)
         call export(this%tmp,un)
         call export(this%isp,un)
         call export(this%unsteady_lines,un)
         call export(this%unsteady_planes,un)
         call export(this%unsteady_field,un)
       end subroutine

       subroutine export_many_VAR(this,un)
         implicit none
         type(var),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_VAR(this,un)
         implicit none
         type(var),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%ic
         read(un,*) this%bc
         call import(this%ss,un)
         call import(this%mfp,un)
         call import(this%tmp,un)
         call import(this%isp,un)
         call import(this%unsteady_lines,un)
         call import(this%unsteady_planes,un)
         call import(this%unsteady_field,un)
       end subroutine

       subroutine import_many_VAR(this,un)
         implicit none
         type(var),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_VAR(this,dir,name)
         implicit none
         type(var),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_VAR(this,dir,name)
         implicit none
         type(var),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module