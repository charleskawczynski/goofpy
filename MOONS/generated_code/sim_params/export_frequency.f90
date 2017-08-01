       module EXPORT_FREQUENCY_mod
       use IO_tools_mod
       use export_frequency_params_mod
       use string_mod
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
       public :: EXPORT_FREQUENCY
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_export_frequency;          end interface
       interface init;   module procedure init_many_export_frequency;     end interface
       interface delete; module procedure delete_export_frequency;        end interface
       interface delete; module procedure delete_many_export_frequency;   end interface
       interface display;module procedure display_export_frequency;       end interface
       interface display;module procedure display_many_export_frequency;  end interface
       interface print;  module procedure print_export_frequency;         end interface
       interface print;  module procedure print_many_export_frequency;    end interface
       interface export; module procedure export_export_frequency;        end interface
       interface export; module procedure export_many_export_frequency;   end interface
       interface import; module procedure import_export_frequency;        end interface
       interface import; module procedure import_many_export_frequency;   end interface
       interface export; module procedure export_wrapper_export_frequency;end interface
       interface import; module procedure import_wrapper_export_frequency;end interface

       type EXPORT_FREQUENCY
         private
         type(export_frequency_params) :: info
         type(export_frequency_params) :: unsteady_0d
         type(export_frequency_params) :: unsteady_1d
         type(export_frequency_params) :: unsteady_2d
         type(export_frequency_params) :: unsteady_3d
         type(export_frequency_params) :: final_solution
         type(export_frequency_params) :: restart_files
         type(string) :: dir
         type(string) :: name
       end type

       contains

       subroutine init_EXPORT_FREQUENCY(this,that)
         implicit none
         type(export_frequency),intent(inout) :: this
         type(export_frequency),intent(in) :: that
         call delete(this)
         call init(this%info,that%info)
         call init(this%unsteady_0d,that%unsteady_0d)
         call init(this%unsteady_1d,that%unsteady_1d)
         call init(this%unsteady_2d,that%unsteady_2d)
         call init(this%unsteady_3d,that%unsteady_3d)
         call init(this%final_solution,that%final_solution)
         call init(this%restart_files,that%restart_files)
         call init(this%dir,that%dir)
         call init(this%name,that%name)
       end subroutine

       subroutine init_many_EXPORT_FREQUENCY(this,that)
         implicit none
         type(export_frequency),dimension(:),intent(inout) :: this
         type(export_frequency),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_EXPORT_FREQUENCY(this)
         implicit none
         type(export_frequency),intent(inout) :: this
         call delete(this%info)
         call delete(this%unsteady_0d)
         call delete(this%unsteady_1d)
         call delete(this%unsteady_2d)
         call delete(this%unsteady_3d)
         call delete(this%final_solution)
         call delete(this%restart_files)
         call delete(this%dir)
         call delete(this%name)
       end subroutine

       subroutine delete_many_EXPORT_FREQUENCY(this)
         implicit none
         type(export_frequency),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_EXPORT_FREQUENCY(this,un)
         implicit none
         type(export_frequency),intent(in) :: this
         integer,intent(in) :: un
         call display(this%info,un)
         call display(this%unsteady_0d,un)
         call display(this%unsteady_1d,un)
         call display(this%unsteady_2d,un)
         call display(this%unsteady_3d,un)
         call display(this%final_solution,un)
         call display(this%restart_files,un)
         call display(this%dir,un)
         call display(this%name,un)
       end subroutine

       subroutine display_many_EXPORT_FREQUENCY(this,un)
         implicit none
         type(export_frequency),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_EXPORT_FREQUENCY(this)
         implicit none
         type(export_frequency),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_EXPORT_FREQUENCY(this)
         implicit none
         type(export_frequency),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_EXPORT_FREQUENCY(this,un)
         implicit none
         type(export_frequency),intent(in) :: this
         integer,intent(in) :: un
         call export(this%info,un)
         call export(this%unsteady_0d,un)
         call export(this%unsteady_1d,un)
         call export(this%unsteady_2d,un)
         call export(this%unsteady_3d,un)
         call export(this%final_solution,un)
         call export(this%restart_files,un)
         call export(this%dir,un)
         call export(this%name,un)
       end subroutine

       subroutine export_many_EXPORT_FREQUENCY(this,un)
         implicit none
         type(export_frequency),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_EXPORT_FREQUENCY(this,un)
         implicit none
         type(export_frequency),intent(inout) :: this
         integer,intent(in) :: un
         call import(this%info,un)
         call import(this%unsteady_0d,un)
         call import(this%unsteady_1d,un)
         call import(this%unsteady_2d,un)
         call import(this%unsteady_3d,un)
         call import(this%final_solution,un)
         call import(this%restart_files,un)
         call import(this%dir,un)
         call import(this%name,un)
       end subroutine

       subroutine import_many_EXPORT_FREQUENCY(this,un)
         implicit none
         type(export_frequency),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_EXPORT_FREQUENCY(this,dir,name)
         implicit none
         type(export_frequency),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_EXPORT_FREQUENCY(this,dir,name)
         implicit none
         type(export_frequency),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module