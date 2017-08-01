       module EXPORT_LINES_mod
       use IO_tools_mod
       use export_line_mod
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
       public :: EXPORT_LINES
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_export_lines;          end interface
       interface init;   module procedure init_many_export_lines;     end interface
       interface delete; module procedure delete_export_lines;        end interface
       interface delete; module procedure delete_many_export_lines;   end interface
       interface display;module procedure display_export_lines;       end interface
       interface display;module procedure display_many_export_lines;  end interface
       interface print;  module procedure print_export_lines;         end interface
       interface print;  module procedure print_many_export_lines;    end interface
       interface export; module procedure export_export_lines;        end interface
       interface export; module procedure export_many_export_lines;   end interface
       interface import; module procedure import_export_lines;        end interface
       interface import; module procedure import_many_export_lines;   end interface
       interface export; module procedure export_wrapper_export_lines;end interface
       interface import; module procedure import_wrapper_export_lines;end interface

       type EXPORT_LINES
         private
         type(export_line),dimension(:),allocatable :: el
         integer :: n = 0
       end type

       contains

       subroutine init_EXPORT_LINES(this,that)
         implicit none
         type(export_lines),intent(inout) :: this
         type(export_lines),intent(in) :: that
         integer :: i_iter
         call delete(this)
         if (allocated(that%el)) then
           allocate(this%el(size(that%el)))
           do i_iter=1,size(this%el)
             call init(this%el(i_iter),that%el(i_iter))
           enddo
         endif
         this%n = that%n
       end subroutine

       subroutine init_many_EXPORT_LINES(this,that)
         implicit none
         type(export_lines),dimension(:),intent(inout) :: this
         type(export_lines),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_EXPORT_LINES(this)
         implicit none
         type(export_lines),intent(inout) :: this
         integer :: i_iter
         if (allocated(this%el)) then
           do i_iter=1,size(this%el)
             call delete(this%el(i_iter))
           enddo
           deallocate(this%el)
         endif
         this%n = 0
       end subroutine

       subroutine delete_many_EXPORT_LINES(this)
         implicit none
         type(export_lines),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_EXPORT_LINES(this,un)
         implicit none
         type(export_lines),intent(in) :: this
         integer,intent(in) :: un
         call display(this%el,un)
         write(un,*) 'n  = ',this%n
       end subroutine

       subroutine display_many_EXPORT_LINES(this,un)
         implicit none
         type(export_lines),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_EXPORT_LINES(this)
         implicit none
         type(export_lines),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_EXPORT_LINES(this)
         implicit none
         type(export_lines),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_EXPORT_LINES(this,un)
         implicit none
         type(export_lines),intent(in) :: this
         integer,intent(in) :: un
         call export(this%el,un)
         write(un,*) this%n
       end subroutine

       subroutine export_many_EXPORT_LINES(this,un)
         implicit none
         type(export_lines),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_EXPORT_LINES(this,un)
         implicit none
         type(export_lines),intent(inout) :: this
         integer,intent(in) :: un
         call import(this%el,un)
         read(un,*) this%n
       end subroutine

       subroutine import_many_EXPORT_LINES(this,un)
         implicit none
         type(export_lines),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_EXPORT_LINES(this,dir,name)
         implicit none
         type(export_lines),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_EXPORT_LINES(this,dir,name)
         implicit none
         type(export_lines),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module