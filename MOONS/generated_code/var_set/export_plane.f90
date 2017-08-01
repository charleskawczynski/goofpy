       module EXPORT_PLANE_mod
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
       public :: EXPORT_PLANE
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_export_plane;          end interface
       interface init;   module procedure init_many_export_plane;     end interface
       interface delete; module procedure delete_export_plane;        end interface
       interface delete; module procedure delete_many_export_plane;   end interface
       interface display;module procedure display_export_plane;       end interface
       interface display;module procedure display_many_export_plane;  end interface
       interface print;  module procedure print_export_plane;         end interface
       interface print;  module procedure print_many_export_plane;    end interface
       interface export; module procedure export_export_plane;        end interface
       interface export; module procedure export_many_export_plane;   end interface
       interface import; module procedure import_export_plane;        end interface
       interface import; module procedure import_many_export_plane;   end interface
       interface export; module procedure export_wrapper_export_plane;end interface
       interface import; module procedure import_wrapper_export_plane;end interface

       type EXPORT_PLANE
         private
         logical :: export_ever = .false.
         integer :: dir = 0
         integer,dimension(2) :: plane = 0
         character(len=1) :: suffix = ' '
       end type

       contains

       subroutine init_EXPORT_PLANE(this,that)
         implicit none
         type(export_plane),intent(inout) :: this
         type(export_plane),intent(in) :: that
         call delete(this)
         this%export_ever = that%export_ever
         this%dir = that%dir
         this%plane = that%plane
         this%suffix = that%suffix
       end subroutine

       subroutine init_many_EXPORT_PLANE(this,that)
         implicit none
         type(export_plane),dimension(:),intent(inout) :: this
         type(export_plane),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_EXPORT_PLANE(this)
         implicit none
         type(export_plane),intent(inout) :: this
         this%export_ever = .false.
         this%dir = 0
         this%plane = 0
         this%suffix = ' '
       end subroutine

       subroutine delete_many_EXPORT_PLANE(this)
         implicit none
         type(export_plane),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_EXPORT_PLANE(this,un)
         implicit none
         type(export_plane),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'export_ever = ',this%export_ever
         write(un,*) 'dir         = ',this%dir
         write(un,*) 'plane       = ',this%plane
         write(un,*) 'suffix      = ',this%suffix
       end subroutine

       subroutine display_many_EXPORT_PLANE(this,un)
         implicit none
         type(export_plane),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_EXPORT_PLANE(this)
         implicit none
         type(export_plane),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_EXPORT_PLANE(this)
         implicit none
         type(export_plane),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_EXPORT_PLANE(this,un)
         implicit none
         type(export_plane),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%export_ever
         write(un,*) this%dir
         write(un,*) this%plane
         write(un,*) this%suffix
       end subroutine

       subroutine export_many_EXPORT_PLANE(this,un)
         implicit none
         type(export_plane),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_EXPORT_PLANE(this,un)
         implicit none
         type(export_plane),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%export_ever
         read(un,*) this%dir
         read(un,*) this%plane
         read(un,*) this%suffix
       end subroutine

       subroutine import_many_EXPORT_PLANE(this,un)
         implicit none
         type(export_plane),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_EXPORT_PLANE(this,dir,name)
         implicit none
         type(export_plane),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_EXPORT_PLANE(this,dir,name)
         implicit none
         type(export_plane),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module