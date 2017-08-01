       module OVERLAP_mod
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
       public :: OVERLAP
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_overlap;          end interface
       interface init;   module procedure init_many_overlap;     end interface
       interface delete; module procedure delete_overlap;        end interface
       interface delete; module procedure delete_many_overlap;   end interface
       interface display;module procedure display_overlap;       end interface
       interface display;module procedure display_many_overlap;  end interface
       interface print;  module procedure print_overlap;         end interface
       interface print;  module procedure print_many_overlap;    end interface
       interface export; module procedure export_overlap;        end interface
       interface export; module procedure export_many_overlap;   end interface
       interface import; module procedure import_overlap;        end interface
       interface import; module procedure import_many_overlap;   end interface
       interface export; module procedure export_wrapper_overlap;end interface
       interface import; module procedure import_wrapper_overlap;end interface

       type OVERLAP
         private
         integer,dimension(2) :: i1 = 0
         integer,dimension(2) :: i2 = 0
         integer :: ir = 0
         logical :: success = .false.
       end type

       contains

       subroutine init_OVERLAP(this,that)
         implicit none
         type(overlap),intent(inout) :: this
         type(overlap),intent(in) :: that
         call delete(this)
         this%i1 = that%i1
         this%i2 = that%i2
         this%ir = that%ir
         this%success = that%success
       end subroutine

       subroutine init_many_OVERLAP(this,that)
         implicit none
         type(overlap),dimension(:),intent(inout) :: this
         type(overlap),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_OVERLAP(this)
         implicit none
         type(overlap),intent(inout) :: this
         this%i1 = 0
         this%i2 = 0
         this%ir = 0
         this%success = .false.
       end subroutine

       subroutine delete_many_OVERLAP(this)
         implicit none
         type(overlap),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_OVERLAP(this,un)
         implicit none
         type(overlap),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'i1      = ',this%i1
         write(un,*) 'i2      = ',this%i2
         write(un,*) 'ir      = ',this%ir
         write(un,*) 'success = ',this%success
       end subroutine

       subroutine display_many_OVERLAP(this,un)
         implicit none
         type(overlap),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_OVERLAP(this)
         implicit none
         type(overlap),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_OVERLAP(this)
         implicit none
         type(overlap),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_OVERLAP(this,un)
         implicit none
         type(overlap),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%i1
         write(un,*) this%i2
         write(un,*) this%ir
         write(un,*) this%success
       end subroutine

       subroutine export_many_OVERLAP(this,un)
         implicit none
         type(overlap),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_OVERLAP(this,un)
         implicit none
         type(overlap),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%i1
         read(un,*) this%i2
         read(un,*) this%ir
         read(un,*) this%success
       end subroutine

       subroutine import_many_OVERLAP(this,un)
         implicit none
         type(overlap),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_OVERLAP(this,dir,name)
         implicit none
         type(overlap),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_OVERLAP(this,dir,name)
         implicit none
         type(overlap),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module