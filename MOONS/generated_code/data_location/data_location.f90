       module DATA_LOCATION_mod
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
       public :: DATA_LOCATION
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_data_location;          end interface
       interface init;   module procedure init_many_data_location;     end interface
       interface delete; module procedure delete_data_location;        end interface
       interface delete; module procedure delete_many_data_location;   end interface
       interface display;module procedure display_data_location;       end interface
       interface display;module procedure display_many_data_location;  end interface
       interface print;  module procedure print_data_location;         end interface
       interface print;  module procedure print_many_data_location;    end interface
       interface export; module procedure export_data_location;        end interface
       interface export; module procedure export_many_data_location;   end interface
       interface import; module procedure import_data_location;        end interface
       interface import; module procedure import_many_data_location;   end interface
       interface export; module procedure export_wrapper_data_location;end interface
       interface import; module procedure import_wrapper_data_location;end interface

       type DATA_LOCATION
         private
         logical :: c = .false.
         logical :: n = .false.
         logical :: e = .false.
         logical :: f = .false.
         logical :: face = .false.
         logical :: edge = .false.
         logical,dimension(3) :: cc_along = .false.
         logical,dimension(3) :: n_along = .false.
         integer,dimension(3) :: cc_eye = 0
         integer,dimension(3) :: n_eye = 0
         logical :: defined = .false.
       end type

       contains

       subroutine init_DATA_LOCATION(this,that)
         implicit none
         type(data_location),intent(inout) :: this
         type(data_location),intent(in) :: that
         call delete(this)
         this%c = that%c
         this%n = that%n
         this%e = that%e
         this%f = that%f
         this%face = that%face
         this%edge = that%edge
         this%cc_along = that%cc_along
         this%n_along = that%n_along
         this%cc_eye = that%cc_eye
         this%n_eye = that%n_eye
         this%defined = that%defined
       end subroutine

       subroutine init_many_DATA_LOCATION(this,that)
         implicit none
         type(data_location),dimension(:),intent(inout) :: this
         type(data_location),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_DATA_LOCATION(this)
         implicit none
         type(data_location),intent(inout) :: this
         this%c = .false.
         this%n = .false.
         this%e = .false.
         this%f = .false.
         this%face = .false.
         this%edge = .false.
         this%cc_along = .false.
         this%n_along = .false.
         this%cc_eye = 0
         this%n_eye = 0
         this%defined = .false.
       end subroutine

       subroutine delete_many_DATA_LOCATION(this)
         implicit none
         type(data_location),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_DATA_LOCATION(this,un)
         implicit none
         type(data_location),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'c        = ',this%c
         write(un,*) 'n        = ',this%n
         write(un,*) 'e        = ',this%e
         write(un,*) 'f        = ',this%f
         write(un,*) 'face     = ',this%face
         write(un,*) 'edge     = ',this%edge
         write(un,*) 'cc_along = ',this%cc_along
         write(un,*) 'n_along  = ',this%n_along
         write(un,*) 'cc_eye   = ',this%cc_eye
         write(un,*) 'n_eye    = ',this%n_eye
         write(un,*) 'defined  = ',this%defined
       end subroutine

       subroutine display_many_DATA_LOCATION(this,un)
         implicit none
         type(data_location),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_DATA_LOCATION(this)
         implicit none
         type(data_location),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_DATA_LOCATION(this)
         implicit none
         type(data_location),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_DATA_LOCATION(this,un)
         implicit none
         type(data_location),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%c
         write(un,*) this%n
         write(un,*) this%e
         write(un,*) this%f
         write(un,*) this%face
         write(un,*) this%edge
         write(un,*) this%cc_along
         write(un,*) this%n_along
         write(un,*) this%cc_eye
         write(un,*) this%n_eye
         write(un,*) this%defined
       end subroutine

       subroutine export_many_DATA_LOCATION(this,un)
         implicit none
         type(data_location),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_DATA_LOCATION(this,un)
         implicit none
         type(data_location),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%c
         read(un,*) this%n
         read(un,*) this%e
         read(un,*) this%f
         read(un,*) this%face
         read(un,*) this%edge
         read(un,*) this%cc_along
         read(un,*) this%n_along
         read(un,*) this%cc_eye
         read(un,*) this%n_eye
         read(un,*) this%defined
       end subroutine

       subroutine import_many_DATA_LOCATION(this,un)
         implicit none
         type(data_location),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_DATA_LOCATION(this,dir,name)
         implicit none
         type(data_location),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_DATA_LOCATION(this,dir,name)
         implicit none
         type(data_location),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module