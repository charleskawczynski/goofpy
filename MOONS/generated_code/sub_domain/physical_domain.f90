       module PHYSICAL_DOMAIN_mod
       use IO_tools_mod
       use physical_sub_domain_mod
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
       public :: PHYSICAL_DOMAIN
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_physical_domain;          end interface
       interface init;   module procedure init_many_physical_domain;     end interface
       interface delete; module procedure delete_physical_domain;        end interface
       interface delete; module procedure delete_many_physical_domain;   end interface
       interface display;module procedure display_physical_domain;       end interface
       interface display;module procedure display_many_physical_domain;  end interface
       interface print;  module procedure print_physical_domain;         end interface
       interface print;  module procedure print_many_physical_domain;    end interface
       interface export; module procedure export_physical_domain;        end interface
       interface export; module procedure export_many_physical_domain;   end interface
       interface import; module procedure import_physical_domain;        end interface
       interface import; module procedure import_many_physical_domain;   end interface
       interface export; module procedure export_wrapper_physical_domain;end interface
       interface import; module procedure import_wrapper_physical_domain;end interface

       type PHYSICAL_DOMAIN
         private
         integer :: s = 0
         type(physical_sub_domain),dimension(:),allocatable :: sd
         logical :: defined = .false.
       end type

       contains

       subroutine init_PHYSICAL_DOMAIN(this,that)
         implicit none
         type(physical_domain),intent(inout) :: this
         type(physical_domain),intent(in) :: that
         integer :: i_iter
         call delete(this)
         this%s = that%s
         if (allocated(that%sd)) then
           allocate(this%sd(size(that%sd)))
           do i_iter=1,size(this%sd)
             call init(this%sd(i_iter),that%sd(i_iter))
           enddo
         endif
         this%defined = that%defined
       end subroutine

       subroutine init_many_PHYSICAL_DOMAIN(this,that)
         implicit none
         type(physical_domain),dimension(:),intent(inout) :: this
         type(physical_domain),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_PHYSICAL_DOMAIN(this)
         implicit none
         type(physical_domain),intent(inout) :: this
         integer :: i_iter
         this%s = 0
         if (allocated(this%sd)) then
           do i_iter=1,size(this%sd)
             call delete(this%sd(i_iter))
           enddo
           deallocate(this%sd)
         endif
         this%defined = .false.
       end subroutine

       subroutine delete_many_PHYSICAL_DOMAIN(this)
         implicit none
         type(physical_domain),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_PHYSICAL_DOMAIN(this,un)
         implicit none
         type(physical_domain),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 's       = ',this%s
         call display(this%sd,un)
         write(un,*) 'defined = ',this%defined
       end subroutine

       subroutine display_many_PHYSICAL_DOMAIN(this,un)
         implicit none
         type(physical_domain),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_PHYSICAL_DOMAIN(this)
         implicit none
         type(physical_domain),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_PHYSICAL_DOMAIN(this)
         implicit none
         type(physical_domain),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_PHYSICAL_DOMAIN(this,un)
         implicit none
         type(physical_domain),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%s
         call export(this%sd,un)
         write(un,*) this%defined
       end subroutine

       subroutine export_many_PHYSICAL_DOMAIN(this,un)
         implicit none
         type(physical_domain),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_PHYSICAL_DOMAIN(this,un)
         implicit none
         type(physical_domain),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%s
         call import(this%sd,un)
         read(un,*) this%defined
       end subroutine

       subroutine import_many_PHYSICAL_DOMAIN(this,un)
         implicit none
         type(physical_domain),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_PHYSICAL_DOMAIN(this,dir,name)
         implicit none
         type(physical_domain),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_PHYSICAL_DOMAIN(this,dir,name)
         implicit none
         type(physical_domain),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module