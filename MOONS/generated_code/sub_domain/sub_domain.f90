       module SUB_DOMAIN_mod
       use IO_tools_mod
       use overlap_mod
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
       public :: SUB_DOMAIN
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_sub_domain;          end interface
       interface init;   module procedure init_many_sub_domain;     end interface
       interface delete; module procedure delete_sub_domain;        end interface
       interface delete; module procedure delete_many_sub_domain;   end interface
       interface display;module procedure display_sub_domain;       end interface
       interface display;module procedure display_many_sub_domain;  end interface
       interface print;  module procedure print_sub_domain;         end interface
       interface print;  module procedure print_many_sub_domain;    end interface
       interface export; module procedure export_sub_domain;        end interface
       interface export; module procedure export_many_sub_domain;   end interface
       interface import; module procedure import_sub_domain;        end interface
       interface import; module procedure import_many_sub_domain;   end interface
       interface export; module procedure export_wrapper_sub_domain;end interface
       interface import; module procedure import_wrapper_sub_domain;end interface

       type SUB_DOMAIN
         private
         type(overlap),dimension(3) :: c
         type(overlap),dimension(3) :: n
         type(overlap),dimension(3) :: m
         logical :: defined = .false.
         integer :: g_r1_id = 0
         integer :: g_r2_id = 0
       end type

       contains

       subroutine init_SUB_DOMAIN(this,that)
         implicit none
         type(sub_domain),intent(inout) :: this
         type(sub_domain),intent(in) :: that
         call delete(this)
         call init(this%c,that%c)
         call init(this%n,that%n)
         call init(this%m,that%m)
         this%defined = that%defined
         this%g_r1_id = that%g_r1_id
         this%g_r2_id = that%g_r2_id
       end subroutine

       subroutine init_many_SUB_DOMAIN(this,that)
         implicit none
         type(sub_domain),dimension(:),intent(inout) :: this
         type(sub_domain),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_SUB_DOMAIN(this)
         implicit none
         type(sub_domain),intent(inout) :: this
         call delete(this%c)
         call delete(this%n)
         call delete(this%m)
         this%defined = .false.
         this%g_r1_id = 0
         this%g_r2_id = 0
       end subroutine

       subroutine delete_many_SUB_DOMAIN(this)
         implicit none
         type(sub_domain),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_SUB_DOMAIN(this,un)
         implicit none
         type(sub_domain),intent(in) :: this
         integer,intent(in) :: un
         call display(this%c,un)
         call display(this%n,un)
         call display(this%m,un)
         write(un,*) 'defined = ',this%defined
         write(un,*) 'g_r1_id = ',this%g_r1_id
         write(un,*) 'g_r2_id = ',this%g_r2_id
       end subroutine

       subroutine display_many_SUB_DOMAIN(this,un)
         implicit none
         type(sub_domain),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_SUB_DOMAIN(this)
         implicit none
         type(sub_domain),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_SUB_DOMAIN(this)
         implicit none
         type(sub_domain),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_SUB_DOMAIN(this,un)
         implicit none
         type(sub_domain),intent(in) :: this
         integer,intent(in) :: un
         call export(this%c,un)
         call export(this%n,un)
         call export(this%m,un)
         write(un,*) this%defined
         write(un,*) this%g_r1_id
         write(un,*) this%g_r2_id
       end subroutine

       subroutine export_many_SUB_DOMAIN(this,un)
         implicit none
         type(sub_domain),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_SUB_DOMAIN(this,un)
         implicit none
         type(sub_domain),intent(inout) :: this
         integer,intent(in) :: un
         call import(this%c,un)
         call import(this%n,un)
         call import(this%m,un)
         read(un,*) this%defined
         read(un,*) this%g_r1_id
         read(un,*) this%g_r2_id
       end subroutine

       subroutine import_many_SUB_DOMAIN(this,un)
         implicit none
         type(sub_domain),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_SUB_DOMAIN(this,dir,name)
         implicit none
         type(sub_domain),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_SUB_DOMAIN(this,dir,name)
         implicit none
         type(sub_domain),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module