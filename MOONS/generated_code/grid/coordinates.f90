       module COORDINATES_mod
       use IO_tools_mod
       use array_mod
       use sparse_mod
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
       public :: COORDINATES
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_coordinates;          end interface
       interface init;   module procedure init_many_coordinates;     end interface
       interface delete; module procedure delete_coordinates;        end interface
       interface delete; module procedure delete_many_coordinates;   end interface
       interface display;module procedure display_coordinates;       end interface
       interface display;module procedure display_many_coordinates;  end interface
       interface print;  module procedure print_coordinates;         end interface
       interface print;  module procedure print_many_coordinates;    end interface
       interface export; module procedure export_coordinates;        end interface
       interface export; module procedure export_many_coordinates;   end interface
       interface import; module procedure import_coordinates;        end interface
       interface import; module procedure import_many_coordinates;   end interface
       interface export; module procedure export_wrapper_coordinates;end interface
       interface import; module procedure import_wrapper_coordinates;end interface

       type COORDINATES
         private
         real(cp) :: hmin = 0.0_cp
         real(cp) :: hmax = 0.0_cp
         real(cp) :: amin = 0.0_cp
         real(cp) :: amax = 0.0_cp
         real(cp) :: maxrange = 0.0_cp
         real(cp) :: dhmin = 0.0_cp
         real(cp) :: dhmax = 0.0_cp
         real(cp) :: dhc_e = 0.0_cp
         real(cp) :: dhn_e = 0.0_cp
         real(cp) :: hc_e = 0.0_cp
         real(cp) :: hn_e = 0.0_cp
         integer :: sc = 0
         integer :: sn = 0
         integer :: n = 0
         logical :: defined = .false.
         integer :: i_midplane = 0
         logical :: stencils_defined = .false.
         logical,dimension(2) :: stencils_modified = .false.
         type(sparse) :: stagcc2n
         type(sparse) :: stagn2cc
         type(sparse) :: theta
         type(sparse),dimension(2) :: colcc
         type(sparse),dimension(2) :: coln
         type(sparse),dimension(2) :: colcc_centered
         type(array) :: hn
         type(array) :: hc
         type(array) :: dhn
         type(array) :: dhc
       end type

       contains

       subroutine init_COORDINATES(this,that)
         implicit none
         type(coordinates),intent(inout) :: this
         type(coordinates),intent(in) :: that
         call delete(this)
         this%hmin = that%hmin
         this%hmax = that%hmax
         this%amin = that%amin
         this%amax = that%amax
         this%maxrange = that%maxrange
         this%dhmin = that%dhmin
         this%dhmax = that%dhmax
         this%dhc_e = that%dhc_e
         this%dhn_e = that%dhn_e
         this%hc_e = that%hc_e
         this%hn_e = that%hn_e
         this%sc = that%sc
         this%sn = that%sn
         this%n = that%n
         this%defined = that%defined
         this%i_midplane = that%i_midplane
         this%stencils_defined = that%stencils_defined
         this%stencils_modified = that%stencils_modified
         call init(this%stagcc2n,that%stagcc2n)
         call init(this%stagn2cc,that%stagn2cc)
         call init(this%theta,that%theta)
         call init(this%colcc,that%colcc)
         call init(this%coln,that%coln)
         call init(this%colcc_centered,that%colcc_centered)
         call init(this%hn,that%hn)
         call init(this%hc,that%hc)
         call init(this%dhn,that%dhn)
         call init(this%dhc,that%dhc)
       end subroutine

       subroutine init_many_COORDINATES(this,that)
         implicit none
         type(coordinates),dimension(:),intent(inout) :: this
         type(coordinates),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_COORDINATES(this)
         implicit none
         type(coordinates),intent(inout) :: this
         this%hmin = 0.0_cp
         this%hmax = 0.0_cp
         this%amin = 0.0_cp
         this%amax = 0.0_cp
         this%maxrange = 0.0_cp
         this%dhmin = 0.0_cp
         this%dhmax = 0.0_cp
         this%dhc_e = 0.0_cp
         this%dhn_e = 0.0_cp
         this%hc_e = 0.0_cp
         this%hn_e = 0.0_cp
         this%sc = 0
         this%sn = 0
         this%n = 0
         this%defined = .false.
         this%i_midplane = 0
         this%stencils_defined = .false.
         this%stencils_modified = .false.
         call delete(this%stagcc2n)
         call delete(this%stagn2cc)
         call delete(this%theta)
         call delete(this%colcc)
         call delete(this%coln)
         call delete(this%colcc_centered)
         call delete(this%hn)
         call delete(this%hc)
         call delete(this%dhn)
         call delete(this%dhc)
       end subroutine

       subroutine delete_many_COORDINATES(this)
         implicit none
         type(coordinates),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_COORDINATES(this,un)
         implicit none
         type(coordinates),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'hmin              = ',this%hmin
         write(un,*) 'hmax              = ',this%hmax
         write(un,*) 'amin              = ',this%amin
         write(un,*) 'amax              = ',this%amax
         write(un,*) 'maxrange          = ',this%maxrange
         write(un,*) 'dhmin             = ',this%dhmin
         write(un,*) 'dhmax             = ',this%dhmax
         write(un,*) 'dhc_e             = ',this%dhc_e
         write(un,*) 'dhn_e             = ',this%dhn_e
         write(un,*) 'hc_e              = ',this%hc_e
         write(un,*) 'hn_e              = ',this%hn_e
         write(un,*) 'sc                = ',this%sc
         write(un,*) 'sn                = ',this%sn
         write(un,*) 'n                 = ',this%n
         write(un,*) 'defined           = ',this%defined
         write(un,*) 'i_midplane        = ',this%i_midplane
         write(un,*) 'stencils_defined  = ',this%stencils_defined
         write(un,*) 'stencils_modified = ',this%stencils_modified
         call display(this%stagcc2n,un)
         call display(this%stagn2cc,un)
         call display(this%theta,un)
         call display(this%colcc,un)
         call display(this%coln,un)
         call display(this%colcc_centered,un)
         call display(this%hn,un)
         call display(this%hc,un)
         call display(this%dhn,un)
         call display(this%dhc,un)
       end subroutine

       subroutine display_many_COORDINATES(this,un)
         implicit none
         type(coordinates),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_COORDINATES(this)
         implicit none
         type(coordinates),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_COORDINATES(this)
         implicit none
         type(coordinates),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_COORDINATES(this,un)
         implicit none
         type(coordinates),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%hmin
         write(un,*) this%hmax
         write(un,*) this%amin
         write(un,*) this%amax
         write(un,*) this%maxrange
         write(un,*) this%dhmin
         write(un,*) this%dhmax
         write(un,*) this%dhc_e
         write(un,*) this%dhn_e
         write(un,*) this%hc_e
         write(un,*) this%hn_e
         write(un,*) this%sc
         write(un,*) this%sn
         write(un,*) this%n
         write(un,*) this%defined
         write(un,*) this%i_midplane
         write(un,*) this%stencils_defined
         write(un,*) this%stencils_modified
         call export(this%stagcc2n,un)
         call export(this%stagn2cc,un)
         call export(this%theta,un)
         call export(this%colcc,un)
         call export(this%coln,un)
         call export(this%colcc_centered,un)
         call export(this%hn,un)
         call export(this%hc,un)
         call export(this%dhn,un)
         call export(this%dhc,un)
       end subroutine

       subroutine export_many_COORDINATES(this,un)
         implicit none
         type(coordinates),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_COORDINATES(this,un)
         implicit none
         type(coordinates),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%hmin
         read(un,*) this%hmax
         read(un,*) this%amin
         read(un,*) this%amax
         read(un,*) this%maxrange
         read(un,*) this%dhmin
         read(un,*) this%dhmax
         read(un,*) this%dhc_e
         read(un,*) this%dhn_e
         read(un,*) this%hc_e
         read(un,*) this%hn_e
         read(un,*) this%sc
         read(un,*) this%sn
         read(un,*) this%n
         read(un,*) this%defined
         read(un,*) this%i_midplane
         read(un,*) this%stencils_defined
         read(un,*) this%stencils_modified
         call import(this%stagcc2n,un)
         call import(this%stagn2cc,un)
         call import(this%theta,un)
         call import(this%colcc,un)
         call import(this%coln,un)
         call import(this%colcc_centered,un)
         call import(this%hn,un)
         call import(this%hc,un)
         call import(this%dhn,un)
         call import(this%dhc,un)
       end subroutine

       subroutine import_many_COORDINATES(this,un)
         implicit none
         type(coordinates),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_COORDINATES(this,dir,name)
         implicit none
         type(coordinates),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_COORDINATES(this,dir,name)
         implicit none
         type(coordinates),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module