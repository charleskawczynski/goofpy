       module TIME_STATISTICS_PARAMS_mod
       use IO_tools_mod
       use stats_period_mod
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
       public :: TIME_STATISTICS_PARAMS
       public :: init,delete,display,print,export,import

       interface init;   module procedure init_time_statistics_params;          end interface
       interface init;   module procedure init_many_time_statistics_params;     end interface
       interface delete; module procedure delete_time_statistics_params;        end interface
       interface delete; module procedure delete_many_time_statistics_params;   end interface
       interface display;module procedure display_time_statistics_params;       end interface
       interface display;module procedure display_many_time_statistics_params;  end interface
       interface print;  module procedure print_time_statistics_params;         end interface
       interface print;  module procedure print_many_time_statistics_params;    end interface
       interface export; module procedure export_time_statistics_params;        end interface
       interface export; module procedure export_many_time_statistics_params;   end interface
       interface import; module procedure import_time_statistics_params;        end interface
       interface import; module procedure import_many_time_statistics_params;   end interface
       interface export; module procedure export_wrapper_time_statistics_params;end interface
       interface import; module procedure import_wrapper_time_statistics_params;end interface

       type TIME_STATISTICS_PARAMS
         private
         logical :: collect = .false.
         type(stats_period) :: o1_stats
         type(stats_period) :: o2_stats
       end type

       contains

       subroutine init_TIME_STATISTICS_PARAMS(this,that)
         implicit none
         type(time_statistics_params),intent(inout) :: this
         type(time_statistics_params),intent(in) :: that
         call delete(this)
         this%collect = that%collect
         call init(this%o1_stats,that%o1_stats)
         call init(this%o2_stats,that%o2_stats)
       end subroutine

       subroutine init_many_TIME_STATISTICS_PARAMS(this,that)
         implicit none
         type(time_statistics_params),dimension(:),intent(inout) :: this
         type(time_statistics_params),dimension(:),intent(in) :: that
         integer :: i_iter
         if (size(that).gt.0) then
           do i_iter=1,size(this)
             call init(this(i_iter),that(i_iter))
           enddo
         endif
       end subroutine

       subroutine delete_TIME_STATISTICS_PARAMS(this)
         implicit none
         type(time_statistics_params),intent(inout) :: this
         this%collect = .false.
         call delete(this%o1_stats)
         call delete(this%o2_stats)
       end subroutine

       subroutine delete_many_TIME_STATISTICS_PARAMS(this)
         implicit none
         type(time_statistics_params),dimension(:),intent(inout) :: this
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call delete(this(i_iter))
           enddo
         endif
       end subroutine

       subroutine display_TIME_STATISTICS_PARAMS(this,un)
         implicit none
         type(time_statistics_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) 'collect  = ',this%collect
         call display(this%o1_stats,un)
         call display(this%o2_stats,un)
       end subroutine

       subroutine display_many_TIME_STATISTICS_PARAMS(this,un)
         implicit none
         type(time_statistics_params),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call display(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine print_TIME_STATISTICS_PARAMS(this)
         implicit none
         type(time_statistics_params),intent(in) :: this
         call display(this,6)
       end subroutine

       subroutine print_many_TIME_STATISTICS_PARAMS(this)
         implicit none
         type(time_statistics_params),dimension(:),intent(in),allocatable :: this
         call display(this,6)
       end subroutine

       subroutine export_TIME_STATISTICS_PARAMS(this,un)
         implicit none
         type(time_statistics_params),intent(in) :: this
         integer,intent(in) :: un
         write(un,*) this%collect
         call export(this%o1_stats,un)
         call export(this%o2_stats,un)
       end subroutine

       subroutine export_many_TIME_STATISTICS_PARAMS(this,un)
         implicit none
         type(time_statistics_params),dimension(:),intent(in) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call export(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine import_TIME_STATISTICS_PARAMS(this,un)
         implicit none
         type(time_statistics_params),intent(inout) :: this
         integer,intent(in) :: un
         read(un,*) this%collect
         call import(this%o1_stats,un)
         call import(this%o2_stats,un)
       end subroutine

       subroutine import_many_TIME_STATISTICS_PARAMS(this,un)
         implicit none
         type(time_statistics_params),dimension(:),intent(inout) :: this
         integer,intent(in) :: un
         integer :: i_iter
         if (size(this).gt.0) then
           do i_iter=1,size(this)
             call import(this(i_iter),un)
           enddo
         endif
       end subroutine

       subroutine export_wrapper_TIME_STATISTICS_PARAMS(this,dir,name)
         implicit none
         type(time_statistics_params),intent(in) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call export(this,un)
         close(un)
       end subroutine

       subroutine import_wrapper_TIME_STATISTICS_PARAMS(this,dir,name)
         implicit none
         type(time_statistics_params),intent(inout) :: this
         character(len=*),intent(in) :: dir,name
         integer :: un
         un = new_and_open(dir,name)
         call import(this,un)
         close(un)
       end subroutine

       end module