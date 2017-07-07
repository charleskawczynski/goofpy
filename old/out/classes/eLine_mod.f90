       module eLine_mod
       use allClassFuncs_mod
       use constants_mod
       use simParams_mod
       use debugger_mod
       use vector_mod
       use BCs_mod


       type eLine
         private
         integer :: id
         type(BCs) :: BC
         real(cp) :: a
         real(cp) :: b
         real(cp) :: h
         real(cp) :: k
         logical :: normalOutward
       endtype

       contains

       function setEline(id,BC,a,b,h,k,normalOutward) result(this)
         implicit none
         type(eLine) :: this
         integer, intent(in) :: id
         type(BCs), intent(in) :: BC
         real(cp), intent(in) :: a
         real(cp), intent(in) :: b
         real(cp), intent(in) :: h
         real(cp), intent(in) :: k
         logical, intent(in) :: normalOutward
         call SetElineId(this,id)
         call SetElineBc(this,BC)
         call SetElineA(this,a)
         call SetElineB(this,b)
         call SetElineH(this,h)
         call SetElineK(this,k)
         call SetElineNormaloutward(this,normalOutward)
       end function

       subroutine setElineId(this,id)
         implicit none
         type(eLine),intent(inout) :: this
         integer, intent(in) :: id
         this%id = id
       end subroutine

       subroutine setElineBc(this,BC)
         implicit none
         type(eLine),intent(inout) :: this
         type(BCs), intent(in) :: BC
         this%BC = BC
       end subroutine

       subroutine setElineA(this,a)
         implicit none
         type(eLine),intent(inout) :: this
         real(cp), intent(in) :: a
         this%a = a
       end subroutine

       subroutine setElineB(this,b)
         implicit none
         type(eLine),intent(inout) :: this
         real(cp), intent(in) :: b
         this%b = b
       end subroutine

       subroutine setElineH(this,h)
         implicit none
         type(eLine),intent(inout) :: this
         real(cp), intent(in) :: h
         this%h = h
       end subroutine

       subroutine setElineK(this,k)
         implicit none
         type(eLine),intent(inout) :: this
         real(cp), intent(in) :: k
         this%k = k
       end subroutine

       subroutine setElineNormaloutward(this,normalOutward)
         implicit none
         type(eLine),intent(inout) :: this
         logical, intent(in) :: normalOutward
         this%normalOutward = normalOutward
       end subroutine

       function getElineId(this) result(id)
         implicit none
         type(eLine),intent(in) :: this
         integer :: id
         id = this%id
       end function

       function getElineBc(this) result(BC)
         implicit none
         type(eLine),intent(in) :: this
         type(BCs) :: BC
         BC = this%BC
       end function

       function getElineA(this) result(a)
         implicit none
         type(eLine),intent(in) :: this
         real(cp) :: a
         a = this%a
       end function

       function getElineB(this) result(b)
         implicit none
         type(eLine),intent(in) :: this
         real(cp) :: b
         b = this%b
       end function

       function getElineH(this) result(h)
         implicit none
         type(eLine),intent(in) :: this
         real(cp) :: h
         h = this%h
       end function

       function getElineK(this) result(k)
         implicit none
         type(eLine),intent(in) :: this
         real(cp) :: k
         k = this%k
       end function

       function getElineNormaloutward(this) result(normalOutward)
         implicit none
         type(eLine),intent(in) :: this
         logical :: normalOutward
         normalOutward = this%normalOutward
       end function

       subroutine getEline(this,id,BC,a,b,h,k,normalOutward)
         implicit none
         type(eLine),intent(in) :: this
         integer, intent(out) :: id
         type(BCs), intent(out) :: BC
         real(cp), intent(out) :: a
         real(cp), intent(out) :: b
         real(cp), intent(out) :: h
         real(cp), intent(out) :: k
         logical, intent(out) :: normalOutward
         id = getElineId(this)
         BC = getElineBc(this)
         a = getElineA(this)
         b = getElineB(this)
         h = getElineH(this)
         k = getElineK(this)
         normalOutward = getElineNormaloutward(this)
       end subroutine

       subroutine printElineId(this)
         implicit none
         type(eLine),intent(in) :: this
         integer :: id
         write(*,*) "id: ", this%id
       end subroutine

       subroutine printElineBc(this)
         implicit none
         type(eLine),intent(in) :: this
         type(BCs) :: BC
         call printBcs(this%BC)
       end subroutine

       subroutine printElineA(this)
         implicit none
         type(eLine),intent(in) :: this
         real(cp) :: a
         write(*,*) "a: ", this%a
       end subroutine

       subroutine printElineB(this)
         implicit none
         type(eLine),intent(in) :: this
         real(cp) :: b
         write(*,*) "b: ", this%b
       end subroutine

       subroutine printElineH(this)
         implicit none
         type(eLine),intent(in) :: this
         real(cp) :: h
         write(*,*) "h: ", this%h
       end subroutine

       subroutine printElineK(this)
         implicit none
         type(eLine),intent(in) :: this
         real(cp) :: k
         write(*,*) "k: ", this%k
       end subroutine

       subroutine printElineNormaloutward(this)
         implicit none
         type(eLine),intent(in) :: this
         logical :: normalOutward
         write(*,*) "normalOutward: ", this%normalOutward
       end subroutine

       subroutine printEline(this)
         implicit none
         type(eLine), intent(in) :: this
         write(*,*) 'Printed data for eLine'
         write(*,*) '***************'
         call printElineId(this)
         call printBcs(this%BC)
         call printElineA(this)
         call printElineB(this)
         call printElineH(this)
         call printElineK(this)
         call printElineNormaloutward(this)
         write(*,*) '***************'
         write(*,*) ''
       end subroutine

       subroutine writeElineId(this,dir,parentUnit)
         implicit none
         type(eLine),intent(in) :: this
         integer :: id
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "id: ", this%id
         else
           call newAndOpen(dir,"id")
           write(NewU,*) "id: ", this%id
           call closeAndMessage(NewU,"id")
         endif
       end subroutine

       subroutine writeElineBc(this,dir)
         implicit none
         type(eLine),intent(in) :: this
         character(len=*),intent(in) :: dir
         type(BCs) :: BC
         call writeBcs(this%BC,dir)
       end subroutine

       subroutine writeElineA(this,dir,parentUnit)
         implicit none
         type(eLine),intent(in) :: this
         real(cp) :: a
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "a: ", this%a
         else
           call newAndOpen(dir,"a")
           write(NewU,*) "a: ", this%a
           call closeAndMessage(NewU,"a")
         endif
       end subroutine

       subroutine writeElineB(this,dir,parentUnit)
         implicit none
         type(eLine),intent(in) :: this
         real(cp) :: b
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "b: ", this%b
         else
           call newAndOpen(dir,"b")
           write(NewU,*) "b: ", this%b
           call closeAndMessage(NewU,"b")
         endif
       end subroutine

       subroutine writeElineH(this,dir,parentUnit)
         implicit none
         type(eLine),intent(in) :: this
         real(cp) :: h
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "h: ", this%h
         else
           call newAndOpen(dir,"h")
           write(NewU,*) "h: ", this%h
           call closeAndMessage(NewU,"h")
         endif
       end subroutine

       subroutine writeElineK(this,dir,parentUnit)
         implicit none
         type(eLine),intent(in) :: this
         real(cp) :: k
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "k: ", this%k
         else
           call newAndOpen(dir,"k")
           write(NewU,*) "k: ", this%k
           call closeAndMessage(NewU,"k")
         endif
       end subroutine

       subroutine writeElineNormaloutward(this,dir,parentUnit)
         implicit none
         type(eLine),intent(in) :: this
         logical :: normalOutward
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
           write(NewU,*) "normalOutward: ", this%normalOutward
         else
           call newAndOpen(dir,"normalOutward")
           write(NewU,*) "normalOutward: ", this%normalOutward
           call closeAndMessage(NewU,"normalOutward")
         endif
       end subroutine

       subroutine writeEline(this,dir,parentUnit)
         implicit none
         type(eLine), intent(in) :: this
         character(len=*),intent(in) :: dir
         integer,optional :: parentUnit
         integer :: NewU
         if (present(parentUnit)) then
           NewU = parentUnit
         else
           NewU = newUnit()
           open(NewU,file=trim(dir) //"Eline.txt")
         endif
         call writeElineId(this,dir,NewU)
         call writeBcs(this%BC,dir,NewU)
         call writeElineA(this,dir,NewU)
         call writeElineB(this,dir,NewU)
         call writeElineH(this,dir,NewU)
         call writeElineK(this,dir,NewU)
         call writeElineNormaloutward(this,dir,NewU)
         if (.not. present(parentUnit)) then
           close(NewU)
           write(*,*) '+++ Data for eLine written to file +++'
         endif
       end subroutine

         include '../helper/eLine_helper.f'

       end module eLine_mod