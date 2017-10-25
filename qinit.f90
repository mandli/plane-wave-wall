
subroutine qinit(meqn,mbc,mx,my,xlower,ylower,dx,dy,q,maux,aux)
    
    use qinit_module, only: qinit_type,add_perturbation
    use geoclaw_module, only: sea_level
    
    implicit none
    
    ! Subroutine arguments
    integer, intent(in) :: meqn,mbc,mx,my,maux
    real(kind=8), intent(in) :: xlower,ylower,dx,dy
    real(kind=8), intent(inout) :: q(meqn,1-mbc:mx+mbc,1-mbc:my+mbc)
    real(kind=8), intent(inout) :: aux(maux,1-mbc:mx+mbc,1-mbc:my+mbc)
    
    ! Locals
    integer :: i, j, m
    real(kind=8) :: x
    real(kind=8), parameter :: jump_location = -0.5d0
    real(kind=8), parameter :: deta = 0.2d0
    
    ! Set flat state based on sea_level
    q = 0.d0
    forall(i=1:mx, j=1:my)
        q(1,i,j) = max(0.d0, sea_level - aux(1,i,j))
    end forall

    do i = 1, mx
        x = xlower + (i + 0.5d0) * dx
        if (x < jump_location) then
            q(1, i, :) = q(1, i, :) + deta
        end if
    end do

    
end subroutine qinit
