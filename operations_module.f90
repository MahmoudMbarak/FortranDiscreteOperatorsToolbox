module operations_module
  use grid_module
  implicit none

contains

  subroutine build_ops(grid, D, G, I, M)
    type(grid_type), intent(in) :: grid
    real, allocatable, intent(out) :: D(:,:), G(:,:), I(:,:), M(:,:)
    integer :: ii

    allocate(D(grid%Nx, grid%Nfx), G(grid%Nfx, grid%Nx), I(grid%Nx, grid%Nx), M(grid%Nfx, grid%Nx))
    D = 0.0; G = 0.0; I = 0.0; M = 0.0

    do ii = 1, grid%Nx
      D(ii,ii) = -1.0 / grid%dx
      D(ii,ii+1) = 1.0 / grid%dx
    end do

    G = -transpose(D)
    G(1,:) = 0.0
    G(grid%Nfx,:) = 0.0

    do ii = 1, grid%Nx
      I(ii,ii) = 1.0
    end do

    do ii = 1, grid%Nx
      M(ii,ii) = 0.5
      M(ii+1,ii) = 0.5
    end do
    M(1,1) = 1.0
    M(grid%Nfx, grid%Nx) = 1.0
  end subroutine build_ops

end module operations_module
