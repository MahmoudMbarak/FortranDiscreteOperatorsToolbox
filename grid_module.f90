module grid_module
  implicit none

  type :: grid_type
    real :: xmin, xmax, Lx, dx
    integer :: Nx, Nfx
    real, allocatable :: xc(:), xf(:)
    integer, allocatable :: dof(:)
    integer :: dof_xmin, dof_xmax, dof_f_xmin, dof_f_xmax
  end type grid_type

  type :: boundary_type
    integer, allocatable :: dof_dir(:), dof_neu(:), dof_f_dir(:), dof_f_neu(:)
    real, allocatable :: qb(:), g(:)
  end type boundary_type

contains

  subroutine build_grid(grid)
    type(grid_type), intent(inout) :: grid
    integer :: i

    grid%Lx = grid%xmax - grid%xmin
    grid%dx = grid%Lx / grid%Nx
    grid%Nfx = grid%Nx + 1

    allocate(grid%xc(grid%Nx), grid%xf(grid%Nfx), grid%dof(grid%Nx))

    do i = 1, grid%Nx
      grid%xc(i) = grid%xmin + (i - 0.5) * grid%dx
    end do

    do i = 1, grid%Nfx
      grid%xf(i) = grid%xmin + (i - 1) * grid%dx
    end do

    grid%dof = [(i, i = 1, grid%Nx)]
    grid%dof_xmin = minval(grid%dof)
    grid%dof_xmax = maxval(grid%dof)
    grid%dof_f_xmin = 1
    grid%dof_f_xmax = grid%Nfx
  end subroutine build_grid

end module grid_module
