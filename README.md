# SVG writer for Fortran

<div align="left">
<img src="./assets/svg-f.png" alt="SVG-Fortran" width="220">
</div>

# API
```fortran

class(svg_element), pointer :: p
class(svg_element), pointer :: child

call new_svg(p, kwargs...)
call p.set_attrs(...)

call new_rect(child, kwargs...)
call child.set_attrs(...)
call p.add_child(child)
! child is now unassociated
```

```fortran
! <svg>
! <g>
! <line/>
! </g>
! </svg>
class(svg_element), pointer :: p, g, l

call new_line(l, ...)

call new_group(g, ...)
call g.add_child(l)
! l is unassociated

call new_svg(p, ...)
call p.add_child(g)
! g is unassociated

```
