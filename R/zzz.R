.onAttach  <- function(libname, pkgname)
{
  packageStartupMessage("\n********************************************************")
  packageStartupMessage("Note: As of version 0.9.8, ggcor does not change the")
  packageStartupMessage("  default ggplot2 continuous fill scale anymore. To")
  packageStartupMessage("  recover the previous behavior, execute:")
  packageStartupMessage("    set_scale()\n")
  packageStartupMessage("  Instead of using the set_scale() function, we")
  packageStartupMessage("  recommend adding the 'scale_fill_*()' function")
  packageStartupMessage("  to the plot as needed.")
  packageStartupMessage("********************************************************\n")
}
