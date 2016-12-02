.onLoad <- function (libname, pkgname)
{
    .Call("initialise", PACKAGE="divest")
}
