#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.69])
AC_INIT([divest], [0.3.0], [code@clayden.org])
# AC_CONFIG_SRCDIR([ImageList.h])

# Get compiler flags from R
: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
    AC_MSG_ERROR([Could not determine R_HOME.])
fi
CXX=`"${R_HOME}/bin/R" CMD config CXX`
CXXFLAGS=`"${R_HOME}/bin/R" CMD config CXXFLAGS`
CPPFLAGS=`"${R_HOME}/bin/R" CMD config CPPFLAGS`

AC_LANG(C++)

# Checks for programs.
AC_PROG_CXX
AC_PROG_CC

# Checks for OpenJPEG.
JPEG2K_FLAGS="-DmyDisableOpenJPEG"
JPEG2K_LIBS=""
AC_SEARCH_LIBS([opj_version], [openjp2], [
    AC_CHECK_HEADER([openjpeg-2.1/openjpeg.h], [
        JPEG2K_FLAGS=""
        JPEG2K_LIBS="-lopenjp2"
        AC_MSG_NOTICE(Using OpenJPEG for JPEG2000 support)
    ])
])

# Checks for JasPer (if OpenJPEG was not found).
if test -z "${JPEG2K_LIBS}"; then
    AC_SEARCH_LIBS([jas_getversion], [jasper], [
        AC_CHECK_HEADER([jasper/jasper.h], [
            JPEG2K_FLAGS="-DmyEnableJasper -DmyDisableOpenJPEG"
            JPEG2K_LIBS="-ljasper"
            AC_MSG_NOTICE(Using JasPer for JPEG2000 support)
        ])
    ])
fi

AC_SUBST(JPEG2K_FLAGS)
AC_SUBST(JPEG2K_LIBS)

AC_CONFIG_FILES([src/Makevars])
AC_OUTPUT
