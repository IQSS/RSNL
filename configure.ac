AC_PREREQ(2.50)
AC_INIT([DESCRIPTION])

: ${R_HOME=`R RHOME`}
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CC=`${R_HOME}/bin/R CMD config CC`

AC_PROG_CC

AC_OUTPUT
