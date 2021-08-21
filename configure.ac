dnl AC_PREREQ([2.67])

AC_INIT([h2],[0.1.0],[Chung, Hyung-Hwan (hyunghwan.chung@gmail.com)],[],[http://code.miflux.com/@h2])

AC_CONFIG_HEADER([h2-cfg.h])
AC_CONFIG_AUX_DIR([ac])
AC_CONFIG_MACRO_DIR([m4])

dnl use CC and CPP for compilation tests with .c test programs.
AC_LANG(C)

dnl initialize automake
AM_INIT_AUTOMAKE([-Wall -Werror foreign])
AM_PROG_AR

dnl enable silent rules if automake supports them.
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([no])])
dnl m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

dnl determine a C compiler to use
AC_PROG_CC

dnl determine some key programs
AC_PROG_INSTALL
AC_PROG_EGREP
AC_PROG_LN_S

dnl initialize libtool
LT_INIT
AC_SUBST(LIBTOOL_DEPS)

dnl overrides the default CFLAGS setting
if test "$ac_test_CFLAGS" != "set"
then
	if test "$GCC" = "yes"
	then
		CFLAGS="$CFLAGS -Wall"
	fi
fi

if test "$ac_test_CXXFLAGS" != "set"
then
	if test "$GCC" = "yes"
	then
		CXXFLAGS="$CXXFLAGS -Wall"
	fi
fi

dnl indicate the existence of config.h
CFLAGS="$CFLAGS -DH2_HAVE_CONFIG_H"
CXXFLAGS="$CXXFLAGS -DH2_HAVE_CONFIG_H"

dnl make visible the 64-bit interface to the file system
CFLAGS="$CFLAGS -D_LARGEFILE64_SOURCE"
CXXFLAGS="$CXXFLAGS -D_LARGEFILE64_SOURCE"

dnl define the WIN32 conditional if necessary
case "$host" in
*-*-mingw*|*-*-cygwin*)
	#CFLAGS="$CFLAGS -D_WIN32_WINNT=0x0400"
	platform_win32=yes
	;;
*)
	platform_win32=no
	;;
esac
AM_CONDITIONAL(WIN32, test "${platform_win32}" = "yes" )

dnl check the math library (check if -lm is needed)
LT_LIB_M
AC_SUBST(LIBM, $LIBM)

dnl check header files.
AC_HEADER_STDC

dnl check functions
dnl AC_CHECK_FUNCS([_vsnprintf _vsnwprintf])


dnl check is the import library for unicows.dll exists
dnl this check doesn't look for a particular symbol
dnl but for the symbol "main" since i want to check 
dnl the existence of the libaray.
AC_CHECK_LIB([unicows], [main], [UNICOWS_LIBS="-lunicows"])
AC_SUBST(UNICOWS_LIBS)

AC_MSG_CHECKING([for va_copy])
AC_TRY_LINK(
	[#include <stdarg.h>],
	[
		va_list x, y;
		va_copy(x,y);
	],
	[
		AC_MSG_RESULT(yes)
		AC_DEFINE([HAVE_VA_COPY], [1], [va_copy is available])
	],
	[AC_MSG_RESULT(no)]
)

AC_MSG_CHECKING([for __va_copy])
AC_TRY_LINK(
	[#include <stdarg.h>],
	[
		va_list x, y;
		__va_copy(x,y); 
	],
	[
		AC_MSG_RESULT(yes)
		AC_DEFINE([HAVE___VA_COPY], [1], [__va_copy is available])
	],
	[AC_MSG_RESULT(no)]
)

dnl check the size of primitive data types
AC_CHECK_SIZEOF(char,,[[]])
AC_CHECK_SIZEOF(short,,[[]])
AC_CHECK_SIZEOF(int,,[[]])
AC_CHECK_SIZEOF(long,,[[]])
AC_CHECK_SIZEOF(long long,,[[]])
AC_CHECK_SIZEOF(__int8,,[[]])
AC_CHECK_SIZEOF(__int16,,[[]])
AC_CHECK_SIZEOF(__int32,,[[]])
AC_CHECK_SIZEOF(__int64,,[[]])
AC_CHECK_SIZEOF(__int128,,[[]])
AC_CHECK_SIZEOF(__int8_t,,[[]])
AC_CHECK_SIZEOF(__int16_t,,[[]])
AC_CHECK_SIZEOF(__int32_t,,[[]])
AC_CHECK_SIZEOF(__int64_t,,[[]])
AC_CHECK_SIZEOF(__int128_t,,[[]])
dnl AC_CHECK_SIZEOF doesn't work without white-space between void and *
AC_CHECK_SIZEOF(void *,,[[]])
AC_CHECK_SIZEOF(float,,[[]])
AC_CHECK_SIZEOF(double,,[[]])
AC_CHECK_SIZEOF(long double,,[[]])
AC_CHECK_SIZEOF(__float128,,[[]])

AC_CHECK_SIZEOF(wchar_t)
AC_CHECK_SIZEOF(off_t)
AC_CHECK_SIZEOF(off64_t)

AC_DEFINE_UNQUOTED(H2_SIZEOF_CHAR, ${ac_cv_sizeof_char}, [sizeof(char)])
AC_DEFINE_UNQUOTED(H2_SIZEOF_SHORT, ${ac_cv_sizeof_short}, [sizeof(short)])
AC_DEFINE_UNQUOTED(H2_SIZEOF_INT, ${ac_cv_sizeof_int}, [sizeof(int)])
AC_DEFINE_UNQUOTED(H2_SIZEOF_LONG, ${ac_cv_sizeof_long}, [sizeof(long)])
AC_DEFINE_UNQUOTED(H2_SIZEOF_LONG_LONG, ${ac_cv_sizeof_long_long}, [sizeof(long long)])
AC_DEFINE_UNQUOTED(H2_SIZEOF___INT8, ${ac_cv_sizeof___int8}, [sizeof(__int8)])
AC_DEFINE_UNQUOTED(H2_SIZEOF___INT16, ${ac_cv_sizeof___int16}, [sizeof(__int16)])
AC_DEFINE_UNQUOTED(H2_SIZEOF___INT32, ${ac_cv_sizeof___int32}, [sizeof(__int32)])
AC_DEFINE_UNQUOTED(H2_SIZEOF___INT64, ${ac_cv_sizeof___int64}, [sizeof(__int64)])
AC_DEFINE_UNQUOTED(H2_SIZEOF___INT128, ${ac_cv_sizeof___int128}, [sizeof(__int128)])
AC_DEFINE_UNQUOTED(H2_SIZEOF___INT128_T, ${ac_cv_sizeof___int128_t}, [sizeof(__int128_t)])
AC_DEFINE_UNQUOTED(H2_SIZEOF_VOID_P, ${ac_cv_sizeof_void_p}, [sizeof(void*)])
AC_DEFINE_UNQUOTED(H2_SIZEOF_FLOAT, ${ac_cv_sizeof_float}, [sizeof(float)])
AC_DEFINE_UNQUOTED(H2_SIZEOF_DOUBLE, ${ac_cv_sizeof_double}, [sizeof(double)])
AC_DEFINE_UNQUOTED(H2_SIZEOF_LONG_DOUBLE, ${ac_cv_sizeof_long_double}, [sizeof(long double)])
AC_DEFINE_UNQUOTED(H2_SIZEOF___FLOAT128, ${ac_cv_sizeof___float128}, [sizeof(__float128)])

AC_DEFINE_UNQUOTED(H2_SIZEOF_OFF_T, ${ac_cv_sizeof_off_t}, [sizeof(off_t)])
AC_DEFINE_UNQUOTED(H2_SIZEOF_OFF64_T, ${ac_cv_sizeof_off64_t}, [sizeof(off64_t)])

h2_package_version_major="`echo ${PACKAGE_VERSION} | cut -d. -f1`"
h2_package_version_minor="`echo ${PACKAGE_VERSION} | cut -d. -f2`"
h2_package_version_patch="`echo ${PACKAGE_VERSION} | cut -d. -f3`"
AC_SUBST(PACKAGE_VERSION_MAJOR, ${h2_package_version_major})
AC_SUBST(PACKAGE_VERSION_MINOR, ${h2_package_version_minor})
AC_SUBST(PACKAGE_VERSION_PATCH, ${h2_package_version_patch})

AC_DEFINE_UNQUOTED(H2_PACKAGE_VERSION, "${PACKAGE_VERSION}", [Package version])
dnl AC_DEFINE_UNQUOTED(H2_PACKAGE_VERSION_MAJOR, $(echo ${VERSION} | cut -d. -f1), [Major version number])
dnl AC_DEFINE_UNQUOTED(H2_PACKAGE_VERSION_MINOR, $(echo ${VERSION} | cut -d. -f2), [Minor version number])
dnl AC_DEFINE_UNQUOTED(H2_PACKAGE_VERSION_PATCH, $(echo ${VERSION} | cut -d. -f3), [Patch level])
AC_DEFINE_UNQUOTED(H2_PACKAGE_VERSION_MAJOR, ${h2_package_version_major}, [Major version number])
AC_DEFINE_UNQUOTED(H2_PACKAGE_VERSION_MINOR, ${h2_package_version_minor}, [Minor version number])
AC_DEFINE_UNQUOTED(H2_PACKAGE_VERSION_PATCH, ${h2_package_version_patch}, [Patch level])

AC_DEFINE_UNQUOTED(H2_PACKAGE_AUTHOR,"${PACKAGE_BUGREPORT}", [Author])
AC_DEFINE_UNQUOTED(H2_PACKAGE_NAME,"${PACKAGE_NAME}", [package name])
AC_DEFINE_UNQUOTED(H2_PACKAGE_URL, "${PACKAGE_URL}", [Project URL])

AC_C_BIGENDIAN(
	[AC_DEFINE([H2_ENDIAN_BIG],[],[Big Endian])],
	[AC_DEFINE([H2_ENDIAN_LITTLE],[],[Little Endian])],
	[AC_DEFINE([H2_ENDIAN_UNKNOWN],[],[Unknown Endian])])

AC_ARG_WITH([char-type],
	[AS_HELP_STRING(--with-char-type,[specify the default character type. one of mchar, wchar, wwchar])],
	[char_type=$withval]
)
if test "${char_type}" = "mchar"
then
	AC_DEFINE([H2_CHAR_IS_MCHAR],[],[Define h2_char_t to h2_mchar_t])
	AC_SUBST(CHAR_TYPE, "H2_CHAR_IS_MCHAR")
elif test "$char_type" = "wchar" 
then
	AC_DEFINE([H2_CHAR_IS_WCHAR],[],[Define h2_char_t to h2_wchar_t])
	AC_SUBST(CHAR_TYPE, "H2_CHAR_IS_WCHAR")
elif test "$char_type" = "wwchar"
then
	AC_DEFINE([H2_CHAR_IS_WWCHAR],[],[Define h2_char_t to h2_wwchar_t])
	AC_SUBST(CHAR_TYPE, "H2_CHAR_IS_WWCHAR")
else
	AC_MSG_ERROR([The default character type(--with-char-type) must be one of mchar, wchar, wwchar and it must be enabled.])
fi

AC_ARG_WITH([ada-objdir],
	[AS_HELP_STRING(--with-ada-objdir,specify ada object directory path)],
	[ada_objdir=$withval]
)
if test -n "$ada_objdir"
then
        AC_SUBST(ADA_OBJDIR, "$ada_objdir")
else
        AC_SUBST(ADA_OBJDIR, "objdir")
fi

if test "${enable_shared}" = "no" -a "${enable_static}" = "yes"
then
	AC_DEFINE([H2_ENABLE_STATIC_MODULE],[],[link modules statically into the main library])
fi
AM_CONDITIONAL(ENABLE_STATIC_MODULE, test "${enable_shared}" = "no" -a "${enable_static}" = "yes")

AC_SUBST(H2_SIZEOF_LONG_LONG, $ac_cv_sizeof_long_long)
AC_SUBST(H2_SIZEOF_LONG, $ac_cv_sizeof_long)
AC_SUBST(H2_SIZEOF_INT, $ac_cv_sizeof_int)
AC_SUBST(H2_SIZEOF_SHORT, $ac_cv_sizeof_short)
AC_SUBST(H2_SIZEOF_CHAR, $ac_cv_sizeof_char)
AC_SUBST(H2_SIZEOF_VOID_P, $ac_cv_sizeof_void_p)
AC_SUBST(H2_SIZEOF_FLOAT, $ac_cv_sizeof_float)
AC_SUBST(H2_SIZEOF_DOUBLE, $ac_cv_sizeof_double)
AC_SUBST(H2_SIZEOF_LONG_DOUBLE, $ac_cv_sizeof_long_double)
AC_SUBST(H2_SIZEOF_OFF_T, $ac_cv_sizeof_off_t)
AC_SUBST(H2_SIZEOF_OFF64_T, $ac_cv_sizeof_off64_t)

AC_SUBST(H2_PROJECT_AUTHOR, "${PACKAGE_BUGREPORT}")
AC_SUBST(H2_PROJECT_URL, "${PACKAGE_URL}")


AC_CONFIG_FILES([
	Makefile
	lib/Makefile
	lib/lib.gpr
	lib/libh2.gpr
	bin/Makefile
	bin/scheme.gpr
])

AC_OUTPUT

[
echo 
echo "-[SUMMARY]---------------------------------------------------------------"
echo "Configured for ${host}"
echo "  Build mode : ${BUILD_MODE}"
echo "  Source directory: ${srcdir}"
echo "  Installation directory: ${prefix}"
echo "  C compiler: ${CC} ${CFLAGS}"
echo "  Math library: ${LIBM}"
echo "-------------------------------------------------------------------------"
]
