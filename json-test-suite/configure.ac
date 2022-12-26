dnl Process this file with autoconf to produce a configure script.

AC_PREREQ([2.50])

AC_INIT([json-test-suite], [1.0], [rs@ralph-schleicher.de])
AM_INIT_AUTOMAKE([foreign no-define])

AC_CONFIG_SRCDIR([run_tests.py])
AC_CONFIG_FILES([Makefile
		 parsers/Makefile
		 parsers/test_cl_json/Makefile
		 parsers/test_jonathan/Makefile
		 parsers/test_json_streams/Makefile
		 parsers/test_jsown/Makefile
		 parsers/test_shasht/Makefile
		 parsers/test_st_json/Makefile
		 parsers/test_yason/Makefile
		 parsers/test_yason_strict/Makefile])

AC_CHECK_PROGS([SBCL], [sbcl], [true])
AC_CHECK_PROGS([CCL], [ccl], [true])

AC_SUBST([PACKAGE_NAME])
AC_SUBST([PACKAGE_VERSION])
AC_SUBST([PACKAGE_STRING])
AC_SUBST([PACKAGE_BUGREPORT])
AC_SUBST([PACKAGE_TARNAME])

AC_OUTPUT
