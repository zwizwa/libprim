#!/usr/bin/env perl

use File::Basename;
use File::Compare;
use File::Copy;
use Cwd;

# set sefaults for all options

# the 3 paths below all need to be set if they are not defaults!

# these are INSTALL prefixes, and are not saved in any binaries
$var{prefix}  = "/usr/local";


$var{debug}   = "no";
$var{pthread} = "no";
$var{profile} = "no";
$var{static}  = "no";
$var{efence} = "no";

$var{ex} = "yes";
$var{sc} = "yes";
$var{pf} = "yes";

$var{silent}   = "yes";
$var{jobs}     = "1";
$var{finkpath} = "/sw";

# set defaults for all environment variables
# TODO: these are not saved in reconfigure
$var{cc}        = "cc";
$var{make}      = "make";

$var{static}    = "no";

# determine dirs. we use absolute paths for convenience
# this means that if you move the project, you need to re-run
# configure, or modify the SRCDIR and BUILDIR variables in Makefile.defs

$var{builddir} = $ENV{BUILDDIR};
$var{srcdir} = $ENV{SRCDIR}; 
$var{ldflags} = $ENV{LDFLAGS};

$LOGFILE = "$var{builddir}/build.log" ;


# set default target name
open UNAME, "uname|";
$target  = <UNAME>;
$target  =~ s/\n//;
close UNAME;

$var{target} = $target;

# default prefix is still /usr/local

#if ("Darwin" eq $target){
#   $var{prefix}="/sw";
#}

# perform a test in the pbuild/tests/ directory. return false on success.
sub troubles {
    my $testname = shift;
    my @args = ($var{make}, "-s", "-C", "test", "$testname\.test");
    my $retval = system (@args);
    return $retval;
}

sub errorexit {
    print "Test Failed. Check your build environment and/or configure flags.\n" ;
    print "See $LOGFILE for details.\n" ;
    exit(1);
}

# perform a required test
sub check_deps {
    my $thing = shift;
    my $error_msg = shift;
    print "checking $thing";
    my $retval = troubles($thing);
    if ($retval) { print $error_msg; }
    print "\n";
    return $retval;
}

# print valid options

sub print_options {
    foreach $key (keys %var){
	print "\t--$key=$var{$key}\n";
    }
}

# save the current variable state to reconfigure
sub save_reconfigure {
    chdir $var{builddir};
    open STATE, ">./reconfigure";
    print STATE "#/bin/bash\n";
    print STATE "cd `dirname \$0`\n"; # return to the builddir where reconfigure is located
    print STATE "exec $var{srcdir}/configure \\\n";
    foreach $key (keys %var){
	print STATE "\t--$key=$var{$key} \\\n";
    }
    print STATE "\t\$*\n";
    close STATE;
    chmod 0755, "./reconfigure";
}

sub do_help { 
    print "\navailable options with defaults: \n";
    print_options; 
    print "\n--enable|disable-<thing> is equivalent to --<thing>=yes|no\n";
    exit(0); 
}

# override with command line args
while ($a = shift) {
    if ($a eq "--help") { do_help; }
    elsif ($a =~ m/^--enable-(.+)/) { $var{$1} = "yes"; }
    elsif ($a =~ m/^--disable-(.+)/) { $var{$1} = "no"; }
    elsif ($a =~ m/^--(.+?)=(.+)/)  { $var{$1} = $2; }
    else  {print "invalid argument ".$a."\n"; do_help;}
}


# all options parsed: start config


if ($var{builddir} eq $var{srcdir}) {
    print "Building in source directory: " . $var{builddir} . "\n";
}
else {
    # mkdir "test"; cp $var{srcdir}
}


# create config files
chdir $var{builddir};
open PKGCONFIG, ">libprim.pc";
sub pkgconfig {
    my $line = shift;
    print PKGCONFIG $line . "\n";
}

# FIXME: this isn't correct for debian package
pkgconfig "home=$var{prefix}/share/prim";
pkgconfig "";
pkgconfig "Name: libprim";
pkgconfig "Description: libprim: a collection of primitive language objects + scripting languages.";
$VERSION=`$var{srcdir}/bin/version`;
pkgconfig "Version: $VERSION";


open LOCALENV, ">$var{builddir}/vars";
sub localenv {
    my $line = shift;
    print LOCALENV $line . "\n";
}

open MAKEFILE, ">Makefile.defs";
sub makefile {
    my $line = shift;
    print MAKEFILE $line . "\n";
}

# config tools
if ($ENV{CC}){$var{cc} = $ENV{CC};} # get CC, MAKE from environment if defined
if ($ENV{MAKE}){$var{make} = $ENV{MAKE};}


makefile "LDFLAGS = $var{ldflags}" ;
makefile "PREFIX = $var{prefix}" ;
makefile "SRCDIR = $var{srcdir}" ;
makefile "BUILDDIR = $var{builddir}" ;
makefile "CPPFLAGS += -I$var{builddir} -I$var{srcdir}";
makefile "LOG = $LOGFILE" ;
makefile "SDL_CONFIG = $var{sdl}-config" ;
makefile "CFLAGS += -fPIC" ;

makefile "CEXT = c";  # default is c. for cc see build/plugins/ftgl/Makefile
makefile "COMPILE = $CC";

makefile "MZSCHEME = mzscheme"; # FIXME: check this!

if ($var{jobs} > 1){
    makefile "MAKE += -j$var{jobs}"; # run multiple jobs
}

if ($var{static} eq "yes") {
    makefile "APP_LDFLAGS += -static";
}


open HEADER, ">config.h.new";
sub header {
    my $line = shift;    print HEADER $line . "\n";
}

header "#ifndef __PF_CONFIG__";
header "#define __PF_CONFIG__";
header "#define PRIM_HOME \"$var{prefix}/share/prim/\"";


# start tests and fill config files for base system
chdir "$var{builddir}/tests"; # dir where we will run the tests

# keep track of libs used for base package
# to fill pkgconfig file
@LIBS = (); 

sub check_base {
    push @LIBS, "-lm";
    makefile "LIBS += -lm" ; 
    if (check_deps "base-deps") {
	errorexit;
    }
}


sub check_dl {
    push @LIBS, "-ldl";
    makefile "LIBS += -ldl" ; 
    if (check_deps "libdl") {
	errorexit;
    }
}

sub toolchain {
    makefile "CC = $var{cc}" ;
    makefile "MAKE = $var{make}" ;
    if ($var{silent} eq "yes"){
	makefile "MAKE += -s"; # have a bit less verbose build
    }
}



# add prefix lib & include path 
makefile "LIBS += -L$var{prefix}/lib";
makefile "CPPFLAGS += -I$var{prefix}/include";

# link to pf lib
makefile "LDFLAGS_PF = -L$var{builddir}/libpf -lpf";
makefile "MODEXT = pfo";


# static modules
if ($var{static} eq "yes"){
    header "#define PF_STATIC";               # compile static modules
    makefile "COMPILE_TARGET = echo skipping CC";  # skip target compilation
}
else {
    makefile "COMPILE_TARGET = \$(CC)";    
}
makefile "COMPILE_COMPONENT = \$(CC)";


# check GCC
print "gcc:     ";
$gccver = `$var{cc} -dumpversion`;
chomp $gccver;
if (not $gccver){
    print "C compiler is not gcc.\n";
    exit(1);
}
@gccver = split '\.', $gccver;

if ($gccver[0] < 3){
    print "Need gcc version >= 3\n";
    exit(1);
}
elsif ($gccver[0] == 3){
    header "#define GCC3";
}

print $gccver . "\n";



# todo: include prefix etc
# goal: to compile run in non-root environment for testing
# this means locally some packages need to be installed
# testsuite = pack of libraries.

# platform specific setup
printf "target:  $var{target}\n";
if ("Linux" eq $var{target}){

    # on Darwin, lots of header files are not strict, so i do this only on linux.
    $EXTRA_DEBUG_CFLAGS="-Wstrict-prototypes";  
    $DLIB_PATH="LD_LIBRARY_PATH";
    makefile "DLEXT = so";
    makefile "LDFLAGS_SHARED = -shared";
    makefile "LDFLAGS_MODULE = -shared"; # same on linux
    makefile "OPT_MODULES += v4l rtc SDL";
    toolchain;
    check_base;
    check_dl;
}
elsif ("FreeBSD" eq $var{target}){

    $DLIB_PATH="LD_LIBRARY_PATH";
    $var{make}="gmake";
    makefile "DLEXT = so";
    makefile "LDFLAGS_SHARED = -shared";
    makefile "LDFLAGS_MODULE = -shared"; # same on linux
    toolchain;
    check_base;
}
elsif ("Darwin" eq $var{target}){

    $EXTRA_DEBUG_CFLAGS="-Wno-long-double";
    $DLIB_PATH="DYLD_LIBRARY_PATH";
    makefile "CPPFLAGS += -I$var{finkpath}/include -fpascal-strings";
    makefile "LIBS += -L$var{finkpath}/lib";
    makefile "DLEXT = dylib";
    makefile 'LDFLAGS_SHARED = -dynamiclib -install_name $(PREFIX)/lib/$(TARGET)';
    makefile "LDFLAGS_MODULE = -dynamic -bundle -flat_namespace -undefined suppress" ;
    makefile "CFLAGS += -fPIC -dynamic";
    makefile "OPT_MODULES += carbon";
    push @LIBS, "-L$var{finkpath}/lib";
    toolchain;
    check_base;
    check_dl;
}

# However, if you are building a dll as an export library, 
# you will probably want to use the complete syntax:

# gcc -shared -o cyg${module}.dll \
#    -Wl,--out-implib=lib${module}.dll.a \
#    -Wl,--export-all-symbols \
#    -Wl,--enable-auto-import \
#    -Wl,--whole-archive ${old_libs} \
#    -Wl,--no-whole-archive ${dependency_libs}

# The name of your library is ${module}, prefixed with cyg
# for the DLL and lib for the import library. Cygwin DLLs
# use the cyg prefix to differentiate them from native-Windows
# MinGW DLLs, see the MinGW website for more details. ${old_libs}
# are all your object files, bundled together in static libs or
# single object files and the ${dependency_libs} are import libs
# you need to link against, e.g '-lpng -lz -L/usr/local/special -lmyspeciallib'.

# TODO: use libtool, or add a platform dependent linker script

elsif ("CYGWIN_NT-5.0" eq $var{target}){
    makefile "DLEXT = dll";
    makefile "LDFLAGS_SHARED = -shared";
    makefile "LDFLAGS_MODULE = -shared";
    toolchain;
    check_base;
    #check_dl;  # dlopen is 'just there' on cygwin
}
else {
    printf "Target not supported.\n";
    errorexit();
}


# tools
 

# create file containing environment variables
localenv "export $DLIB_PATH=$var{builddir}/libpf";
localenv "export PRIM_HOME=$var{builddir}/lib/pf";
localenv "export PRIM_SRC=$var{srcdir}";
localenv "export PRIM_BUILD=$var{builddir}";

header "#ifndef HAVE_LIBDL";
header "#define HAVE_LIBDL";
header "#endif";



# DEBUG
if ($var{debug} eq "yes"){
    makefile "CC += $EXTRA_DEBUG_CFLAGS";
    makefile "DEBUG_CFLAGS = -g -Wall -Wno-unused -Wno-parentheses -Werror";
    # perantheses warning is actually quite valueable.. i still make mistakes to if (a = b) vs (a == b) things
    # but most of the code triggers warnings here.. clean up some day.
    header "#define PRIM_DEBUG 1";  # this is changed to not conflict with pf
}
else{
    makefile "OPTI_CFLAGS = -O3 -ffast-math -funroll-loops";
    if ($var{profile} eq "yes") {
	makefile "OPTI_CFLAGS += -pg -g -O3";
	makefile "LIBS += -pg";
    }
    else {
	makefile "OPTI_CFLAGS += -fomit-frame-pointer";
    }
    header "#define PRIM_DEBUG 0";
}

# PTHREAD
if ($var{pthread} eq "yes"){
    header "#define HAVE_PTHREAD";
    push @LIBS, "-lpthread";
    makefile "LIBS += -lpthread";
    makefile "OPT_MODULES += console";
    check_deps "libpthread";
}


# Optional builds
if ($var{media} eq "yes") { makefile "MEDIA = media" }; 
if ($var{pf} eq "yes")    { makefile "PF = pf" }; 
if ($var{sc} eq "yes")    { makefile "SC = sc" }; 
if ($var{ex} eq "yes")    { makefile "EX = ex" }; 
if ($var{jni} eq "yes")   { makefile "JNI = jni" };



pkgconfig "Libs: -L$var{prefix}/lib -lprim_sc -lprim_ex -lprim -lprim_leaf -lprim_media" . join " ", @LIBS;
pkgconfig "Cflags: -I$var{prefix}/include/prim";


# don't include efence in LIBS or pgkconfig
if ($var{efence} eq "yes"){
    makefile "DEBUG_LIBS += -lefence"; 
}

# 


header "#endif";
close HEADER;
close MAKEFILE;
close PGKCONFIG;
close FAKEPF;
save_reconfigure;


# only update header file if C defs have changed
# this is to speed up rebuilding a bit


if (compare("config.h.new","config.h") == 0) {
    # print "config.h did not change\n";
}
else {
    # print "updating config.h\n";
    copy("config.h.new","config.h");
}

print "Created reconfigure, libprim.pc, Makefile.defs, config.h\n";
print "Type 'make install' to build and install the package.\n\n";
exit(0);


