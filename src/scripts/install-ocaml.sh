#! /bin/sh
################################################################################
# Commands to install OCaml and related libraries as needed for NYU
# projects.
#
# Desired usage:
#
#     install-ocaml.sh (bowery | rabbot | ashish | sebastien | wso) /installation/path
#
# but unfortunately it is difficult to fully automate the
# installation. More realistically, this script serves as a guide to
# commands that will need to be entered manually.

# variables used throughout script
if [ $# -ne 2 ] ; then
    echo "usage: $0 <computer-flavor> <install-path>"
    echo "where computer-flavor may be bowery, rabbot, ashish, sebastien, or wso." 
    exit 2
fi
COMPUTER=$1
GODI_PREFIX=$2
SCRATCH=/tmp/install-ocaml-$$
rm -fr $SCRATCH
mkdir -p $SCRATCH

#DATETAG=`date +%Y%m%d`
#export GODI_PREFIX=$HOME/usr/godi$DATETAG # the installation directory for GODI
export PATH=$GODI_PREFIX/sbin:$GODI_PREFIX/bin:$PATH

if [ "$COMPUTER" = "bowery" ]; then
    unset ARCHIVE
    . /share/apps/git/1.7.6.3/intel/env.sh
    module add postgresql
fi
if [ "$COMPUTER" = "rabbot" ]; then
    module () { 
        eval `/opt/modules/bin/modulecmd bash $*`
    }
    module load git
    module load postgresql
fi

function mktmp {
    if [ "$COMPUTER" = "ashish" ]; then
        local a=`mktemp /tmp/tmp.XXXXXXXXXX`
    else
        local a=`mktemp`
    fi
    echo $a
}

do_smth () {
    tmp1=`mktmp`
    $1 > $tmp1 2>&1
    if [ $? -ne 0 ]; then
        echo "$1 ---> Not 0 (`cat $tmp1`)"
        tmp3=`mktmp`
        eval "$2" > $tmp3 2>&1
        tmp2=`mktmp`
        $1 > $tmp2 2>&1
        if [ $? -ne 0 ]; then
            echo "$1 ---> Not 0 Again (`cat $tmp2`)\n  and $2 said\n (`cat $tmp3`)\n"
            exit 2
        else
            echo "$1 ----> SUCCESS!"
        fi
    else
        echo "$1  ------>  OK!"
    fi
}

root_method="sudo"
if [ "$COMPUTER" = "bowery" ]; then
    root_method="echo"
fi
if [ "$COMPUTER" = "rabbot" ]; then
    root_method="echo"
fi
do_smth "$GODI_PREFIX/sbin/godi_perform -help" "
    rm -rf $GODI_PREFIX
    cd $SCRATCH
    wget http://download.camlcity.org/download/godi-rocketboost-20110811.tar.gz
    tar xzvf godi-rocketboost-20110811.tar.gz
    cd godi-rocketboost-20110811

    ./bootstrap --prefix=$GODI_PREFIX --section 3.12 --batch --root-method $root_method
    "

# if [ $COMPUTER = "bowery" ]; then
#     When prompted with message regarding missing ncurses package,
#     simply select option 4 and continue without installing it. Things
#     work anyway for some reason.
# fi

do_smth "ledit -v" " godi_perform -build apps-ledit "

#godi_perform -build apps-oasis
do_smth "ocamlfind query oUnit" "godi_perform -build godi-ounit"

do_smth "ocamlfind query pgocaml" "godi_perform -build godi-pgocaml"

do_smth "ocamlscript -version" "godi_perform -build godi-ocamlscript"

do_smth "ocamlfind query xmlm" "godi_perform -build godi-xmlm"
do_smth "ocamlfind query pxp" "godi_perform -build godi-pxp"

do_smth "ocamlfind query getopt" "godi_perform -build godi-getopt"

do_smth "ocamlfind query bisect"  "godi_perform -build godi-bisect"   # for batteries
do_smth "ocamlfind query res" "godi_perform -build godi-res"      # for core

do_smth "ocamlfind query odn" "godi_perform -build godi-ocaml-data-notation"


do_smth "ocamlmod -help" "godi_perform -build apps-ocamlmod"

do_smth "ocamlify -help"  "godi_perform -build apps-ocamlify"

do_smth "ocamlfind query zip" "
godi_perform -build godi-zip
mkdir -p  $GODI_PREFIX/lib/ocaml/pkg-lib/zip
echo 'directory = \"$GODI_PREFIX/lib/ocaml/pkg-lib/camlzip\" ' > \
          $GODI_PREFIX/lib/ocaml/pkg-lib/zip/META
"

do_smth "oasis version" "
# install oasis
cd $SCRATCH
wget http://cims.nyu.edu/~agarwal/download/oasis-6c6b33b2f6b7fe84edd384641ba334ce.tgz
tar xzvf oasis-6c6b33b2f6b7fe84edd384641ba334ce.tgz
cd oasis-6c6b33b2f6b7fe84edd384641ba334ce
ocaml setup.ml -configure --prefix $GODI_PREFIX --enable-docs --enable-libraries
ocaml setup.ml -build
ocaml setup.ml -reinstall
"

# install cmdliner
do_smth "ocamlfind query cmdliner" "
cd $SCRATCH
wget http://erratique.ch/software/cmdliner/releases/cmdliner-0.9.1.tbz
tar xjvf cmdliner-0.9.1.tbz
cd cmdliner-0.9.1
ocaml setup.ml -configure --prefix $GODI_PREFIX
ocaml setup.ml -build
ocaml setup.ml -install
"

# install ocaml-sqlite3
if [ "$COMPUTER" = "bowery" ]; then
do_smth "ocamlfind query sqlite3" "
    module load sqlite
    export C_INCLUDE_PATH=/share/apps/sqlite/3.7.7/intel/include
    export LIBRARY_PATH=/share/apps/sqlite/3.7.7/intel/lib
    godi_perform -build godi-sqlite3 && \
    echo 'linkopts = \"-cclib -L/share/apps/sqlite/3.7.7/intel/lib\"' >> $GODI_PREFIX/lib/ocaml/pkg-lib/sqlite3/META
"
else
do_smth "ocamlfind query sqlite3" "
    godi_perform -build godi-sqlite3
"
fi


# install otags
do_smth "otags -version" "
cd $SCRATCH
wget http://askra.de/software/otags/otags-3.12.5.tar.gz
tar xzvf otags-3.12.5.tar.gz
cd otags-3.12.5
./configure --prefix $GODI_PREFIX
make all
make install
"
# install batteries
do_smth "ocamlfind query batteries" "
cd $SCRATCH
git clone git://github.com/ocaml-batteries-team/batteries-included.git
cd batteries-included/
git checkout -b nyu-compatible 91a9464bf7f2d153eeaa576fe232ab2ba0eb5fc2 && \
ocaml setup.ml -configure --prefix $GODI_PREFIX && \
make && \
make install
"

# install core
CORE_URL=https://ocaml.janestreet.com/ocaml-core/108.00.02/core-suite-108.00.02.tar.gz
do_smth "ocamlfind query async" "
ocamlfind remove -destdir $GODI_PREFIX/lib/ocaml/pkg-lib/ type_conv
cd $SCRATCH
wget --no-check-certificate $CORE_URL
tar xzvf core-suite-108.00.02.tar.gz
cd ocaml-core-108.00.02
./build-and-install
"

if [ "$COMPUTER" = "sebastien" ] ; then
    if [ `dpkg -l | egrep 'ii *libev-dev' | wc -l` -ne 1 ] ; then
        echo 'Please install libev-dev and restart this'
        echo '===='
        exit 2
    fi
fi 
if [ "$COMPUTER" = "wso" ] ; then
    if ls /usr/include/libev/ev.h ; then
        echo 'Found libev!'
    else
        echo 'Please install libev-devel and restart this'
        echo '===='
        exit 2
    fi
    export C_INCLUDE_PATH=/usr/include/libev
    export LIBRARY_PATH=/usr/lib 
fi

if [ "$COMPUTER" = "sebastien" ] || [ "$COMPUTER" = "wso" ] ; then
    do_smth "ocamlfind query ssl" "godi_perform -build godi-ocaml-ssl"
    do_smth "ocamlfind query text" "godi_perform -build godi-ocaml-text"

#    do_smth "ocamlfind query lwt" "
#    echo GODI_LWT_GLIB=no >> $GODI_PREFIX/etc/godi.conf
#    godi_perform -build godi-lwt
#    "

    do_smth "ocamlfind query react" "godi_perform -build godi-react"
    do_smth "ocamlfind query cryptokit" "godi_perform -build godi-cryptokit"
    

    OCSIGEN_BUNDLE=ocsigen-bundle-2.2.2
    OCSIGEN_URL=http://ocsigen.org/download/$OCSIGEN_BUNDLE.tar.gz
    do_smth "ocamlfind query eliom" "
      cd $SCRATCH
      wget $OCSIGEN_URL
      tar xvfz $OCSIGEN_BUNDLE.tar.gz
      cd $OCSIGEN_BUNDLE
      ./configure --prefix $GODI_PREFIX \
         OCSIGEN_USER=${USER} OCSIGEN_GROUP=${USER}  \
         --enable-oclosure --with-ocamldsort
      echo 'SITELIB := \${exec_prefix}/lib/ocaml/site-lib' > Makefile.local
      echo 'LDCONF  := \${exec_prefix}/etc/ld.conf' >> Makefile.local
      echo 'STUBDIR := \${SITELIB}/stublibs' >> Makefile.local
      make pull
      make
      make install
    "
fi

if [ "$COMPUTER" = "bowery" ]; then
   do_smth "ls $GODI_PREFIX/include/ev.h" "
    cd $SCRATCH
    wget http://dist.schmorp.de/libev/libev-4.11.tar.gz && \
    tar xzvf libev-4.11.tar.gz && \
    cd libev-4.11 && \
    ./configure --prefix=$GODI_PREFIX && \
    make && \
    make install
   "
   do_smth "ocamlfind query ssl" "
    cd $SCRATCH
    wget http://sourceforge.net/projects/savonet/files/ocaml-ssl/0.4.6/ocaml-ssl-0.4.6.tar.gz/download && \
    tar xzvf ocaml-ssl-0.4.6.tar.gz && \
    cd ocaml-ssl-0.4.6 && \
    ./configure --prefix $GODI_PREFIX LDFLAGS=-L/share/apps/openssl/1.0.0d/gnu/lib CFLAGS=-I/share/apps/openssl/1.0.0d/gnu/include && \
    make && \
    make install && \
    echo 'linkopts = \"-cclib -L/share/apps/openssl/1.0.0d/gnu/lib\"' >> $GODI_PREFIX/lib/ocaml/site-lib/ssl/META
   "
   LWT_VERSION="2.4.1"
   do_smth "ocamlfind query lwt" "
    cd $SCRATCH
    wget http://ocsigen.org/download/lwt-$LWT_VERSION.tar.gz && \
    tar xzvf lwt-$LWT_VERSION.tar.gz && \
    cd lwt-$LWT_VERSION && \
    export C_INCLUDE_PATH=$GODI_PREFIX/include && \
    export LIBRARY_PATH=$GODI_PREFIX/lib && \
    ./configure --prefix $GODI_PREFIX --enable-ssl && \
    make  && \
    make install
   "
fi

if [ "$COMPUTER" = "rabbot" ]; then
   do_smth "ls $GODI_PREFIX/include/ev.h" "
    cd $SCRATCH
    wget http://dist.schmorp.de/libev/libev-4.11.tar.gz && \
    tar xzvf libev-4.11.tar.gz && \
    cd libev-4.11 && \
    ./configure --prefix=$GODI_PREFIX && \
    make && \
    make install
   "
   do_smth "ocamlfind query ssl" "godi_perform -build godi-ocaml-ssl"

   LWT_VERSION="2.4.1"
   do_smth "ocamlfind query lwt" "
    cd $SCRATCH
    wget http://ocsigen.org/download/lwt-$LWT_VERSION.tar.gz && \
    tar xzvf lwt-$LWT_VERSION.tar.gz && \
    cd lwt-$LWT_VERSION && \
    export C_INCLUDE_PATH=$GODI_PREFIX/include && \
    export LIBRARY_PATH=$GODI_PREFIX/lib && \
    ./configure --prefix $GODI_PREFIX --enable-ssl && \
    make  && \
    make install
   "
fi
echo "SCRATCH was $SCRATCH"

################################################################################
# Official end of the script
exit 2

# Here follow some notes from previous configurations or for other computers;

# install ocaml-ssl and lwt
if [ $COMPUTER="bowery" ]; then
    # install libev, needed for lwt
    cd $SCRATCH
    wget http://dist.schmorp.de/libev/libev-4.11.tar.gz
    tar xzvf libev-4.11.tar.gz
    cd libev-4.11
    ./configure --prefix=$GODI_PREFIX
    make
    make install

    cd $SCRATCH
    wget http://sourceforge.net/projects/savonet/files/ocaml-ssl/0.4.6/ocaml-ssl-0.4.6.tar.gz/download
    tar xzvf ocaml-ssl-0.4.6.tar.gz
    cd ocaml-ssl-0.4.6
    ./configure --prefix $GODI_PREFIX LDFLAGS=-L/share/apps/openssl/1.0.0d/gnu/lib CFLAGS=-I/share/apps/openssl/1.0.0d/gnu/include
    make
    make install

    echo linkopts = \"-cclib -L/share/apps/openssl/1.0.0d/gnu/lib\" >> $GODI_PREFIX/lib/ocaml/site-lib/ssl/META

    cd $SCRATCH
    wget http://ocsigen.org/download/lwt-2.4.1.tar.gz
    tar xzvf lwt-2.3.2.tar.gz
    cd lwt-2.3.2
    export C_INCLUDE_PATH=$GODI_PREFIX/include
    export LIBRARY_PATH=$GODI_PREFIX/lib
    ./configure --prefix $GODI_PREFIX --enable-ssl
    make 
    make install

elif [ $COMPUTER = "rabbot" ]; then
    godi_perform -build godi-ocaml-ssl

    echo GODI_LWT_GLIB=no >> $GODI_PREFIX/etc/godi.conf
    echo GODI_LWT_OCAMLTEXT=no >> $GODI_PREFIX/etc/godi.conf

    export C_INCLUDE_PATH=$GODI_PREFIX/include
    export LIBRARY_PATH=$GODI_PREFIX/lib
    godi_perform -build godi-lwt

elif [ $COMPUTER = "ashish" ]; then
    godi_perform -build godi-ocaml-ssl

    echo GODI_LWT_GLIB=no >> $GODI_PREFIX/etc/godi.conf
    echo GODI_LWT_OCAMLTEXT=no >> $GODI_PREFIX/etc/godi.conf

    cd $SCRATCH
    wget http://ocsigen.org/download/lwt-2.3.2.tar.gz
    tar xzvf lwt-2.3.2.tar.gz
    cd lwt-2.3.2
    export C_INCLUDE_PATH=$GODI_PREFIX/include
    export LIBRARY_PATH=$GODI_PREFIX/lib
    ./configure --prefix $GODI_PREFIX --enable-ssl
    make 
    make install

fi

# remove temporary files
cd $HOME
rm -rf $SCRATCH
