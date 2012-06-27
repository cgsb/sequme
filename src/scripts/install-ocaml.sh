######################################################################
# Commands to install OCaml and related libraries as needed for NYU
# projects.
#
# Desired usage:
#
#     install-ocaml.sh (bowery | rabbot | ashish | sebastien)
#
# but unfortunately it is difficult to fully automate the
# installation. More realistically, this script serves as a guide to
# commands that will need to be entered manually.

# variables used throughout script
COMPUTER=$1
OCAMLPREFIX=$HOME/usr/godi312 # the installation directory
SCRATCH=/tmp/install-ocaml
mkdir -p $SCRATCH
PATH=$OCAMLPREFIX/sbin:$OCAMLPREFIX/bin:$PATH

# cleanup to start from scratch
rm -rf $OCAMLPREFIX


if [ $COMPUTER = "bowery" ]; then
    unset ARCHIVE
fi

# install godi
cd $SCRATCH
wget http://download.camlcity.org/download/godi-rocketboost-20110811.tar.gz
tar xzvf godi-rocketboost-20110811.tar.gz
cd godi-rocketboost-20110811

./bootstrap --prefix=$OCAMLPREFIX
# if [ $COMPUTER = "bowery" ]; then
#     When prompted with message regarding missing ncurses package,
#     simply select option 4 and continue without installing it. Things
#     work anyway for some reason.
# fi

godi_perform -build apps-ledit
godi_perform -build apps-oasis
godi_perform -build godi-ounit
godi_perform -build godi-pgocaml
godi_perform -build godi-ocamlscript
godi_perform -build godi-xmlm
godi_perform -build godi-pxp
godi_perform -build godi-bisect   # for batteries
godi_perform -build godi-res      # for core

# install cmdliner
cd $SCRATCH
wget http://erratique.ch/software/cmdliner/releases/cmdliner-0.9.1.tbz
tar xjvf cmdliner-0.9.1.tbz
cd cmdliner-0.9.1
ocaml setup.ml -configure --prefix $OCAMLPREFIX
ocaml setup.ml -build
ocaml setup.ml -install

# install ocaml-sqlite3
if [ $COMPUTER = "bowery"]; then
    export C_INCLUDE_PATH=/share/apps/sqlite/3.7.7/intel/include
    export LIBRARY_PATH=/share/apps/sqlite/3.7.7/intel/lib
fi
godi_perform -build godi-sqlite3

# install libev, needed for lwt
cd $SCRATCH
wget http://dist.schmorp.de/libev/libev-4.11.tar.gz
tar xzvf libev-4.11.tar.gz
cd libev-4.11
./configure --prefix=$OCAMLPREFIX
make
make install

# install ocaml-ssl and lwt
# if [ $COMPUTER="bowery"]; then
#     download ocaml-ssl from http://sourceforge.net/projects/savonet/files/ocaml-ssl/
      mv ocaml-ssl-0.4.6.tar.gz $SCRATCH
      cd $SCRATCH
      tar xzvf ocaml-ssl-0.4.6.tar.gz
      cd ocaml-ssl-0.4.6
      ./configure --prefix $OCAMLPREFIX LDFLAGS=-L/share/apps/openssl/1.0.0d/gnu/lib CFLAGS=-I/share/apps/openssl/1.0.0d/gnu/include
      make
      make install

      echo linkopts = \"-cclib -L/share/apps/openssl/1.0.0d/gnu/lib\" >> $INSTALL_DIR/lib/ocaml/site-lib/ssl/META

      cd $SCRATCH
      wget http://ocsigen.org/download/lwt-2.3.2.tar.gz
      tar xzvf lwt-2.3.2.tar.gz
      cd lwt-2.3.2
      export C_INCLUDE_PATH=$OCAMLPREFIX/include
      export LIBRARY_PATH=$OCAMLPREFIX/lib
      ./configure --prefix $OCAMLPREFIX --enable-ssl
      make 
      make install

# elif [ $COMPUTER = "rabbot" ]; then
      godi_perform -build godi-ocaml-ssl

      echo GODI_LWT_GLIB=no >> $OCAMLPREFIX/etc/godi.conf
      echo GODI_LWT_OCAMLTEXT=no >> $OCAMLPREFIX/etc/godi.conf

      export C_INCLUDE_PATH=$OCAMLPREFIX/include
      export LIBRARY_PATH=$OCAMLPREFIX/lib
      godi_perform -build godi-lwt

# elif [ $COMPUTER = "ashish" ]; then
      godi_perform -build godi-ocaml-ssl

      echo GODI_LWT_GLIB=no >> $OCAMLPREFIX/etc/godi.conf
      echo GODI_LWT_OCAMLTEXT=no >> $OCAMLPREFIX/etc/godi.conf

      cd $SCRATCH
      wget http://ocsigen.org/download/lwt-2.3.2.tar.gz
      tar xzvf lwt-2.3.2.tar.gz
      cd lwt-2.3.2
      export C_INCLUDE_PATH=$OCAMLPREFIX/include
      export LIBRARY_PATH=$OCAMLPREFIX/lib
      ./configure --prefix $OCAMLPREFIX --enable-ssl
      make 
      make install

# fi

# install batteries
cd $SCRATCH
git clone git://github.com/ocaml-batteries-team/batteries-included.git
cd batteries-included/
git checkout -b nyu-compatible 91a9464bf7f2d153eeaa576fe232ab2ba0eb5fc2
ocaml setup.ml -configure --prefix $OCAMLPREFIX
make
make install

# install core
ocamlfind remove -destdir $OCAMLPREFIX/lib/ocaml/pkg-lib/ type_conv
cd $SCRATCH
if [ $COMPUTER = "ashish" ]; then
    wget --no-check-certificate https://bitbucket.org/yminsky/ocaml-core/downloads/core-suite-108.00.01.tar.gz
else
    wget https://bitbucket.org/yminsky/ocaml-core/downloads/core-suite-108.00.01.tar.gz
fi
tar xzvf core-suite-108.00.01.tar.gz
cd core-suite-108.00.01
./build-and-install

# remove temporary files
cd $HOME
rm -rf $SCRATCH
