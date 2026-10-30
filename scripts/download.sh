# this script downloads files from my CBIO webspace to the local
# directory, so we can build my thesis locally.
PRE="http://cbio.ensmp.fr/~thocking/thesis"
RELPATH=$1
BASE=$(basename $RELPATH)
URL="$PRE/$BASE"
wget $URL -O $RELPATH
touch $RELPATH
