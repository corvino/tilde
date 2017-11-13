# Defines a bash function that makes a directory and then changes into
# that directory.
#
# This has to be done in a function, not a script that is invoked as a
# subprocess, for the cd -P to work.
mkcd ()
{
  mkdir -p -- "$1" &&
  cd -P -- "$1"
}
