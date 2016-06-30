# /bin/bash

if [[ !($1 =~ ^(delete|echo|read|write)$) ]]; then
  echo "Invalid subcommand $1" >&2
  exit 1
fi
subcommand=$1
shift

vault_path=`echo $1 | sed -e 's/\./\//g'`

case "$subcommand" in
  delete)
      if [[ 1 != $# ]]; then
          echo "read expects one argument"
          exit 1
      fi
      vault delete "$vault_path"
  ;;
  echo)
      if [[ 1 != $# ]]; then
          echo "echo expects two arguments"
          exit 1
      fi
      echo "$vault_path"
  ;;
  read)
      if [[ 1 != $# ]]; then
          echo "read expects one argument"
          exit 1
      fi
      vault read "$vault_path"
  ;;
  write)
      if [[ 2 != $# ]]; then
          echo "write expects two arguments"
          exit 1
      fi
      vault write "$vault_path" value="$2"
  ;;
esac
