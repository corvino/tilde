#! /usr/bin/env bash

export SOURCE_DIR=$(dirname "$0")

repo_info() {
    SUBPATH=$(dirname "$1")
    cd ${SUBPATH}
    BRANCH=$("${SOURCE_DIR}/git-branch-name.sh")
    COMMIT=$(git rev-parse HEAD)
    DATE=$(git show -s --format=%at)
    DIRTY=$(git diff --quiet || echo '*')
    echo ${SUBPATH}:${BRANCH}:${COMMIT}:${DATE}:${DIRTY}
}

# Eh. Let's try https://medium.com/@Drew_Stokes/bash-argument-parsing-54f3b81a6a8f
while (( "$#" )); do
  case "$1" in
    -o|--only-branches)
      BRANCHES=true
      shift
      ;;
    -n|--no-current)
      NO_CURRENT=true
      shift
      ;;
    -c|--no-commit)
      COMMIT=true
      shift
      ;;
    -p|--preserve-separator)
      PRESERVE=true
      shift
      ;;
    -*|--*=) # unsupported flags
      echo "Error: Unsupported flag $1" >&2
      exit 1
      ;;
    *) # preserve positional arguments
      PARAMS="$PARAMS $1"
      shift
      ;;
  esac
done

export -f repo_info

find . -name '.git' -exec bash -c 'repo_info "$0"' {}  \; \
    | ([[ ${BRANCHES} ]] && awk 'BEGIN { FS=":" }; $2 !~ /^\(/ { print $0 }' || cat) \
    | ([[ ${NO_CURRENT} ]] && awk 'BEGIN { FS=":" }; $1 != "." { print $0 }' || cat) \
    | awk '
      BEGIN { FS = ":"; OFS = ":" }
      {
        (("date -r " $4 " \"+%Y.%m.%d-%H.%M\"")|getline date)
         print $1, $2, $3, date, $5
      }' \
    | ([[ ! ${COMMIT} ]] && awk 'BEGIN { FS = ":"; OFS = ":" }; { $3=""; print $0 }' || cat) \
    | ([[ ! ${PRESERVE} ]] && column -t -s':' || cat) \
    | sort \
