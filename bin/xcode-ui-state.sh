#! /usr/bin/env bash

# Super annoying bug where Xcode crashes trying to restore tab state, showing
#   -[IDEWorkspaceDocument _restoreSelectedTabsByIdentifierFromStateSaving:
# in the stack trace.
#
# https://stackoverflow.com/questions/58902776/xcode-error-report-xcode-quit-unexpectedly
# https://forums.developer.apple.com/thread/123204

while (( "$#" )); do
  case "$1" in
    -d|--delete)
      DELETE=true
      shift
      ;;
    *) # unsupported flags
      echo "Error: Unsupported parameter $1" >&2
      exit 1
      ;;
  esac
done

if [[ ${DELETE} ]]; then
    find . -name 'UserInterfaceState.xcuserstate' -exec rm -f {} \;
else
    find . -name 'UserInterfaceState.xcuserstate'
fi
