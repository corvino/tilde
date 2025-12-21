#! /usr/bin/env bash

search_cart() {
    echo '****' "$1"
    find . -name 'Cartfile' -exec echo {} \; -exec cat {} \; | grep 'git' | grep "$1"
}

search_cart DuetAppleConnectionFramework
search_cart DuetAppleVideo
search_cart TCMPortMapper
search_cart DuetAppleRemoteDesktop
search_cart CocoaLumberjack
search_cart DuetCommon
search_cart DuetAppleServiceSessions
search_cart UICKeyChainStore
search_cart DuetAppleScreenCapture
