#! /usr/bin/env bash

sync_checkout() {
    WORKING_DIR=`pwd`
    cd "$1"
    git checkout $2
    cd "${WORKING_DIR}"
}

#sync_checkout Carthage/Checkouts/DuetAppleConnectionFramework 1.3.4
#sync_checkout Carthage/Checkouts/DuetAppleVideo 1.1.6
#?sync_checkout Carthage/Checkouts/TCMPortMapper develop
sync_checkout Carthage/Checkouts/DuetAppleRemoteDesktop feature/connectionProgressPublished
sync_checkout Carthage/Checkouts/DuetAppleServiceSessions feature/addIsSessionDevice
#sync_checkout Carthage/Checkouts/DuetCommon 1.2.1
sync_checkout Carthage/Checkouts/CocoaLumberjack master
sync_checkout Carthage/Checkouts/DuetAppleScreenCapture 1.0.5
#sync_checkout Carthage/Checkouts/UICKeyChainStore
