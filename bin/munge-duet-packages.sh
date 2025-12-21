#! /usr/bin/env bash

MACOS_START=`sed -n '/#else/=' Package.swift`
PACKAGE_INSERT=$((${MACOS_START} + 3))

sed -i '' -e s/swift-tools-version:4.0/swift-tools-version:5.1.0/ \
           -e s/'fluent-sqlite.git", from: "3.0.0-rc.2"'/'fluent-postgresql.git", from: "1.0.0-rc.4.1"/' \
           -e s/FluentSQLite/FluentPostgreSQL/ \
           -e "${PACKAGE_INSERT}i\\
\		platforms: [ .macOS(.v10_14), ]," \
           $1
