#! /usr/bin/env bash

# Thanks @waltertyree!
# https://github.com/waltertyree/project-spelunking

read -d '' AWK_SCRIPT << 'EOF'
{
    date = $1
    $1 = ""
    author = $0
    if (author in authors) {
        count = authors[$0]
        count++
        authors[author] = count
    } else {
        authors[author] = 1
        dates[author] = date
    }
}
END {
    for (author in authors) {
        count = authors[author]

        cmd = "date -r " dates[author] " '+%Y.%m.%d %H:%M'"
        ((cmd)|getline date)

        # We suck in some extraneous whitespace with $0, so trim it.
        # Also, protect our separator from malicious authors.
        gsub(/^[ \t]+/, "", author)
        gsub(/[ \t]+$/, "", author)
        gsub(/\\|+/, "_", author)

        print (author "|" count "|" date);

        # Not actually positive this is needed; maybe for a large number of authors?
        close(cmd)
    }
}
EOF

# Sort by count
SORT_ORDER='BEGIN {FS = "|"} ; { printf "%10d|%s|%s\n", $2, $1, $3 }'
PRINT_ORDER='BEGIN {FS = "|"} ; { printf "%s|%d|%s\n", $2, $1, $3 }'
TARGET_PATH=$1

if [[ "-d" == $1 ]]; then
    #Sort by date
    SORT_ORDER='BEGIN {FS = "|"} ; { print $3 "|" $1 "|" $2 }'
    PRINT_ORDER='BEGIN {FS = "|"} ; { printf "%s|%d|%s\n", $2, $3, $1 }'
    TARGET_PATH=$2
fi

git log --pretty=format:'%at %an' "${TARGET_PATH:-.}" \
    | awk "${AWK_SCRIPT}" \
    | awk "$SORT_ORDER" \
    | sort -r \
    | awk "$PRINT_ORDER" \
    | column -t -s'|'
