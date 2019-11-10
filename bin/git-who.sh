#! /usr/bin/env bash

# Thanks @waltertyree!
# https://github.com/waltertyree/project-spelunking

read -d '' AWK_SCRIPT << 'EOF'
{
    if ($1 ~ /Author:/) {a = $2 " " $3};
    if ($1 ~ /Date:/) {b = $2 " " $3 " " $4 " " $5 " " $6};
    if (a != "" && b !="") {
        if (a in authors)
        {  c = authors[a];
            c++;
            authors[a] = c;
        }
        else
        {
            authors[a] = 1;
            dates[a] = b;
        }
        a = "";
        b = "";
    }
}
END {
    for (a in authors)
        # backslash needs to be escaped to survive the read step
        print (a "\\t" authors[a] "\\t" dates[a]);
}
EOF

git log "${1:-.}" | awk "${AWK_SCRIPT}"
