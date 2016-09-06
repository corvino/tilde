#! /bin/bash

red='\033[0;31m'
green='\033[0;32m'
bold='\033[1m'
end='\033[0m'

fetch_emacswiki_file() {
    if [ -z "$1" ]; then
        echo -e "${red}fetch_emacswiki_file called without a package name.${end}"
        return -1
    fi

    echo -e "${green}Fetching ${bold}$1${green} from www.emacswiki.org.${end}"

    curl "https://www.emacswiki.org/emacs/download/$1.el" > ~/.elisp/"$1".el
}

fetch_elpa_package() {
    if [ -z "$1" ]; then
        echo -e "${red}fetch_elpa_package called without a package name.${end}"
        return -1
    fi

    echo -e "${green}Fetching ${bold}$1${green} from ELPA.${end}"

    # This string of Perl gunk comes from http://blog.binchen.org/posts/no-worries-when-elpa-is-down.html
    # It means that the latest version will be parsed from the ELPA directory. So kind of a win?
    curl -L https://elpa.gnu.org/packages/archive-contents | perl -pe 's/(^\(1|\n)//g' | perl -pe 's/\]\)/])\n/g' | perl -pe 's/^ *\(([a-z0-9A-Z-]*).*\[\(([0-9 ]*).*(single|tar).*/\1-\2.\3/g' | perl -pe 's/ /./g' | perl -pe 's/single/el/g' | perl -pe 's/\)//g' | grep -i -e "$1" | xargs -I {} curl -L  https://elpa.gnu.org/packages/{} -o ~/.elisp/$1.el
}


fetch_emacswiki_file 'apache-mode'
fetch_emacswiki_file 'diff-mode-'
fetch_emacswiki_file 'etags-select'

echo -e "${green}Fetching {bold}protobuf-mode{$green} from github.com:google/protobuf.${end}"
curl 'https://raw.githubusercontent.com/google/protobuf/master/editors/protobuf-mode.el' > ~/.elisp/protobuf-mode.el

fetch_elpa_package 'sml-mode'
