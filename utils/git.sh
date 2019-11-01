#!/usr/bin/env bash

# Convert git@github.com:org/repo.git into https://github.com/org/repo
_urlify() {
    url=$(echo "$1" | sed 's/:/\//g' | sed 's/git@/https:\/\//g')
    echo "${url%.git}"
}

# Get an https:// web URL for the origin of the provided repo.
git_web_url() {
    repo="$1"
    url=$(git -C "$repo" remote get-url origin)
    case $url in
        http*) ;;
        git*)
            url=$(_urlify "$url")
            ;;
        ssh*)
            url=${url#"ssh://"}
            url=$(_urlify "$url")
            ;;
        *)
            echo "Unknown URL" >&2
            return 1
            ;;
    esac
    echo "$url"
}
