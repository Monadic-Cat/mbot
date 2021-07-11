#!/bin/sh

find_project_root () {
    local current_dir="$1"
    if [ -f "$current_dir/Cargo.toml" ] ; then
        printf "$current_dir"
    else
        local par_dir="$current_dir/.."
        local next_dir="$(realpath $par_dir)"
        if [ "$current_dir" = "$next_dir" ] ; then
            return 1
        else
            find_project_root "$next_dir"
        fi
    fi
}

project_root="$(find_project_root $(pwd))"
if [ $? != 0 ] ; then
    echo "Couldn't find Cargo.toml. Make sure to run this from a project directory."
else
    rm "$project_root/dev.db"
    cat "$project_root/src/init.sql" | sqlite3 "$project_root/dev.db"
    
    if [ $# -gt 0 ] ; then
        # This is a maintainer only flag.
        # I use it to update our version of Maddie's Masks data.
        # Since it's committed to Git, a regular contributor
        # doesn't need to worry about this.
        if [ "$1" = "vendor-data" ] ; then
            curl "https://raw.githubusercontent.com/harkano/maddie/master/language_files/en.json" \
                 -o "$project_root/data/maddie.json"
        elif ["$1" = "musl-release" ] ; then
            # We've got a dependency, servo-fontconfig-sys, which requires these variables be set
            # for musl builds, as it uses CMake to build C dependencies.
            CC="$(which musl-gcc)" AR="$(which ar)" cargo build --release --target=x86_64-unknown-linux-musl
        else
            cargo $@
        fi
    fi
fi
