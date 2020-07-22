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
        cargo $@
    fi
fi
