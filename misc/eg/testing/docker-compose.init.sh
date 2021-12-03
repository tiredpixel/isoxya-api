#!/bin/bash

dir=$(dirname "$(readlink -f "${BASH_SOURCE[0]}")")


touch "$dir/sqlite3.db"
