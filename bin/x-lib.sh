#!/bin/bash -eu
set -o pipefail

export DIR=".isoxya"
export KEY_PRI_PEM="$DIR/pri.pem"
export KEY_PUB_PEM="$DIR/pub.pem"
export NETRC="$DIR/netrc"
export SES_JSON="$DIR/ses.json"
export SES_JSON_SIG="$DIR/ses.json.sha256.sig.base64"

ISOXYA_DEBUG=${ISOXYA_DEBUG:-0}
#-------------------------------------------------------------------------------
function ask() {
    local msg=$1
    ASK_BUF=$2
    
    echo -n "$msg [$ASK_BUF]: "
    read -r ask_tmp
    [ -n "$ask_tmp" ] && ASK_BUF=$ask_tmp
    
    true
}

function ask_opts() {
    local msg=$1
    ASK_BUF=$2
    shift 2
    local opts=( "$@" )
    
    echo "$msg:"
    
    for i in "${!opts[@]}" ; do
        echo "    $i: ${opts[i]}"
    done
    
    ask " " "$ASK_BUF"
    ASK_BUF=${opts[$ASK_BUF]}
    
    echo "  $ASK_BUF"
    
    true
}

function cache_var() {
    local v=$1
    local ns=${2:-}
    
    local v_="${v}_"
    export declare "$v_=$DIR$ns/$v"
    # shellcheck disable=SC2015
    test -f "${!v_}" && export declare "$v=$(tail -n1 "${!v_}")" || true
}

function curl_() {
    if [ "$ISOXYA_DEBUG" == "1" ]; then
        curl -sv "$@"
    else
        curl -s "$@"
    fi
}

function curla() {
    curl_ --netrc-file "$DIR/netrc" "$@"
}

function jq_() {
    jq -S "$@"
}

# shellcheck disable=SC2120
function jqd() {
    if [ "$ISOXYA_DEBUG" == "1" ]; then
        jq_ "$@"
    fi
}

function reload_vars() {
    # shellcheck source=x-lib.sh
    source "$(readlink -f "${BASH_SOURCE[0]}")"
}
#-------------------------------------------------------------------------------
mkdir -p "$DIR"

cache_var ENDPOINT_URL

cache_var CRAWL_HREF
cache_var LIST_HREF
cache_var PROCESSOR_HREF
cache_var SITE_HREF
cache_var STREAMER_HREF
