if [[ $_ = $0 ]]; then
    echo >&2 "This script is meant to be sourced. Doing nothing."
    return 1
fi

export NWG_CAPTURE=$(dirname $0)/../
export NWG_CAPTURE_SCRIPTS=$(dirname $0)

source "$NWG_CAPTURE/user-config.sh"

fpath=( $NWG_CAPTURE_SCRIPTS/functions $fpath )
autoload -U ${NWG_CAPTURE_SCRIPTS}/functions/*(.:t)
