# The path to an emacs app
export NWG_EMACS_APP="/Applications/MacPorts/Emacs.app"

# The path to the emacsclient to use when contacting the server
export NWG_EMACSCLIENT="${NWG_EMACS_APP}/Contents/MacOS/bin/emacsclient"

# The template used to store emails
export NWG_CAPTURE_EMAIL_TEMPLATE=e
export NWG_CAPTURE_EMAIL_URL_DESCRIPTION="%sender %subject"
export NWG_CAPTURE_EMAIL_INITIAL_CONTENT="Read Email"
