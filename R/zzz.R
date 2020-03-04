packageStartupMessage(
  "Welcome to rctools.  Please Note:\n",
  " - Run 'rc_setup' before using other functions.\n",
  " - If you do not have API access to your REDCap project, you can still use\n",
  "  	non-import/export functions by providing metadata where needed.\n",
  " - 'rc_setup' saves its result to an option by default. You can access\n",
  "  	project metadata using getOption('redcap_bundle')"
)

.onLoad <- function(libname,pkgname) {
  options(redcap_error_handling = "null")
  if (is.null(getOption('redcap_token')))
		options(redcap_token = NULL)
	if (is.null(getOption('redcap_bundle')))
		options(redcap_bundle = 
			structure(list(
										redcap_url = NULL,
										data_dict = NULL,
										users = NULL,
										form_perm = NULL,
										instruments = NULL,
										event_data = NULL,
										arms = NULL,
										mappings = NULL,
										proj_info = NULL,
										version = NULL
								),
								class = c("redcapBundle", "list")
			)
		)
}
.onUnload <- function(libPath) {
  options(redcap_token = NULL,
					redcap_bundle = NULL,
					redcap_error_handling = NULL)
}
