packageStartupMessage(
  "Welcome to rctools.  Please Note:\n",
  " - Start by running 'rc_bundle' to export project metadata.\n",
  " - If you do not have API access to your REDCap project, you can still use\n",
  "  	non-import/export functions by providing metadata where needed.\n",
  " - For more details see help documentation or https://github.com/chillywings/rctools"
)

# Not sure instantiation is actually necessary
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
# # This is more annoying than helpful. Options are cleared on session close
# .onUnload <- function(libPath) {
#   options(redcap_token = NULL,
# 					redcap_bundle = NULL,
# 					redcap_error_handling = NULL)
# }
