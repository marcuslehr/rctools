#packageStartupMessage(
#  "Welcome to redcapAPI 2.0.  Please Note:\n",
#  " - redcapDbConnection methods have all been deprecated.\n",
#  " - 'exportProjectInfo' has been deprecated and replaced with 'rc_setup'.\n",
#  " - 'rc_setup' saves its result to an option. Consider discontinuing\n",
#  "   use of bundle objects unless working with multiple REDCap projects in one session.")

.onLoad <- function(libname,pkgname)
{
  options(redcap_error_handling = "null",
          redcap_token = NULL,
		  redcap_bundle = 
            structure(
              list(
				redcap_url = NULL,
                meta_data = NULL,
                users = NULL,
				form_perm = NULL,
                instruments = NULL,
                events = NULL,
                arms = NULL,
                mappings = NULL,
				proj_info = NULL,
				version = NULL
              ),
              class = c("redcapBundle", "list")
            )
  )
}

.onUnload <- function(libPath)
{
  options(redcap_bundle = NULL,
		  redcap_error_handling = NULL)
}
