# Package Startup ---------------------------------------------------------

packageStartupMessage(
  "Welcome to rctools.  Please Note:\n",
  " - Start by running 'rc_bundle' to export project metadata.\n",
  " - If you do not have API access to your REDCap project, you can still use\n",
  "  	non-import/export functions by providing metadata where needed.\n",
  " - For more details see help documentation or https://github.com/marcuslehr/rctools"
)

# Instantiation actually seems unnecessary. Provides only minor protection against token/bundle mix ups
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


# Imports -----------------------------------------------------------------

# Importing functions allows for their use in package functions without explicitly specifying
# the source package (ie namespace). Declaring the namespace is preferred, but is impractical
# in some cases

#' @importFrom dplyr '%>%'
#' @importFrom rlang .data


# Globals -----------------------------------------------------------------

# The purpose of this file is to satisfy R CMD Check requirements for the purposes of being CRAN ready.
# Non-standard evaluation of variables (eg unquoted dataframe column references) will be flagged as 
# a note by R CMD Check. Most commonly this occurs in dplyr::select() statements. 
# Use of the globalVariables() function bypasses the issue.

# Common NSE vars used in various functions without a suitable SE replacement
utils::globalVariables(c('.',':='))


# Function specific SE vars or vars that could be rewritten with SE syntax
utils::globalVariables(c('redcap_event_name','redcap_repeat_instrument','redcap_repeat_instance','unique_event_name','value', # Common
                         'field_name','rc_vars','form','form_name.x','form_name.y','variable','form_name', # add_form_names
                         'username','forms','forms_export', # export_users
                         'row_sums','var_count','data_following','view', # rc_missing
                         'count','prop','ypos', # rc_missing_plot
                         'outlier', # rc_outliers
                         'sd', # rc_plot
                         'n' # rc_pool
))

# Suggested solution for SE syntax by dplyr. See vignette("programming")
# #' @importFrom rlang .data
# my_summary_function <- function(data) {
#   data %>% 
#     filter(.data$x > 0) %>% 
#     group_by(.data$grp) %>% 
#     summarise(y = mean(.data$y), n = n())
# }