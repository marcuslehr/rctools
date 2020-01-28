rctools
======
**Note this package is currently under heavy development. Please use with caution until there is a stable release**


The goal of this package is to provide simple, streamlined functions for interfacing with the REDCap API (http://www.project-redcap.org/) and working with REDCap data sets. It was forked from [redcapAPI](https://github.com/nutterb/redcapAPI) and is currently under active development. 

In an effort to both reduce the number of API calls (important for network traffic and audits) and streamline the package, `rctools` makes use of R's "options". Options are variables which are instantiated at the start of an R session. You can see a list of all options by running `options()` or access individual items using the `getOption()` function. However, all functions are written with the flexibility to be used without options, provided the user is able to supply the necessary arguments. 

The workflow of `rctools` is- 1)Run `rc_setup()`, 2)Run other import/export functions as needed. By default, `rc_setup()` will export all project metadata and save that metadata to an option called "redcap_bundle". For security purposes, your token will be saved as a separate option called "redcap_token". Neither of these options will persist across R sessions- if you wish to further reduce API calls, consider saving the bundle for further use. 
```r
# Gather project metadata for use with other functions
rc_setup(url = [REDCap API URL address for your institution],
         token = [API token for your project])

# Save metadata for use in future sessions
saveRDS(getOption('redcap_bundle'), 'redcap_bundle.RDS')


# Alternatively, you can use the 'return.object' argument to save the 
# bundle as an object for easy access. It will still be saved to an 
# option unless 'create.option' is set to FALSE. 
bundle = rc_setup(url = [REDCap API URL address for your institution],
                  token = [API token for your project],
                  return.object = TRUE)
                  
saveRDS(bundle, 'redcap_bundle.RDS')
```
Now that you've secured the project metadata, you're ready to use other functions for import, export, or validation. For example, we can export a REDCap report, then check it to see if any data appears to be missing.
```r
reportData = rc_export(report.id = [report_ID])
missingData = rc_missing(data = reportData,
                         completion.field = 'study_complete')
```

Keep in mind that all functions other than import/export functions can be used without API access. All functions have arguments for the pieces of metadata they need, so that that data can be supplied without the use of `rc_setup()` or API access. For example, the most important piece of metadata is the "Data Dictionary", which can be downloaded manually from REDCap and supplied via the `dict` argument. 
