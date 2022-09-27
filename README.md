rctools
======

The goal of this package is to provide simple, streamlined functions for interfacing with the REDCap API (http://www.project-redcap.org/) and working with REDCap data sets. rctools is a heavily modified fork of the [redcapAPI](https://github.com/nutterb/redcapAPI) package.

This package is not yet on [CRAN](https://cran.r-project.org/), however it can be easily installed from GitHub or by downloading the .tar.gz file in the main directory of this repository:
```r
# First install package dependencies from CRAN
install.packages(c('checkmate','httr','chron','lubridate','labelVector','Hmisc',
                   'readr','dplyr','stringr','tidyr','magrittr','reshape2','ggplot2'))
									 
# Install rctools from GitHub
devtools::install_github('https://github.com/marcuslehr/rctools', upgrade = F, quiet = T)

# Install rctools from local download
install.packages("Path/to/rctools_0.4.9.tar.gz", repos = NULL, type = "source", quiet = T)

# You may now load the package with the normal library() syntax
library(rctools)
```
In an effort to both reduce the number of API calls (important for network traffic and audits) and streamline the package, rctools makes use of R's "session options". These are a special set of variables which are instantiated at the beginning of an R session and do not persist beyond that session. You can see a list of all options by running `options()` or access individual items using `getOption("option_name")`. 

The intended workflow of rctools is to run `rc_bundle()` then run other functions as needed. `rc_bundle()` downloads project metadata then uploads it to an option called "redcap_bundle". For security purposes, your token is not saved in the bundle but rather as a separate option called "redcap_token". Additionally, a bundle object will be returned so that it is readily accessible. Ensure that the token is stored as-is in a plain text file. You will also want to ensure that your token is stored in a secure place that is not publicly accessible. 

To demonstrate, we'll walk through an example workflow. Beginning with running `rc_bundle()`:
```r
# Run rc_bundle() to download metadata and make it accessible to other rc_ functions
bundle = rc_bundle(url = 'https://your.redcap.server.com/api/',
                   token = 'path/to/token.txt')
```
Your project metadata has now been uploaded to the "redcap_bundle" option and other rctools functions will check there by default for metadata. However, all the functions have metadata arguments so it can be supplied manually and are written to operate without project metadata where possible. Note that if you are manually supplying metadata, it should be formatted exactly as exported by REDCap or as it is stored in the rctools metadata bundle. 

Note that the session options do not persist across R sessions. If you would like to reduce API calls/network traffic, you can save the bundle locally for future use. You can then use `rc_to_options()` to upload your bundle and token to the session options when beginning a new session.
```
# Save metadata for use in future sessions
saveRDS(bundle, 'redcap_bundle.RDS')

# Load bundle from disk and upload the bundle and token to the session options
bundle = readRDS('Path/to/redcap_bundle.RDS')
rc_to_options(bundle = bundle, token = 'path/to/token.txt')
```
Now let's try exporting some data from REDCap:
```r
# For this example we'll use a previously setup REDCap report
report_data = rc_export(<Your report ID number>)

> head(report_data)
  record_id redcap_event_name visit_1_sex visit_1_weight visit_2_weight visit_1_temp visit_2_temp
1     11435     visit_1_arm_1           0             65             NA         96.7           NA
2     11435     visit_2_arm_1        <NA>             NA             89           NA         98.6
3     11945     visit_1_arm_1           1             72             NA         97.8           NA
4     11945     visit_2_arm_1        <NA>             NA             64           NA         96.9
5     26479     visit_1_arm_1           0             56             NA         98.1           NA
6     26479     visit_2_arm_1        <NA>             NA             72           NA         99.1
```
As you can see, `rc_export` automatically converts factors from numerical coding (e.g. 0,1) to the human-readable choices in the data dictionary. It does this by calling `rc_format()`- and you can use this function at any point to change the formatting of the data. For example, we can quickly change our sex variable back to numerical codings like so:
```r
> head(rc_format(report_data, factors = F))
  record_id redcap_event_name visit_1_sex visit_1_weight visit_2_weight visit_1_temp visit_2_temp
1     11435     visit_1_arm_1           0             65             NA         96.7           NA
2     11435     visit_2_arm_1        <NA>             NA             89           NA         98.6
3     11945     visit_1_arm_1           1             72             NA         97.8           NA
4     11945     visit_2_arm_1        <NA>             NA             64           NA         96.9
5     26479     visit_1_arm_1           0             56             NA         98.1           NA
6     26479     visit_2_arm_1        <NA>             NA             72           NA         99.1
```
You may also notice that our project has been setup such that some measures are being captured in multiple variables. We can pool those into a single variable via the `rc_pool()` function, using either exact (`fields_list`) or partial (`var_roots`) column name matching.
```r
# This will collapse all columns matching each argument into a single column
pooled_data = rc_pool(record_data = report_data,
                      var_roots = c('weight','temp'))
                      
# Now we have single variables for weight and temp so we can analyze them together
> head(pooled_data)
  record_id redcap_event_name visit_1_sex weight temp
1     11435     visit_1_arm_1      Female     65 96.7
2     11435     visit_2_arm_1        <NA>     89 98.6
3     11945     visit_1_arm_1        Male     72 97.8
4     11945     visit_2_arm_1        <NA>     64 96.9
5     26479     visit_1_arm_1      Female     56 98.1
6     26479     visit_2_arm_1        <NA>     72 99.1

# We can view the pooling record at any time by accessing the pooling attribute
# attached to the data.frame
> attributes(pooled_data)$pooled_vars
$weight
[1] "visit_1_weight"   "visit_2_weight"
$temp
[1] "visit_1_temp"   "visit_2_temp"
```
Categorical data such as demographics are typically only captured once during a study and therefore only occupy a single row for each subject. In our case, we have a sex variable that follows this pattern. If we want to filter or group by sex during our analysis, it'll be much easier if we fill in that information for all rows, which is readily accomplished with `rc_fill()`.
```r
# By default, the data will be grouped by the 'record_id' field. Additionally,
# multiple fields can be filled simultaneously if desired
pooled_data = rc_fill(record_data = pooled_data, "visit_1_sex")

> head(pooled_data)
# A tibble: 6 x 5
  record_id redcap_event_name visit_1_sex weight  temp
      <int> <fct>             <fct>        <int> <dbl>
1     11435 visit_1_arm_1     Female          65  96.7
2     11435 visit_2_arm_1     Female          89  98.6
3     11945 visit_1_arm_1     Male            72  97.8
4     11945 visit_2_arm_1     Male            64  96.9
5     26479 visit_1_arm_1     Female          56  98.1
6     26479 visit_2_arm_1     Female          72  99.1
```
Another useful thing to do is to check for any data that appears to be missing. The `rc_missing()` function will produce a report of such data. The essential logic of the function is such that if a particular event/variable combination has a value for any record, it is expected in all records. For more details, see the function documentation(`?rc_missing`).
```r
# We will go ahead and invoke the `plot` and `table` arguments. These are useful
# for investigating the structure of the missing data
missing_data = rc_missing(record_data = pooled_data,
                          plot = T, table = T)
                          
# Because we used `table = T` a list is returned, rather than a dataframe. The
# first item in the list is the dataframe containing the missing data
> head(missing_data$missing_data)
# A tibble: 6 x 3
  record_id redcap_event_name variable
      <int> <fct>             <chr>   
1     11945 visit_1_arm_1     weight
2     26479 visit_1_arm_1     temp
3     27699 visit_1_arm_1     weight
4     37757 visit_2_arm_1     weight
5     74240 visit_1_arm_1     temp
6     74240 visit_2_arm_1     temp

# To view formatted versions of the tables, call them using `[` indexing
missing_data["IDs_table"]
missing_data["variables_table"]
```
We can also check for outliers, as defined by the number of standard deviations from the mean. Similar to `rc_missing()`, we can create a simple plot of the resulting data while we're at it. Note that this function will attempt automatically strip out any non-numeric data.
```r
outlier_data = rc_outliers(record_data = pooled_data, 
                           sex_var = "visit_1_sex",
                           plot = T)

> head(outlier_data)
# A tibble: 1 x 5
  record_id visit_1_sex redcap_event_name variable value
      <int> <fct>       <fct>             <fct>    <dbl>
1     30803 Male        visit_2_arm_1     weight     113
```
One last trick we can do with rctools is to perform some cursory checks on the branching logic using `rc_logic_check()`. Note that this function does not check syntax (and it likely never will). What it does check for are common errors that REDCap won't pick up. Most notably, when using the pattern `[event-name] = "event_name"` REDCap does not verify that the right-hand string actually contains a valid event name. This function will check these event names and bracketed event names preceeding a variable name. 
Additionaly, it will look for inconsistent event name logic where two pieces of `[event-name] = "event_name"` logic are joined by `AND` or two pieces of `[event-name] != "event_name"` are joined by `OR`. These logic patterns will result in fields that are either always hidden or always displayed (respectively). These checks can be quite useful for projects under active development, where event and variable names may change frequently. 
```r
# Let's have a look at an example project
>rc_logic_check()
Invalid event names were found in the following fields:
[1] "vd_icf_screen" "bd_hcg"        "bd_adequate"  
Invalid event names:
[1] "screeing_visit_arm_1"    "screening_visit_1_arm_1"

No invalid variable names were found

Use of "[event-name]='event_name' AND [event-name]='event_name'" logic found.
This will result in the field always being hidden. Please review branching
logic in the following fields:
[1] "vd_hchange"
```