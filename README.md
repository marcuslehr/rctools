rctools
======

# Introduction

The goal of this package is to provide simple, streamlined functions for interfacing with the REDCap API (http://www.project-redcap.org/) and working with REDCap data sets. rctools is a heavily modified fork of the [redcapAPI](https://github.com/nutterb/redcapAPI) package.

This package is not yet on [CRAN](https://cran.r-project.org/), however it can be easily installed from GitHub or by downloading the .tar.gz file in the main directory of this repository:
```r
# First install package dependencies from CRAN
install.packages(c('checkmate','httr','chron','lubridate','labelVector','Hmisc',
                   'readr','dplyr','stringr','tidyr','magrittr','reshape2','ggplot2'))
									 
# Install rctools from GitHub
devtools::install_github('https://github.com/marcuslehr/rctools', upgrade = F, quiet = T)

# Install rctools from local download
install.packages("Path/to/rctools_0.5.4.tar.gz", repos = NULL, type = "source", quiet = T)

# You may now load the package with the normal library() syntax
library(rctools)
```
In an effort to both reduce the number of API calls and streamline the package, rctools makes use of R's "session options". These are a special set of variables which are instantiated at the beginning of each R session and do not persist beyond that session. You can see a list of all options by running `options()` or access individual items using `getOption("[option_name]")`. 

The intended workflow of rctools is to run `rc_bundle()` then run other functions as needed. `rc_bundle()` downloads project metadata and saves it as an option called "redcap_bundle". It also returns the metadata bundle as an object which can be saved to the global environment for easy access. For security purposes, your token is not saved in the bundle but rather as a separate option called "redcap_token". It is recommended to store tokens as-is in plain text files. These token files should be stored in a secure place that is not publicly accessible. From there, you can simply feed the token path to rctools functions. 


# Vingette

To demonstrate, we'll walk through an example workflow. Beginning with running `rc_bundle()`:
```r
# Run rc_bundle() to download metadata and make it accessible to other rc_ functions
bundle = rc_bundle(url = 'https://your.redcap.server.com/api/',
                   token = 'path/to/token.txt')
```
Your project metadata has now been uploaded to the "redcap_bundle" option and other rctools functions will check there by default for metadata. However, all the functions have metadata arguments so it can be supplied manually and are written to operate without project metadata where possible. Note that if you are manually supplying metadata, it should be formatted exactly as exported by REDCap or as it is stored in the rctools metadata bundle. 

Note that the session options do not persist across R sessions. If you would like to reduce API calls, you can save the bundle locally for future use. You can then use `rc_to_options()` to upload your bundle and token to the session options when beginning a new session.
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
2     11435     visit_2_arm_1        	 NA             NA             89           NA         98.6
3     11945     visit_1_arm_1           1             72             NA         97.8           NA
4     11945     visit_2_arm_1        	 NA             NA             64           NA         96.9
5     26479     visit_1_arm_1           0             56             NA         98.1           NA
6     26479     visit_2_arm_1          NA             NA             72           NA         99.1
```
Note that you can also perform custom exports, pulling specific forms and/or fields. If no arguments are given to `rc_export()`, all data will be exported. Conversely, if you are interested in importing data into a project you can use `rc_import()`.

Now that we have some data, we can format it if desired by calling `rc_format()`. This uses the data dictionary to convert numerically coded values to labeled factors, adds column labels, and can convert the redcap_event_name column to event label values as well. Additionally, this function is reversible (with some exceptions for condensed checkbox fields). In our example data, we can quickly change our sex variable back to numerical codings like so:
```r
> head(rc_format(report_data, factors = F))
  record_id redcap_event_name visit_1_sex visit_1_weight visit_2_weight visit_1_temp visit_2_temp
1     11435     visit_1_arm_1      Female             65             NA         96.7           NA
2     11435     visit_2_arm_1        <NA>             NA             89           NA         98.6
3     11945     visit_1_arm_1        Male             NA             NA         97.8           NA
4     11945     visit_2_arm_1        <NA>             NA             64           NA         96.9
5     26479     visit_1_arm_1      Female             56             NA         98.1           NA
6     26479     visit_2_arm_1        <NA>             NA             72           NA         99.1
```
You may also notice that our project has been setup such that some measures are being captured in multiple variables. This is not an ideal way to structure project data, but should you come across it you can pool those into a single variable via the `rc_pool()` function, using either exact (`fields_list`) or partial (`var_roots`) column name matching.
```r
# This will collapse all columns matching each argument into a single column
pooled_data = rc_pool(record_data = report_data,
                      var_roots = c('weight','temp'))
                      
# Now we have single variables for weight and temp so we can analyze them together
> head(pooled_data)
  record_id redcap_event_name visit_1_sex weight temp
1     11435     visit_1_arm_1      Female     65 96.7
2     11435     visit_2_arm_1        <NA>     89 98.6
3     11945     visit_1_arm_1        Male     NA 97.8
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
3     11945 visit_1_arm_1     Male            NA  97.8
4     11945 visit_2_arm_1     Male            64  96.9
5     26479 visit_1_arm_1     Female          56  98.1
6     26479 visit_2_arm_1     Female          72  99.1
```
Another useful thing to do is to check for any data that appears to be missing. The `rc_missing()` function will produce a report of such data. The essential logic of the function is such that if a particular event/variable combination has a value for any record, it is expected in all records. For more details, see the function documentation(`?rc_missing`).
```r
# We will go ahead and invoke the `plot` and `table` arguments. These are useful
# for investigating the structure of the missing data
missing_data = rc_missing(record_data = pooled_data)
> head(missing_data)
# A tibble: 6 x 3
  record_id redcap_event_name form_name   variable
      <int> <fct>             <chr>			  <chr>   
1     11945 visit_1_arm_1     anthro		  weight
2     26479 visit_1_arm_1     vitals			temp
3     27699 visit_1_arm_1     	anthro			weight
4     37757 visit_2_arm_1     	anthro			weight
5     74240 visit_1_arm_1     	vitals			temp
6     74240 visit_2_arm_1     	vitals			temp
```
If you have a lot of missing data, `rc_plot_missing()` will produce pie charts for each column to give a quick breakdown of the results. This helps to idenify problem areas with lots of missing data. 

We can also check for outliers, as defined by the number of standard deviations from the mean. Note that this function will ignore any non-numeric data. In future versions, `rc_outliers()` will accept custom detection functions.
```r
outlier_data = rc_outliers(record_data = pooled_data, sex_var = "visit_1_sex")

> head(outlier_data)
# A tibble: 1 x 5
  record_id visit_1_sex redcap_event_name form_name variable value outlier
      <int> <fct>       <fct>             <chr>     <fct>    <dbl> <lgl>
1     30803 Male        visit_2_arm_1     anthro    weight     113 TRUE
```

A useful follow-up after checking for outliers is to plot the data via `rc_plot()`. This will create a subplot for each variable with the record_id on the x-axis and values on the y-axis. Outliers will be highlighted in red. Horizontal lines indicate +/- 3 standard deviation levels. This function was originally designed specifically for plotting outliers, however it can be fed any long format data without outliers and is quite useful for quickly visualizing your data. A limitation of `rc_plot()` is it puts all the variables into a single figure, so if you have a large number it will be useful to subset them.


# Other useful functions

The following are other package functions, for which full examples may be added in the future.
- `rc_cast()`: Converts REDCap data into wide format (1 row per record).
- `rc_logic_check()`: Checks the data dictionary for common branching logic mistakes.
- `rc_strip()`: Removes all empty rows and columns. This takes the default REDCap columns into account when evaluating rows. 
- `rc_import()`: For importing data into a project.
- `rc_api_call()`: A generic API interface function which can be used with methods not included in this package. 
