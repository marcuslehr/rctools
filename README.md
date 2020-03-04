rctools
======

The goal of this package is to provide simple, streamlined functions for interfacing with the REDCap API (http://www.project-redcap.org/) and working with REDCap data sets. rctools is an actively developed fork of [redcapAPI](https://github.com/nutterb/redcapAPI). It is not yet on [CRAN](https://cran.r-project.org/), but can be installed via
```r
devtools::install_github('https://github.com/chillywings/rctools', upgrade = F, quiet = T)
```
In an effort to both reduce the number of API calls (important for network traffic and audits) and streamline the package, rctools makes use of R's "options". Options are variables which are instantiated at the start of an R session. You can see a list of all options by running `options()` or access individual items using the `getOption()` function. However, all rctools functions are written with the flexibility to be used without options, provided the user is able to supply the necessary arguments.

The workflow of rctools is- 1)Run `rc_setup()`, 2)Run other functions as needed. `rc_setup()` downloads project metadata from the REDCap API and makes it available for other rc_tools functions. However, for those without API access, all functions have metadata arguments so that the necessary items can be supplied directly and are written to operate without project metadata where necessary (if possible).

By default, `rc_setup()` will export all project metadata and save that metadata to an option called "redcap_bundle". For security purposes, your token will be saved as a separate option called "redcap_token". Neither of these options will persist across R sessions- if you wish to further reduce API calls, consider saving the bundle for future use. 
```r
# Gather project metadata for use with other functions
rc_setup(url = [Your REDCap API URL],
         token = [Your project API token])

# Save metadata for use in future sessions
saveRDS(getOption('redcap_bundle'), 'redcap_bundle.RDS')

# If you already have a saved bundle, load it as an option
options(redcap_bundle = readRDS("Path/to/bundle.RDS"))


# Additionally, you can use the `return.object` argument to save the 
# bundle as an object for easy access. It will still be saved to an 
# option unless `create.option` is set to FALSE. 
bundle = rc_setup(url = [Your REDCap API URL],
                  token = [Your project API token],
                  return.object = TRUE)
```
Now that you've loaded the project metadata, you're ready to use other functions. Keep in mind that all functions other than import/export functions can be used without API access. All functions have arguments for the pieces of metadata they need, so that that data can be supplied without the use of `rc_setup()`/API access. For example, the most important piece of metadata is the "Data Dictionary", which can be downloaded manually from REDCap and supplied via `data_dict` arguments. 

However, assuming we've saved our metadata to an option like we did above, let's go ahead and walk through an example workflow, starting with exporting the data.
```r
# For this example we'll use a previously setup REDCap report
report_data = rc_export([report_ID])

> head(report_data)
  record_id redcap_event_name visit_1_sex visit_1_weight visit_2_weight visit_1_temp visit_2_temp
1     11435     visit_1_arm_1      Female             65             NA         96.7           NA
2     11435     visit_2_arm_1        <NA>             NA             89           NA         98.6
3     11945     visit_1_arm_1        Male             72             NA         97.8           NA
4     11945     visit_2_arm_1        <NA>             NA             64           NA         96.9
5     26479     visit_1_arm_1      Female             56             NA         98.1           NA
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
Another useful thing to do is to check for any data that appears to be missing. The `rc_missing()` function will produce a report of such data. The essential logic of the function is that if a particular event/variable combination has a value for any record, it is expected in all records. For more details, see the function documentation(`?rc_missing`).
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