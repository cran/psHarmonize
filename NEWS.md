# psHarmonize 0.3.5

* Removed unnecessary spaces in DESCRIPTION file.
* Added verbose argument to `harmonization()` and `create_long_dataset()` functions.

# psHarmonize 0.3.4

* Changed title in description file to title case.
* Added data harmonization references in description file.

# psHarmonize 0.3.3

* Fixed notes and warnings in `check()` in preparation for submission to CRAN.
* Added rendered .Rmd readme.
* Added `.data` prefix in dplyr functions where appropriate.

# psHarmonize 0.3.2

* Fixed issue that prevented some numeric harmonized values from plotting.

# psHarmonize 0.3.1

* Added code to recognize incorrectly entered `id_var` in harmonization sheet. The 
cohort shell function will not use the corresponding dataset. Harmonization
will skip this row in the harmonization sheet, and record reason in error log.

* Added documentation for multiple R functions (mainly return values and examples).

* Added NA values for `possible_range` when not specified in harmonization sheet. 
Previously, an error occured when `possible_range` was absent.

# psHarmonize 0.3.0

* Added S3 summary method for psHarmonize return object object.

# psHarmonize 0.2.9

* Fix bug in categorical `possible_range` function.
* Added additional test for categorical `possible_range` function.

# psHarmonize 0.2.8

* Added feature that allows user to define the set of possible categorical values. 
* This is done by listing possible values in `possible_range` column in harmonization sheet.
* A user could enter "a, b, c, d" in the `possible_range` column if they wanted any other harmonized values (other than "a, b, c, d") set to NA.

# psHarmonize 0.2.7

* Added additional RMarkdown files for comparison reports, when multiple input variables are used.

# psHarmonize 0.2.6

* Fixed errors that occurred with some using `create_summary_report()` .

# psHarmonize 0.2.5

* Fixed error when using `create_error_log_report()`.

# psHarmonize 0.2.4

* Merged in pull request #1, that fixed error when source dataset was not present.

# psHarmonize 0.2.3

* Added warning message

# psHarmonize 0.2.2

* Added `reorder_factors()` function
* Fixed graph issue in `create_summary_report()`. Categorical variables with many values were not being plotted.
* Added a `NEWS.md` file to track changes to the package.
