Assignment B1
================
Andrew J. Fullerton
2024-10-31

## Exercise 1 & 2: make and document a function

``` r
#' @title Basic Summary Statistics by Strata
#' @description Basic function to calculate summary statistics within levels of a categorical variable.
#'
#' @param data a data frame or tibble containing `x` and `by`.
#' @param x a numeric variable in the dataset to compute summary statistics for. `x` was chosen as a name since it is commonly used as a standard name for the primary function argument.
#' @param by a categorical variable in the dataset by which to compute summary statistics. `by` was chosen as a name since it is is suggestive of the argument's purpose in specifying the categorical variable by which to stratify summary statistics.
#' @param na.rm a logical evaluation to TRUE or FALSE indicating whether `NA` values should be stripped. Default is `TRUE`. `na.rm` was chosen as the argument name to be consistent with usage and nomenclature in other functions in R.
#' @param ... for further arguments to modify the calculation and formatting of the summary statistics.
#'
#' @return a tibble with mean, median, range, and sample size calculations by strata.
#' @import tidyverse
#'
#' @examples
#' # Basic usage:
#' basic_stats(penguins, bill_length_mm, species) # displays summary stats by species
#' # Advanced usage:
#' basic_stats(penguins, bill_length_mm, species, na.rm = FALSE, trim = 0.1) # trims 10% of largest/smallest values when computing mean and includes NAs
basic_stats <- function(data, x, by, na.rm = TRUE, ...) {
  # Check if dependencies are installed and load
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
  stop("The tidyverse package is not installed. Please install it using install.packages('tidyverse').\n")
  } else {
  suppressMessages(library(tidyverse)) # load tidyverse without console message
  }
  
  # Check if input 'data' is a non-empty data frame or tibble
  if (!is.data.frame(data) || nrow(data) == 0 || ncol(data) == 0) {
    stop("Input data must be a non-empty dataframe or tibble.")
  }

  # Convert to tibble for consistency
  data <- as_tibble(data) 

  # Store variable names for use later in function
  x_name <- deparse(substitute(x))
  by_name <- deparse(substitute(by))
  
  # Check if 'x' exists and is numeric
  if (!x_name %in% names(data)) {
    stop(x_name, " not found in the data.")
  } else if (length(na.omit(data |> pull({{ x }}))) == 0) {
    stop(x_name, " doesn't contain any non-missing values.")
  } else if (!is.numeric(data |> pull({{ x }}))) {
    stop("Variable ", x_name, " must be numeric.")
  }

  # Check if 'by' exists and is usable
  if (!by_name %in% names(data)) {
    stop("Grouping variable ", by_name, " not found in the data.")
  } else if (length(na.omit(data |> pull({{ by }}))) == 0) {
    stop("Grouping variable ", by_name, "doesn't contain any non-missing values.")
  }
  
  by_var <- data |> pull({{ by }}) # extract column from data frame as vector
  
  if (!is.factor(by_var)) {
    unique_count <- length(unique(by_var)) # count unique values in grouping var
    
    if (is.character(by_var) && unique_count < 20) {
      data <- data |> mutate({{ by }} := as.factor({{ by }})) # convert to factor
      warning(by_name, " has been converted to a factor with ", unique_count, " levels.")
    } else {
      stop("Grouping variable must be a non-empty factor or a character variable with fewer than 20 unique values.")
    }
  }
  
  # Final check for for sufficient 'by' factor levels
  if (length(unique(by_var)) < 2) {
    warning("Grouping variable ", by_name, " only has 1 level.")
  }

  # Run the summary
  result <- data |>
    group_by({{ by }}) |>
    summarise(
      mean = mean({{ x }}, na.rm = na.rm, ...),
      median = median({{ x }}, na.rm = na.rm, ...),
      range = paste0(min({{ x }}, na.rm = na.rm, ...), " - ",
                     max({{ x }}, na.rm = na.rm, ...)),
      n = n()
    )

  return(result)
}
```

## Exercise 3: examples

To demonstrate how this function works (and doesn’t work), we can use a
tried-and-true dataset among R users: `penguins`!

``` r
library(tidyverse) # load tidyverse for access to functions
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(palmerpenguins) # load library containing penguins dataset

glimpse(penguins)
```

    ## Rows: 344
    ## Columns: 8
    ## $ species           <fct> Adelie, Adelie, Adelie, Adelie, Adelie, Adelie, Adel…
    ## $ island            <fct> Torgersen, Torgersen, Torgersen, Torgersen, Torgerse…
    ## $ bill_length_mm    <dbl> 39.1, 39.5, 40.3, NA, 36.7, 39.3, 38.9, 39.2, 34.1, …
    ## $ bill_depth_mm     <dbl> 18.7, 17.4, 18.0, NA, 19.3, 20.6, 17.8, 19.6, 18.1, …
    ## $ flipper_length_mm <int> 181, 186, 195, NA, 193, 190, 181, 195, 193, 190, 186…
    ## $ body_mass_g       <int> 3750, 3800, 3250, NA, 3450, 3650, 3625, 4675, 3475, …
    ## $ sex               <fct> male, female, female, NA, female, male, female, male…
    ## $ year              <int> 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007, 2007…

`penguins` is a dataset composed of 8 variables (5 numeric variables,
and 3 factor variables) and 344 rows that we can use to demonstrate how
`basic_stats()` works!

### How it works:

`basic_stats()` takes as input a data frame or a tibble (`data`), a
numeric variable to compute summary statistics (mean, median, range, and
sample size) for (`x`) and a grouping variable by which to stratify
those summary statistics (`by`). Let’s see it in action:

``` r
basic_stats(penguins, bill_length_mm, species)
```

    ## # A tibble: 3 × 5
    ##   species    mean median range           n
    ##   <fct>     <dbl>  <dbl> <chr>       <int>
    ## 1 Adelie     38.8   38.8 32.1 - 46     152
    ## 2 Chinstrap  48.8   49.6 40.9 - 58      68
    ## 3 Gentoo     47.5   47.3 40.9 - 59.6   124

### Example 1: data frame and tibble inputs

To be `base R` friendly and `tidyverse` friendly, `basic_stats()`
accepts either a data frame or a tibble as input data. Here’s an example
with `penguins` as a data frame:

``` r
penguins |> 
  as.data.frame() |> # convert penguins to dataframe
  basic_stats(bill_length_mm, species)
```

    ## # A tibble: 3 × 5
    ##   species    mean median range           n
    ##   <fct>     <dbl>  <dbl> <chr>       <int>
    ## 1 Adelie     38.8   38.8 32.1 - 46     152
    ## 2 Chinstrap  48.8   49.6 40.9 - 58      68
    ## 3 Gentoo     47.5   47.3 40.9 - 59.6   124

Here’s an example using `penguins` as a tibble:

``` r
penguins |> 
  as_tibble() |> # convert penguins to tibble
  basic_stats(bill_length_mm, species)
```

    ## # A tibble: 3 × 5
    ##   species    mean median range           n
    ##   <fct>     <dbl>  <dbl> <chr>       <int>
    ## 1 Adelie     38.8   38.8 32.1 - 46     152
    ## 2 Chinstrap  48.8   49.6 40.9 - 58      68
    ## 3 Gentoo     47.5   47.3 40.9 - 59.6   124

Regardless of whether or a data frame or a tibble is passed in, the
final output from the function is a tibble.

### Example 2: factor and character variable inputs

Sometimes when we import a dataset into R, our categorical variables
will be classified as `chr` variables. For this reason, the `by`
argument can accept either a `fctr` or a `chr` as input. It should,
however, be noted that this function assumes that a `chr` variable with
more than 20 unique values is, in fact, storing text data rather than
categorical data; to avoid printing hundred of stratified statistics,
the function will throw an error instead of converting the `chr` to a
`fctr`. Here’s an example with species as a `fctr`:

``` r
basic_stats(penguins, bill_length_mm, species)
```

    ## # A tibble: 3 × 5
    ##   species    mean median range           n
    ##   <fct>     <dbl>  <dbl> <chr>       <int>
    ## 1 Adelie     38.8   38.8 32.1 - 46     152
    ## 2 Chinstrap  48.8   49.6 40.9 - 58      68
    ## 3 Gentoo     47.5   47.3 40.9 - 59.6   124

Here’s an example passing `species` into our function once again, but
this time as a `chr`.

``` r
penguins |> 
  mutate(species = as.character(species)) |> # convert species to character
  basic_stats(bill_length_mm, species)
```

    ## Warning in basic_stats(mutate(penguins, species = as.character(species)), :
    ## species has been converted to a factor with 3 levels.

    ## # A tibble: 3 × 5
    ##   species    mean median range           n
    ##   <fct>     <dbl>  <dbl> <chr>       <int>
    ## 1 Adelie     38.8   38.8 32.1 - 46     152
    ## 2 Chinstrap  48.8   49.6 40.9 - 58      68
    ## 3 Gentoo     47.5   47.3 40.9 - 59.6   124

As you can see, the outputs are identical, except for a warning to let
us know that the `chr` variable we passed into the function has been
converted to a `fctr`.

To demonstrate scenarios where a `chr` will not be converted to a
`fctr`, here’s one more example trying to pass `body_mass_g` into the
`by` argument of our function as a `chr`.

``` r
penguins |> 
  mutate(body_mass_g = as.character(body_mass_g)) |> # convert body mass to character
  basic_stats(bill_length_mm, body_mass_g)
```

    ## Error in basic_stats(mutate(penguins, body_mass_g = as.character(body_mass_g)), : Grouping variable must be a non-empty factor or a character variable with fewer than 20 unique values.

`body_mass_g` is a numeric variable with hundreds of unique values. To
avoid producing hundreds of rows worth of summary statistics, the
function throws an error here instead.

### Example 3: additional functionality using na.rm and …

By default, `basic_stats()` removes missing values from the data to
enable a clean computation. But, if you like to live dangerously, you
can change this by explicitly passing the argument `na.rm = FALSE` into
the function. Let’s see it in action:

``` r
basic_stats(penguins, bill_length_mm, species, na.rm = FALSE)
```

    ## # A tibble: 3 × 5
    ##   species    mean median range         n
    ##   <fct>     <dbl>  <dbl> <chr>     <int>
    ## 1 Adelie     NA     NA   NA - NA     152
    ## 2 Chinstrap  48.8   49.6 40.9 - 58    68
    ## 3 Gentoo     NA     NA   NA - NA     124

We can also pass additional arguments into the function (courtesy of
`...`) to customize and tweak the summary statistics. By passing
`trim = 0.1` into the function, for example, we can remove the most
extreme (i.e. highest/lowest) 10% of values before computing our summary
statistics.

``` r
basic_stats(penguins, bill_length_mm, species, trim = 0.1)
```

    ## # A tibble: 3 × 5
    ##   species    mean median range          n
    ##   <fct>     <dbl>  <dbl> <chr>      <int>
    ## 1 Adelie     38.8   38.8 0.1 - 46     152
    ## 2 Chinstrap  48.9   49.6 0.1 - 58      68
    ## 3 Gentoo     47.4   47.3 0.1 - 59.6   124

*And there you have `basic_stats()`!*

## Exercise 4: test the Function

Now that we’ve demonstrated what does/doesn’t work when using
`basic_stats()`, we should formally test a few things.

``` r
suppressMessages(library(testthat))

# Create test dataset with NAs
set.seed(12) # set starting point for randomization
iris_with_na <- iris |>
  mutate(Sepal.Length = replace(Sepal.Length, sample(n(), 10), NA)) # insert 10 NA values into the Sepal.Length column at random

test_that("mean, median, range, and n are computed and output is structured as intended", {
  result <- basic_stats(iris_with_na, Sepal.Length, Species)
  
  expect_s3_class(result, "tbl_df") # tibble output
  expect_equal(nrow(result), length(levels(iris$Species))) # 1 row per species
  expect_true(all(c("mean", "median", "range", "n") %in% names(result))) # summary stats as columns
  expect_false(any(is.na(result))) # no null values made it into the output
})
```

    ## Test passed 🎉

``` r
# Create test dataset with type incompatibilities
iris_chr_test <- iris |>
  mutate(
    Sepal.Length = as.character(Sepal.Length),
    Species = as.character(Species)
    )

iris_num_test <- iris |>
  mutate(Sepal.Length = as.factor(Sepal.Length))

test_that("variable types are handled appropriately", {
  
  expect_error(basic_stats(iris_chr_test, Sepal.Width, Sepal.Length), 
               "Grouping variable must be a non-empty factor or a character variable with fewer than 20 unique values.")
  expect_warning(basic_stats(iris_chr_test, Sepal.Width, Species), 
                 "Species has been converted to a factor with 3 levels.")
  expect_error(basic_stats(iris_num_test, Sepal.Length, Species),
             "Variable Sepal.Length must be numeric.")
})
```

    ## Test passed 🌈
