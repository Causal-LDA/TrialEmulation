# Outcome Data Accessor and Setter

**\[experimental\]**

## Usage

``` r
outcome_data(object)

outcome_data(object) <- value

# S4 method for class 'trial_sequence'
outcome_data(object)

# S4 method for class 'trial_sequence'
outcome_data(object) <- value
```

## Arguments

- object:

  `trial_sequence` object

- value:

  `data.table` to replace and update in `@outcome_data`

## Value

The `object` with updated outcome data

## Details

Generic function to outcome data

## Examples

``` r
ts <- trial_sequence("ITT")
new_data <- data.table::data.table(vignette_switch_data[1:200, ])
new_data$weight <- 1
outcome_data(ts) <- new_data
```
