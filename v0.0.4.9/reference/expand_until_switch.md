# Check Expand Flag After Treatment Switch

Check if patients have switched treatment in eligible trials and set
`expand = 0`.

## Usage

``` r
expand_until_switch(s, n)
```

## Arguments

- s:

  numeric vector where `1` indicates a treatment switch in that period

- n:

  length of s

## Value

Vector of indicator values up until first switch.
