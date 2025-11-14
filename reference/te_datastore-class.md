# te_datastore

This is the parent class for classes which define how the expanded trial
data should be stored. To define a new storage type, a new class should
be defined which inherits from `te_datastore`. In addition, methods
[save_expanded_data](https://causal-lda.github.io/TrialEmulation/reference/save_expanded_data.md)
and `read_expanded_data` need to be defined for the new class.

## Value

A 'te_datastore' object

## Slots

- `N`:

  The number of observations in this data. Initially 0.
