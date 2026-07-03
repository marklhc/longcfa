# Sample Ecological Momentary Assessment Data

Daily diary data from an ecological momentary assessment study examining
state social anxiety and perfectionistic self-presentation across 20
days.

## Usage

``` r
mackinnon_etal_long

mackinnon_etal_wide
```

## Format

A data frame with 4,252 rows and 12 columns (long format):

- id:

  Participant identifier

- day:

  Day of measurement (1-8)

- ssa1:

  State social anxiety item 1

- ssa2:

  State social anxiety item 2

- ssa3:

  State social anxiety item 3

- ssa4:

  State social anxiety item 4

- ssa5:

  State social anxiety item 5

- ssa6:

  State social anxiety item 6

- ssa7:

  State social anxiety item 7

- psp1:

  Perfectionistic self-presentation item 1

- psp2:

  Perfectionistic self-presentation item 2

- psp3:

  Perfectionistic self-presentation item 3

The wide format data only covers days 2-8, with 261 rows and 71 columns,
and the days are indicated by suffixes (e.g., `ssa1_2` for day 2).

## Source

Data available at: <https://osf.io/hwkem/files/r2qx4>,

## References

Mackinnon, S., Curtis, R., & O'Connor, R. (2022). A tutorial in
longitudinal measurement invariance and cross-lagged panel models using
lavaan. Meta-Psychology, 6. <https://doi.org/10.15626/MP.2020.2595>

Project page: <https://osf.io/hwkem/>, <https://osf.io/gduy4/>
