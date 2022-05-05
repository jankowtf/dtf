# dti 0.0.0.9003 (2022-05-05)

drop

----------

# dti 0.0.0.9002 (2022-05-02)

Refactored bundle code

----------

# dti 0.0.0.9001 (2022-04-25)

Renamed to 'dti'

- Refactored bundle code: 
    - `dt_bundle_autofill()`
    - `dt_bundle_buttons()`
    - `dt_bundle_colreorder()`
    - `dt_bundle_fixedcolumns()`
    - `dt_bundle_fixedheader()`
- Modified column position lookup which resulted in new function `lookup_column_positions()` (previous function was deleted)
- Added prod dependencies
    - `logger`
    - `snakecase`
    - `rappster/drop`
- Unit tests not completely refactored yet

----------

# dti 0.0.0.9000 (2022-04-14)

- Added a `NEWS.md` file to track changes to the package.
- Added `R/dt.R` code from another project
- Added prod dependencies
    - `DT`
    - `rappster/valid`
    - `rappster/confx`
    - `assertthat`
