# dti 0.0.0.9010 (2022-05-15)

Fixed UI module

- Module UI uses `DT::dataTableOutput()` instead of `shiny::dataTableOutput()` as this seems to fix the issue of the DT not being displayed correctly. Not sure why the one works while the other doesn't, though. Also added some tracing info for namespacing stuff just in case

----------

# dti 0.0.0.9009 (2022-05-15)

Roxygen and colreorder

----------

# dti 0.0.0.9008 (2022-05-15)

Shiny modules exported

----------

# dti 0.0.0.9007 (2022-05-15)

Small fixes

- Fixed forgotten `postprocess()`
- Fixed dependency issue with `drop` (hopefully)

----------

# dti 0.0.0.9006 (2022-05-15)

Shiny module

- Modified `datatable2()`: 
    - `.verbose`
    - `logger::log_trace()`
- Fixed multi dom inputs in `dt_process_bundles_list()`
- Added internal util functions:
    - `dt_process_bundles_list_merge()`
    - `dt_process_bundles_list_postprocess()`
    - `dt_process_bundles_list_postprocess_align_dom()`
    - `dt_process_bundles_merge()` (legacy)
    - `dt_process_bundles_postprocess()` (legacy)
- Disclaimers: unit test still need to be aligned but glancing over the test results only indicates deviances wrt default `dom` structure/letter order or options structure wrt `dom`

----------

# dti 0.0.0.9005 (2022-05-15)

Fixed list bundle processing

- Fixed `dt_process_bundles()` for lists (`dt_process_bundles_list()`)
- Refactored `dt_bundle_dom()`
- Removed arg `.verbose` from `dt_bundles_*()` functions. Tracing better happens
either through `dt_process_bundles()` or through `datatable2(..., verbose = TRUE)`
- Remaining TODO: align unit tests

----------

# dti 0.0.0.9004 (2022-05-14)

Length menue + internationalization

- Refactored `dt_bundle_lengthmenue()`
- Refactored `dt_bundle_internationalization()`

----------

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
