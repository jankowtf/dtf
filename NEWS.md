# dtf 0.0.0.9020 (2022-05-31)

Filter & default bundles

- Added `valid_dt_filter_value()`
- Modified module `mod_render_dt.R`: 
    - added arg `filter` to enable column filters
    - added arg `.bundles_default` to better control default bundles

----------

# dtf 0.0.0.9019 (2022-05-31)

Fixed verbose

- Fixed type in `mod_render_dt_server`: `.verbose = verbose` in call to `datatable2()`

----------

# dtf 0.0.0.9018 (2022-05-31)

Verbose

- Modified module `mod_render_dt.R`: renamed arg from `.verbose` to `verbose` to make it more consistent with other packages (e.g. `shimo.eda`)

----------

# dtf 0.0.0.9017 (2022-05-30)

Bugfix in tracing shiny NS info

- Fixed `shiny_trace_ns()`: now split into `shiny_trace_ns_ui()` and `shiny_trace_ns_server()`
- Modified `mod_render_dt_ui`: uses `shiny_trace_ns_ui()`
- Modified `mod_render_dt_server`: uses `shiny_trace_ns_server()`

----------

# dtf 0.0.0.9016 (2022-05-30)

Tracing shiny NS info

- Added `shiny_trace_ns()` to trace shiny NS info
- Modified module `mod_render_dt.R`: refactred tracing part by using `shiny_trace_ns()`

----------

# dtf 0.0.0.9015 (2022-05-30)

Fixed verbose for UI function (no input)

- Updated tracing info when setting `.verbose = TRUE` in `mod_render_dt_ui()`

----------

# dtf 0.0.0.9014 (2022-05-30)

Verbose

- Updated tracing info when setting `.verbose = TRUE` in module `mod_render_dt.R`

----------

# dtf 0.0.0.9013 (2022-05-30)

Renamed to 'dtf'

- Renamed the package to `dtf` as I discovered that `dti` was already taken by a
CRAN package

----------

# dtf 0.0.0.9012 (2022-05-30)

New default: id = character()

- Modified module `mod_render_dt.R`: `id = character()`
- Modified `dt_bundle_dom()`: new order logic (capital letters first then lowercase letters)
- Aligned unit tests to new `dom` order logic

----------

# dtf 0.0.0.9011 (2022-05-15)

Internationalization

- Added convenience functions for internationalization: 
    - `dt_bundle_internationalization_en()`
    - `dt_bundle_internationalization_de()`

----------

# dtf 0.0.0.9010 (2022-05-15)

Fixed UI module

- Module UI uses `DT::dataTableOutput()` instead of `shiny::dataTableOutput()` as this seems to fix the issue of the DT not being displayed correctly. Not sure why the one works while the other doesn't, though. Also added some tracing info for namespacing stuff just in case

----------

# dtf 0.0.0.9009 (2022-05-15)

Roxygen and colreorder

----------

# dtf 0.0.0.9008 (2022-05-15)

Shiny modules exported

----------

# dtf 0.0.0.9007 (2022-05-15)

Small fixes

- Fixed forgotten `postprocess()`
- Fixed dependency issue with `drop` (hopefully)

----------

# dtf 0.0.0.9006 (2022-05-15)

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

# dtf 0.0.0.9005 (2022-05-15)

Fixed list bundle processing

- Fixed `dt_process_bundles()` for lists (`dt_process_bundles_list()`)
- Refactored `dt_bundle_dom()`
- Removed arg `.verbose` from `dt_bundles_*()` functions. Tracing better happens
either through `dt_process_bundles()` or through `datatable2(..., verbose = TRUE)`
- Remaining TODO: align unit tests

----------

# dtf 0.0.0.9004 (2022-05-14)

Length menue + internationalization

- Refactored `dt_bundle_lengthmenue()`
- Refactored `dt_bundle_internationalization()`

----------

# dtf 0.0.0.9003 (2022-05-05)

drop

----------

# dtf 0.0.0.9002 (2022-05-02)

Refactored bundle code

----------

# dtf 0.0.0.9001 (2022-04-25)

Renamed to 'dtf'

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

# dtf 0.0.0.9000 (2022-04-14)

- Added a `NEWS.md` file to track changes to the package.
- Added `R/dt.R` code from another project
- Added prod dependencies
    - `DT`
    - `rappster/valid`
    - `rappster/confx`
    - `assertthat`
