# Extensions --------------------------------------------------------------

#' Valid DT extensions
#'
#' See https://datatables.net/extensions/index
#'
#' @param ...
#' @param reverse ([logical]) Reverse order yes/no
#' @param flip ([logical]) Flip names and choices yes/no
#' @param unname ([logical]) Drop names yes/no
#'
#' @return
#' @export
#'
#' @examples
#' valid_dt_extensions()
#' valid_dt_extensions(unname = FALSE)
#' valid_dt_extensions(flip = TRUE)
#' valid_dt_extensions(flip = TRUE, unname = FALSE)
valid_dt_extensions <- function(
    ...,
    .unname = TRUE
) {
    choice <- rlang::list2(...) %>%
        unlist()

    choices <- c(
        "AutoFill",
        "Buttons",
        "ColReorder",
        "Editor",
        "FixedColumns",
        "FixedHeader",
        "KeyTable",
        "Responsive",
        "RowGroup",
        "RowReorder",
        "Scroller",
        "SearchBuilder",
        "Select",
        "SearchPanes",
        "ColVis"
    )
    names(choices) <- choices #%>% snakecase::to_snake_case()

    # valid::valid(
    #     ...,
    #     # choice = choice,
    #     choices = choices,
    #     # ...,
    #     # reverse = .reverse,
    #     # flip = .flip,
    #     unname = .unname
    # )
    valid::valid2(..., .choices = choices, .unname = .unname)
}

# Options -----------------------------------------------------------------


#' Valid DT options
#'
#' See https://datatables.net/reference/option/
#'
#' @param ...
#' @param reverse
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options <- function(
    ...,
    .unname = TRUE
) {
    # value <- rlang::list2(...) %>%
    #     unlist()

    choices <- c(
        "deferLoading",
        "destroy",
        "displayStart",
        "dom",
        "lengthMenu",
        "order",
        "orderCellsTop",
        "orderClasses",
        "orderFixed",
        "orderMulti",
        "pageLength",
        "pagingType",
        "renderer",
        "retrieve",
        "rowId",
        "scrollCollapse",
        "search",
        "search.caseInsensitive",
        "search.reges",
        "search.search",
        "search.smart",
        "searchCols",
        "searchDelay",
        "stateDuration",
        "stripeClasses",
        "tabIndex"
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid DT options for `Features`
#'
#' See https://datatables.net/reference/option/
#'
#' @param ...
#' @param reverse
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options_features <- function(
    ...,
    .unname = TRUE
) {
    # value <- rlang::list2(...) %>%
    #     unlist()

    choices <- c(
        "autoWidth",
        "deferRender",
        "info",
        "lengthChange",
        "ordering",
        "paging",
        "processing",
        "scrollX",
        "scrollY",
        "searching",
        "serverSide",
        "stateSave"
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid DT options for `Data`
#'
#' See https://datatables.net/reference/option/
#'
#' @param ...
#' @param reverse
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options_data <- function(
    ...,
    .unname = TRUE
) {
    value <- rlang::list2(...) %>%
        unlist()

    choices <- c(
        "ajax",
        "ajax.data",
        "ajax.data$rc",
        "data"
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid DT options for `Callbacks`
#'
#' See https://datatables.net/reference/option/
#'
#' @param ...
#' @param reverse
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options_callbacks <- function(
    ...,
    .unname = TRUE
) {
    value <- rlang::list2(...) %>%
        unlist()

    choices <- c(
        "createdRow",     # Callback for whenever a TR element is created for the table's body.
        "drawCallback",   # Function that is called every time DataTables performs a draw.
        "footerCallback", # Footer display callback function.
        "formatNumber",   # Number formatting callback function.
        "headerCallback", # Header display callback function.
        "infoCallback",   # Table summary information display callback.
        "initComplete",   # Initialisation complete callback.
        "preDrawCallback", # Pre-draw callback.
        "rowCallback", # Row draw callback.
        "stateLoadCallback", # Callback that defines where and how a saved state should be loaded.
        "stateLoadParams", # State loaded - data manipulation callback
        "stateLoaded", # State loaded callback.
        "stateSaveCallback", # Callback that defines how the table state is stored and where.
        "stateSaveParams" # State save - data manipulation callback
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid DT options for `Columns`
#'
#' See https://datatables.net/reference/option/
#'
#' @param ...
#' @param reverse
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options_columns <- function(
    ...,
    .unname = TRUE
) {
    value <- rlang::list2(...) %>%
        unlist()

    choices <- c(
        "columnDefs",
        # Set column definition initialisation properties.
        "columnDefs.targets",
        # Assign a column definition to one or more columns.
        "columns",
        # Set column specific initialisation properties.
        "columns.cellType",
        # Cell type to be created for a column
        "columns.className",
        # Class to assign to each cell in the column
        "columns.contentPadding",
        # Add padding to the text content used when calculating the optimal width for a table.
        "columns.createdCell",
        # Cell created callback to allow DOM manipulation
        "columns.data",
        # Set the data source for the column from the rows data object / array
        "columns.defaultContent",
        # Set default, static, content for a column
        "columns.name",
        # Set a descriptive name for a column
        "columns.orderData",
        # Define multiple column ordering as the default order for a column
        "columns.orderDataType",
        # Live DOM sorting type assignment
        "columns.orderSequence",
        # Order direction application sequence
        "columns.orderable",
        # Enable or disable ordering on this column
        "columns.render",
        # Render (process) the data for use in the table
        "columns.searchable",
        # Enable or disable search on the data in this column
        "columns.title",
        # Set the column title
        "columns.type",
        # Set the column type - used for filtering and sorting string processing
        "columns.visible",
        # Enable or disable the display of this column
        "columns.width"
        # Column width assignment
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid DT options for `Internationalization`
#'
#' See https://datatables.net/reference/option/
#'
#' @param ...
#' @param reverse
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options_internationalization <- function(
    ...,
    .unname = TRUE
) {
    value <- rlang::list2(...) %>%
        unlist()

    choices <- c(
        "language",
        # Language configuration options for DataTables
        "language.aria",
        # Language strings used for WAI-ARIA specific attributes
        "language.aria.paginate",
        # WAI-ARIA labels for pagination buttons
        "language.aria.paginate.first",
        # WAI-ARIA label for the first pagination button
        "language.aria.paginate.last",
        # WAI-ARIA label for the last pagination button
        "language.aria.paginate.next",
        # WAI-ARIA label for the next pagination button
        "language.aria.paginate.previous",
        # WAI-ARIA label for the previous pagination button
        "language.aria.sortAscending",
        # Language strings used for WAI-ARIA specific attributes
        "language.aria.sortDescending",
        # Language strings used for WAI-ARIA specific attributes
        "language.decimal",
        # Decimal place character
        "language.emptyTable",
        # Table has no records string
        "language.info",
        # Table summary information display string
        "language.infoEmpty",
        # Table summary information string used when the table is empty of records
        "language.infoFiltered",
        # Appended string to the summary information when the table is filtered
        "language.infoPostFix",
        # String to append to all other summary information strings
        "language.lengthMenu",
        # Page length options string
        "language.loadingRecords",
        # Loading information display string - shown when Ajax loading data
        "language.paginate",
        # Pagination specific language strings
        "language.paginate.first",
        # Pagination 'first' button string
        "language.paginate.last",
        # Pagination 'last' button string
        "language.paginate.next",
        # Pagination 'next' button string
        "language.paginate.previous",
        # Pagination 'previous' button string
        "language.processing",
        # Processing indicator string
        "language.search",
        # Search input string
        "language.searchPlaceholder",
        # Search input element placeholder attribute
        "language.thousands",
        # Thousands separator
        "language.url",
        # Load language information from remote file
        "language.zeroRecords"
        # Table empty as a result of filtering string
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid DT options for `Static`
#'
#' See https://datatables.net/reference/option/
#'
#' @param ...
#' @param reverse
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options_static <- function(
    ...,
    .unname = TRUE
) {
    value <- rlang::list2(...) %>%
        unlist()

    choices <- c(
        "$.fn.dataTable.ext.errMode"
        # Set how DataTables will report detected errors
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid DT options for `AutoFill`
#'
#' See https://datatables.net/reference/option/#AutoFill
#'
#' @param choice
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options_autofill <- function(
    ...,
    .unname = TRUE
) {
    # value <- rlang::list2(...) %>%
    #     unlist()

    choices <- c(
        "autoFill",
        # Enable and configure the AutoFill extension for DataTables
        "autoFill.alwaysAsk",
        # Always ask the end user if an action should be taken or not
        "autoFill.columns",
        # Select the columns that can be auto filled
        "autoFill.editor",
        # Attach an Editor instance for database updating
        "autoFill.enable",
        # Initial enablement state of AutoFill
        "autoFill.focus",
        # Action that will cause the auto fill drag handle to appear in a cell
        "autoFill.horizontal",
        # Enable / disable user ability to horizontally drag and fill
        "autoFill.update",
        # Control automatic update of data when a fill drag is completed
        "autoFill.vertical",
        # Enable / disable user ability to vertically drag and fill
        "language.autoFill",
        # Container object for language strings used by AutoFill
        "language.autoFill.button",
        # Multi-fill selector button text
        "language.autoFill.cancel",
        # Multi-fill selector cancel option message
        "language.autoFill.fill",
        # Multi-fill selector message for the full fill fill type
        "language.autoFill.fillHorizontal",
        # Multi-fill selector message for the horizontal fill fill type
        "language.autoFill.fillVertical",
        # Multi-fill selector message for the vertical fill fill type
        "language.autoFill.increment",
        # Multi-fill selector message for the increment fill type
        "language.autoFill.info"
        # Information message shown at the top of the fill type selector
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid DT options for `AutoFill`
#'
#' See https://datatables.net/reference/option/#AutoFill
#'
#' @param choice
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options_autofill_focus <- function(
    ...,
    .unname = TRUE
) {
    choices <- c(
        "null",
        # Automatic detection - focus if KeyTable is enabled on a table and hover otherwise
        "click",
        # Display when a cell is clicked upon
        "focus",
        # Display when a cell gains focus - for integration with KeyTable
        "hover"
        # Display when a cell is hovered over
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid DT options for `Buttons`
#'
#' See https://datatables.net/reference/option/
#'
#' @param ...
#' @param reverse
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options_buttons <- function(
    ...,
    .unname = TRUE
) {
    value <- rlang::list2(...) %>%
        unlist()

    choices <- c(
        "buttons",
        # Buttons configuration object
        "buttons.buttons",
        # List of buttons to be created
        "buttons.buttons.action",
        # Action to take when the button is activated
        "buttons.buttons.attr",
        # Collection of attribute key / choices to set for a button
        "buttons.buttons.available",
        # Ensure that any requirements have been satisfied before initialising a button
        "buttons.buttons.className",
        # Set the class name for the button
        "buttons.buttons.destroy",
        # Function that is called when the button is destroyed
        "buttons.buttons.enabled",
        # Set a button's initial enabled state
        "buttons.buttons.extend",
        # Define which button type the button should be based on
        "buttons.buttons.init",
        # Initialisation function that can be used to add events specific to this button
        "buttons.buttons.key",
        # Define an activation key for a button
        "buttons.buttons.name",
        # Set a name for each selection
        "buttons.buttons.namespace",
        # Unique namespace for every button
        "buttons.buttons.tag",
        # Set the tag for the button
        "buttons.buttons.text",
        # The text to show in the button
        "buttons.buttons.titleAttr",
        # Button title attribute text
        "buttons.dom",
        # Options to control the DOM structure Buttons creates
        "buttons.dom.button",
        # DOM configuration for individual button elements
        "buttons.dom.buttonContainer",
        # DOM configuration of a button container element
        "buttons.dom.buttonLiner",
        # DOM configuration of a button liner element
        "buttons.dom.collection",
        # DOM configuration of the collection display
        "buttons.dom.container",
        # DOM configuration of the Buttons container element
        "buttons.name"
        # Set a name for the instance for the group selector
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

valid_dt_options_buttons_names <- function(
    ...,
    .unname = TRUE
) {
    choices <- c(
        "colvis",
        "copy",
        "csv",
        "excel",
        "pdf",
        "print"
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

valid_dt_options_buttons_select_names <- function(
    ...,
    .unname = TRUE
) {
    choices <- c(
        "selectAll",
        "selectNone",
        "selectRows",
        "selectColumns",
        "selectCells"
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid DT options for `ColReorder`
#'
#' See https://datatables.net/reference/option/
#'
#' @param ...
#' @param reverse
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options_colreorder <- function(
    ...,
    .unname = TRUE
) {
    value <- rlang::list2(...) %>%
        unlist()

    choices <- c(
        "colReorder",
        # Enable and configure the ColReorder extension for DataTables
        "colReorder.enable",
        # Initial enablement state of ColReorder
        "colReorder.fixedColumnsLeft",
        # Disallow x columns from reordering (counting from the left)
        "colReorder.fixedColumnsRight",
        # Disallow x columns from reordering (counting from the right)
        "colReorder.order",
        # Set a default order for the columns in the table
        "colReorder.realtime"
        # Enable / disable live reordering of columns during a drag
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid DT options for `FixedColumns`
#'
#' See https://datatables.net/reference/option/
#'
#' @param ...
#' @param reverse
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options_fixedcolumns <- function(
    ...,
    .unname = TRUE
) {
    value <- rlang::list2(...) %>%
        unlist()

    choices <- c(
        "fixedColumns",
        # Enable and configure the FixedColumns extension for DataTables
        "fixedColumns.heightMatch",
        # Row height matching algorithm to use
        "fixedColumns.leftColumns",
        # Number of columns to fix to the left of the table
        "fixedColumns.rightColumns"
        # Number of columns to fix to the right of the table
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid DT options for `FixedHeader`
#'
#' See https://datatables.net/reference/option/
#'
#' @param ...
#' @param reverse
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options_fixedheader <- function(
    ...,
    .unname = TRUE
) {
    value <- rlang::list2(...) %>%
        unlist()

    choices <- c(
        "fixedHeader",
        # FixedHeader configuration object
        "fixedHeader.footer",
        # Enable / disable fixed footer
        "fixedHeader.footerOffset",
        # Offset the table's fixed footer
        "fixedHeader.header",
        # Enable / disable fixed header
        "fixedHeader.headerOffset"
        # Offset the table's fixed header
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid DT options for `KeyTable`
#'
#' See https://datatables.net/reference/option/
#'
#' @param ...
#' @param reverse
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options_keytable <- function(
    ...,
    .unname = TRUE
) {
    value <- rlang::list2(...) %>%
        unlist()

    choices <- c(
        "keys",
        # Enable and configure the KeyTable extension for DataTables
        "keys.blurable",
        # Allow KeyTable's focus to be blurred (removed) from a table
        "keys.className",
        # Set the class name used for the focused cell
        "keys.clipboard",
        # Enable / disable clipboard interaction with KeyTable
        "keys.clipboardOrthogonal",
        # Set the orthogonal data to copy to clipboard
        "keys.columns",
        # Select the columns that can gain focus
        "keys.editOnFocus",
        # Control if editing should be activated immediately upon focus
        "keys.editor",
        # Attach an Editor instance for Excel like editing
        "keys.focus",
        # Cell to receive initial focus in the table
        "keys.keys",
        # Limit the keys that KeyTable will listen for and take action on
        "keys.tabIndex"
        # Set the table's tab index for when it will receive focus
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid DT options for `Responsive`
#'
#' See https://datatables.net/reference/option/
#'
#' @param ...
#' @param reverse
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options_responsive <- function(
    ...,
    .unname = TRUE
) {
    value <- rlang::list2(...) %>%
        unlist()

    choices <- c(
        "columns.responsivePriority",
        # Set column's visibility priority
        "responsive",
        # Enable and configure the Responsive extension for DataTables
        "responsive.breakpoints",
        # Set the breakpoints for a Responsive instance
        "responsive.details",
        # Enable and configure the child rows shown by Responsive for collapsed tables
        "responsive.details.display",
        # Define how the hidden information should be displayed to the end user
        "responsive.details.renderer",
        # Define the renderer used to display the child rows
        "responsive.details.target",
        # Column / selector for child row display control when using column details type
        "responsive.details.type",
        # Set the child row display control type
        "responsive.orthogonal"
        # Set the orthogonal data request type for the hidden information display
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid DT options for `RowGroup`
#'
#' See https://datatables.net/reference/option/
#'
#' @param ...
#' @param reverse
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options_rowgroup <- function(
    ...,
    .unname = TRUE
) {
    value <- rlang::list2(...) %>%
        unlist()

    choices <- c(
        "rowGroup",
        # Enable and configure the RowGroup extension for DataTables
        "rowGroup.className",
        # Set the class name to be used for the grouping rows
        "rowGroup.dataSrc",
        # Set the data point to use as the grouping data source
        "rowGroup.emptyDataGroup",
        # Text to show for rows which have null, undefined or empty string group data
        "rowGroup.enable",
        # Provides the ability to disable row grouping at initialisation
        "rowGroup.endClassName",
        # Set the class name to be used for the grouping end rows
        "rowGroup.endRender",
        # Provide a function that can be used to control the data shown in the end grouping row.
        "rowGroup.startClassName",
        # Set the class name to be used for the grouping start rows
        "rowGroup.startRender"
        # Provide a function that can be used to control the data shown in the start grouping row.
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid DT options for `RowReorder`
#'
#' See https://datatables.net/reference/option/
#'
#' @param ...
#' @param reverse
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options_rowreorder <- function(
    ...,
    .unname = TRUE
) {
    value <- rlang::list2(...) %>%
        unlist()

    choices <- c(
        "rowReorder",
        # Enable and configure the RowReorder extension for DataTables
        "rowReorder.dataSrc",
        # Configure the data point that will be used for the reordering data
        "rowReorder.editor",
        # Attach an Editor instance for database updating
        "rowReorder.enable",
        # Enable / disable RowReorder's user interaction
        "rowReorder.formOptions",
        # Set the options for the Editor form when submitting data
        "rowReorder.selector",
        # Define the selector used to pick the elements that will start a drag
        "rowReorder.snapX",
        # Horizontal position control of the row being dragged
        "rowReorder.update"
        # Control automatic of data when a row is dropped
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid DT options for `Scroller`
#'
#' See https://datatables.net/reference/option/
#'
#' @param ...
#' @param reverse
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options_scroller <- function(
    ...,
    .unname = TRUE
) {
    value <- rlang::list2(...) %>%
        unlist()

    choices <- c(
        "scroller",
        # Enable and configure the Scroller extension for DataTables
        "scroller.boundaryScale",
        # Set the point at which new data will be loaded and drawn
        "scroller.displayBuffer",
        # The amount of data that Scroller should pre-buffer to ensure smooth scrolling
        "scroller.loadingIndicator",
        # Display a loading message while Scroller is loading additional data
        "scroller.rowHeight",
        # Set the row height, or how the row height is calculated
        "scroller.serverWait"
        # Time delay before new data is requested when server-side processing is enabled
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid DT options for `SearchBuilder`
#'
#' See https://datatables.net/reference/option/
#'
#' @param ...
#' @param reverse
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options_searchbuilder <- function(
    ...,
    .unname = TRUE
) {
    stop("Not implemented yet")
    value <- rlang::list2(...) %>%
        unlist()

    choices <- c(

    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid DT options for `SearchPanes`
#'
#' See https://datatables.net/reference/option/
#'
#' @param ...
#' @param reverse
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options_searchpanes <- function(
    ...,
    .unname = TRUE
) {
    # stop("Not implemented yet")
    value <- rlang::list2(...) %>%
        unlist()

    choices <- c(
        "searchPanes"
        # Enable Search Panes
    )
    # TODO: implement remaining options

    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid DT options for `Select`
#'
#' See https://datatables.net/reference/option/
#'
#' @param ...
#' @param reverse
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
valid_dt_options_select <- function(
    ...,
    .unname = TRUE
) {
    value <- rlang::list2(...) %>%
        unlist()

    choices <- c(
        "language.select",
        # Container object for language strings used by Select
        "language.select.cells",
        # Table information summary string for the number of cells selected
        "language.select.columns",
        # Table information summary string for the number of columns selected
        "language.select.rows",
        # Table information summary string for the number of rows selected
        "select",
        # Select configuration object
        "select.blurable",
        # Indicate if the selected items will be removed when clicking outside of the table
        "select.className",
        # Set the class name that will be applied to selected items
        "select.info",
        # Enable / disable the display for item selection information in the table summary.
        "select.items",
        # Set which table items to select (rows, columns or cells)
        "select.selector",
        # Set the element selector used for mouse event capture to select items
        "select.style",
        # Set the selection style for end user interaction with the table
        "select.toggleable"
        # Disable the deselection of selected rows when clicked
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

#' Valid values for DT argument `filter`
#'
#' See https://rstudio.github.io/DT/
#'
#' @param ...
#' @param .unname
#'
#' @return
#' @export
#'
#' @examples
#' valid_dt_filter_values()
#' valid_dt_filter_values(1)
#' valid_dt_filter_values("top")
#' try(valid_dt_filter_values("invalid"))
valid_dt_filter_values <- function(
    ...,
    .unname = TRUE
) {
    value <- rlang::list2(...) %>%
        unlist()

    choices <- c(
        "none",
        "bottom",
        "top"
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}

if (FALSE) {
    "language.select


Container object for language strings used by Select

language.select.cells


Table information summary string for the number of cells selected

language.select.columns


Table information summary string for the number of columns selected

language.select.rows


Table information summary string for the number of rows selected

select


Select configuration object

select.blurable


Indicate if the selected items will be removed when clicking outside of the table

select.className


Set the class name that will be applied to selected items

select.info


Enable / disable the display for item selection information in the table summary.

select.items


Set which table items to select (rows, columns or cells)

select.selector


Set the element selector used for mouse event capture to select items

select.style


Set the selection style for end user interaction with the table

select.toggleable


Disable the deselection of selected rows when clicked" %>%
        stringr::str_split("\\n") %>%
        unlist() %>%
        `[`(. != c("")) %>%
        `[`(. != "\t") %>%
        cat(sep = "\n")
}

# Argument selection ------------------------------------------------------

#' Valid DT argument: selection
#'
#' See [DT::datatable]
#'
#' @param ...
#' @param reverse
#' @param flip
#'
#' @return
#' @export
#'
#' @examples
valid_dt_arg_selection <- function(
    ...,
    .unname = TRUE
) {
    value <- rlang::list2(...) %>%
        unlist()

    choices <- c(
        "none",
        "single",
        "multiple"
    )
    names(choices) <- choices

    valid::valid2(..., .choices = choices, .unname = .unname)
}
