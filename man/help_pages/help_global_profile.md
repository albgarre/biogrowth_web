Input for global fitting
------------------------

The user must input the (dynamic) environmental conditions during each
one of the experiments.. These are defined using an MS Excel file with
the following restrictions:

-   The data of each experiment must be contained in a single Excel
    sheet.
-   There should not be empty columns or rows in each sheet.
-   No additional data (other than the values of the environmental
    conditions) should be present in the sheet.
-   No additional sheets should be present in the Excel file.
-   Each sheet must contain a column named `time` including the elapsed
    time.
-   It has to contain one additional column per environmental condition.
    The tool can handle an arbitrary number of environmental conditions.
-   The first row must provide the column names.
-   The remaining rows define the value of each environmental condition
    at the elapsed time. Their values for intermediate values is
    calculated by linear interpolation. For values outside the range
    defined by `time`, the value of the closest limit is used.

Clicking the `Download example` link, downloads an example file with two
environmental conditions: temperature and pH.
