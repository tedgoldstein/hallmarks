# Output example

hallmark chart


`shiny::runGitHub("hallmarks", "tedgoldstein", subdir="output")`

## Implementation

`hallmarks.R` contains reusable R functions `radarChartOutput` and
`renderRadarChart` that can be called from a Shiny app's `ui.R` and
`server.R` (respectively) to add Hallmark Radar charts.

The file `www/hallmarks-binding.js` contains the JavaScript code
that defines a custom Shiny output binding for the Hallmark Radar charts.
It's loaded into the app implicitly by the `radarChartOutput` function.

The `www/d3` directory contain the 3rd party
libraries [D3](http://d3js.org/). They
also are loaded from `radarChartOutput`.

## Packaging note

This example is set up for easy reading of the code and easy running
via `shiny::runGitHub()`.

If instead we wanted to make this Hallmark Radar chart component easily
distributable to other Shiny users, we would set it up as a package.
`hallmarks.R` would go into the `R` subdirectory. The contents of
`www` would be moved to `inst`, and `radarChartOutput` would call
`shiny::addResourcePath` to make them available at a URL prefix
like `"hallmarks"`. See
