# Michigan 50 Safest Cities

## Description

Building on backgroundcheck.org's article [50 Safest Cities in Michigan 2016](http://backgroundchecks.org/50-safest-cities-in-michigan-2016.html), build an explorable, interactive interface to crime data:

* Sortable graphs showing the violent crime rate and property crime rate for each city, allowing
  * filtering by minimum population (e.g. "show populations of 10,000 or greater");
  * displaying the top $n$, up to 50;
  * sort from best to worst or from worst to best;
* A table allowing exploration of all the data;
* An explorable comparisons graph allowing plotting of any one variable against any other variable.

## Version History

v 0.1: Sortable graphs built as static, local display.

v 0.5: Deployed as Shiny app without the comparisons graphs.

v 1.0: Deployed as Shiny app with comparisons graphs. Code cleaned up to put all data gathering within a local function, rather than creating global variables.

v 1.0.1: Added this readme file.

v 1.1: Made comparisons graph input selection dynamic based on data frame column names.

v 1.2: Added summary of correlation and r-squared between variables selected on x-y releationship graph.

## To Do

* Provide human-readable axis labels for comparison graph and for inputSelect() boxes.
* Change comparisons graph to ggvis and add tooltip for city, x and y variable.
