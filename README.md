# Cherry Chill

## Overview

This is the 'lightweight' repo containing a Shiny app and a R package (eventually) for computing chill portions based on the modeled bark temperature.

This repo does *not* contain the heavy weight code for downloading and gap filling CIMIS data. For that, see the cleancimis package. This repo does however use gap-filled 

\

## Cherry Tree-Chill Shiny App

This Shiny app creates a web app that computes and visualizes chill portions using a new "Tree Chill" method (as opposed to air temperature). The "tree chill" method models bark temperature using a statistical method that includes a number of weather variables (more than just air temperature).

### Questions about Features

Do we need / want print functionality?

Should chill portions be summarized by day (instead of hour)? Would make for a smoother graph, 

Which columns to include in the downloaded CSV?

### To Do - Shiny App

[ ] need content for the instructions

[ ] support cookies for the coordinates and crop year

[ ] when someone clicks on 'Calculate', turn the cursor into a hourglass until the computations are complete

[ ] get all active stations, find the closest n stations - can do this without sf package

[ ] replace stn_data with DuckDB

[ ] figure out how large of a geographic area we can expand to

[ ] add some unit tests

[ ] phone users - disable keyboard for select input (drop down)

[ ] phone users - disable pan and zoom controls on plotly

[x] eliminate packages not needed to save memory

\

## Cherry Tree-Chill R Package

Biggest challenge is how to serve the gap-filled CIMIS data


 