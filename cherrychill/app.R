###################################################################################
# Cherry Chill Web App
# Authors: Andy Lyons
# Description: 
# Reactivity diagram: https://miro.com/app/xxxxxxxxx (coming soon)
#
# Live: https://ucanr-igis.shinyapps.io/cherrychill/
# GH:   https://github.com/ucanr-igis/cherrychill
###############################################################################

app_version_num <- "0.2.2"
app_version_date <- as.Date("2025-08-20")

cat(crayon::green("STARTING UP CHERRY CHILL APP VER ", app_version_num, "\n", sep = ""))

## Get the baseline memory used and print it to the console
## We're doing this in order to diagnose where memory is being used, and how to optimize it.
library(lobstr)
memory_used <- lobstr::mem_used()
cat(" - Memory used before loading packages: ", format(memory_used, "MB"), "\n", sep = "")

## Load the packages dynamically, so we can see their impact on memory
cat(crayon::green("Loading packages \n"))

req_pkg <- c(
  "shiny",
  "data.table",
  "dplyr",
  "magrittr",
  "bslib",
  "purrr",
  "readr",
  "lubridate",
  "conflicted",
  "cookies",
  "crayon",
  "FNN",
  "glue",
  "httr2",
  "leaflet",
  #"mgcv",     ## apparently not needed, saving ~74 MB
  #"oce",      ## pulled out just sunAngle, saving ~33 MB
  "plotly",
  "readr",
  "sf",
  "shinyhelper",
  "stringr",
  #"tidyverse",   ## individual tidyverse packages loaded, small memory savings 
  "tidyr",
  "tsibble",
  "ChillModels"
  #"chillR"      ## apparently not needed, saving 36 MB
)

for (pkg in req_pkg) {
  if (pkg %in% .packages()) {
    cat(crayon::silver(" - ", pkg, " already on the search path \n", sep = ""))  ## Was probably in the DEPENDS section of a previous package
  } else {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE, warn.conflicts = FALSE)) 
    memory_used_now <- lobstr::mem_used()
    cat(crayon::silver(" - loaded ", pkg, " (memory used = ", format(memory_used_now - memory_used, "MB"), ")\n", sep = ""))
    memory_used <- memory_used_now
  }
}

## Next, in order to to help rsconnect find all dependent packages when we publish this to ShinyApps.io, we repeat the 
## library calls using traditional library() (should have no impact on memory and be very fast).
## Details: https://rstudio.github.io/rsconnect/reference/appDependencies.html
library(shiny); library(data.table); library(dplyr); library(magrittr); library(bslib); library(purrr); library(readr); 
library(lubridate); library(conflicted); library(cookies); library(crayon); library(FNN); library(glue); library(httr2); 
library(leaflet); library(plotly); library(readr); library(sf); library(shinyhelper); library(stringr); library(tidyr); 
library(tsibble); library(ChillModels)

cat(" - After loading packages, the memory used is",  format(lobstr::mem_used(), "MB"), "\n\n")

## Next, declare session-wide function preferences
conflicted::conflicts_prefer(dplyr::filter, dplyr::select, 
                             lubridate::yday, lubridate::year, lubridate::month, lubridate::hour, lubridate::minute, lubridate::second,
                             .quiet = TRUE)

## Source scripts in "Rscripts" directory
## I have switched to sourcing the scripts rather than put them in the "R" directory and have them auto-run, so that
## I can track the size (memory) of the objects created by them (notably datasets and the GAM model). When they auto-run,
## they don't appear in ls() so I'm not sure which environment they get stored in.
cat(crayon::green("Sourcing scripts \n"))
scripts_dir <- "Rscripts"
scripts_fn <- list.files(scripts_dir)

# cat(" - Working directory:", getwd(), "\n")
# cat(" - RScripts directory exists:", dir.exists(scripts_dir), "\n")
# cat(" - Found these R files: ", paste(scripts_fn, collapse = ", "), "\n", sep = "")

for (one_script in scripts_fn) {
  cat(crayon::silver(" - Sourcing ", one_script, "\n", sep = ""))
  source(paste0(scripts_dir, "/", one_script), local = TRUE)
  cat(crayon::silver("   - Done. Memory used is now ",  format(lobstr::mem_used(), "MB"), "\n", sep = ""))
}
cat(" - After sourcing all scripts, the memory used is",  format(lobstr::mem_used(), "MB"), "\n\n")

# View objects in memory
# sort( sapply(ls(),function(x) format(object.size(get(x)), units = "Mb")))
#  - if needed use ls(.GlobalEnv)
# The two big ones are 
# stn_data, "152.9 Mb"
# gam_T_tree_0, "164.1 Mb" 

## Select a bootstrap theme
my_bs_theme <- bs_theme(version = 5, bootswatch = "sandstone")

## Define the values for the crop years select input
## We use a named vector where the values are date-strings and the names are labels which appear in the dropdown
years_supported <- 2022:2025
cropyears <- setNames(paste0("1/1/", years_supported), paste0((years_supported - 1), "-", substr(years_supported, 3, 4)))

## Import the AOI boundary for the leaflet map
visual_aoi_bnd_sf <- sf::st_read("data/cherry_regions_bnd.geojson", quiet = TRUE)

## Import the gap-filled CIMIS stations for the leaflet map
cherry_stns_4leaf_sf <- sf::st_read("data/cherry_cimis.geojson", quiet = TRUE)

## Tidy up my memory
rm(list = c("req_pkg", "pkg", "scripts_dir", "scripts_fn", "one_script"))

cat(crayon::green("Setup Complete \n"))
cat(" - At the end of the preamble, memory used is",  format(lobstr::mem_used(), "MB"), "\n\n")

## Setup UI
ui <- add_cookie_handlers(
    fluidPage(
      #useBusyIndicators(),   ## this was resulting in an spinner that doesn't stop
      style = "max-width: 680px; padding:15px;",
      theme = my_bs_theme,
        
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "cherrychill.css"),
            includeHTML("cherrychill_gtag.js")
        ),
        
        ## This CSS does *not* go in the head (TODO: find a more elegant way to do this)
        tags$style(type="text/css",
                   ".shinyhelper-container {display: inline-block; position: relative; margin-left: 1em;}"),
        
        titlePanel(title = "Tree Chill Calculator for Cherry",
                   windowTitle = "Cherry Chill"),
        
        navbarPage(
            "",  # navigation bar title, leave blank
            
            tabPanel("Compute Chill",
                     
                     includeMarkdown("mds/introduction.md"),
                     
                     tags$details(
                       tags$summary(
                         "Additional Notes",
                         id = "notes"
                       ),
                       includeMarkdown("mds/notes.md")
                     ),
                     
                     h3("1. Select location"),
                     
                     #p("Only locations in San Joaquin County are currently supported.", style="font-style:italic;"),
                     p("The estimates of tree chill will work best in the cherry growing regions shown in blue. The red dots are CIMIS weather stations.", style="font-style:italic;"),
                     
                     div(leafletOutput("mymap", height = 400), style = "max-width:600px; margin-bottom:1em;"),
                     
                     textInput("in_coordstxt", label = "Coordinates: ", placeholder = "37.365, -120.425") |>
                         shinytag_add_class("space_above_below") |>
                         shinytag_add_class("iblock") |>
                         helper(type = "markdown",
                                icon = "circle-info",
                                content = "coordinates",
                                buttonLabel = "OK",
                                size = "m"),
                     
                     textOutput("out_coordstxt_errmsg") |> shinytag_add_class("error-msg"),
                     
                     h3("2. Select crop year"),
                     
                     p("The web app will only show chill for a single crop year. Researchers who want to compute chill for multiple years can use the R package.", 
                       style="font-style:italic;"),
                     
                     selectInput("intxt_cropyear", label = "",
                                 choices = cropyears, selected = cropyears[length(cropyears)], multiple = FALSE), 
                     
                     # h3("3. Select chill portion thresholds"),
                     # 
                     # p("The idea here is to allow the user to enter one or more accumulated chill portions to highlight in the results (i.e., with a vertical line in the plot, with a red font in the table. But only if that would be useful", 
                     #   style = "font-style:italic; margin-bottom:2em;"),
                     
                     actionButton("cmd_calc", "Calculate", width = "80%"),
                     
                     htmlOutput("out_txt_cmdrun_errmsg") |> shinytag_add_class("error-msg"),
                     
                     conditionalPanel(
                         condition = "output.showrpt",
                         hr(),
                         h4("Results", class = "space-above"),
                         
                         #uiOutput("out_ui_reporthead"),
                         
                         # p("Note: The following are real data, but not for the selected location.", 
                         #   style = "text-align:center; font-style:italic; color:red;"),
                         
                         plotlyOutput("outply_treechill", inline = FALSE),
                         
                         p(strong("Notes")),
                         
                         tags$ul(
                           tags$li("Location: ", textOutput("outtxt_coords", inline = TRUE)),
                           tags$li("Crop year: ", textOutput("outtxt_cropyear", inline = TRUE)),
                           tags$li("CIMIS station used: ", textOutput("outtxt_cimis_stn", inline = TRUE)),
                           tags$li(downloadLink("cmd_download_csv", label = "Download data"))
                         ),
                         
                         # uiOutput("outui_plotnotes"),
                         
                         # p("Next Steps:"),
                         # tags$ol(
                         #     tags$li("TREE CHILL"),
                         #     tags$ol(
                         #         tags$li("Find the closest CIMIS station and the 3 nearest neighbors"),
                         #         tags$li("Identify the RDS file that has the variables for the tree chill GAM"),
                         #         tags$li("Download the file from S3 (could skip this for the prototype app)"),
                         #         tags$li("Apply the prediction")
                         #     ),
                         #     tags$li("AIR CHILL"),
                         #     tags$ol(
                         #         tags$li("Get the RDS file for the closest CIMIS station"),
                         #         tags$li("Download the file from S3 (could skip this for the prototype app)"),
                         #         tags$li("Compute chill portions")
                         #     ),
                         #     tags$li("Present results as a i) plot, and ii) DT table"),
                         # ),
                         
                         #h4("Results", class = "space-above"),
                         #DT::dataTableOutput("milestones_tbl", width = "600px")
                     ),
                     br()
                     
            ), ## end tab panel calculate
            
            tabPanel("More Info",
                     includeMarkdown("mds/moreinfo.md")
            ),

            tabPanel("Contact Us",
                     # h3("Contact Us"),
                     div(tags$iframe("Loading...",
                                     src = "https://docs.google.com/forms/d/e/1FAIpQLSd6Flaf5FzFV4YcGV2TYaaH9le4qbtOfIujuMh7CBbjcJZyUQ/viewform?embedded=true",
                                     width = "700", height = "1150",
                                     frameborder = "0", marginheight = "0", marginwidth = "0"),
                         style="max-width:900px;")
            )
            
        ), ## end of NavBarPage
        
        ### Footer Row 
        fluidRow(
            #includeHTML("footer.html")
            column(12,
                   id = "footer",
                   style = "margin:2em 0; padding-top:1em; border-top:4px solid #ddd;",
                   div(style="display:flex; flex-wrap:wrap; justify-content:space-between; align-items:start; margin-bottom:24px; border:none;",
                       
                       div(style="border:none; padding:12px;",
                           a(href = "https://www.plantsciences.ucdavis.edu/", target="_blank", rel="noopener", 
                             img(src="ucd-plantsciences-logo_315x50.png"))),
                       
                       div(
                         style = "padding:12px;",
                         a(id = "igis",
                           href = "https://igis.ucanr.edu/", target="_blank", rel="noopener",
                           div(
                             style="display:flex; flex-wrap:wrap; align-items:center;",
                             div("Another R-Shiny", br(), "app from IGIS", 
                                 style = "font-size:90%; font-style:italic; text-align:center;"),
                             div(img(src = "igis-logo_60x60.png", height="60", width="60",
                                     style = "padding-left:5px;"))
                             )  
                           )
                       )
                       
                   )  ## end footer main div
            )
        )
        
        
        
    )
    
)

server <- function(input, output) {
    
    ## Add an observer to watch out for clicks on the shinyhelper buttons.
    observe_helpers()
    
    ## ----- loc_xy() (initialize) --------
    loc_xy <- reactiveVal()
    
    ## ----- treechill_tbl() (initialize) --------
    ## This reactive val wil be populated with a tibble with three cols: date, cp_air, cp_tree
    treechill_tbl <- reactiveVal()
    
    ## ----- rpt_summary_html() (initialize) --------  NO LONGER NEEDED
    # rpt_summary_html <- reactiveVal()
    
    ## ---- output$showrpt (reactive) ----
    output$showrpt <- reactive({
        !is.null(treechill_tbl())                                     
    })
    outputOptions(output, "showrpt", suspendWhenHidden = FALSE)
    
    # ## ------ start_dt() reactive -------
    # start_dt <- reactive({
    #     make_date(year = as.integer(input$intxt_cropyear) - 1, month = 9, day = 1)
    # })
    # 
    # ## ------ end_dt() reactive -------
    # end_dt <- reactive({
    #     make_date(year = as.integer(input$intxt_cropyear), month = 8, day = 31)
    # })
    
    ## ------ observeEvent input$intxt_cropyear --------------
    observeEvent(input$intxt_cropyear, {
        ## Nuke treechill_tbl() which will cause the report to hide
        treechill_tbl(NULL)
    })
    
    ## --- output$mymap (initialize) ----
    ## Add layers and set the initial map extent (called once but never called again after that)
    output$mymap <- renderLeaflet({
      
      leaflet(data = visual_aoi_bnd_sf,
              options = leafletOptions(minZoom = 6, maxZoom = 18)) |> 
        addTiles(group = "Open Street Map") |> 
        addProviderTiles("Esri.WorldImagery", group = "Satellite") |> 
        addLayersControl(baseGroups = c("Open Street Map", "Satellite"),
                         options = layersControlOptions(collapsed = FALSE)) |>  
        addPolygons(fillOpacity = 0, options = pathOptions(interactive = FALSE)) |> 
        addCircles(data = cherry_stns_4leaf_sf |> select(stid, name), 
                   stroke = TRUE, color = "#f00", opacity = 1, weight = 4,
                   fill = TRUE, fillOpacity = 1, popup = ~paste0(stid, ": ", name)) |> 
        setMaxBounds(lng1 = -124.41, lat1 = 32.5, lng2 = -114.13, lat2 = 42.0) |> 
        fitBounds(lng1 = -124.41, lat1 = 32.5, lng2 = -114.13, lat2 = 42.0) 
      
    })
    
    ## The following will run whenever input$mymap_click changes
    observeEvent(input$mymap_click, {
      
      ## Update loc_xy()
      ## This will trigger an observeEvent() to update the location of the marker on the map
      loc_xy(c(input$mymap_click$lng, input$mymap_click$lat))
        
      ## Update in_coordstxt
      updateTextInput(inputId = "in_coordstxt", value = paste(round(loc_xy()[2],4), round(loc_xy()[1],4), sep = ", "))
    })
    
    ## The following will run whenever in_coordstxt is updated
    ## It will parse the text and update loc_xy().
    observeEvent(input$in_coordstxt, {
        
        ## Stop here if input$in_coordstxt == ""
        req(input$in_coordstxt != "")
        
        ## input$in_coordstxt will always be a character of length 1, so the 
        ## following will always be a list of length 1
        coords_split <- stringr::str_split(input$in_coordstxt, ",")
        
        ## Only continue if the first element has two elements (meaning exactly one ',')
        if (length(coords_split[[1]]) == 2) {
            ## Parse out the coordinates
            mycoords <- as.numeric(trimws(coords_split[[1]]))
            if (NA %in% mycoords) {
                output$out_coordstxt_errmsg <- renderText("Enter the latitude and longitude coordinates in decimal degrees, separated by a comma. Example: 36.450, -120.226")
            } else {
                output$out_coordstxt_errmsg <- renderText(NULL)
                loc_xy(c(mycoords[2], mycoords[1]))
            }
        }
        
        ## Get rid of any error message below the cmd_run button
        output$out_txt_cmdrun_errmsg <- renderUI(NULL)
        
    })
    
    ## The following will run whenever loc_xy() changes. It will update the leaflet map.
    observeEvent(loc_xy(), {
        
        req(loc_xy()[1] >= -180 && loc_xy()[1] <= 180)
        req(loc_xy()[2] >= -90 && loc_xy()[1] <= 90)
        
        ## Clear existing markers and add a marker at the new location
        leafletProxy('mymap') |>
            clearMarkers() |>
            addMarkers(lng = loc_xy()[1],
                       lat = loc_xy()[2])
        
        # ## Update the error message about being in the county
        # ## 8/18/2025: We decided at the last meeting to *not* require selecting a point within an aoi
        # if (cc_pointinpoly(loc_xy(), sanjoaquin_bnd_sf)) {
        #     output$out_coordstxt_errmsg <- renderText(NULL)
        # } else {
        #     output$out_coordstxt_errmsg <- renderText("Please enter a location within San Joaquin County.")    
        # }

        ## Nuke treechill_tbl() which will cause the report DIV to hide
        treechill_tbl(NULL)
    })
    
    #------ input$cmd_calc (observeEvent) ----------------------
    observeEvent(input$cmd_calc, {
      
      cat(crayon::green(" - Starting cmd_calc, memory used is",  format(lobstr::mem_used(), "MB"), "\n"))

      ## Delete any existing error message
      output$out_txt_cmdrun_errmsg <- renderUI(NULL)
      
      ## Location check - not null
      if (is.null(loc_xy())) {
          output$out_txt_cmdrun_errmsg <- renderUI(HTML("No location selected."))
          return(NULL)
      }
      
      ## Location check - within boundary
      ## 8/18/2025: We decided at the last meeting to *not* require selecting a point within an aoi
      # if (!cc_pointinpoly(loc_xy(), sanjoaquin_bnd_sf)) {
      #     output$out_txt_cmdrun_errmsg <- renderText("Please enter a location within San Joaquin County.")    
      # }
      
      ## Populate treechill_tbl(). This will trigger other updates
      xx_tbl <- tree_chill(
        .orch_name = "My Orchard", 
          .orch_lat_dd = loc_xy()[2], 
          .orch_lon_dd = loc_xy()[1], 
          .date = input$intxt_cropyear
        ) |> 
        ungroup() |> 
        select(date_time, tree_temp = pred_avg_t_tree, air_temp = st_t_air_C_A,
               bark_chill_cum = cum_chill_pTtree, air_chill_cum = cum_chill_st_t_air_A)
      
      cat(crayon::green(" - After running tree_chill(), memory used is",  format(lobstr::mem_used(), "MB"), "\n"))
      
      treechill_tbl(xx_tbl)
      
      ## Other columns returned by tree_chill():
      # [1] "date_time"            "hour_pst"             "crop_year"            "hour_wave"            "doy_wave"             "st_et_mm_B"           "st_et_mm_C"          
      # [8] "st_et_mm_A"           "st_slr_w_m2_B"        "st_slr_w_m2_C"        "st_slr_w_m2_A"        "st_t_air_C_B"         "st_t_air_C_C"         "st_t_air_C_A"        
      # [15] "st_rh_pcnt_B"         "st_rh_pcnt_C"         "st_rh_pcnt_A"         "st_dew_point_C_B"     "st_dew_point_C_C"     "st_dew_point_C_A"     "st_wind_m_s_B"       
      # [22] "st_wind_m_s_C"        "st_wind_m_s_A"        "st_t_soil_C_B"        "st_t_soil_C_C"        "st_t_soil_C_A"        "st_ppt_mm_B"          "st_ppt_mm_C"         
      # [29] "st_ppt_mm_A"          "st_ppt_ma_A"          "st_ppt_ma_B"          "st_ppt_ma_C"          "dec_hour"             "date_time4sun"        "orch_sun_azmth"      
      # [36] "orch_sun_alt"         "orch_sun_alt_day"     "doy"                  "date"                 "st_et_mm_A_lag1"      "st_et_mm_A_lag2"      "st_et_mm_A_lag3"     
      # [43] "st_slr_A_lag1"        "st_slr_A_lag2"        "st_slr_A_lag3"        "st_t_air_A_lag1"      "st_t_air_A_lag2"      "st_t_air_A_lag3"      "st_t_soil_A_lag1"    
      # [50] "st_t_soil_A_lag2"     "st_t_soil_A_lag3"     "st_wind_A_lag1"       "st_wind_A_lag2"       "st_wind_A_lag3"       "cum_slr_A"            "orch"                
      # [57] "pred_avg_t_tree"      "chill_pTtree"         "chill_st_t_air_A"     "cum_chill_pTtree"     "cum_chill_st_t_air_A"
      
    })
    
    # output$out_summary <- renderUI(
    #   rpt_summary_html()
    # )
    
    #----------- output$outply_treechill (render) ------------------
    output$outply_treechill <- renderPlotly({
      req(treechill_tbl())
      
      orch_sf <- orch_coords2sf(.orch_name = "My Orchard",
                                .orch_lon_dd = isolate(loc_xy())[1],
                                .orch_lat_dd = isolate(loc_xy())[2])
      
      orch_stid <- cc_stn_nn_eal(cherry_stns_sf, orch_sf, nn = 1)
      
      treechill_tbl() |> 
        group_by(date = date(date_time)) |> 
        summarize(bark_chill_cum_dly = round(sum(bark_chill_cum), 1),
                  air_chill_cum_dly = round(sum(air_chill_cum), 1)) |> 
        plot_ly(x = ~date) |>
        add_lines(y = ~bark_chill_cum_dly, color = I("orange"), name = "Bark Chill (Cherry)") |>
        add_lines(y = ~air_chill_cum_dly, color = I("blue"), name = paste0("Air Chill (CIMIS #", closest_stn(), ")")) |>
        plotly::layout(title = list(text = "Chill Portions", x = 0.5),
          xaxis = list(title = ""),
          yaxis = list(title = "chill portions"),
          hovermode = 'x',
          legend = list(orientation = "h",   # show legend entries horizontally
                        xanchor = "center",  # use center of legend as anchor
                        x = 0.5)) |>
        config(
          displayModeBar = TRUE,
          staticPlot = FALSE,       ## default
          showTips = FALSE,         ## ??
          displaylogo = FALSE,       ## don't show the plotly logo
          modeBarButtonsToRemove = c("zoom", "pan", "autoScale", "zoomIn", "zoomOut", "resetScale")
        )
        
    })
    
    #----------- output$outtxt_coords (render) ------------------
    output$outtxt_coords <- renderText({
      req(loc_xy())
      paste(round(loc_xy(), 2)[c(2,1)], collapse = ", ")
    })
    
    #----------- output$closest_stn() (reactive) ------------------
    closest_stn <- reactive({
      req(loc_xy())
      orch_sf <- orch_coords2sf(.orch_name = "My Orchard",
                                .orch_lon_dd = loc_xy()[1], 
                                .orch_lat_dd = loc_xy()[2])
      ## Return the closest station
      cc_stn_nn_eal(cherry_stns_sf, orch_sf, nn = 1)
    })
    
    #----------- output$outtxt_cimis_stn (render) ------------------
    output$outtxt_cimis_stn <- renderText({
      closest_stn()
    })
    
    #----------- output$outtxt_cropyear (render) ------------------
    output$outtxt_cropyear <- renderText({
      req(input$intxt_cropyear)
      names(cropyears)[cropyears == input$intxt_cropyear]
    })
    
    #----------- output$downloadData (downloadHandler) ------------------
    output$cmd_download_csv <- downloadHandler(
      filename = function() {
        # Use the selected dataset as the suggested file name
        paste0("chill-portions_", names(cropyears)[cropyears == input$intxt_cropyear], ".csv")
      },
      content = function(file) {
        # Write the dataset to the `file` that will be downloaded
        write.csv(treechill_tbl(), file = file, row.names = FALSE)
      }
    )
    
    #----------- output$outui_plotnotes (render) ------------------
    # output$outui_plotnotes <- renderUI(
    #   tagList(
    #     p(strong("Notes")),
    #     tags$ul(
    #       tags$li("Location: ", paste(round(loc_xy(), 2), collapse = ", ")),
    #       tags$li("CIMIS Station: ", paste(round(loc_xy(), 2), collapse = ", ")),
    #       tags$li("Crop year: ", names(cropyears)[cropyears == input$intxt_cropyear])
    #     )
    #   )
    # )
    # 
    
}

# Run the application 
shinyApp(
    ui = ui,
    server = server,
    options = list(launch.browser = TRUE) # Cookies only work in an actual browser.
)
