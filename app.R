#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(tidyverse)

library(vroom)

library(sf)

library(tigris)

library(leaflet)

library(htmlwidgets)

library(shiny)

library(googleway)

library(leaflet.extras)

library(leaflet.extras2)

library(mapboxapi)

library(fontawesome)

library(mapview)

library(shinyjs)

library(prettymapr)

#Set the working directory
setwd("D:/R Exports/ShineyApp/SNA")
SNALayer <- readRDS("data/SNALayer.RDS")

ULEZLayer <- readRDS("data/ULEZ.RDS")

BoroughLayer <- readRDS("data/borough.RDS")

vars1 <- c(
  "Disable layer..." = "None",
  "Average NO2" = "NO2 Mean",
  "Average NOx" = "NOx Mean"
)

vars2 <- c(
  "Mapbox Standard" = "standard",
  "Mapbox Light" = "light",
  "Mapbox Streets" = "streets",
  "Mapbox Dark" = "dark",
  "Mapbox Satellite" = "satellite"
  
)


# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),
  title = "TfL SNA Tool",
  titlePanel(h5("TfL SNA Tool",align = "center",style="background-color:#10006A;color:#FFFFFF;font-size:5em;font-weight:bold;")),
  sidebarPanel(id="sidebar",h5("Pollution Map",style="color:#FFFFFF;font-size:30px;font-weight:bold;"),
               width = 3,
               h5("The pollution map includes a number of air and noise pollution datasets that are aggregated to the SNA cells as average values.
                  The headline data is LAEI data produced to show key air quality metrics such as NO2, NOx and PM2.5/10. More information about the LAEI data
                  can be found in the latest Travel in London report:",
                  style="color:#FFFFFF;font-size:16px;"),
               tags$a(href="https://tfl.gov.uk/corporate/publications-and-reports/travel-in-london-reports", h5("Travel in London Report",style="color:#68e2ed;font-size:16px;"),target="blank"),
               h5("Toggle Map Options:",style="color:#FFFFFF;font-size:20px;font-weight:bold;"),
               checkboxInput(label = h5("On map legend*",style="color:#FFFFFF;font-size:16px;position:relative;top:-7px;"),
                                       inputId = "maplegend",
                                       value=FALSE),
               checkboxInput(label = h5("Scalebar*",style="color:#FFFFFF;font-size:16px;position:relative;top:-7px;"),
                             inputId = "mapscale",
                             value=FALSE),
               checkboxInput(label = h5("Auto-generated title*",style="color:#FFFFFF;font-size:16px;position:relative;top:-7px;"),
                             inputId = "title",
                             value=FALSE),
               h5("* These items will render when the map is downloaded. It is recomended that these items be switched on before the Download Map option is used.",
                  style="color:#FFFFFF;font-size:16px;"),
               h5("Toggle App Options:",style="color:#FFFFFF;font-size:20px;font-weight:bold;"),
               checkboxInput(label = h5("Map Controls**",style="color:#FFFFFF;font-size:16px;position:relative;top:-7px;"),
                             inputId = "mapcontrol",
                             value=TRUE),
               checkboxInput(label = h5("SNA Layer Controls**",style="color:#FFFFFF;font-size:16px;position:relative;top:-7px;"),
                             inputId = "snalayercontrol",
                             value=TRUE),
               h5("** These items will not render when the map is downloaded.",
                  style="color:#FFFFFF;font-size:16px;"),
               tags$div(actionButton(inputId = "exp",label = "Set Default Export Options",icon = shiny::icon("fa-solid fa-map-location-dot",verify_fa = FALSE)),align="center",style="padding:5px;"),
               tags$div(actionButton(inputId = "exp_can",label = "Clear Default Export Options",icon = shiny::icon("fa-solid fa-map",verify_fa = FALSE)),align="center",style="padding:5px;"),
               tags$div(actionButton(inputId = "dl",label = "Download Map",icon = shiny::icon("fa-solid fa-download",verify_fa = FALSE)),align="center",style="padding:5px;")),
  tags$head(tags$style(HTML("#sidebar{background-color:#10006A;"))),
  mainPanel(
    leafletOutput("SNA",width = "auto",height = "auto"),tags$style(type = "text/css", "#SNA {height: calc(97vh - 80px) !important;}"),width = 9),
  absolutePanel(id = "controls",class ="panel panel-default",fixed = TRUE,
                draggable = TRUE,top = 110,left = "auto",right = 35, bottom = "auto",width = 250,height = "auto",
                tags$div("SNA Layer Control",align = "center",style="background-color:#10006A;color:#FFFFFF;font-size:2em;font-weight:bold;padding:10px;"),
                selectInput("baselayer",h5("Base Map",style="color:#FFFFFF;font-weight:bold;"),vars2),
                selectInput("snalayer",h5("SNA Selection",style="color:#FFFFFF;font-weight:bold;"),vars1,selected = "NO2 Mean")),
  absolutePanel(id = "mapcontrols",class ="panel panel-default",fixed = TRUE,
                draggable = TRUE,top = 110,left = "auto",right = "70.0%", bottom = "auto",width = 100,height = "auto",
                tags$div("Map",tags$br(),"Controls",align = "center",style="background-color:#10006A;color:#FFFFFF;font-size:1em;font-weight:bold;padding:10px;"),
                tags$div(actionButton(inputId = "map_zoom_in",shiny::icon("fa-solid fa-plus",verify_fa = FALSE),title ="Zoom In"),actionButton(inputId = "map_zoom_out",shiny::icon("fa-solid fa-minus",verify_fa = FALSE),title ="Zoom Out"),align="center"),
                tags$div(actionButton(inputId = "reset",shiny::icon("fa-solid fa-arrows-to-circle",verify_fa = FALSE)),align="center", style = "padding:10px",title ="Reset Default Extent")),
  absolutePanel(id = "boroughselect",class ="panel panel-default",fixed = TRUE,
               draggable = TRUE,top = 975,left = "auto",right = "60.0%", bottom = "auto",width = 350,height = "auto",
               tags$div("Borough Select",align = "center",style="background-color:#10006A;color:#FFFFFF;font-size:2em;font-weight:bold;padding:10px;"),
               tags$div(leafletOutput("Borough1",width = 300,height = 250),align="center")),
  tags$head(tags$style(HTML("#controls {opacity:0.6;background-color:#10006A;padding:0 20px 20px 20px;transition:opacity 1000ms;transition-delay:500ms;z-index:9998 !important;}#controls:hover{opacity:1;transition:opacity 1000ms;}"))),
  tags$head(tags$style(HTML("#mapcontrols {opacity:0.6;background-color:#10006A;padding:0 10px 10px 10px;transition:opacity 1000ms;transition-delay:500ms;z-index:9999 !important;}#mapcontrols:hover{opacity:1;transition:opacity 1000ms;}"))),
  tags$head(tags$style(HTML("#boroughselect {opacity:0.6;background-color:#10006A;padding:0 10px 10px 10px;transition:opacity 1000ms;transition-delay:500ms;z-index:9999 !important;}#boroughselect:hover{opacity:1;transition:opacity 1000ms;}"))),
  tags$head(tags$style(HTML("#Borough1 {background:#10006A;}")))
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  
  
  
    output$SNA <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 11,zoomControl=FALSE)) %>%
        addMapPane("base",zIndex = 410) %>%
        addMapPane("pollution",zIndex = 420) %>%
        addMapPane("overlay",zIndex = 430) %>%
        setView(lng = -0.10932017,
                lat = 51.500493,
                zoom = 11) %>%
        setMaxBounds(lng1 = -0.64469747,
                     lat1 = 51.832822,
                     lng2 = 0.46418831,
                     lat2 = 51.244695) %>%
        addEasyprint(options = easyprintOptions(hidden = TRUE,exportOnly = TRUE,sizeModes = "CurrentSize",hideControlContainer = FALSE))
})  
    output$Borough1 <- renderLeaflet({
      labels4 <- sprintf("<strong>Borough Name:</strong> %s",
                        BoroughLayer$BOROUGH) %>%
        lapply(htmltools::HTML)
      leaflet(options = leafletOptions(minZoom = 8.75,maxZoom = 8.75, zoomControl=FALSE,dragging=FALSE,keyboard=FALSE,tap=FALSE,touchzoom=FALSE,doubleClickZoom=FALSE)) %>%
        setView(lng = -0.10932017,
                lat = 51.500493,
                zoom = 8.75) %>%
        setMaxBounds(lng1 = -0.64469747,
                     lat1 = 51.832822,
                     lng2 = 0.46418831,
                     lat2 = 51.244695) %>%
        addPolygons(data = BoroughLayer,
                    stroke=TRUE,
                    color="white",
                    weight=1,
                    label = labels4,
                    layerId = BoroughLayer$BOROUGH,
                    highlightOptions = highlightOptions(weight = 2,
                                                        color="yellow",
                                                        bringToFront = TRUE,
                                                        fillColor = "#001630"
                    ))
    })
    
    sna_click <- reactiveVal()
    
    observeEvent(input$Borough1_shape_click,{
      if(!is.null(sna_click()) && sna_click() == input$Borough1_shape_click$id) {
        sna_click(NULL)
        }
      else {
        sna_click(input$Borough1_shape_click$id)
        }
    })
    
    filtered_sna <- reactive({
      if (is.null(sna_click())) {
        SNALayer
      }
      else if (!is.null(sna_click())){
        SNALayer %>% dplyr::filter(SNALayer$SNAMaster_Borough %in% sna_click())
      }
        })
    
    
    

    observeEvent(input$exp,{
      updateCheckboxInput(getDefaultReactiveDomain(),"mapscale",value = TRUE)
    })
    
    observeEvent(input$exp,{
      updateCheckboxInput(getDefaultReactiveDomain(),"maplegend",value = TRUE)
    })
    
    observeEvent(input$exp,{
      updateCheckboxInput(getDefaultReactiveDomain(),"title",value = TRUE)
    })
    
    observeEvent(input$exp_can,{
      updateCheckboxInput(getDefaultReactiveDomain(),"mapscale",value = FALSE)
    })
    
    observeEvent(input$exp_can,{
      updateCheckboxInput(getDefaultReactiveDomain(),"maplegend",value = FALSE)
    })
    
    observeEvent(input$exp_can,{
      updateCheckboxInput(getDefaultReactiveDomain(),"title",value = FALSE)
    })
    
    observeEvent(input$mapcontrol,{
      if (input$mapcontrol == TRUE){
        show("mapcontrols",anim = TRUE,animType = "slide")
      }
      else {
        hide("mapcontrols",anim = TRUE,animType = "slide")
      }
    })
    
    observeEvent(input$snalayercontrol,{
      if (input$snalayercontrol == TRUE){
        show("controls",anim = TRUE,animType = "slide")
      }
      else {
        hide("controls",anim = TRUE,animType = "slide")
      }
    })
    
    TitleName <- reactive({
    if (input$snalayer == "None"){
      TitleName <- sprintf("No SNA Layer Selected...")
    }
    else if (input$snalayer == "NO2 Mean"){
      TitleName <- sprintf("Annual Mean NO<sub style='font-size:20px;'>2</sub> (<span>&#xB5;</span>g/m<sup style='font-size:20px;'>3</sup>) LAEI 2019")
    }
    else if (input$snalayer == "NOx Mean"){
      TitleName <- sprintf("Annual Mean NO<sub style='font-size:20px;'>X</sub> (<span>&#xB5;</span>g/m<sup style='font-size:20px;'>3</sup>) LAEI 2019")
    }
  })
    
    observeEvent(input$reset,{
      leafletProxy("SNA") %>%
        setView(lng = -0.10932017,
                lat = 51.500493,
                zoom = 11)
    })
    
    observeEvent(input$map_zoom_in,{
      leafletProxy("SNA") %>%
        setView(lat  = (input$SNA_bounds$north + input$SNA_bounds$south) / 2,
                lng  = (input$SNA_bounds$east + input$SNA_bounds$wes) / 2,
                zoom = input$SNA_zoom + 1)
    })
    
    observeEvent(input$map_zoom_out,{
      leafletProxy("SNA") %>%
        setView(lat  = (input$SNA_bounds$north + input$SNA_bounds$south) / 2,
                lng  = (input$SNA_bounds$east + input$SNA_bounds$wes) / 2,
                zoom = input$SNA_zoom - 1)
    })
 
    observe({
      proxy4 <- leafletProxy("SNA")
      if (input$mapscale == TRUE){
        proxy4 %>%
        addScaleBar(position = "bottomleft",options = scaleBarOptions(maxWidth = 250, metric = TRUE, imperial = TRUE,updateWhenIdle = FALSE))
      }
      else {
        proxy4 %>%
        removeScaleBar()
      }
    })
    
    observe({
      proxy6 <- leafletProxy("SNA")
      if (input$title == TRUE){
        proxy6 %>%
        removeControl("title1") %>%
        addControl(sprintf("<strong style='font-size:40px;color:white;text-shadow:-1px 0 black, 0 1px black, 1px 0 black, 0 -1px black;font-family:arial;'>SNA Pollution Map: %s</strong>",TitleName()),position = "topleft",layerId = "title1",className = "fieldset{border:0}")
      }
      else {
        proxy6 %>%
          removeControl("title1")
      }
    })

    observeEvent(input$baselayer,{
      proxy2 <- leafletProxy("SNA")
      if (input$baselayer == "standard"){
        proxy2 %>%
          clearTiles() %>%
          addMapboxTiles(style_url = "mapbox://styles/joemarangos/cl2g1ochw001s14muumlqzopk", access_token = "pk.eyJ1Ijoiam9lbWFyYW5nb3MiLCJhIjoiY2wyZXI1eWdyMDI4OTNpbzNjYXlhcGxhYSJ9.gEJgFPqnRhd_MRL309zwcw",group = "standard",options = pathOptions(pane = "base"))
      }
      else if (input$baselayer == "light"){
        proxy2 %>%
          clearTiles() %>%
          addMapboxTiles(style_url = "mapbox://styles/joemarangos/cl2g1wt17002m14piyxlvvdat", access_token = "pk.eyJ1Ijoiam9lbWFyYW5nb3MiLCJhIjoiY2wyZXI1eWdyMDI4OTNpbzNjYXlhcGxhYSJ9.gEJgFPqnRhd_MRL309zwcw",group = "light",options = pathOptions(pane = "base"))
      }
      else if (input$baselayer == "streets"){
        proxy2 %>%
          clearTiles() %>%
          addMapboxTiles(style_url = "mapbox://styles/joemarangos/cl2g1z2ai002b14ksruxzlpe9", access_token = "pk.eyJ1Ijoiam9lbWFyYW5nb3MiLCJhIjoiY2wyZXI1eWdyMDI4OTNpbzNjYXlhcGxhYSJ9.gEJgFPqnRhd_MRL309zwcw",group = "streets",options = pathOptions(pane = "base"))
      }
      else if (input$baselayer == "dark"){
        proxy2 %>%
          clearTiles() %>%
          addMapboxTiles(style_url = "mapbox://styles/joemarangos/cl2ex322t001814piusq8f1p0", access_token = "pk.eyJ1Ijoiam9lbWFyYW5nb3MiLCJhIjoiY2wyZXI1eWdyMDI4OTNpbzNjYXlhcGxhYSJ9.gEJgFPqnRhd_MRL309zwcw",group = "dark",options = pathOptions(pane = "base"))
      }
      else if (input$baselayer == "satellite"){
        proxy2 %>%
          clearTiles() %>%
          addMapboxTiles(style_url = "mapbox://styles/joemarangos/cl2g1ucmp00d817prb81qbg43", access_token = "pk.eyJ1Ijoiam9lbWFyYW5nb3MiLCJhIjoiY2wyZXI1eWdyMDI4OTNpbzNjYXlhcGxhYSJ9.gEJgFPqnRhd_MRL309zwcw",group = "satellite",options = pathOptions(pane = "base"))
      }
    })
    
    observeEvent(input$snalayer, {
        proxy1 <- leafletProxy("SNA")
        labels <- sprintf("<strong>SNA ID:</strong> %s <br/> <strong>Average Polution Value:</strong> %s",
                          SNALayer$SNA_ID,SNALayer$SNAMaster_NO2_mean) %>%
          lapply(htmltools::HTML)
        
        labels2 <- sprintf("<strong>ULEZ</strong>") %>%
          lapply(htmltools::HTML)
        
        labels3 <- sprintf("<strong>SNA ID:</strong> %s <br/> <strong>Average Polution Value:</strong> %s",
                           SNALayer$SNA_ID,SNALayer$SNAMaster_NOX_mean) %>%
          lapply(htmltools::HTML)
        if (input$snalayer == "NO2 Mean"){
          proxy1 %>%
            clearShapes() %>%
            addPolygons(data = filtered_sna(), label = labels,
                        stroke=TRUE,
                        color = "white",
                        weight = .5,
                        smoothFactor = .5,
                        opacity = 1,
                        fillOpacity = 0.6,
                        fillColor = SNALayer$LegendColNO2,
                        group = "NO2 Mean",
                        popup = labels,
                        options = pathOptions(pane = "pollution"),
                        highlightOptions = highlightOptions(weight = 2,
                                                            fillOpacity = 0.8,
                                                            color = "yellow",
                                                            opacity = 1,
                                                            bringToFront = TRUE))}
        else if (input$snalayer == "NOx Mean"){
          proxy1 %>%
            clearShapes() %>%
            addPolygons(data = filtered_sna(), label = labels3,
                        stroke = TRUE,
                        color = "white",
                        weight = .5,
                        smoothFactor = .5,
                        opacity = 1,
                        fillOpacity = 0.6,
                        fillColor = SNALayer$LegendColNOX,
                        group = "NOx Mean",
                        popup = labels3,
                        options = pathOptions(pane = "pollution"),
                        highlightOptions = highlightOptions(weight = 2,
                                                            fillOpacity = 0.8,
                                                            color = "yellow",
                                                            opacity = 1,
                                                            bringToFront = TRUE))
          }
        else if (input$snalayer == "None"){
          proxy1 %>%
            clearShapes()
        }
        })
    
    observeEvent(input$dl,{
      leafletProxy("SNA") %>%
        easyprintMap(sizeModes = "CurrentSize",filename = paste0(Sys.Date(),"_SNA_Custom"))
    })

}


# Run the application 
shinyApp(ui = ui, server = server)
