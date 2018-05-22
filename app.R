#library(profvis)
## packages ----
library(rvest)
library(rgdal)
library(stplanr)
library(sp)
library(ggplot2)
library(maptools)
library(broom)
library(scales)
library(readr)
library(dplyr)
#library(ggspatial)
library(ggpolypath)
library(DT)
library(shinyBS)
library(colourpicker)
#profvis({
## load data ----
#roads.fort <- read_csv("https://raw.githubusercontent.com/danbernstein/personalridemap/master/roads.csv")
water.df <- read_csv("https://raw.githubusercontent.com/danbernstein/personalridemap/master/waterbodies.csv")
play <- read.csv("https://raw.githubusercontent.com/danbernstein/personalridemap/master/toproutes.csv")

stations <- read_csv("https://raw.githubusercontent.com/danbernstein/capitalbikeshareviz/master/data/bikestations_data.csv") %>% 
  select(ADDRESS, LONGITUDE, LATITUDE, TERMINAL_NUMBER)

cabi <- html_session("https://secure.capitalbikeshare.com/profile/login")
cabi.form <- html_form(cabi)

routing_function <- function(odf){
  odf$ID <- seq.int(nrow(odf))
  
  l <- vector("list", nrow(odf))
  
  withProgress(message = 'Routing Rides', value = 0, {
    n <- nrow(odf)
    
  for(i in 1:nrow(odf)){
    o = c(odf$start.lon[i], odf$start.lat[i])
    d = c(odf$end.lon[i], odf$end.lat[i])
    l[[i]] <- sp::Lines(list(sp::Line(rbind(o, d))), as.character(i))
    
    incProgress(1/n, detail = paste("Routing", i))
    
  }
    
    })
  
  l <- sp::SpatialLines(l)
  proj4string(l) <- CRS("+init=epsg:4326")
  l <- SpatialLinesDataFrame(l, odf, match.ID = "ID")
  
  routes_fast <- line2route(l = l, route_fun = route_osrm, profile = "cycling")
}

## ui ----
ui <- shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  fluidPage(
    mainPanel(
      bsModal(id = 'startupModal', title = 'Welcome! Let the webscraping begin!', trigger = 'submit',
              size = 'medium', 
              div(style = "text-align: justify; text-justify: inter-word;", 
              "This app provides two options for exploring bikeshare data: you can either enter your capital bikeshare account credentials to enable webscraping, or 
              you can explore some of the most popular routes on the network.",   tags$br(), tags$br(),
              "I am sure that providing your login information is a little scary. While I do not have a degree in computer science, I can tell you that
              this application is hosted on", a("shinyapps.io", href = "https://www.shinyapps.io/"), "which is 'secure-by-design',
              all access to the website is SSL encrypted, meaning that all information passed between your computer and the website is entirely encrypted.", tags$br(), tags$br(),
              "The app will scrape your entire ride history, unless you select for only the most recent 100 rides, in case you would like to explore a smaller dataset." 
              ),
              div(style = "display: flex; margin:30px;height:40%;",
              div(style = "border-right: solid; border-width: thin;
                  width: 60%;height:100%;text-align:center;
                  padding-right:40px;
                  ",
                #style ="display: flex; justify-content: space-around;margin:40px;",
              textInput("username", "Username", "", width = "100%"),
              passwordInput("password", "Password", "", width = "100%"),
           #   checkboxInput("checkbox", "Only scrape the last 100 rides", FALSE),
              actionButton("submit", "Submit")),
              bsTooltip("submit", "some rides will be excluded, such as those that start and end at the same location and rides where the bike does not 
           completely lock in at the destination station.",
                                                         "right", options = list(container = "body")),
              div(style = "width:50%;text-align:center;padding: 60px 0px; margin-left:40px;", 
                  actionButton("playdata", "Just let me explore!"))),
              tags$head(tags$style("#startupModal .modal-footer{ display:none}")),
              tags$head(tags$style("button#submit.btn.btn-default.action-button.shiny-bound.input{ display:none}")),
              tags$style(type="text/css",".recalculating {opacity: 1.0;}")
              ))),
  div(class = "mytitle", 
      "Personalized Illustrations of Capital Bikeshare Rides"),
   
  fluidRow(style = "border-bottom:1px solid;margin-bottom:30px;padding:20px;
           display: flex; justify-content: space-around;",
      column(width = 4, 
        fluidRow(class = "sidebar", height = "auto",
          numericInput("pages", "Number of Rides", "100", min = NA, max = NA),
          dateRangeInput('dateRange',
                         label = 'Date Range',
                         start = "2010-01-01", end = Sys.Date()),
          div(class = "button-align", actionButton("filter", "Filter"),
              actionButton("reset_filters", "Reset"))
                         ),
        fluidRow(class = "sidebar", 
               colourInput("ride_color", "Ride Route Color", value = "red", showColour = "background"),
               colourInput("background_color", "Background Color", value = "black", showColour = "background"),
               colourInput("water_color", "Water Color", value = "#708090", showColour = "background"),
               sliderInput("linewidth_slider", "Line Width",
                              min = 0.1, max = 2.0,
                              value = c(0.3,1.0), step = 0.1),
               div(class = "button-align", actionButton("update", "Update"),
                 actionButton("reset_colors", "Reset"))
               ),
        div(style = "text-align:center;",
        downloadButton("downloadMap", label = "Download Map"))
     ),
     column(width = 1),
      column(width = 6, height = "auto", id = "plot", 
        #plotOutput("rendered.map", height = "100%", width = "100%")
        plotOutput("image1", height = "100%", width = "100%")
      ),
     column(width = 1
            )),
  fluidRow(
    conditionalPanel(id = "table",
                      "input$submit == 1",
                    dataTableOutput("ntext", width = "100%", height = "auto"))
    )
  ))
#})



## server ----
server <- function(input, output, session) {

toggleModal(session, "startupModal", toggle = "submit")
  
## close the dialog box when the play data is selected
observeEvent(input$playdata, {
    toggleModal(session, 'startupModal', toggle = "submit")}, 
    ignoreInit = T)



## webscrape all data ----
scraperesults <- eventReactive(input$submit, {
    validate(
      need(input$username != "" &
             input$password != "", "Please input login information"))
  
    login <- set_values(cabi.form[[2]],
                        '_username'  = input$username,
                        '_password'  = input$password)
    
    gotin <- cabi %>% submit_form(login) 
    
    validate(
      need(gotin$url == "https://secure.capitalbikeshare.com/profile/",
           "connection failed"))

    #   This section finds the last page of rides to give the length of the vector of urls to loop through
    
 #   url_base <- paste0("https://secure.capitalbikeshare.com", gotin %>% 
 #                     html_node(".ed-profile-menu__link_trips a") %>% 
#                       html_attr("href"))

 #   last_page_html <- gotin %>% 
 #     follow_link("Oldest")
 #   
 #   last_page_number <- last_page_html$url %>% 
 #     gsub("^.*?=","",.)
    
  #  l_out <- ifelse(input$checkbox == FALSE, as.numeric(last_page_number), 10)#as.numeric(last_page_number)  ## this needs to be set to the user's number of pages
    
    
    results<- data.frame()  

  #  withProgress(message = 'Gathering Your Rides', value = 0, {
      # Number of times we'll go through the loop
     # n <- l_out
      
      for(i in 1:length(urls)){
        url.i <- gotin %>% 
          jump_to(urls[i]) %>% 
          read_html()
        
        startdatetime <-  url.i %>% 
          html_nodes(".ed-table__col_trip-start-date") %>% 
          html_text() %>% 
          parse_character() %>% 
          as.data.frame()
        
        enddatetime <-  url.i  %>% 
          html_nodes(".ed-table__col_trip-end-date") %>% 
          html_text() %>% 
          parse_character() %>% 
          as.data.frame()
        
        startstation <- url.i %>% 
          html_nodes(".ed-table__col_trip-start-station") %>% 
          html_text() %>% 
          parse_character() %>% 
          as.data.frame()
        
        endstation <- url.i %>% 
          html_nodes(".ed-table__col_trip-end-station") %>% 
          html_text() %>% 
          parse_character() %>% 
          as.data.frame()
        
        duration <- url.i %>% 
          html_nodes(".ed-table__item__info_trip-duration") %>% 
          html_text() %>% 
          parse_character() %>% 
          as.data.frame()
        
        bind <- cbind(startdatetime, enddatetime, startstation, endstation, duration)
        
        results <- rbind(results, bind)
      
   #   incProgress(1/n, detail = paste0("Scraping Page ", i, " out of ", l_out))
      
    }
      #})
    
    names(results) <- c("startdatetime", "enddatetime", "startstation", "endstation", "duration")
      
    results0 <- results %>% 
      mutate(startdatetime = lubridate::mdy_hms(gsub("Start                        ", "", startdatetime)),
             enddatetime = lubridate::mdy_hms(gsub("End                        ", "", enddatetime)),
             startdate = lubridate::as_date(startdatetime),
             enddate = lubridate::as_date(enddatetime),
             startstation = gsub("\n                                ", "", startstation),
             endstation = gsub("\n                                ", "", endstation),
             duration = gsub("\n                                ", "", duration),
             
             hour = if_else(grepl(" h ", duration), as.character(str_match(duration, "(.*?) h")[,2]), "00"),
             hour = ifelse(nchar(hour) == 1, paste0("0", hour), hour),
             
             minute = if_else(grepl(" h ", duration), 
                              str_match(duration, " h (.*?) m")[,2], 
                              as.character(str_match(duration, "(.*?) m")[,2])),
             
             minute = ifelse(nchar(minute) == 1, paste0("0", minute), minute),
             
             second = str_match(duration, " m (.*?) s")[,2],
             second = ifelse(nchar(second) == 1, paste0("0", second), second),
             
             time = paste0(hour, ":", minute, ":", second)
      )
    
    me.locs <- left_join(results0, stations, by = c("startstation" = "ADDRESS")) %>% 
      left_join(., stations, by = c("endstation" = "ADDRESS")) %>% 
      rename(start.lon = `LONGITUDE.x`,
             start.lat = `LATITUDE.x`,
             end.lon = `LONGITUDE.y`,
             end.lat = `LATITUDE.y`,
             terminal.number = `TERMINAL_NUMBER.y`) %>%
      select(-TERMINAL_NUMBER.x) %>% 
      filter(!is.na(end.lat) & startstation != endstation) 
    
    me.locs
    
  })    

#observeEvent(input$submit, {
#  updateNumericInput(session, "pages", value = nrow(scraperesults()))})

## subset data for DT and format ----
dttable  <- eventReactive(input$submit | input$filter, {
  filtered <- scraperesults() %>% 
    filter(startdate >= min(input$dateRange) &
             startdate <= max(input$dateRange)) %>% 
    select(startstation, endstation, startdatetime, enddatetime, time) %>% 
    rename('Start Station' = startstation,
           'End Station' = endstation,
           'Start Date and Time' = startdatetime,
           'End Date and Time' = enddatetime,
           'Duration' = time)
  
  filtered
})

## reset filters and color palette options
observeEvent(input$reset_colors, {
  updateColourInput(session, "background_color", value = "black")
  updateColourInput(session, "water_color", value = "#708090")
  updateColourInput(session, "ride_color", value = "red")
  updateSliderInput(session, "linewidth_slider", value = c(0.3,1.0)) 
})

observeEvent(input$reset_filters, {
  updateNumericInput(session, "pages", value = "100")
  updateDateRangeInput(session, "dateRange", start = "2010-01-01", end = Sys.Date())
})


## routing ----
map.out  <- eventReactive(input$submit | input$filter | input$playdata, {
  ok <- scraperesults() %>% 
    filter(startdate >= min(input$dateRange) &
             startdate <= max(input$dateRange)) %>% 
    sample_n(., input$pages, replace = F)
                
             #   as.numeric(input$pages))
  out<- routing_function(ok)
  
  ha <- fortify(out)
  hap <- gsection(out)
  
  head(out@data)
  out@data$count = 1
  
  har <- overline(out, attrib = "count")
  har@data$id = rownames(har@data)
  har.fort <- tidy(har)
  mapa.df  <- merge(har.fort,har@data, by="id")  
  mapa.df$scals <- scales::rescale(mapa.df$count, to = c(min(input$linewidth_slider),max(input$linewidth_slider)))
  mapa.df$scale.norm <- scales::rescale(mapa.df$count, to = c(0.8,1))
  
  mapa.df$count.log <- log(mapa.df$count)
  
  mapa.df
})


map.base.play <- eventReactive(input$playdata | input$filter | input$update, {
  mapa.df <- play
  
  mapa.df$scale.norm <- scales::rescale(mapa.df$freq, 
                                        to = c(min(input$linewidth_slider),max(input$linewidth_slider)))
  mapa.df$scale.norm.log <- scales::rescale(mapa.df$count.log, to = c(0.1,1))
  
  total <-  ggplot() + 
    coord_map(xlim=c(min(mapa.df$long)-.01, max(mapa.df$long)+0.01),
              ylim=c(min(mapa.df$lat)-0.01, max(mapa.df$lat)+0.01)) +
    geom_path(data = mapa.df, aes(x=long, y=lat, group = group), 
              size = mapa.df$scale.norm, color = input$ride_color, lineend = "round")+
    ggpolypath::geom_polypath(data = water.df, aes(long, lat, group = group), fill = input$water_color)+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
          panel.background=element_blank()) + 
    ## background color
    theme(plot.background=element_rect(fill="grey"),
          panel.background=element_rect(fill=input$background_color), 
          legend.background= element_rect(fill="black", colour=NA),
          legend.key = element_rect(colour = NA, col = "grey", 
                                    size = .5, fill = 'black'),
          axis.text = element_blank(),
          legend.position = "none",
          axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title=element_blank())
  
  total
})

map.base  <- eventReactive(input$submit | input$filter | input$update | input$playdata, {
  mapa.df <- map.out()

  mapa.df$scale.norm <- scales::rescale(mapa.df$count, 
                                        to = c(min(input$linewidth_slider),max(input$linewidth_slider)))
  mapa.df$scale.norm.log <- scales::rescale(mapa.df$count.log, to = c(0.1,1))
  
 total <-  ggplot() + 
    coord_map(xlim=c(min(mapa.df$long)-.01, max(mapa.df$long)+0.01),
              ylim=c(min(mapa.df$lat)-0.01, max(mapa.df$lat)+0.01)) +
    geom_path(data = mapa.df, aes(x=long, y=lat, group = group), 
              size = mapa.df$scale.norm, color = input$ride_color, lineend = "round")+
    #scale_color_gradient(low = "blue", high = "red")+
    theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
          panel.background=element_blank()) + 
    ## background color
    theme(plot.background=element_rect(fill="grey"),
          panel.background=element_rect(fill=input$background_color), 
          legend.background= element_rect(fill="black", colour=NA),
          legend.key = element_rect(colour = NA, col = "grey", 
                                    size = .5, fill = 'black'),
          axis.text = element_blank(),
          legend.position = "none",
          axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title=element_blank())

    total
})

## add water ----
map.base.water  <- eventReactive(input$submit | input$filter | input$update | input$playdata, {
  ok <- map.base()
  ok+
  ggpolypath::geom_polypath(data = water.df, aes(long, lat, group = group), fill = input$water_color)
})



## add water and DC roads ----
#map.base.water.roads  <- eventReactive(input$submit, {
#  ok <- map.base.water()
#  ok+
#    geom_path(data = roads.fort, aes(x=long, y=lat, group = group),
#              linetype = "solid", alpha = 0.5, size = 0.1, color = "white") ## background color
#})
#
output$ntext <- DT::renderDataTable({
  DT::datatable(dttable(), options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
})

output$rendered.map <- renderPlot({
                        if (input$submit < 1) {
                          plot(map.base.play())
                        }
                        else {
                          plot(map.base.water())
                        }})

output$downloadMap <- downloadHandler(
  filename = function() { paste("bikeshareviz-map", '.pdf', sep='') },
  content = function(file) {
    pdf(file,width=7,height=9)
    if (input$submit < 1) {
      plot(map.base.play())
    }
    else {
      plot(map.base.water())}
    dev.off()
  })


output$image1 <- renderImage({
  # A temp file to save the output.
  # This file will be automatically removed later by
  # renderImage, because of the deleteFile=TRUE argument.
  outfile <- tempfile(fileext = ".png")
  # Generate the image and write it to file
  png(outfile, width = 500, height = 600)
  pic <- if (input$submit < 1) {plot(map.base.play())}else{plot(map.base.water())}
  dev.off()
  
  list(src = outfile,
       alt = "This is alternate text")
}, deleteFile = TRUE)

}

# Run the application  -----
shinyApp(ui = ui, server = server)