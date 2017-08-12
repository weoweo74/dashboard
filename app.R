# Shiny dashboard v1

# Some notes about notes:
#   'tab' means tabPanel in ui, 'page' means output page as named in provided 
#     excel file, 'page x' = 'tab x+1'
#   plots are named sequentially (plot1, plot2, ...) though sometimes refer to
#     things like 'first plot on tab 4' in the (perhaps mistaken) hope of 
#     adding a bit more clarity

# Packages used:
# shiny         ggplot2         plotly          XLConnect
# dplyr         lubridate       leaflet         ggmap
# rmarkdown     webshot         htmlwidgets     reshape2
# RColorBrewer  viridis

# Also needed:
# PhantomJS     pdflatex

packages <- c('ggplot2', 'plotly', 'XLConnect', 'dplyr', 'lubridate', 'leaflet', 
              'ggmap', 'rmarkdown', 'webshot', 'htmlwidgets', 'reshape2', 
              'shinydashboard', 'RColorBrewer', 'viridis')

for (package in packages) {
    if (!(package %in% installed.packages()))
        install.packages(package)
}


library(shiny)
library(ggplot2)
library(plotly)
library(XLConnect)
library(dplyr)
library(lubridate)
library(leaflet)
library(ggmap)
library(shinythemes)
library(reshape2)
library(RColorBrewer)
library(viridis)



downloadBtnMsg = "Download to PDF"



# Helper function to filter data by time inputs
filter_by_time <- function(dat, btn, inYear, inQuarter, inMonth) {
    if (btn == "month"){
        if (inYear == "" &
            (inQuarter != "" | inMonth != ""))
            return(NULL)
        if (inQuarter == "" & inMonth != "")
            return(NULL)
        if (inYear == "")
            return(dat)
        new_time_df <- filter(dat, Jaar == inYear)
        if (inQuarter == "")
            return(new_time_df)
        new_time_df <- filter(new_time_df, Kwartaal == inQuarter)
        if (inMonth == "")
            return(new_time_df)
        new_time_df <- filter(new_time_df, Maand == inMonth)
    } else if (btn == "quarter") {
        if (inYear == "" & inQuarter != "")
            return(NULL)
        if (inYear == "")
            return(dat)
        new_time_df <- filter(dat, Jaar == inYear)
        if (inQuarter == "")
            return(new_time_df)
        new_time_df <- filter(new_time_df, Kwartaal == inQuarter)
    } else {
        if (inYear == "")
            return(dat)
        new_time_df <- filter(dat, Jaar == inYear)
    }
    new_time_df
}


ui <- tagList(
    tags$head(
        tags$style(
            HTML(
                "nav:first-child
                    {background-color: rgb(0, 147, 208); color: white;}

                .navbar-default .navbar-header span{color: white;}

                .navbar-default .navbar-nav li a{color: white;}
                            
                .navbar-default .navbar-nav .active > a 
                    {background-color: rgb(0, 70, 126); color: white;}
                
                .navbar-default .navbar-nav .active > a:hover 
                    {background-color: rgb(0, 70, 126); color: white;}
                
                .navbar-default .navbar-nav .active > a:focus 
                    {background-color: rgb(0, 70, 126); color: white;}"
            )
        )
    ),
    navbarPage(
    theme = shinytheme("yeti"),
    title = "Dashboard",
    
    tabPanel(        # Intro page
        title = "INTRO", 
        h2("Upload your data here"),
        fileInput(inputId = "file1", label = "Choose an excel file")
    ), 
    tabPanel(        # Output page 1
        title = "SAMENVATTING", 
        fluidRow(
            column(
                width = 1, offset = 1, "Jaar"), 
            column(
                width = 1, textInput(inputId = "year", label = NULL)),
            column(
                width = 1, "Kwartaal"), 
            column(
                width = 1, textInput(inputId = "quarter", label = NULL)),
            column(
                width = 1, "Maand"), 
            column(
                width = 1, textInput(inputId = "month", label = NULL)),
            column(
                width = 1, "Prijs"), 
            column(
                width = 1, textInput(
                    inputId = "price", label = NULL, value = "0"))
        ),
        dataTableOutput("table"), 
        downloadButton(outputId = "dlTable", label = downloadBtnMsg)
    ), 
    tabPanel(        # Output page 2
        title = "TIJDSANALYSE",
        fluidRow(
            column(
                width = 10,
                offset = 2, 
                radioButtons(
                    inputId = "plot1Btn", label = "Select Unit of Time", 
                    choices = list(
                        "Jaar" = "year", "Kwartaal" = "quarter", "Maand" = "month"
                    ), 
                    selected = "month", 
                    inline = TRUE
                )
            )
        ),    # End row
        fluidRow(
            column(
                width = 5, 
                plotlyOutput(
                    outputId = "plot1"
                )
            ), 
            column(
                width = 7, 
                plotlyOutput(
                    outputId = "plot2"
                )
            )
        ),    # End row
        fluidRow(
            column(
                width = 6,
                downloadButton(outputId = "dlPlot1", label = downloadBtnMsg)
            ), 
            column(
                width = 6,
                uiOutput("plot2ChkGrp"), 
                downloadButton(outputId = "dlPlot2", label = downloadBtnMsg)
            )
        )    # End row
    ),    # End tab
    tabPanel(        # Output page 3
        title = "LOCATIE", 
        fluidRow(
            column(
                width = 12, 
                sidebarLayout(
                    sidebarPanel(
                        fluidRow(
                            column(
                                width = 4, 
                                textInput(inputId = "tab4Year", value = "", label = "Jaar")
                            ),
                            column(
                                width = 4, 
                                uiOutput(outputId = "tab4QuarterIn")
                            ),
                            column(
                                width = 4, 
                                uiOutput(outputId = "tab4MonthIn")
                            )
                        ),
                        downloadButton(outputId = "dlPlot3", label = downloadBtnMsg)
                    ), 
                    mainPanel(
                        plotlyOutput(
                            outputId = "plot3"
                        )
                    )
                )
            )
        ),    # End row
        fluidRow(
            column(
                width = 12,
                sidebarLayout(
                    sidebarPanel(
                        fluidRow(
                            column(
                                width = 6,
                                uiOutput("plot4OpdrChkGrp")
                            ),
                            column(
                                width = 6,
                                uiOutput("plot4TypewChkGrp"), 
                                downloadButton(
                                    outputId = "dlPlot4", 
                                    label = downloadBtnMsg)
                            )
                        )
                    ),
                    mainPanel(
                        leafletOutput(
                            outputId = "plot4"
                        )
                    )
                )
            )
        )    # End row
    ),    # End tab
    tabPanel(        # Output page 4
        title = "COMPLEXEN PER PERSOON", 
        fluidRow(
            column(
                width = 2,
                offset = 2, 
                textInput(inputId = "tab5Year", value = "", label = "Jaar")
            ), 
            column(
                width = 2,
                uiOutput(outputId = "tab5QuarterIn")
            ),
            column(
                width = 2, 
                uiOutput(outputId = "tab5MonthIn") 
            )
        ),    # End row
        fluidRow(
            column(
                width = 5, 
                plotlyOutput(
                    outputId = "plot5"
                )
            ), 
            column(
                width = 1, 
                uiOutput("plot6ChkGrp")
            ),
            column(
                width = 6, 
                plotlyOutput(
                    outputId = "plot6"
                )
            )
        ),    # End row
        fluidRow(
            column(
                width = 6,
                downloadButton(outputId = "dlPlot5", label = downloadBtnMsg)
            ),
            column(
                width = 6,
                downloadButton(outputId = "dlPlot6", label = downloadBtnMsg)
            )
        )    # End row
    ),    # End tab
    tabPanel(        # Output page 5
        title = "OPDRACHTGEVERS", 
        fluidRow(
            column(
                width = 2, offset = 2, 
                textInput(inputId = "tab6Year", value = "", label = "Jaar")
            ),
            column(
                width = 2,
                uiOutput(outputId = "tab6QuarterIn")
            ),
            column(
                width = 2,
                uiOutput(outputId = "tab6MonthIn")
            )
        ),
        plotlyOutput(outputId = "plot7"), 
        downloadButton(outputId = "dlPlot7", label = downloadBtnMsg)
    ),    # End tab
    tabPanel(        # Output page 6
        title = "STATISTIEKEN", 
        fluidRow(
            column(
                width = 4, 
                offset = 2, 
                radioButtons(
                    inputId = "plot8Btn", label = "Select Unit of Time", 
                    choices = list(
                        "Jaar" = "year", "Kwartaal" = "quarter", "Maand" = "month"
                    ), 
                    selected = "month", 
                    inline = TRUE
                )
            ), 
            column(
                width = 1, 
                uiOutput(outputId = "tab7YearIn")
            ),
            column(
                width = 1, 
                offset = 1, 
                uiOutput(outputId = "tab7QuarterIn")
            )
        ),    # End row
        fluidRow(
            column(
                width = 12, 
                sidebarLayout(
                    position = "right", 
                    sidebarPanel(
                        fluidRow(
                            column(
                                width = 6, 
                                uiOutput("plot8WoonChkGrp")
                            ), 
                            column(
                                width = 6,
                                uiOutput("plot8WoningChkGrp")
                            )
                        ), 
                        fluidRow(
                            column(
                                width = 6, 
                                uiOutput("plot8TypewChkGrp")
                            ), 
                            column(
                                width = 6, 
                                downloadButton(
                                    outputId = "dlPlot8", 
                                    label = downloadBtnMsg)
                            )
                        )
                    ),    # End sidebarPanel
                    mainPanel(plotlyOutput(outputId = "plot8") 
                    )
                )
            )
        ),    # End row
        fluidRow(
            column(
                width = 12, 
                plotlyOutput(outputId = "plot9"), 
                downloadButton(outputId = "dlPlot9", label = downloadBtnMsg)
            )
        ),    # End row
        fluidRow(
            column(
                width = 12, 
                sidebarLayout(
                    position = "right", 
                    sidebarPanel(
                        radioButtons(
                            inputId = "plot10Btn", 
                            choices = list("uit" = "uit", "aan" = "aan"), 
                            selected = "uit", inline = TRUE, label = "Select"), 
                        downloadButton(outputId = "dlPlot10", 
                                       label = downloadBtnMsg)
                    ),
                    mainPanel(
                        plotlyOutput(outputId = "plot10")
                    )
                )
            )
        )    # End row
    )    # End tab
))

###############################################################################
###############################################################################

server <- function(input, output, session) {
    
    ####       Tab 1   ####
    
    # Generates data frame from uploaded data
    df <- reactive({
        inFile <- input$file1
        wk <- loadWorkbook(inFile$datapath)
        readWorksheet(wk, sheet = 1)
    })
    
    
    ####       Tab 2    ####
    
    # Creates new data frame on tab 2 from user settings
    tab2df <- reactive({
        dft <- df()
        
        # filter data by values in tab 2 text inputs
        if (input$year != "" & input$quarter != "" & input$month != "") {
            dft <- filter(dft, Jaar==input$year)
            dft <- filter(dft, Kwartaal==input$quarter)
            dft <- filter(dft, Maand==input$month)
        } else if (input$month != "") {
            return(NULL)
        } else if (input$year != "" & input$quarter != ""){
            dft <- filter(dft, Jaar==input$year)
            dft <- filter(dft, Kwartaal==input$quarter)
        } else if (input$quarter != "") {
            return(NULL)
        } else if (input$year != ""){
            dft <- filter(dft, Jaar==input$year)
        }
        
        # Remove duplicate IDs select only desired columns
        dft <- dft %>% 
            group_by(ID_forUniqueness) %>% 
            filter(row_number() == 1) %>% 
            rename(Complex = Complexnummer) %>% 
            ungroup %>%
            select(
                Complex, N_objecten, Woonplaats, Postcode, Straatnaam)
        
        # Add column for Prijs and row for totals
        if (nrow(dft) > 0) {
            dft <- cbind(dft, Prijs = rep(as.numeric(input$price), nrow(dft)))
            dft <- cbind(Rij = seq(nrow(dft)), dft)
            dft <- rbind(
                dft, c("Total",
                       length(dft$Complex), sum(as.numeric(dft$N_objecten)), 
                       length(unique(dft$Woonplaats)), length(dft$Postcode), 
                       length(dft$Straatnaam), sum(as.numeric(dft$Prijs))
                ))
        }
        dft
    })
    
    # Renders table on tab 2
    output$table <- renderDataTable({
        if (is.null(input$file1))
            return(NULL)
        tab2df()
    })
    
    # File handler for table on tab 2
    output$dlTable <- downloadHandler(
        filename = "table.pdf", 
        content = function(file) {
            tempTable <- file.path(tempdir(), "table.Rmd")
            file.copy("table.Rmd", tempTable, overwrite = TRUE)
            params <- list(df = as.data.frame(tab2df()))
            rmarkdown::render(
                tempTable, output_format = "pdf_document",
                output_file = file, params = params, 
                envir = new.env(parent = globalenv()))
        }
    )
    
    
    ####       Tab 3    ####
    
    # Creates checkbox group for selecting levels in Type Waardering on tab 3
    output$plot2ChkGrp <- renderUI({
        if (is.null(input$file1))
            return(NULL)
        types <- unique(df()$Type_Waardering)
        names(types) <- types
        typesList <- as.list(types)
        checkboxGroupInput(
            inputId = "plot2Chk", label = "Select Type Waardering", 
            choices = typesList, 
            inline = TRUE
        )
    })
    
    # Organizes data according to radio buttons selected on tab 3
    # Returns data frame with new variable 'timeGroup' and only one row for 
    # each unique timeGroup-ID_forUniqueness combination
    df_selTime <- reactive({
        df0 <- df()
        
        # Take radio button input for selecting organization by time
        if (input$plot1Btn == "year") {
            timeVec <- df0$Jaar
        } else if (input$plot1Btn == "quarter") {
            timeVec <- paste(df0$Jaar, df0$Kwartaal)
        } else {
            timeVec <- paste(df0$Jaar, df0$Kwartaal, df0$Maand)
        }
        df0 <- cbind(df0, timeGroup = timeVec)
        
        df0 <- df0 %>% 
            group_by(ID_forUniqueness, timeGroup) %>% 
            filter(row_number() == 1) %>% 
            ungroup
        
        df0
    })
    
    # Generates first plotly bar plot on tab 3
    plt1 <- reactive({
        df1 <- df_selTime()
        
        # Make flat data
        df1 <- df1 %>%
            group_by(timeGroup) %>%
            summarize(total = n())
        
        df1 <- data.frame(df1)
        
        # Construct plot
        plot_ly(
            data = df1, x = ~timeGroup, y = ~total, type = "bar") %>% 
            layout(margin=list(b=80), xaxis = list(title = "", tickangle = -45), 
                   yaxis = list(title = ""))
    })
    
    # Sends plot1 to ui
    output$plot1 <- renderPlotly({
        if (is.null(input$file1))
            return(NULL)
        plt1()
    })
    
    # Generates second plotly bar plot on tab 3
    plt2 <- reactive({
        df2 <- df_selTime()
        
        # Filter data by checkbox values
        if (!is.null(input$plot2Chk)) {
            filt_df <- data.frame()
            for (typeW in input$plot2Chk) {
                filt_df <- rbind(
                    filt_df, 
                    filter(df2, Type_Waardering == typeW))
            }
            df2 <- filt_df
        }
        
        # Make flat data
        df2 <- df2 %>%
            group_by(Type_Waardering, timeGroup) %>%
            summarize(total = n())
        
        df2 <- data.frame(df2)
        
        # Construct plot
        plot_ly(
            data = df2, x = ~timeGroup, y = ~total, color = ~Type_Waardering, 
            type = "bar", 
            colors = brewer.pal(
                n = length(unique(df2$Type_Waardering)), name = "Blues")) %>% 
            layout(margin=list(b=80), xaxis = list(title = "", tickangle = -45), 
                   yaxis = list(title = ""), barmode = "stack")
        
    })
    
    # Sends plot2 to ui
    output$plot2 <- renderPlotly({
        if (is.null(input$file1))
            return(NULL)
        
        plt2()
    })
    
    output$dlPlot1 <- downloadHandler(
        filename = "plot1.pdf", 
        content = function(file) {
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            
            htmlwidgets::saveWidget(plt1(), "temp.html", selfcontained = FALSE)
            webshot::webshot("temp.html", file = file, cliprect = "viewport")
        }
    )
    
    output$dlPlot2 <- downloadHandler(
        filename = "plot2.pdf", 
        content = function(file) {
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            
            htmlwidgets::saveWidget(plt2(), "temp.html", selfcontained = FALSE)
            webshot::webshot("temp.html", file = file, cliprect = "viewport")
        }
    )
    
    ####       Tab 4    ####
    
    # Text input for quarter on tab 4
    output$tab4QuarterIn <- renderUI({
        if (input$plot1Btn == 'year')
            return(NULL)
        textInput(inputId = "tab4Quarter",value = "", label = "Kwartaal")
    })
    
    # Text input for month on tab 4
    output$tab4MonthIn <- renderUI({
        if (input$plot1Btn == 'year' | input$plot1Btn == 'quarter')
            return(NULL)
        textInput(inputId = "tab4Month", value = "", label = "Maand")
    })
    
    # Creates checkbox group for selecting levels in Opdrachtgever on tab 4
    output$plot4OpdrChkGrp <- renderUI({
        if (is.null(input$file1))
            return(NULL)
        types <- unique(df()$Opdrachtgever)
        names(types) <- types
        typesList <- as.list(types)
        checkboxGroupInput(
            inputId = "plot4OpdrChk", label = "Select Opdrachtgever", 
            choices = typesList, 
            inline = FALSE
        )
    })
    
    # Creates checkbox group for selecting levels in Type Waardering on tab 4
    output$plot4TypewChkGrp <- renderUI({
        if (is.null(input$file1))
            return(NULL)
        types <- unique(df()$Type_Waardering)
        names(types) <- types
        typesList <- as.list(types)
        checkboxGroupInput(
            inputId = "plot4TypewChk", label = "Select Type Waardering", 
            choices = typesList, 
            inline = FALSE
        )
    })
    
    # Retrieves unique timeGroup-ID_forUniqueness rows from tab 3 func and
    # filters by text input values from tab 4
    df4 <- reactive({
        filter_by_time(
            df_selTime(), input$plot1Btn, input$tab4Year, input$tab4Quarter, 
            input$tab4Month)
    })
    
    
    # Generates first plot on tab 4 in plotly (pie chart)
    plt3 <- reactive({
        df_p3 <- df4() %>%
            group_by(Woonplaats) %>%
            summarize(total = n())
        
        df_p3 <- data.frame(df_p3)
        
        plot_ly(
            data = df_p3, labels = ~Woonplaats, values = ~total, 
            type = 'pie', 
            marker = list(colors = brewer.pal(
                n = length(unique(df_p3$Woonplaats)), name = "Blues"))) %>% 
            layout(
                xaxis = list(
                    showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                yaxis = list(
                    showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    })
    
    # Renders sends pie chart to ui
    output$plot3 <- renderPlotly({
        if (is.null(input$file1))
            return(NULL)
        plt3()
    })
    
    
    # Generates second plot on tab 4 (map)
    plt4 <- reactive({
        df_p4 <- df4()
        
        # Subset data by selections in Opdrachtgever and Type_Waardering
        filt_df4 <- data.frame()
        if (!is.null(input$plot4OpdrChk) & !is.null(input$plot4TypewChk)) {
            for (typeW in input$plot4TypewChk) {
                for (opdr in input$plot4OpdrChk) {
                    filt_df4 <- rbind(
                        filt_df4, 
                        filter(df4(), 
                               Opdrachtgever == opdr & Type_Waardering == typeW))
                }
            }
            df_p4 <- filt_df4
        } else if (!is.null(input$plot4OpdrChk)) {
            for (opdr in input$plot4OpdrChk) {
                filt_df4 <- rbind(
                    filt_df4, 
                    filter(df4(), Opdrachtgever == opdr))
            }
            df_p4 <- filt_df4
        } else if (!is.null(input$plot4TypewChk)) {
            for (typeW in input$plot4TypewChk) {
                filt_df4 <- rbind(
                    filt_df4,
                    filter(df4(), Type_Waardering == typeW))
            }
            df_p4 <- filt_df4
        }
        
        # Create plot
        leaflet() %>%
            addTiles() %>%
            addMarkers(data = df_p4, lng = ~Longitude, lat = ~Latitude)
    })
    
    # Sends plot to ui
    output$plot4 <- renderLeaflet({
        if (is.null(input$file1))
            return(NULL)
        plt4()
    })
    
    # File handler for pie chart on tab 4
    output$dlPlot3 <- downloadHandler(
        filename = "pie.pdf", 
        content = function(file) {
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            
            htmlwidgets::saveWidget(plt3(), "temp.html", selfcontained = FALSE)
            webshot::webshot("temp.html", file = file, cliprect = "viewport")
        }
    )
    
    # File handler for map on tab 4
    output$dlPlot4 <- downloadHandler(
        filename = "map.pdf", 
        content = function(file) {
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            
            htmlwidgets::saveWidget(plt4(), "temp.html", selfcontained = FALSE)
            webshot::webshot("temp.html", file = file, cliprect = "viewport")
        }
    )
    
    
    ####    Tab 5   ####
    
    # Text input for quarter on tab 5
    output$tab5QuarterIn <- renderUI({
        if (input$plot1Btn == 'year')
            return(NULL)
        textInput(inputId = "tab5Quarter",value = input$tab4Quarter, 
                  label = "Kwartaal")
    })
    
    # Text input for month on tab 5
    output$tab5MonthIn <- renderUI({
        if (input$plot1Btn == 'year' | input$plot1Btn == 'quarter')
            return(NULL)
        textInput(inputId = "tab5Month", value = input$tab4Month, 
                  label = "Maand")
    })
    
    # Updates tab 5 text inputs based on tab 4 text inputs
    observe({
        updateTextInput(session, "tab5Year", value = input$tab4Year)
    })
    observe({
        updateTextInput(session, "tab5Quarter", value = input$tab4Quarter)
    })
    observe({
        updateTextInput(session, "tab5Month", value = input$tab4Month)
    })
    
    # Creates checkbox group for selecting levels in Type_Waardering on tab 5
    output$plot6ChkGrp <- renderUI({
        if (is.null(input$file1))
            return(NULL)
        types <- unique(df()$Type_Waardering)
        names(types) <- types
        typesList <- as.list(types)
        checkboxGroupInput(
            inputId = "plot6Chk", label = "Type Waardering", 
            choices = typesList, 
            inline = FALSE
        )
    })
    
    # Retrieves unique timeGroup-ID_forUniqueness rows from tab 3 func and
    # filters by text input values from tab 5
    df5 <- reactive({
        filter_by_time(
            df_selTime(), input$plot1Btn, input$tab5Year, input$tab5Quarter, 
            input$tab5Month)
    })
    
    # Generates first plotly bar plot on tab 5
    plt5 <- reactive({
        df5p <- df5()
        
        # Make flat data
        df5p <- df5p %>%
            group_by(Taxateur_1) %>%
            summarize(total = n())
        
        df5p <- data.frame(df5p)
        
        # Construct plot
        plot_ly(
            data = df5p, x = ~Taxateur_1, y = ~total, type = "bar") %>% 
            layout(
                showlegend = FALSE, margin=list(b=85), 
                xaxis = list(title = "", tickangle = -45), 
                yaxis = list(title = ""), 
                annotations = list(x = ~Taxateur_1, y = ~total, text = ~total,
                                   xanchor = 'center', yanchor = 'bottom', 
                                   showarrow = FALSE))
    })
    
    # Sends first bar plot on tab 5 to ui
    output$plot5 <- renderPlotly({
        if (is.null(input$file1))
            return(NULL)
        plt5() 
    })
    
    # Generates second plotly bar plot on tab 5
    plt6 <- reactive({
        df6 <- df5()
        
        # Filter data by checkbox values
        if (!is.null(input$plot6Chk)) {
            filt_df <- data.frame()
            for (typeW in input$plot6Chk) {
                filt_df <- rbind(
                    filt_df, 
                    filter(df6, Type_Waardering == typeW))
            }
            df6 <- filt_df
        }
        
        # Make flat data
        df6 <- df6 %>%
            group_by(Type_Waardering, Taxateur_1) %>%
            summarize(total = n())
        
        df6 <- data.frame(df6)
        
        # Construct plot
        plot_ly(
            data = df6, x = ~Taxateur_1, y = ~total, color = ~Type_Waardering, 
            type = "bar", 
            colors = brewer.pal(
                n = length(unique(df6$Type_Waardering)), name = "Blues")) %>% 
            layout(margin=list(b=85), xaxis = list(title = "", tickangle = -45), 
                   yaxis = list(title = ""), 
                   annotations = list(
                       x = ~Taxateur_1, y = ~total, text = ~total, 
                       xanchor = 'center', yanchor = 'bottom', showarrow = FALSE))
        
    })
    
    # Sends plot6 to ui
    output$plot6 <- renderPlotly({
        if (is.null(input$file1))
            return(NULL)
        plt6()
    })
    
    # File handler for first plotly bar plot on tab 5
    output$dlPlot5 <- downloadHandler(
        filename = "plot5.pdf", 
        content = function(file) {
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            
            htmlwidgets::saveWidget(plt5(), "temp.html", selfcontained = FALSE)
            webshot::webshot("temp.html", file = file, cliprect = "viewport")
        }
    )
    
    # File handler for second plotly bar plot on tab 5
    output$dlPlot6 <- downloadHandler(
        filename = "plot6.pdf", 
        content = function(file) {
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            
            htmlwidgets::saveWidget(plt6(), "temp.html", selfcontained = FALSE)
            webshot::webshot("temp.html", file = file, cliprect = "viewport")
        }
    )
    
    
    ####    Tab 6   ####
    
    # Text input for quarter on tab 6
    output$tab6QuarterIn <- renderUI({
        if (input$plot1Btn == 'year')
            return(NULL)
        textInput(inputId = "tab6Quarter",value = input$tab5Quarter, 
                  label = "Kwartaal")
    })
    
    # Text input for month on tab 6
    output$tab6MonthIn <- renderUI({
        if (input$plot1Btn == 'year' | input$plot1Btn == 'quarter')
            return(NULL)
        textInput(inputId = "tab6Month", value = input$tab5Month, 
                  label = "Maand")
    })
    
    # Updates tab 6 text inputs based on tab 5 text inputs
    observe({
        updateTextInput(session, "tab6Year", value = input$tab5Year)
    })
    observe({
        updateTextInput(session, "tab6Quarter", value = input$tab5Quarter)
    })
    observe({
        updateTextInput(session, "tab6Month", value = input$tab5Month)
    })
    
    # Generates tile chart on tab 6
    plt7 <- reactive({
        df7 <- df_selTime()
        
        # Filter data according to tab 6 text inputs
        df7 <- filter_by_time(
            df7, input$plot1Btn, input$tab6Year, input$tab6Quarter, 
            input$tab6Month)
        
        # Make flat data
        df7 <- df7 %>%
            group_by(timeGroup, Opdrachtgever) %>%
            summarize(total = n())
        
        df7 <- data.frame(df7)
        
        # plotly tile chart
        plot_ly(data = df7, x = ~Opdrachtgever, y = ~timeGroup,
                z = ~total, type = "heatmap") %>%
            add_annotations(
                x=df7$Opdrachtgever, y=df7$timeGroup, text=df7$Opdrachtgever,
                xref = "x", yref = "y", showarrow=FALSE) %>% 
            layout(
                margin = list(b = 80, l = 80), 
                xaxis = list(title = "", tickangle = -45), 
                yaxis = list(title = ""))
        
        # # ggplot tile chart
        # p <- qplot(
        #     data = df7, x = Opdrachtgever, y = timeGroup,
        #     fill = total, geom = "tile") +
        #     xlab("") + ylab("") + scale_fill_viridis() +
        #     geom_text(
        #         aes(x = Opdrachtgever, y = timeGroup, label = Opdrachtgever))
        # 
        # ggplotly(p)
        
    })
    
    # Sends tile chart to ui on tab 6
    output$plot7 <- renderPlotly({
        plt7()
    })
    
    # Plotly file handler for tile chart on tab 6
    output$dlPlot7 <- downloadHandler(
        filename = "plot7.pdf",
        content = function(file) {
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            
            htmlwidgets::saveWidget(plt7(), "temp.html", selfcontained = FALSE)
            webshot::webshot("temp.html", file = file, cliprect = "viewport")
        }
    )
    
    # # ggplot2 file handler for tile chart on tab 6
    # output$dlPlot7 <- downloadHandler(
    #     filename = "plot7.pdf", 
    #     content = function(file) {
    #         tempFile <- file.path(tempdir(), "plot.Rmd")
    #         file.copy("plot.Rmd", tempFile, overwrite = TRUE)
    #         params <- list(p1 = plt7())
    #         rmarkdown::render(
    #             tempFile, output_format = "pdf_document",
    #             output_file = file, params = params, 
    #             envir = new.env(parent = globalenv()))
    #     }
    # )
    
    
    ####    Tab 7   ####
    
    # Text input for year on tab 7
    output$tab7YearIn <- renderUI({
        if (input$plot8Btn == 'year')
            return(NULL)
        textInput(inputId = "tab7Year", value = "", label = "Jaar")
    })
    
    output$tab7QuarterIn <- renderUI({
        if (input$plot8Btn == 'year' | input$plot8Btn == 'quarter')
            return(NULL)
        textInput(inputId = "tab7Quarter", value = "", label = "Kwartaal")
    })
    
    # Creates checkbox group for selecting levels in Type_Waardering on tab 7
    output$plot8TypewChkGrp <- renderUI({
        if (is.null(input$file1))
            return(NULL)
        types <- unique(df()$Type_Waardering)
        names(types) <- types
        typesList <- as.list(types)
        checkboxGroupInput(
            inputId = "plot8TypewChk", label = "Type Waardering", 
            choices = typesList, 
            inline = FALSE
        )
    })
    
    # Creates checkbox group for selecting levels in Woonplaats on tab 7
    output$plot8WoonChkGrp <- renderUI({
        if (is.null(input$file1))
            return(NULL)
        types <- unique(df()$Woonplaats)
        names(types) <- types
        typesList <- as.list(types)
        checkboxGroupInput(
            inputId = "plot8WoonChk", label = "Woonplaats", 
            choices = typesList, 
            inline = FALSE
        )
    })
    
    # Creates checkbox group for selecting levels in Woningtype_Voorkeur on tab 7
    output$plot8WoningChkGrp <- renderUI({
        if (is.null(input$file1))
            return(NULL)
        types <- unique(df()$Woningtype_Voorkeur)
        names(types) <- types
        typesList <- as.list(types)
        checkboxGroupInput(
            inputId = "plot8WoningChk", label = "Woningtype_Voorkeur", 
            choices = typesList, 
            inline = FALSE
        )
    })
    
    # Adds timeGroup column to data by unit selected in radio buttons
    df_timeGroups <- reactive({
        df0 <- df()
        
        # Take radio button input for selecting organization by time
        if (input$plot8Btn == "year") {
            timeVec <- df0$Jaar
        } else if (input$plot8Btn == "quarter") {
            timeVec <- paste(df0$Jaar, df0$Kwartaal)
        } else {
            timeVec <- paste(df0$Jaar, df0$Kwartaal, df0$Maand)
        }
        df0 <- cbind(df0, timeGroup = timeVec)
        
        df0
    })
    
    # Subsets data by text inputs
    df_sub <- reactive({
        filter_by_time(
            dat = df_timeGroups(), btn = input$plot8Btn, 
            inYear = input$tab7Year, inQuarter = input$tab7Quarter, 
            inMonth = "")
    })
    
    # Generates plot 8
    plt8 <- reactive({
        df8 <- df_sub()
        
        # Subset data by selections in Woonplaats, Woningtype_Voorkeur 
        # and Type_Waardering
        filt_df8 <- data.frame()
        if (!is.null(input$plot8TypewChk) & !is.null(input$plot8WoonChk)
            & ! is.null(input$plot8WoningChk)) {
            for (typeW in input$plot8TypewChk) {
                for (woon in input$plot8WoonChk) {
                    for (woning in input$plot8WoningChk) {
                        filt_df8 <- rbind(
                            filt_df8, 
                            filter(df8, 
                                   Type_Waardering == typeW & 
                                       Woonplaats == woon & 
                                       Woningtype_Voorkeur == woning))
                    }
                }
            }
            df8 <- filt_df8
        }else if (!is.null(input$plot8TypewChk) & !is.null(input$plot8WoonChk)) {
            for (typeW in input$plot8TypewChk) {
                for (woon in input$plot8WoonChk) {
                    filt_df8 <- rbind(
                        filt_df8, 
                        filter(df8, 
                               Type_Waardering == typeW & 
                                   Woonplaats == woon))
                }
            }
            df8 <- filt_df8
        }else if (!is.null(input$plot8WoonChk) & !is.null(input$plot8WoningChk)) {
            for (woon in input$plot8WoonChk) {
                for (woning in input$plot8WoningChk) {
                    filt_df8 <- rbind(
                        filt_df8, 
                        filter(df8, 
                               Woonplaats == woon & 
                                   Woningtype_Voorkeur == woning))
                }
            }
            df8 <- filt_df8
        } else if (!is.null(input$plot8TypewChk) & !is.null(input$plot8WoningChk)) {
            for (typeW in input$plot8TypewChk) {
                for (woning in input$plot8WoningChk) {
                    filt_df8 <- rbind(
                        filt_df8, 
                        filter(df8, 
                               Type_Waardering == typeW & 
                                   Woningtype_Voorkeur == woning))
                }
            }
            df8 <- filt_df8
        } else if (!is.null(input$plot8TypewChk)) {
            for (typeW in input$plot8TypewChk) {
                filt_df8 <- rbind(
                    filt_df8, 
                    filter(df8, Type_Waardering == typeW))
            }
            df8 <- filt_df8
        } else if (!is.null(input$plot8WoonChk)) {
            for (woon in input$plot8WoonChk) {
                filt_df8 <- rbind(
                    filt_df8,
                    filter(df8, Woonplaats == woon))
            }
            df8 <- filt_df8
        } else if (!is.null(input$plot8WoningChk)) {
            for (woning in input$plot8WoningChk) {
                filt_df8 <- rbind(
                    filt_df8,
                    filter(df8, Woningtype_Voorkeur == woning))
            }
            df8 <- filt_df8
        }
        
        # Reshape data
        df8 <- df8 %>% select(timeGroup, Lookback_Koop, Lookback_Huur)
        df8 <- melt(df8, id.vars = "timeGroup")
        df8 <- df8 %>% 
            group_by(timeGroup, variable) %>%
            summarize(mean = mean(value), min = min(value), max = max(value))
        
        # Create plot
        p <- ggplot(
            data = df8, aes(x = factor(timeGroup), y = mean, group = variable)) + 
            geom_line(aes(color = variable), size = 5) + theme_bw() + 
            theme(axis.text.x = element_text(angle = 35)) + 
            geom_errorbar(aes(ymin = min, ymax = max), color = '#958D9A') + 
            xlab("") + ylab("") + scale_color_brewer("Blues")
        
        ggplotly(p) %>% layout(margin = list(b = 75, l = 35))
        
    })
    
    # Sends plot 8 to ui
    output$plot8 <- renderPlotly({
        plt8()
    })
    
    # File handler for plot 8 (plotly line chart) on tab 7
    output$dlPlot8 <- downloadHandler(
        filename = "plot8.pdf", 
        content = function(file) {
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            
            htmlwidgets::saveWidget(plt8(), "temp.html", selfcontained = FALSE)
            webshot::webshot("temp.html", file = file, cliprect = "viewport")
        }
    )
    
    # Generates plot 9
    plt9 <- reactive({
        df9 <- df_sub() %>% count(timeGroup, Type)
        
        plot_ly(data = df9, x = ~factor(timeGroup), y = ~n, 
                color = ~factor(Type), type = "bar", 
                colors = brewer.pal(
                    n = length(unique(df9$Type)), 
                    name = "Blues")) %>% 
            layout(xaxis = list(title = ""), yaxis = list(title = ""))
    })
    
    
    # Sends plot 9 to ui
    output$plot9 <- renderPlotly({
        plt9()
    })
    
    
    # File handler for plot 9 (plotly grouped bar chart) on tab 7
    output$dlPlot9 <- downloadHandler(
        filename = "plot9.pdf", 
        content = function(file) {
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            
            htmlwidgets::saveWidget(plt9(), "temp.html", selfcontained = FALSE)
            webshot::webshot("temp.html", file = file, cliprect = "viewport")
        }
    )
    
    # Reshapes data for plot 10
    df_long <- reactive({
        dfl <- df_sub()
        dfl <- select(
            dfl, timeGroup, Fotos_aanbod_huur, Fotos_aanbod_koop, 
            Fotos_trans_huur, Fotos_trans_koop)
        dfl <- melt(dfl, id.vars = "timeGroup")
        count(dfl, timeGroup, variable, value)
    })
    
    # Generates plot 10
    plt10 <- reactive({
        df10 <- df_long()
        
        # Filter data according to radio button inputs
        if (input$plot10Btn == "aan") {
            df10 <- filter(df10, value == "aan")
        } else {
            df10 <- filter(df10, value == "uit")
        }
        
        # Create plot
        plot_ly(data = df10, x = ~factor(timeGroup), y = ~n, 
                color = ~factor(variable), type = "bar", 
                colors = brewer.pal(
                    n = length(unique(df10$variable)), 
                    name = "Blues")) %>%
            layout(xaxis = list(title = "", tickangle = -45), 
                   yaxis = list(title = ""), margin=list(b=100))
        
    })
    
    
    # Sends plot 10 to ui
    output$plot10 <- renderPlotly({
        plt10()
    })
    
    # File handler for plot 10 (plotly grouped bar chart) on tab 7
    output$dlPlot10 <- downloadHandler(
        filename = "plot10.pdf", 
        content = function(file) {
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            
            htmlwidgets::saveWidget(plt10(), "temp.html", selfcontained = FALSE)
            webshot::webshot("temp.html", file = file, cliprect = "viewport")
        }
    )
    
    
    
    
}


shinyApp(ui = ui, server = server)