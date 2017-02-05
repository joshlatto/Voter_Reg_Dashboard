# Copyright 2016 Josh Latto

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

library(shinydashboard)
library(shiny)
library(DT)
library(rsconnect)
library(sqldf)
library(dygraphs)
library(xts)
library(rmarkdown)
library(ggplot2)


function(input, output) {
  
  ##################################################################################
  ######################## Variables Used Throughout ###############################
  ##################################################################################
  
  # The factor that will be used to set the range of the y-axis in dygraphs.
  # The highest value on the Y axis will be the total reg for the geography * this factor
  
  y_axis_scale_factor <- .6
  next_day <- "2016-10-01"
  
  # processInputs is used throughout to take the user inputs and process them into a form
  # suitable for insertion into sql queries. 
  
  processInputs <- function(county_in, cd_in, sd_in, hd_in) {
    all_counties <-  "('BAKER', 'BENTON', 'CLACKAMAS', 'CLATSOP', 'COLUMBIA', 'COOS', 'CROOK', 'CURRY', 'DESCHUTES', 'DOUGLAS', 'GILLIAM', 'GRANT', 'HARNEY', 'HOOD RIVER', 'JACKSON', 'JEFFERSON', 'JOSEPHINE', 'KLAMATH', 'LAKE', 'LANE', 'LINCOLN', 'LINN', 'MALHEUR', 'MARION', 'MORROW', 'MULTNOMAH', 'POLK', 'SHERMAN', 'TILLAMOOK', 'UMATILLA', 'UNION', 'WALLOWA', 'WASCO', 'WASHINGTON', 'WHEELER', 'YAMHILL')"
    all_sds <- "(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30)"
    all_hds <- "(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60)"
    all_cds <- "(1, 2, 3, 4, 5)"
    
    if(is.null(county_in)) {
      county <- all_counties
    } else {
      if(county_in == "All") {
        county <- all_counties
      } else {
        county <- sprintf("('%s')", county_in)
      }
    }
    if(is.null(cd_in)) {
      cd <- all_cds
    } else {
      if(cd_in == "All") {
        cd <- all_cds
      } else {
        cd <- sprintf("(%s)", cd_in)
      }
    }
    if(is.null(sd_in)) {
      sd <- all_sds
    } else {
      if(sd_in == "All") {
        sd <- all_sds
      } else {
        sd <- sprintf("(%s)", sd_in)
      }
    }
    if(is.null(hd_in)) {
      hd <- all_hds
    } else {
      if(hd_in == "All") {
        hd <- all_hds
      } else {
        hd <- sprintf("(%s)", hd_in)
      }
    }
    return(c(county, cd, sd, hd))
  }
  
  
  ##################################################################################
  ######################## Code for Reading in Data ################################
  ##################################################################################  
  
  ## Read in the registration trends data 
  
  reg_in <- read.table(file = "raw_data_through_august_2016_dates_shifted_to_first_of_next_month.txt", header = TRUE, sep = '\t')
  
  reg_in$year_month <- as.Date(reg_in$year_month,"%m/%d/%Y")
  
  ##################################################################################
  ##################### Server side setup of user inputs ###########################
  ##################################################################################
  
  counties <-  c('All', 'BAKER', 'BENTON', 'CLACKAMAS', 'CLATSOP', 'COLUMBIA', 'COOS', 'CROOK', 'CURRY', 'DESCHUTES', 'DOUGLAS', 'GILLIAM', 'GRANT', 'HARNEY', 'HOOD RIVER', 'JACKSON', 'JEFFERSON', 'JOSEPHINE', 'KLAMATH', 'LAKE', 'LANE', 'LINCOLN', 'LINN', 'MALHEUR', 'MARION', 'MORROW', 'MULTNOMAH', 'POLK', 'SH    ERMAN', 'TILLAMOOK', 'UMATILLA', 'UNION', 'WALLOWA', 'WASCO', 'WASHINGTON', 'WHEELER', 'YAMHILL')

  output$countySelector <- renderUI({
    selectInput("county", "County: ", counties)
  })
  
  cds <- c('All', '1', '2', '3',  '4', '5')
  
  output$cdSelector <- renderUI({
    selectInput("cd", "Congressional District: ", cds)
  })
  
  house_districts <- c('All', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '30', '31', '32', '33', '34', '35', '36', '37', '38', '39', '4    0', '41', '42', '43', '44', '45', '46', '47', '48', '49', '50', '51', '52', '53', '54', '55', '56', '57', '58', '59', '60')
  
  output$hdSelector <- renderUI({
    selectInput("hd", "House District: ", house_districts)
  })
  
  senate_districts <- c('All', '1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16', '17', '18', '19', '20', '21', '22', '23', '24', '25', '26', '27', '28', '29', '30')
  
  output$sdSelector <- renderUI({
    selectInput("sd", "Senate District: ", senate_districts)
  })
  
  ##################################################################################
  #################### Server side report generation code ##########################
  ##################################################################################
  
  output$reg_report <- downloadHandler(
    filename = "Registration_Trends_Report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "Voter_Reg_Trends_Report.Rmd")
      file.copy("Voter_Reg_Trends_Report.Rmd", tempReport, overwrite = TRUE)

      # Process the inputs and assign the processed versions to the
      # appropriate variables 
      inputs <- processInputs(input$county, input$cd, input$sd, input$hd)
      county <- inputs[1]
      cd <- inputs[2]
      sd <- inputs[3]
      hd <- inputs[4]
      
      query <- sprintf("select year_month, sum(count) as registrants from reg_in where county in %s and cd in %s and sd in %s and hd in %s group by 1 order by 1 desc", county, cd, sd, hd)
      reg_collapsed <- sqldf(query)
      #####################################
      ######### Validate Inputs ###########
      #####################################
      validate(
        need(nrow(reg_collapsed) > 0, "Selected geographies do not ovlerap.\nPlease change your selections.")
      )

      params <- list(county = county, cd = cd, hd = hd, sd = sd, reg_data = reg_in, raw_county = input$county, raw_cd = input$cd, raw_hd = input$hd, raw_sd = input$sd)

      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).

      rmarkdown::render(tempReport, output_file = file, params = params, envir = new.env(parent = globalenv()))
    }
  )
  
  ##################################################################################
  ######################## Code for Rending Dygraphs ###############################
  ##################################################################################
  
  ## Total Reg Dygraph
  output$total_reg_dygraph <-  renderDygraph({
    # Process the inputs and assign the processed versions to the
    # appropriate variables 
    inputs <- processInputs(input$county, input$cd, input$sd, input$hd)
    county <- inputs[1]
    cd <- inputs[2]
    sd <- inputs[3]
    hd <- inputs[4]
    
    query <- sprintf("select year_month, sum(count) as registrants from reg_in where county in %s and cd in %s and sd in %s and hd in %s group by 1 order by 1 desc", county, cd, sd, hd)
    reg_collapsed <- sqldf(query)
    #####################################
    ######### Validate Inputs ###########
    #####################################
    validate(
      need(nrow(reg_collapsed) > 0, "Selected geographies do not ovlerap.\nPlease change your selections.")
    )
    reg_data_xts <- as.xts(reg_collapsed[,2], order.by = reg_collapsed[,1])
    reg_data_xts <- c(reg_data_xts, xts(as.integer(NA), as.Date(next_day,"%Y-%m-%d")))
    names(reg_data_xts)[1] <- "Registrants"
    dygraph(reg_data_xts) %>%
      dyAxis(
        "y",
        label = "",
        #http://stackoverflow.com/questions/2901102/how-to-print-a-number-with-commas-as-thousands-separators-in-javascript
        valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
        axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'
      ) %>%
      dySeries("Registrants", label = "Total Registrants", color = "black", strokeWidth = 2) %>%
      dyLegend(width = 400) %>%
      dyOptions(
        connectSeparatedPoints = TRUE
        #stackedGraph = TRUE, fillGraph = TRUE
      ) %>%
      #dyOptions(stackedGraph = TRUE) %>%
      dyRangeSelector(height = 20)
  })
  
  ## Registrants by Party Dygraph
  output$reg_by_party_dygraph <-  renderDygraph({
    # Process the inputs and assign the processed versions to the
    # appropriate variables 
    inputs <- processInputs(input$county, input$cd, input$sd, input$hd)
    county <- inputs[1]
    cd <- inputs[2]
    sd <- inputs[3]
    hd <- inputs[4]
  
    query <- sprintf("select year_month, sum(count) as registrants from reg_in where county in %s and cd in %s and sd in %s and hd in %s group by 1 order by 1 desc", county, cd, sd, hd)
    reg_collapsed <- sqldf(query)
    #####################################
    ######### Validate Inputs ###########
    #####################################
    validate(
      need(nrow(reg_collapsed) > 0, "Selected geographies do not ovlerap.\nPlease change your selections.")
    )
    dem_query <- sprintf("select year_month, sum(count) as Registrants from reg_in where party = 'Democrat' and county in %s and cd in %s and sd in %s and hd in %s group by 1 order by 1 asc", county, cd, sd, hd)
    dem_data <- sqldf(dem_query)
    dem_data_xts <- as.xts(dem_data[,2], order.by = dem_data[,1])
    dem_data_xts <- c(dem_data_xts, xts(as.integer(NA), as.Date(next_day,"%Y-%m-%d")))
    
    rep_query <- sprintf("select year_month, sum(count) as Registrants from reg_in where party = 'Republican' and county in %s and cd in %s and sd in %s and hd in %s group by 1 order by 1 asc", county, cd, sd, hd)
    rep_data <- sqldf(rep_query)
    rep_data_xts <- as.xts(rep_data[,2], order.by = rep_data[,1])
    rep_data_xts <- c(rep_data_xts, xts(as.integer(NA), as.Date(next_day,"%Y-%m-%d")))
     
    ipo_query <- sprintf("select year_month, sum(count) as Registrants from reg_in where party = 'Independent Party' and county in %s and cd in %s and sd in %s and hd in %s group by 1 order by 1 asc", county, cd, sd, hd)
    ipo_data <- sqldf(ipo_query)
    ipo_data_xts <- as.xts(ipo_data[,2], order.by = ipo_data[,1])
    ipo_data_xts <- c(ipo_data_xts, xts(as.integer(NA), as.Date(next_day,"%Y-%m-%d")))
     
    nav_query <- sprintf("select year_month, sum(count) as Registrants from reg_in where party = 'NAV+' and county in %s and cd in %s and sd in %s and hd in %s group by 1 order by 1 asc", county, cd, sd, hd)
    nav_data <- sqldf(nav_query)
    nav_data_xts <- as.xts(nav_data[,2], order.by = nav_data[,1])
    nav_data_xts <- c(nav_data_xts, xts(as.integer(NA), as.Date(next_day,"%Y-%m-%d")))
    
  
    combined <- cbind(dem_data_xts,rep_data_xts,ipo_data_xts,nav_data_xts)
    
    dygraph(combined) %>%
      dyAxis(
        "y",
        label = "",
        #http://stackoverflow.com/questions/2901102/how-to-print-a-number-with-commas-as-thousands-separators-in-javascript
        valueFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}',
        axisLabelFormatter = 'function(d){return d.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ",");}'
      ) %>%
      dySeries("..1", label = "Democrats", color = "blue", strokeWidth = 2) %>%
      dySeries("..2", label = "Republicans", color = "red", strokeWidth = 2)  %>%
      dySeries("..3", label = "Independents", color = "green", strokeWidth = 2) %>%
      dySeries("..4", label = "NAV+", color = "purple", strokeWidth = 2) %>%
      dyLegend(width = 600) %>%
      dyOptions(
        connectSeparatedPoints = TRUE
        #stackedGraph = TRUE, fillGraph = TRUE
      ) %>%
      #dyOptions(stackedGraph = TRUE) %>%
      dyRangeSelector(height = 20)
  })
  
  ## Percent of total registrants by party
  output$percent_by_party_dygraph <-  renderDygraph({
    # Process the inputs and assign the processed versions to the
    # appropriate variables 
    inputs <- processInputs(input$county, input$cd, input$sd, input$hd)
    county <- inputs[1]
    cd <- inputs[2]
    sd <- inputs[3]
    hd <- inputs[4]
    
    query <- sprintf("select year_month, sum(count) as registrants from reg_in where county in %s and cd in %s and sd in %s and hd in %s group by 1 order by 1 desc", county, cd, sd, hd)
    reg_collapsed <- sqldf(query)
    #####################################
    ######### Validate Inputs ###########
    #####################################
    validate(
      need(nrow(reg_collapsed) > 0, "Selected geographies do not ovlerap.\nPlease change your selections.")
    )
    dem_query <- sprintf("select year_month, sum(count) as Registrants from reg_in where party = 'Democrat' and county in %s and cd in %s and sd in %s and hd in %s group by 1 order by 1 asc", county, cd, sd, hd)
    dem_data <- sqldf(dem_query)
    dem_data <- sqldf("
      select a.*, b.registrants as total_reg
      from dem_data a
      left join reg_collapsed b on a.year_month = b.year_month
                      ")
    dem_data$percent <- (dem_data$Registrants / dem_data$total_reg) * 100
    dem_data <- dem_data[,c(1,4)]
    dem_data_xts <- as.xts(dem_data[,2], order.by = dem_data[,1])
    dem_data_xts <- c(dem_data_xts, xts(as.integer(NA), as.Date(next_day,"%Y-%m-%d")))
    
    rep_query <- sprintf("select year_month, sum(count) as Registrants from reg_in where party = 'Republican' and county in %s and cd in %s and sd in %s and hd in %s group by 1 order by 1 asc", county, cd, sd, hd)
    rep_data <- sqldf(rep_query)
    rep_data <- sqldf("
                      select a.*, b.registrants as total_reg
                      from rep_data a
                      left join reg_collapsed b on a.year_month = b.year_month
                      ")
    rep_data$percent <- (rep_data$Registrants / rep_data$total_reg) * 100
    rep_data <- rep_data[,c(1,4)]
    rep_data_xts <- as.xts(rep_data[,2], order.by = rep_data[,1])
    rep_data_xts <- c(rep_data_xts, xts(as.integer(NA), as.Date(next_day,"%Y-%m-%d")))
    
    ipo_query <- sprintf("select year_month, sum(count) as Registrants from reg_in where party = 'Independent Party' and county in %s and cd in %s and sd in %s and hd in %s group by 1 order by 1 asc", county, cd, sd, hd)
    ipo_data <- sqldf(ipo_query)
    ipo_data <- sqldf("
                      select a.*, b.registrants as total_reg
                      from ipo_data a
                      left join reg_collapsed b on a.year_month = b.year_month
                      ")
    ipo_data$percent <- (ipo_data$Registrants / ipo_data$total_reg) * 100
    ipo_data <- ipo_data[,c(1,4)]
    ipo_data_xts <- as.xts(ipo_data[,2], order.by = ipo_data[,1])
    ipo_data_xts <- c(ipo_data_xts, xts(as.integer(NA), as.Date(next_day,"%Y-%m-%d")))
    
    nav_query <- sprintf("select year_month, sum(count) as Registrants from reg_in where party = 'NAV+' and county in %s and cd in %s and sd in %s and hd in %s group by 1 order by 1 asc", county, cd, sd, hd)
    nav_data <- sqldf(nav_query)
    nav_data <- sqldf("
                      select a.*, b.registrants as total_reg
                      from nav_data a
                      left join reg_collapsed b on a.year_month = b.year_month
                      ")
    nav_data$percent <- (nav_data$Registrants / nav_data$total_reg) * 100
    nav_data <- nav_data[,c(1,4)]
    nav_data_xts <- as.xts(nav_data[,2], order.by = nav_data[,1])
    nav_data_xts <- c(nav_data_xts, xts(as.integer(NA), as.Date(next_day,"%Y-%m-%d")))

    
    combined <- cbind(dem_data_xts,rep_data_xts,ipo_data_xts,nav_data_xts)

    dygraph(combined) %>%
      dyAxis(
        "y",
        label = "percentage",
        independentTicks = TRUE,
        valueFormatter = 'function(d){return Math.round(d*100)/1e2 + "%"}',
        axisLabelFormatter = 'function(d){return Math.round(d*100)/1e2 + "%"}'
      ) %>%
      dySeries("..1", label = "Democrats", color = "blue", strokeWidth = 2) %>%
      dySeries("..2", label = "Republicans", color = "red", strokeWidth = 2)  %>%
      dySeries("..3", label = "Independents", color = "green", strokeWidth = 2) %>%
      dySeries("..4", label = "NAV+", color = "purple", strokeWidth = 2) %>%
      dyLegend(width = 700) %>%
      dyOptions(
        connectSeparatedPoints = TRUE
        #stackedGraph = TRUE, fillGraph = TRUE
      ) %>%
      #dyOptions(stackedGraph = TRUE) %>%
      dyRangeSelector(height = 20)
  })
}
      

 