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
library(dygraphs)


dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Voter Registration Trends",
    titleWidth = 300,
    tags$li(class = "dropdown",
      tags$div(href='http://ouroregon.org',
                  img(src='oo_logo_2.png', height = '50px'))
    )
  ),
  dashboardSidebar(
    uiOutput("countySelector"),
    uiOutput("cdSelector"),
    uiOutput("hdSelector"),
    uiOutput("sdSelector")
  ),
  dashboardBody(
      fluidPage(
        fluidRow(
          box(
            title = "Total Registrants by Month", status = "primary", solidHeader = TRUE,
            dygraphOutput("total_reg_dygraph"),
            width = NULL
          )
        ),
        fluidRow(
          box(
            title = "Registrants by Party by Month", status = "primary", solidHeader = TRUE,
            dygraphOutput("reg_by_party_dygraph"),
            width = NULL
          ),
          box(
            title = "Share of Registered Voters by Party", status = "primary", solidHeader = TRUE,
            dygraphOutput("percent_by_party_dygraph"),
            width = NULL
          )
        ),
        fluidRow(
          downloadButton("reg_report", "Generate report")
        )
    )
  )
)
  

