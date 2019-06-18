#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

setwd("~/R/Jorge/shiny_fx_corisk")

library(shiny)
library(dygraphs)
library(plotly)
library(shinythemes)
library(plyr)
library(dplyr)
library(tidyr)
library(openxlsx)

rm(list=ls())

pval_threshold = 0.05
load('roll_fx_coefficients.RData')

date_list  = names(coef_pnt_array[1,1,])

## Convert 3D array to data.frame (this takes a while even with vector operation)

coef_array_list = list(coef_lwr_array, coef_pnt_array, coef_upr_array, pval_array)
names(coef_array_list) = c('lwr','pnt','upr','pval')

coef_df_list = lapply(coef_array_list, adply, 1:3 )

colnames(coef_df_list$lwr) = c('Source', 'Sink', 'Period', 'lwr')
colnames(coef_df_list$pnt) = c('Source', 'Sink', 'Period', 'pnt')
colnames(coef_df_list$upr) = c('Source', 'Sink', 'Period', 'upr')
colnames(coef_df_list$pval) = c('Source', 'Sink', 'Period', 'pval')


## Combine them to one big dataframe
coef_df = Reduce( function(df1,df2) full_join(df1, df2, by = c('Source','Sink','Period')), coef_df_list)

# Save
write.xlsx(coef_df, 'fulldata_long.xlsx')

# process the other rolling results tables
# 
# load('roll_impact_fx.RData')
# 
# country_list = colnames(roll_coef)
# 
# roll_coef = roll_coef%>%
#     mutate(Period = row.names(roll_coef))%>%
#     gather(key = 'Country', value = 'coeff', country_list)
# 
# roll_impact = roll_impact%>%
#     mutate(Period = row.names(roll_impact))%>%
#     gather(key = 'Country', value = 'impact', country_list)
# 
# 
# full_join(roll_coef,roll_impact, by = c('Country','Period'))



# 
# 
# 
# 
# # Define UI for application that draws a histogram
# ui <- fluidPage(
#    theme = shinytheme("cyborg"),
#    #shinythemes::themeSelector(),
#    # Application title
#    titlePanel("CoRisk analysis: Exchange Rates"),
#    
#    # Sidebar with a slider input for number of bins 
#    sidebarLayout(
#       sidebarPanel(
#         width = 2,
#          #selectInput("asset_class", label = "Asset class",
#         #             choices = c("Forex", "Equity", "Sovereign")),
#         tags$h4("Dynamic analysis"),
#         selectInput("src_ctry", label = "Source country",
#                     choices = sort(rownames(coef_pnt_array[, ,1])),
#                     selected = "China"),
#         selectInput("snk_ctry", label = "Sink country",
#                     choices = sort(rownames(coef_pnt_array[, ,1])),
#                     selected = "Singapore"),
#         tags$h4("Static analysis"),
#         selectInput("date1", label = "Date 1",
#                     choices = date_list,
#                     selected = date_list[1]),
#         selectInput("date2", label = "Date 2",
#                     choices = date_list,
#                     selected = date_list[length(date_list)])
#       ),
# 
#       mainPanel(
#         tabsetPanel(
#           tabPanel("Dynamic", dygraphOutput("CoRiskPlot", height="800px")),
#           tabPanel("Static",  plotlyOutput("snapshot", height = "800px")),
#           tabPanel("Impact",
#                    dygraphOutput("impactplot", height = "800px"))
#         ),
#         style = 'width:1500px; height:1000px'
#       )
#    )
# )
# 
# pval_threshold = 0.05
# 
# server <- function(input, output) { 
# 
#   output$CoRiskPlot <- renderDygraph({
#     
#     src_ctry = input$src_ctry;
#     snk_ctry = input$snk_ctry;
#     
#     coef_pnt = coef_pnt_array[src_ctry, ,]   # country source, point estimate slice
#     coef_lwr = coef_lwr_array[src_ctry, ,]   # country source, lower bound
#     coef_upr = coef_upr_array[src_ctry, ,]   # country source, upper bound
#     
#     ctry_list = rownames(coef_pnt)
#     ctry_list = setdiff(ctry_list, src_ctry)
#     
#     coef_pnt_src = coef_pnt[ctry_list,]
#     coef_lwr_src = coef_lwr[ctry_list,]
#     coef_upr_src = coef_upr[ctry_list,]
#     
#     plot_df = data.frame(coef_lwr_src[snk_ctry,], 
#                          coef_pnt_src[snk_ctry,], 
#                          coef_upr_src[snk_ctry,])
#     colnames(plot_df) = c('lwr_bound', 'estimate', 'upr_bound')
#     
#     ## Sink country database ----
#     
#     coef_pnt = coef_pnt_array[snk_ctry, ,]   # country source, point estimate slice
#     coef_lwr = coef_lwr_array[snk_ctry, ,]   # country source, lower bound
#     coef_upr = coef_upr_array[snk_ctry, ,]   # country source, upper bound
#     
#     ctry_list = rownames(coef_pnt)
#     ctry_list = setdiff(ctry_list, snk_ctry)
#     
#     coef_pnt_snk = coef_pnt[ctry_list,]
#     coef_lwr_snk = coef_lwr[ctry_list,]
#     coef_upr_snk = coef_upr[ctry_list,]
#     
#     plot_df_fdback = data.frame(coef_lwr_snk[src_ctry,], 
#                                 coef_pnt_snk[src_ctry,], 
#                                 coef_upr_snk[src_ctry,])
#     colnames(plot_df_fdback) = c('lwr_bound', 'estimate', 'upr_bound')
#     
#     abc = data.frame(plot_df, plot_df_fdback)
#     
#     graph_title = paste("Corisk from (CoWeakness to) ", src_ctry, "to (from)", snk_ctry, sep = " ")
#     dygraph(abc, main = graph_title, ylab = "CoRisk, CoWeakness") %>% 
#       dyRangeSelector() %>%
#       dySeries(c("lwr_bound", "estimate", "upr_bound"), strokeWidth = 4, label = "Corisk") %>%
#       dySeries(c("lwr_bound.1", "estimate.1", "upr_bound.1"), strokeWidth= 3.5, label = "CoWeakness") %>%
#       dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1"), sigFigs =2) %>%
#       dyHighlight(highlightCircleSize = 10, highlightSeriesBackgroundAlpha = 3) %>%
#       dyAxis("y", axisLineColor = "white", axisLabelColor="white", axisLabelFontSize = 20) %>%
#       dyAxis("x", axisLineColor = "white", axisLabelColor="white", axisLabelFontSize = 14) %>%
#       dyEvent("2008-09-30", "Lehman Brothers", labelLoc = "bottom", color = "white") %>%
#       dyEvent("2010-05-31", "Greece debt crisis", labelLoc = "bottom", color = "white") %>%
#       dyEvent("2015-06-30", "China bubble bursts", labelLoc = "bottom", color = "white") %>%
#       dyEvent("2016-11-30", "Trump, first period", labelLoc = "bottom", color = "white") %>%
#       dyRoller(rollPeriod = 6) 
#     
#   })
#   
#   output$snapshot <- renderPlotly({
#     
#     src_ctry = input$src_ctry
#     snk_ctry = input$snk_ctry
#     
#     coef_pnt = coef_pnt_array[src_ctry, ,]   # country source, point estimate slice
#     ctry_list = rownames(coef_pnt)
#     ctry_list = setdiff(ctry_list, src_ctry)
#     coef_pnt_src = coef_pnt[ctry_list,]
# 
#     #bubble_dates = names(coef_pnt_array[src_ctry, snk_ctry, ])
#     the_date = input$date1
#     
#     coef_pnt_snk = coef_pnt_array[,src_ctry, ]
#     coef_pnt_snk = coef_pnt_snk[, the_date]
#     ctry_list = setdiff(names(coef_pnt_snk), src_ctry)
#     coef_pnt_snk = coef_pnt_snk[ctry_list]
#     
#     # Create data frame for bubble chart 
#     #   first column: CoRisk; second column: CoVulnerability
#     df_src = data.frame(coef_pnt_src[,the_date], coef_pnt_snk)
#     colnames(df_src) = c('source', 'sink')
#     df_src$country = rownames(df_src)
#     
#     ## Set to zero coefficients with p-val less than threshold value
#     #
#     #  Source country spillovers to other countries
#     
#     pval_src = pval_array[src_ctry, ,]  
#     pval_src = pval_src[,the_date]
#     ctry_valid_src = names(pval_src[(pval_src<pval_threshold)])
#     ctry_valid_src = ctry_valid_src[!is.na(ctry_valid_src)]
#     ctry_null_src = setdiff(df_src$country, ctry_valid_src)
#     if (length(df_src[df_src$source<0,]$source)!=0){
#       df_src[ctry_null_src,]$source = 0
#     }
#     if (length(df_src[df_src$source<0,]$source)!=0){
#       df_src[df_src$source<0,]$source = 0
#     }
#     
#     #  Source country as sink of other countries spillovers
#     
#     pval_snk = pval_array[,src_ctry ,]
#     pval_snk = pval_snk[, the_date]
#     ctry_valid_snk = names(pval_src[(pval_src<pval_threshold)])
#     ctry_valid_snk = ctry_valid_snk[!is.na(ctry_valid_snk)]
#     ctry_null_snk = setdiff(df_src$country, ctry_valid_snk)
#     if (length(df_src[ctry_null_snk,]$sink)!=0){
#       df_src[ctry_null_snk,]$sink = 0
#     }
#     if (length(df_src[df_src$sink<0,]$sink)!=0){
#       df_src[df_src$sink<0,]$sink = 0
#     } 
#     
#     df_src = df_src[df_src$source + df_src$sink>0,]
#     df_src$CoRisk = round(df_src$source*100,0)
#     df_src$impact = round(((abs(df_src$source) + abs(df_src$sink))/0.02),0)
#     
#     fnt_title = list(family = "Arial, sans-serif", color = "white", size="20")
#     font_axis = list(size = 40, color = "white")
#     x <- list(title = "CoRisk", 
#               titlefont =fnt_title,
#               color = "white",
#               showline = TRUE, 
#               zeroline = FALSE,
#               font = font_axis)
#     y <- list(title = "CoWeakness", 
#               titlefont =fnt_title,              
#               color = "white",              
#               showline = TRUE,
#               zeroline = FALSE,
#               font = font_axis)
#     the_title = list(text = paste(src_ctry,": CoRisk and CoWeakness", sep =" "),
#                      font = fnt_title, color = "white"
#                      )
# 
#     p1 = plot_ly(df_src, x = ~CoRisk) %>%
#       add_markers(y=~round(sink*100,0), text = ~country, type = 'scatter',  mode = 'markers',
#                   color = ~CoRisk, colors = 'Accent',
#                   marker = list(size=~impact, opacity = 0.5)) %>%
#       add_trace(y = ~CoRisk, type = 'scatter', mode ='lines',
#                 line = list(color="white")) %>%
#       add_trace(y = ~round(sink*100,0), type = 'scatter', mode = 'text',
#                 text = ~country, color = "orange", textposition = "top right" ) %>%
#       layout(title= the_title,
#              xaxis =x, yaxis=y, plot_bgcolor = "#2d343a", paper_bgcolor = "#2d343a")
#     
#     # second bubble chart
#     the_date = input$date2
#     
#     coef_pnt_snk = coef_pnt_array[,src_ctry, ]
#     coef_pnt_snk = coef_pnt_snk[, the_date]
#     ctry_list = setdiff(names(coef_pnt_snk), src_ctry)
#     coef_pnt_snk = coef_pnt_snk[ctry_list]
#     
#     # Create data frame for bubble chart 
#     #   first column: CoRisk; second column: CoVulnerability
#     df_src = data.frame(coef_pnt_src[,the_date], coef_pnt_snk)
#     colnames(df_src) = c('source', 'sink')
#     df_src$country = rownames(df_src)
#     
#     ## Set to zero coefficients with p-val less than threshold value
#     #
#     #  Source country spillovers to other countries
#     
#     pval_src = pval_array[src_ctry, ,]  
#     pval_src = pval_src[,the_date]
#     ctry_valid_src = names(pval_src[(pval_src<pval_threshold)])
#     ctry_valid_src = ctry_valid_src[!is.na(ctry_valid_src)]
#     ctry_null_src = setdiff(df_src$country, ctry_valid_src)
#     if (length(df_src[df_src$source<0,]$source)!=0){
#       df_src[ctry_null_src,]$source = 0
#     }
#     if (length(df_src[df_src$source<0,]$source)!=0){
#       df_src[df_src$source<0,]$source = 0
#     }
#     
#     #  Source country as sink of other countries spillovers
#     
#     pval_snk = pval_array[,src_ctry ,]
#     pval_snk = pval_snk[, the_date]
#     ctry_valid_snk = names(pval_src[(pval_src<pval_threshold)])
#     ctry_valid_snk = ctry_valid_snk[!is.na(ctry_valid_snk)]
#     ctry_null_snk = setdiff(df_src$country, ctry_valid_snk)
#     if (length(df_src[ctry_null_snk,]$sink)!=0){
#       df_src[ctry_null_snk,]$sink = 0
#     }
#     if (length(df_src[df_src$sink<0,]$sink)!=0){
#       df_src[df_src$sink<0,]$sink = 0
#     } 
#     
#     df_src = df_src[df_src$source + df_src$sink>0,]
#     df_src$CoRisk = round(df_src$source*100,0)
#     df_src$impact = round(((abs(df_src$source) + abs(df_src$sink))/0.02),0)
#     
#     #x <- list(title = "CoRisk", color = "white", showline = FALSE, zeroline = FALSE)
#     #y <- list(title = "CoWeakness", color = "white", showline = FALSE, zeroline = FALSE)
#     
#     p2 = plot_ly(df_src, x = ~CoRisk) %>%
#       add_markers(y=~round(sink*100,0), text = ~country, type = 'scatter',  mode = 'markers',
#                   color = ~CoRisk, colors = 'Accent',
#                   marker = list(size=~impact, opacity = 0.5)) %>%
#       #add_text(textposition = "top right") %>%
#       add_trace(y = ~CoRisk, type = 'scatter', mode ='lines',
#                 line = list(color="white")) %>%
#       add_trace(y = ~round(sink*100,0), type = 'scatter', mode = 'text',
#                 text = ~country, color = "#FFD700", textposition = "top right" ) %>%
#       layout(title= the_title,
#              xaxis =x, yaxis=y, plot_bgcolor = "black", paper_bgcolor = "black")    
#     
#     subplot(p1,p2, titleX = TRUE, titleY = TRUE )
# 
#   })
#   
# 
#   
#   output$impactplot <- renderDygraph({
#     
#     # Modified to simplify charts
#     # Use ceiling to round number of countries
#     df_impact = data.frame(ceiling(roll_impact[input$src_ctry]), roll_coef[input$src_ctry])
#     colnames(df_impact) = c('countries', 'Corisk_avg')
#     
#     graph_title = paste(input$src_ctry,": number of impacted countries", sep = " ")
#     dygraph(df_impact, main = graph_title) %>% 
#       #dygraph(df_impact['countries'], main = graph_title) %>%
#       dySeries("countries", axis= 'y', strokeWidth = 4) %>%
#       dySeries("Corisk_avg", axis = 'y2', strokeWidth = 4) %>%
#       dyRangeSelector() %>%
#       #dySeries(c("lwr_bound", "estimate", "upr_bound"), label = "Corisk") %>%
#       #dySeries(c("lwr_bound.1", "estimate.1", "upr_bound.1"), label = "CoWeakness") %>%
#       dyOptions(colors = RColorBrewer::brewer.pal(9, "Set1"), sigFigs =2) %>%
#       dyHighlight(highlightCircleSize = 10, highlightSeriesBackgroundAlpha = 3) %>%
#       dyAxis('y2', label = "CoRisk, average", independentTicks = FALSE,
#              axisLabelColor="white", axisLabelFontSize = 20) %>%      
#       dyAxis("y", label= "Number of countries", axisLineColor = "white", axisLabelColor="white", 
#              axisLabelFontSize = 20, valueRange = c(0,26), pixelsPerLabel = 40) %>%
#       dyAxis("x", axisLineColor = "white", axisLabelColor="white", axisLabelFontSize = 14,
#              drawGrid = FALSE) %>%
#       dyEvent("2008-09-30", "Lehman Brothers", labelLoc = "bottom", color = "white") %>%
#       dyEvent("2010-05-31", "Greece debt crisis", labelLoc = "bottom", color = "white") %>%
#       #dyEvent("2015-06-30", "China bubble bursts", labelLoc = "bottom", color = "white") %>%
#       dyEvent("2015-07-30", "Change to parity setting", labelLoc = "bottom", color = "white") %>%
#       dyEvent("2016-11-30", "Trump, first period", labelLoc = "bottom", color = "white") %>%
#       dyRoller(rollPeriod = 6) 
#   })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
# 
