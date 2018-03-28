#require(devtools)
#httr::set_config( httr::config( ssl_verifypeer = 0L))
#install_github('ramnathv/rCharts',force=TRUE)

library(RODBC)
library(data.table)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(rCharts)

hopper <- odbcConnect("Hopper")

data_tot_gmv <- data.table(sqlQuery(hopper,"SELECT CK_DATE, TRANSACTION_SITE, SLR_CNTRY, SUM(GMV) AS GMV FROM P_GC_SHP_T.CBT_DIAGNOSE_TOOL_SUMMARY WHERE CK_DATE < CURRENT_DATE - 1 GROUP BY 1,2,3;"))
t_beg <- max(data_tot_gmv$CK_DATE) - 14
t_end <- max(data_tot_gmv$CK_DATE)

# define calculation functions here
abs_dif <- function(pre,post){post / pre - 1}
share_dif <- function(pre,post,pre_all,post_all){(post/post_all)/(pre/pre_all) - 1}
contr <- function(pre){pre / sum(pre)}
contr_var <- function(pre,post){(post - pre) / (sum(post) - sum(pre))}
exp_by_demand <- function(pre,post,pre_all,post_all){(post_all * pre / pre_all - pre) / (post - pre)}

ui <- dashboardPage(
  dashboardHeader(title = 'CBT Diagnose Tool'),
  dashboardSidebar(
    dateRangeInput(inputId = "pre_date",label = "Pre Window", start = t_beg, end = t_beg),
    dateRangeInput(inputId = "post_date",label = "Post Window", start = t_end, end = t_end),
    checkboxGroupInput(inputId = "SLR_CNTRY", label = "Seller Country:",
                       choices = list("GC" = "GC", "SEA" = "SEA", "IL" = "IL", "JP" = "JP", "OTHERS" = "OTHERS"),
                       selected = c("GC")),
    checkboxGroupInput(inputId = "TRANSACTION_SITE", label = "Transaction Site:",
                       choices = list("US" = "US", "UK" = "UK", "DE" = "DE", "FRITES" = "FRITES", "AU" = "AU", "OTHERS" = "OTHERS"),
                       selected = c("US","UK","DE","FRITES","AU","OTHERS")),
    selectInput(inputId = "dimension", label = "Dimension:",
                choices = list("Transaction Site" = "TRANSACTION_SITE",
                               "Listing Site" = "LSTG_SITE",
                               "CBT Type" = "CBT_TYPE",
                               "Corridor Country" = "CORRIDOR_CNTRY",
                               "Corridor" = "CORRIDOR",
                               "Item Location" = "ITEM_LOCATION",
                               "Vertical" = "VERTICAL",
                               "TOP 50 Level 2 Category" = "TOP50_CATEG_LVL2_FLAG",
                               "Top 50 Level 2 Category Name" = "TOP50_CATEG_LVL2_NAME",
                               "Penetration Tranche" = "SLR_CNTRY_PENETRATION_TRANCHE",
                               "Relative ASP Tranche" = "RLTV_ASP_TRANCHE",
                               "Price Tranche" = "PRICE_TRANCHE",
                               "Listing Type" = "LSTG_TYPE",
                               "Listing Tenure" = "LSTG_TENURE",
                               "Listing Velocity" = "LSTG_VELOCITY",
                               "SRP Impression" = "SRP_CNT",
                               "SRP-VIP Conversion Rate" = "SRP_VI_CNV",
                               "Valid EPID" = "EPID_YN",
                               "Branded" = "BRAND_FLAG",
                               "Seller Tenure" = "SLR_TENURE",
                               "Top 50 Brand" = "TOP50_BRAND_FLAG",
                               "Top 50 Brand Name" = "TOP50_BRAND",
                               "Promoted Listing" = "PL_FLAG",
                               "Deal Item" = "DEAL_ITEM_FLAG",
                               "Seller Standard" = "SLR_STD",
                               "Seller Defect Rate" = "SLR_DEFECT_RATE",
                               "Trust Restriction" = "TRUST_RESTRICT_PRE1WK_FLAG",
                               "JSL" = "JSL_PRE1WK_FLAG",
                               "Managed Account" = "MANAGE_MARK",
                               "AMS Segment" = "AMS_SLR_SEGMENT",
                               "Top Seller" = "TOP_SLR_GROUP",
                               "B2C/C2C" = "B2C_C2C_FLAG",
                               "Arbitrage by API" = "ARBITRAGE_API_FLAG",
                               "Arbitrage Demotion" = "ARBITRAGE_DEMOTION_FLAG"),
                selected = "VERTICAL"),
    checkboxGroupInput(inputId = "dimension_cross", label = "Cross Dimensions:",
                       choices = list("Seller Country" = "SLR_CNTRY",
                                      "Transaction Site" = "TRANSACTION_SITE",
                                      "Listing Site" = "LSTG_SITE",
                                      "CBT Type" = "CBT_TYPE",
                                      "Corridor Country" = "CORRIDOR_CNTRY",
                                      "Corridor" = "CORRIDOR",
                                      "Item Location" = "ITEM_LOCATION",
                                      "Vertical" = "VERTICAL",
                                      "TOP 50 Level 2 Category" = "TOP50_CATEG_LVL2_FLAG",
                                      "Top 50 Level 2 Category Name" = "TOP50_CATEG_LVL2_NAME",
                                      "Penetration Tranche" = "SLR_CNTRY_PENETRATION_TRANCHE",
                                      "Relative ASP Tranche" = "RLTV_ASP_TRANCHE",
                                      "Price Tranche" = "PRICE_TRANCHE",
                                      "Listing Type" = "LSTG_TYPE",
                                      "Listing Tenure" = "LSTG_TENURE",
                                      "Listing Velocity" = "LSTG_VELOCITY",
                                      "SRP Impression" = "SRP_CNT",
                                      "SRP-VIP Conversion Rate" = "SRP_VI_CNV",
                                      "Valid EPID" = "EPID_YN",
                                      "Branded" = "BRAND_FLAG",
                                      "Seller Tenure" = "SLR_TENURE",
                                      "Top 50 Brand" = "TOP50_BRAND_FLAG",
                                      "Top 50 Brand Name" = "TOP50_BRAND",
                                      "Promoted Listing" = "PL_FLAG",
                                      "Deal Item" = "DEAL_ITEM_FLAG",
                                      "Seller Standard" = "SLR_STD",
                                      "Seller Defect Rate" = "SLR_DEFECT_RATE",
                                      "Trust Restriction" = "TRUST_RESTRICT_PRE1WK_FLAG",
                                      "JSL" = "JSL_PRE1WK_FLAG",
                                      "Managed Account" = "MANAGE_MARK",
                                      "AMS Segment" = "AMS_SLR_SEGMENT",
                                      "Top Seller" = "TOP_SLR_GROUP",
                                      "B2C/C2C" = "B2C_C2C_FLAG",
                                      "Arbitrage by API" = "ARBITRAGE_API_FLAG",
                                      "Arbitrage Demotion" = "ARBITRAGE_DEMOTION_FLAG"),
                       selected = c("SLR_CNTRY","TRANSACTION_SITE"))
  ),
  dashboardBody(
    fluidRow(
      column(width = 5,
             box(width = NULL,
                 plotOutput("plot_tot_gmv", height = 250))
             ),
      column(width = 5,
             box(width = NULL,
                 plotOutput("plot_cut_gmv", height = 250))
      )
    ),
    fluidRow(
      column(width = 10,
             box(width = NULL,
                 title = "PART 1: Total GMV by Transaction Site",
                 tableOutput(outputId = "pvt1")),
             box(width = NULL,
                 title = "PART 2: Transaction-Site-Level GMV by Dimensions",
                 tableOutput(outputId = "pvt2")),
             box(width = NULL,
                 title = "Data Export: Cross Dimensions",
                 dataTableOutput(outputId = "data_groupby"))             
      )
    )
  )
)


server <- function(input, output){
  output$plot_tot_gmv <- renderPlot({
    gmv_by_cntry <- data_tot_gmv[SLR_CNTRY %in% input$SLR_CNTRY & TRANSACTION_SITE %in% input$TRANSACTION_SITE, .(Sum_GMV = sum(GMV)), keyby = CK_DATE]
    ggplot(gmv_by_cntry, aes(x = as.Date(CK_DATE), y = Sum_GMV)) + 
      geom_line(color = "blue", size = 1) +
      labs(title="Total GMV",x="Date", y = "GMV")
  })
  output$plot_cut_gmv <- renderPlot({
    gmv_by_cut_query <- paste0("SELECT CK_DATE, TRANSACTION_SITE, SLR_CNTRY,",input$dimension, ", SUM(GMV) AS GMV FROM P_GC_SHP_T.CBT_DIAGNOSE_TOOL_SUMMARY WHERE CK_DATE < CURRENT_DATE - 1 GROUP BY 1,2,3,4")
    gmv_by_cut <- data.table(sqlQuery(hopper,gmv_by_cut_query))
    gmv_by_cntry_cut_cmd <- paste0("gmv_by_cntry_cut <- gmv_by_cut[SLR_CNTRY %in% input$SLR_CNTRY & TRANSACTION_SITE %in% input$TRANSACTION_SITE, .(Sum_GMV = sum(GMV)), keyby = .(",input$dimension,", CK_DATE)]")
    eval(parse(text = gmv_by_cntry_cut_cmd))
    plot_cut_cmd <- paste0("ggplot(gmv_by_cntry_cut, aes(x = CK_DATE, y = Sum_GMV, group = ",input$dimension,")) + geom_line(aes(color =",input$dimension,")) + theme(legend.position='bottom') +
                           labs(title='GMV Decomposition',x='Date', y = 'GMV') + theme_classic()")
    eval(parse(text = plot_cut_cmd))
  })
  output$pvt1 <- renderTable({
    gmv_by_site_query_pre <- paste0("SELECT SLR_CNTRY, TRANSACTION_SITE, SUM(GMV) AS GMV FROM P_GC_SHP_T.CBT_DIAGNOSE_TOOL_SUMMARY WHERE CK_DATE BETWEEN '",as.character(input$pre_date[1]),"' AND '",as.character(input$pre_date[2]),"' GROUP BY 1,2")
    gmv_by_site_query_post <- paste0("SELECT SLR_CNTRY, TRANSACTION_SITE, SUM(GMV) AS GMV FROM P_GC_SHP_T.CBT_DIAGNOSE_TOOL_SUMMARY WHERE CK_DATE BETWEEN '",as.character(input$post_date[1]),"' AND '",as.character(input$post_date[2]),"' GROUP BY 1,2")
    gmv_by_site_pre <- data.table(sqlQuery(hopper,gmv_by_site_query_pre))
    gmv_by_site_post <- data.table(sqlQuery(hopper,gmv_by_site_query_post))
    gmv_by_site_out_pre <- gmv_by_site_pre[SLR_CNTRY %in% input$SLR_CNTRY, .(Sum_GMV = sum(GMV)), keyby = TRANSACTION_SITE]
    gmv_by_site_out_post <- gmv_by_site_post[SLR_CNTRY %in% input$SLR_CNTRY, .(Sum_GMV = sum(GMV)), keyby = TRANSACTION_SITE]
    gmv_by_site_all_out_pre <- gmv_by_site_pre[,.(Sum_GMV = sum(GMV)), keyby = TRANSACTION_SITE]
    gmv_by_site_all_out_post <- gmv_by_site_post[,.(Sum_GMV = sum(GMV)), keyby = TRANSACTION_SITE]
    data.frame("Transaction Site" = gmv_by_site_out_post$TRANSACTION_SITE, 
               "GC GMV - Post vs Pre" = abs_dif(gmv_by_site_out_pre$Sum_GMV,gmv_by_site_out_post$Sum_GMV),
               "Site GMV - Post vs Pre" = abs_dif(gmv_by_site_all_out_pre$Sum_GMV,gmv_by_site_all_out_post$Sum_GMV),
               "GC GMV Share - Post vs Pre" = share_dif(gmv_by_site_out_pre$Sum_GMV,gmv_by_site_out_post$Sum_GMV,gmv_by_site_all_out_pre$Sum_GMV,gmv_by_site_all_out_post$Sum_GMV),
               "GC GMV Contribution - Pre" = contr(gmv_by_site_out_pre$Sum_GMV),
               "Contribution to GC GMV Post vs Pre Variance" = contr_var(gmv_by_site_out_pre$Sum_GMV,gmv_by_site_out_post$Sum_GMV),
               "Explained by site demand" = exp_by_demand(gmv_by_site_out_pre$Sum_GMV,gmv_by_site_out_post$Sum_GMV,gmv_by_site_all_out_pre$Sum_GMV,gmv_by_site_all_out_post$Sum_GMV),
               "Explained by GC GMV share change" = 1 - exp_by_demand(gmv_by_site_out_pre$Sum_GMV,gmv_by_site_out_post$Sum_GMV,gmv_by_site_all_out_pre$Sum_GMV,gmv_by_site_all_out_post$Sum_GMV)
               , check.names=FALSE)
  },align = 'c')
  output$pvt2 <- renderTable({
    gmv_by_dim_query_pre <- paste0("SELECT SLR_CNTRY, TRANSACTION_SITE, ",input$dimension, ", SUM(GMV) AS GMV FROM P_GC_SHP_T.CBT_DIAGNOSE_TOOL_SUMMARY WHERE CK_DATE BETWEEN '",as.character(input$pre_date[1]),"' AND '",as.character(input$pre_date[2]),"' GROUP BY 1,2,3")
    gmv_by_dim_query_post <- paste0("SELECT SLR_CNTRY, TRANSACTION_SITE, ",input$dimension, ", SUM(GMV) AS GMV FROM P_GC_SHP_T.CBT_DIAGNOSE_TOOL_SUMMARY WHERE CK_DATE BETWEEN '",as.character(input$post_date[1]),"' AND '",as.character(input$post_date[2]),"' GROUP BY 1,2,3")
    gmv_by_dim_pre <- data.table(sqlQuery(hopper,gmv_by_dim_query_pre))
    gmv_by_dim_post <- data.table(sqlQuery(hopper,gmv_by_dim_query_post))
    gmv_by_dim_out_pre_cmd <- paste0("gmv_by_dim_out_pre <- gmv_by_dim_pre[SLR_CNTRY %in% input$SLR_CNTRY & TRANSACTION_SITE %in% input$TRANSACTION_SITE, .(Sum_GMV = sum(GMV)), keyby = ",input$dimension,"]")
    eval(parse(text = gmv_by_dim_out_pre_cmd))
    gmv_by_dim_out_post_cmd <- paste0("gmv_by_dim_out_post <- gmv_by_dim_post[SLR_CNTRY %in% input$SLR_CNTRY & TRANSACTION_SITE %in% input$TRANSACTION_SITE, .(Sum_GMV = sum(GMV)), keyby = ",input$dimension,"]")
    eval(parse(text = gmv_by_dim_out_post_cmd))
    gmv_by_dim_all_out_pre_cmd <- paste0("gmv_by_dim_all_out_pre <- gmv_by_dim_pre[SLR_CNTRY %in% input$SLR_CNTRY,.(Sum_GMV = sum(GMV)), keyby = ",input$dimension,"]")
    eval(parse(text = gmv_by_dim_all_out_pre_cmd))
    gmv_by_dim_all_out_post_cmd <- paste0("gmv_by_dim_all_out_post <- gmv_by_dim_post[SLR_CNTRY %in% input$SLR_CNTRY,.(Sum_GMV = sum(GMV)), keyby = ",input$dimension,"]")
    eval(parse(text = gmv_by_dim_all_out_post_cmd))
    data.frame("Transaction Site" = gmv_by_dim_out_post[,1], 
               "GC GMV - Post vs Pre" = abs_dif(gmv_by_dim_out_pre$Sum_GMV,gmv_by_dim_out_post$Sum_GMV),
               "Site GMV - Post vs Pre" = abs_dif(gmv_by_dim_all_out_pre$Sum_GMV,gmv_by_dim_all_out_post$Sum_GMV),
               "GC GMV Share - Post vs Pre" = share_dif(gmv_by_dim_out_pre$Sum_GMV,gmv_by_dim_out_post$Sum_GMV,gmv_by_dim_all_out_pre$Sum_GMV,gmv_by_dim_all_out_post$Sum_GMV),
               "GC GMV Contribution - Pre" = contr(gmv_by_dim_out_pre$Sum_GMV),
               "Contribution to GC GMV Post vs Pre Variance" = contr_var(gmv_by_dim_out_pre$Sum_GMV,gmv_by_dim_out_post$Sum_GMV),
               "Explained by site demand" = exp_by_demand(gmv_by_dim_out_pre$Sum_GMV,gmv_by_dim_out_post$Sum_GMV,gmv_by_dim_all_out_pre$Sum_GMV,gmv_by_dim_all_out_post$Sum_GMV),
               "Explained by GC GMV share change" = 1 - exp_by_demand(gmv_by_dim_out_pre$Sum_GMV,gmv_by_dim_out_post$Sum_GMV,gmv_by_dim_all_out_pre$Sum_GMV,gmv_by_dim_all_out_post$Sum_GMV)
               , check.names=FALSE)
  }, align = 'c')
  output$data_groupby <- renderDataTable({
    
    n <- length(input$dimension_cross) - 1
    if(n == 0){
      char_fin <- input$dimension_cross
    }
    else{
      dim_cross <- input$dimension_cross
      for(i in 1:n){
        char_vol <- paste0(dim_cross[i],", ",dim_cross[i+1])
        dim_cross[i+1] <- char_vol
      }
      char_fin <- dim_cross[n+1]
    }
    gmv_groupby <- paste0("SELECT CK_DATE, ",char_fin, ", SUM(GMV) AS GMV FROM P_GC_SHP_T.CBT_DIAGNOSE_TOOL_SUMMARY GROUP BY CK_DATE, ",char_fin)
    gmv_groupby_data <- data.table(sqlQuery(hopper,gmv_groupby))
    gmv_groupby_data
  })
}

shinyApp(ui, server)
