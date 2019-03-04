
#Load required packages
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(ggthemes)
library(scales)
library(reshape2)
library(lubridate)
library(pander)
library(shiny)
library(DBI)
library(RMySQL)
library(DT)
#library(tidyverse)




#Usage
#USERNAME AND PASSWORD REMOVED
mydb = dbConnect(MySQL(), user='', password='', dbname='', host='', port=)
rs = dbSendQuery(mydb, "SELECT Building, Department, trim(Delivery_Group) as Delivery_Group, Location, Floor, Usage_DateTime FROM ;")
#rs = dbSendQuery(mydb, "SELECT*FROM ;")


df = fetch(rs,n=-1)
dbDisconnect(mydb)

#dates for min and max cal

min_max_date=df%>%
  mutate(Date=date(Usage_DateTime))


#USERNAME AND PASSWORD REMOVED
master_dg=unique(df$Delivery_Group)
#TU values
mydb2 = dbConnect(MySQL(), user='', password='', dbname='', host='', port=)
rs2 = dbSendQuery(mydb2, "SELECT* FROM ;")

df2 = fetch(rs2,n=-1)
dbDisconnect(mydb2)


df2$HMRC_Cost=as.numeric(df2$HMRC_Cost)

tu_con=filter(df2,Floor=="All",Location=="All")

#USERNAME AND PASSWORD REMOVED
#allocation values
mydb3 = dbConnect(MySQL(), user='', password='', dbname='', host='', port=)
rs3 = dbSendQuery(mydb3, "SELECT Building, Delivery_Group, Floor, Allocated_Headcount FROM  WHERE Floor = 'All';"
)

df3 = fetch(rs3,n=-1)
dbDisconnect(mydb3)


dg_all_cons=df3%>%
  group_by(Building, Delivery_Group)%>%
  summarise(Allocated_Headcount=sum(Allocated_Headcount))
dg_con_dg=unique(df3$Delivery_Group)
dg_unique=intersect(dg_con_dg,master_dg)


# Define UI for application that draws a histogram
ui <- 
  dashboardPage(skin="yellow",title="Aternity",
                dashboardHeader(title =span(img(src="logo.png", width = 400))),
                dashboardSidebar(
                  sidebarMenu(
                    id="sidebar",
                    menuItem("Builiding Allocated Roles", tabName ="floor", icon = icon("stats", lib = "glyphicon")),
                    menuItem("Business Group - Week", tabName ="business_group", icon = icon("stats", lib = "glyphicon")),
                    menuItem("Business Group - Day",tabName ="business_hourly", icon = icon("stats", lib = "glyphicon"))
                    
                    
                  )
                ),
                dashboardBody(tags$head(tags$style(HTML("
                                                        .skin-yellow .main-header .logo {background-color: #f3f34c;}
                                                        .skin-yellow .main-header .logo:hover {background-color: #f3f34c;}
                                                        .skin-yellow .main-header .navbar {background-color: #f3f34c;;color: #000000;}        
                                                        .skin-yellow .main-sidebar {background-color: #f3f34c;}
                                                        .skin-yellow .main-sidebar .sidebar .sidebar-menu .active a{background-color: #33ccff;border-left:#00c0ef;}
                                                        .skin-yellow .main-sidebar .sidebar .sidebar-menu a{background-color: #f3f34c;color: #000000;}
                                                        .skin-yellow .main-sidebar .sidebar .sidebar-menu a:hover{background-color: #33ccff;border-left:#00c0ef;}
                                                        .skin-yellow .main-sidebar .sidebar .sidebar-menu li.active{background-color: #00c0ef;border-left:#00c0ef;}
                                                        .skin-yellow .main-sidebar .sidebar .sidebar-menu a:hover{background-color: #00c0ef;border-left:#00c0ef;}
                                                        .skin-yellow .main-header .navbar .sidebar-toggle:hover{background-color: #00c0ef;border-left:#00c0ef;color: #000000;}
                                                        .skin-yellow .main-header .navbar .sidebar-toggle{color:#000000;}
                                                        .skin-yellow .main-header .main-sidebar .sidebar .sidebar-menu .sidebar-toggle:hover{background-color:#00c0ef;border-left:#00c0ef;}"))),
                              
                              tabItems(
                                tabItem("floor", 
                                        fluidRow(
                                          box(width=4,status="info", solidHeader=TRUE, title="People (Highest)",
                                              (mainPanel(textOutput("peopleHighest"))) 
                                          ),
                                          (box(width=4,status="info", solidHeader=TRUE,
                                               title = (value=HTML(paste0("Target m2 per person"))),
                                               
                                               (mainPanel(textOutput("targetm2"))))),
                                          (box(width=4,status="info", solidHeader=TRUE,
                                               title = (value=(paste0("Potential additional FTE to utilisation target (14% absence)"))),
                                               (mainPanel(textOutput("potentialFTE")))))),
                                        
                                        fluidRow(
                                          
                                          (box(width=4,status="info", solidHeader=TRUE,
                                               title = (value=HTML(paste0("Running Cost ","&#163;", "m/pa HMRC only"))),
                                               icon = "fa fa-credit-card",
                                               (mainPanel((textOutput("runningCost")))))),
                                          (box(width=4,status="info", solidHeader=TRUE,
                                               title = (value=(paste0("Highest m2 per person"))),
                                               
                                               (mainPanel(textOutput("highestm2"))))),
                                          
                                          
                                          (box(width=4,status="info", solidHeader=TRUE,
                                               title = (value=HTML(paste0("Potential cost saving ","&#163;", "m/pa"))),
                                               (mainPanel(textOutput("potentialSavings")))))),
                                        fluidRow(
                                          (box(width=12,status="info", solidHeader=TRUE, title=("Building Allocated Roles"),icon=icon("stats", lib = "glyphicon"), selectInput("building", label=("Building:"), ""),
                                               dateRangeInput('dateRange', label=("Select date:"), start=min(min_max_date$Date), end=max(min_max_date$Date),format = "dd/mm/yy"),
                                               (mainPanel (width=12, plotOutput(("ut_plot")))))
                                          )
                                          
                                          
                                        )),
                                tabItem("business_group",
                                        fluidRow(
                                          (box(width=6,status="info", solidHeader=TRUE, title=(""),
                                               (mainPanel( tableOutput("allocation_table"))))),
                                          (box(width=6,status="info", solidHeader=TRUE, title=("People to move to equal day allocation"),
                                               (mainPanel( tableOutput("people_table"))))),
                                          
                                          (box(width=12,status="info", solidHeader=TRUE, title=("Delivery Group: Daily"),selectInput("building_2", label=("Building:"), ""),
                                               selectInput("delivery_group", label=("Delivery Group:"), ""),dateRangeInput('dateRange_2', label=("Select date:"), start=min(min_max_date$Date), end=max(min_max_date$Date),format = "dd/mm/yy"),
                                               (mainPanel(width=12, plotOutput(("dg_weekly_plot")))))
                                          )
                                        )
                                        
                                        
                                ),
                                tabItem("business_hourly",
                                        fluidRow(
                                          
                                          (box(width=12,status="info", solidHeader=TRUE, title=("Delivery Group: Hourly"),selectInput("building_3", label=("Building:"), ""),
                                               selectInput("delivery_group_2", label=("Delivery Group:"), ""),dateRangeInput('dateRange_3', label=("Select date:"), start=min(min_max_date$Date), end=max(min_max_date$Date),format = "dd/mm/yy"),
                                               (mainPanel(width=12, plotOutput(("hourly_dg")))))
                                          )
                                        )
                                        
                                        
                                )
                                
                              )
                              
                )
                
                
                #)
                
                #)
                
                
                )#dashboardpage closing bracket



# Define server logic required to draw a histogram
server <- function(input, session, output) {
  #function for determining if number needs k, m, b, t suffix; rounds to one deicmal place
  comprss <- function(tx) { 
    div <- findInterval(as.numeric(gsub("\\,", "", tx)), 
                        c(1, 1e3, 1e6, 1e9,  1e12) )
    paste(round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 1), 
          c("","K","M","B","T")[div] )}
  
  #handles negative numbers
  comprss2 <- function(tx) { 
    div <- findInterval(as.numeric(gsub("\\,", "", (-tx))), 
                        c((1), (1e3), (1e6), (1e9),  (1e12)))
    paste(round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 1), 
          c("","K","M","B","T")[div] )}
  
  observe({
    build=unique(df$Building)
    updateSelectInput(session, "building",choices=c(build))
    
  })
  
  
  ############################ BUILDING ALLOCATION TAB #####################################################
  #puts values from the buildings column in the df
  
  building_allo=df%>%
    group_by(Usage_DateTime,Building)%>%
    summarise(users=n())
  building_allo=building_allo%>%
    mutate(hour=hour(Usage_DateTime),Date=date(Usage_DateTime),day=wday(Usage_DateTime,label=T))
  
  #filter by input building
  building_df=reactive({
    data=filter(building_allo, Building==input$building)
    
  })
  
  #filter by input dates
  dates_df=reactive({
    dates_data=building_df()[building_df()$Date>=(input$dateRange[1])&building_df()$Date<=(input$dateRange[2]),]})
  
  
  #filters the constants table to input building 
  constant=reactive({
    con_data=filter(tu_con,Building==input$building)
  })
  
  #takes filtered df (date and building) and selects the maximum users per building, floor on each date (i.e max value over a 24 hr period) 
  max_floor=reactive({
    max_floor_data=dates_df()%>%
      group_by(Building,Date,day)%>%
      summarise(max_users_floor=max(users))})
  
  
  
  #Takes average floor values for each day of the week.  Need to remove the date at this point at the aggregation won't work (it will
  #aggredate at date level, rather than generic day of week).
  
  #Takes max floor values for each day of the week. 
  daily_floor_avs=reactive({
    daily_floor_data=max_floor()%>%
      group_by(Building,day)%>%
      summarise(av_daily_floor=mean(max_users_floor),max_floor_daily_users=max(max_users_floor))})
  
  #Tables add individual floor averages and maximum values to get overall totals for average and maximum per day for each building.
  av_daily_users=reactive({
    av_daily_data=daily_floor_avs()%>%
      group_by(Building,day)%>%
      summarise(av_daily_users=sum(av_daily_floor),max_daily_users=sum(max_floor_daily_users))})
  
  #Tables add individual floor averages and maximum values to get overall totals for average and maximum per day for each building.
  #av_daily_users=reactive({
  #  av_daily_data=daily_floor_avs()%>%
  #  group_by(Building,day)%>%
  #  summarise(max_daily_users=sum(max_floor_daily_users),av_daily_users=mean(max_floor_daily_users))})
  
  
  output$ut_plot=renderPlot({
    
    av_daily_users2=melt(av_daily_users())
    av_daily_users2$variable=factor(av_daily_users2$variable,levels=c("max_daily_users","av_daily_users"))
    
    from=as.Date(input$dateRange[1],"%Y%m%d")
    from=format(from,format="%d/%m/%y")
    
    to=as.Date(input$dateRange[2],"%Y%m%d")
    to=format(to,format="%d/%m/%y")
    
    
    av_daily_users2$day=ordered(av_daily_users2$day,levels=c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
    graph_title=paste(av_daily_users2$Building, from,"to",to)
    
    p=ggplot(av_daily_users2,aes(x=day,y=value,fill=variable))+
      geom_bar(width=0.7, position=position_dodge(width=0.75), stat="identity")+
      theme_bw()+
      labs(title=graph_title, x="Day of week", y="People")+
      scale_fill_manual(values=c("deepskyblue3","darkorange"),name="Utilisation",labels=c("Maximum Utilisation", "Average Utilisation"))+
      scale_y_continuous(expand =c(0,0),limits=c(0,(constant()$Legal_Max_HMRC[1]*1.1)),breaks=seq(0,(constant()$Legal_Max_HMRC[1]*1.1),200))+
      geom_hline(mapping=aes(yintercept=constant()$Legal_Max_HMRC[1]),color="red",size=2)+
      geom_text(aes(4,constant()$Legal_Max_HMRC[1],label = c("Total Maximum Estate Capacity"),vjust=-0.5))+
      geom_rect(mapping=aes(xmin=0,xmax=8,ymin=(constant()$Target_Utilisation_HMRC[1]*0.9),ymax=(constant()$Target_Utilisation_HMRC[1]*1.1)),fill="darkolivegreen3")+
      geom_text(aes(4,constant()$Target_Utilisation_HMRC[1]*1.1,label = c("Target Utilisation +/- 10%"),vjust=+2.5))
    
    p
    
    
  })
  #highest people box
  output$peopleHighest=renderText({
    comma(max(av_daily_users()$max_daily_users))
  })
  #running costs box
  output$runningCost=renderText({
    comprss(constant()$HMRC_Cost[1])
  })
  #target m2 box
  output$targetm2=renderText({
    constant()$Target_area[1]
  })
  #highest m2 box
  output$highestm2=renderText({
    round(constant()$NIA_HMRC[1]/max(av_daily_users()$max_daily_users),1)
  })
  #potenital fte saving box
  output$potentialFTE=renderText({
    fte=round(((constant()$Target_Utilisation_HMRC[1]*(100/86))-(max(av_daily_users()$max_daily_users))),0)
    fte
  })
  #potential savings box
  output$potentialSavings=renderText({
    fte=round(((constant()$Target_Utilisation_HMRC[1]*(100/86))-(max(av_daily_users()$max_daily_users))),0)
    
    tu=constant()$Target_Utilisation_HMRC[1]
    av=(max(av_daily_users()$max_daily_users))
    #savings=(tu-av)*(100/86)*(constant()$Cost_per_FTE[1])
    savings=fte*(constant()$Cost_per_FTE[1])
    
    savings= if(savings<0){
      savings=comprss2(savings) 
      
    }else{
      savings=comprss(savings)
    }
    savings
  })
  
  
  ######################## Business Group Week plot #################################################  
  
  
  #observes building and delivery group
  observe({
    build2=unique(df$Building)
    updateSelectInput(session, "building_2",choices=c(build2))
    
  })
  observe({
    dg_list=unique(dg_unique)
    updateSelectInput(session, "delivery_group",choices=c("All",dg_list))
    
  })
  
  #filters data and groups by DG
  dg_allo_tb=reactive({
    if (input$delivery_group=="All"){
      dg_allo_df=df%>%
        group_by(Usage_DateTime,Building)%>%
        summarise(users=n())}
    else {
      dg_allo_df=df%>%
        group_by(Usage_DateTime,Building,Delivery_Group)%>%
        summarise(users=n())
      
    }
    
  })
  
  #get time data
  dg_allo=reactive({
    dg_allo_df2=dg_allo_tb()%>%
      mutate(hour=hour(Usage_DateTime),Date=date(Usage_DateTime),day=wday(Usage_DateTime,label=T))})
  
  #removes weekends
  dg_wkday=reactive({
    wkday_df=filter(dg_allo(), day !="Sat" & day !="Sun")
  })
  #reactive dataframe for filtering to building level if "All" selected, or DG level depending on selection
  dg_df=reactive({
    
    if (input$delivery_group=="All"){
      dg_df_data=filter(dg_wkday(), Building==input$building_2)
    } 
    else{
      dg_df_data=filter(dg_wkday(),Building==input$building_2,Delivery_Group==input$delivery_group)
      
    }
  })
  
  #filters df by date
  dg_dates= reactive({
    dg_dates_df=dg_df()[ dg_df()$Date>=(input$dateRange_2[1])& dg_df()$Date<=(input$dateRange_2[2]),]
  }) 
  #summarise the data to get maximum number of people on each floor on each date
  max_delivery_group=reactive({
    if (input$delivery_group=="All"){
      max_dg=dg_dates()%>%
        group_by(Building,Date,day)%>%
        summarise(max_users_floor=max(users))
    }
    else{
      max_dg=dg_dates()%>%
        group_by(Building,Delivery_Group,Date,day)%>%
        summarise(max_users_floor=max(users))
    }  
  })
  
  #working out the sum of max users per building on a given date
  sum_max_delivery_group=reactive({
    if (input$delivery_group=="All"){
      sum_max_dg=max_delivery_group()%>%
        group_by(Building,Date,day)%>%
        summarise(sum_max_users_floor=sum(max_users_floor))
    }
    else{
      sum_max_dg=max_delivery_group()%>%
        group_by(Building,Delivery_Group,Date,day)%>%
        summarise(sum_max_users_floor=sum(max_users_floor))
    }
    
  })
  
  #takes average for the max figures per building
  av_max_delivery_group=reactive({
    if (input$delivery_group=="All"){
      av_max_dg=sum_max_delivery_group()%>%
        group_by(Building,day)%>%
        summarise(av_sum_max_users_floor=mean(sum_max_users_floor))
    }
    else {
      av_max_dg=sum_max_delivery_group()%>%
        group_by(Building, Delivery_Group,day)%>%
        summarise(av_sum_max_users_floor=mean(sum_max_users_floor))
    }
  })
  
  #weekly average value (on a per week average - i.e. M-F is the same value )
  weekly_av_max_delivery_group=reactive({
    if (input$delivery_group=="All"){
      week_av_dg=sum_max_delivery_group()%>%
        group_by(Building)%>%
        summarise(av_sum_max_users_floor=mean(av_max_delivery_group()$av_sum_max_users_floor))}
    else {
      week_av_dg=sum_max_delivery_group()%>%
        group_by(Building,Delivery_Group)%>%
        summarise(av_sum_max_users_floor=mean(av_max_delivery_group()$av_sum_max_users_floor))}
  })
  #plot for the DG with weekly and daily averages
  output$dg_weekly_plot=renderPlot({
    from_2=as.Date(input$dateRange_2[1],"%Y%m%d")
    from_2=format(from_2,format="%d/%m/%y")
    
    ###for dynamic title of graph
    to_2=as.Date(input$dateRange_2[2],"%Y%m%d")
    to_2=format(to_2,format="%d/%m/%y")
    
    graph_title_2=paste(av_max_delivery_group()$Building, from_2,"to",to_2)
    av_allo_line=data.frame(yintercept=weekly_av_max_delivery_group()$av_sum_max_users_floor, cutoff=factor(weekly_av_max_delivery_group()$av_sum_max_users_floor))
    
    #Graph
    p2=ggplot(av_max_delivery_group(),aes(x=day,y=av_sum_max_users_floor))+
      geom_bar(width=0.7, position=position_dodge(width=0.75), stat="identity", fill="deepskyblue3")+
      theme_bw()+
      labs(title=graph_title_2,x="Day of week", y="People")+
      scale_y_continuous(expand =c(0,0),limits=c(0,(dg_constant()$Allocated_Headcount[1])*1.2))+#,breaks=seq(0,(max(av_max_delivery_group()$av_sum_max_users_floor)*1.2),200))+
      geom_hline(mapping=aes(yintercept=weekly_av_max_delivery_group()$av_sum_max_users_floor, linetype="Average Utilisation (Mon-Fri)"),color="gray",size=2)+
      geom_hline(mapping=aes(yintercept=dg_constant()$Allocated_Headcount[1],linetype="Maximum Allocation"),color="darkorange",size=2)+
      scale_linetype_manual(name="", values = c(1,1), guide = guide_legend(override.aes = list(color = c("gray", "darkorange"))))
    
    
    p2
    
    
  })
  
  #constant values required for allocations for the two tables
  constant_dg=reactive({
    con_data_dg=filter(tu_con,Building==input$building_2)
  })
  #filters the constants table to input building 
  dg_constant=reactive({
    
    if (input$delivery_group=="All"){
      dg_all_con_df=filter(dg_all_cons, Building==input$building_2)
      dg_all_con_df=dg_all_con_df%>%
        group_by(Building)%>%
        summarise(Allocated_Headcount=sum(Allocated_Headcount))
      
    }
    else{
      dg_all_con_df=filter(dg_all_cons,Delivery_Group==input$delivery_group, Building==input$building_2)
      
    }
  })
  
  
  #table for utilisation, allocation, variance and cost savings
  output$allocation_table=renderTable({
    
    
    if (input$delivery_group=="All"){
      weekly_av_dg_table=filter(weekly_av_max_delivery_group(), Building==input$building_2)
    }
    else {
      weekly_av_dg_table=filter(weekly_av_max_delivery_group(), Building==input$building_2, Delivery_Group==input$delivery_group)
    }
    
    #average utilisation mon-fri
    av_ut=round((weekly_av_max_delivery_group()$av_sum_max_users_floor[1]),0)
    allocation_value=round(dg_constant()$Allocated_Headcount[1],0)
    variance_percent=round(((allocation_value-av_ut)/allocation_value)*100,0)
    people_to_achieve_allo= round(((100/86)*(allocation_value-weekly_av_dg_table$av_sum_max_users_floor[1])),-1)
    cost_sav=((constant_dg()$Cost_per_FTE[1]*people_to_achieve_allo))
    
    #formats numbers if pos or neg to include k,M,B,T
    cost_sav=
      if (cost_sav <0){
        cost_sav=comprss2(cost_sav)}
    else{
      cost_sav=comprss(cost_sav)
    }
    
    
    Measurement=c("Average Utilisation (Mon-Fri)", "Allocation", "Variance (%)", "People to Achieve Allocation (14% Absence)", "Cost Saving (GBP)")
    Values=c(av_ut,allocation_value,variance_percent,((people_to_achieve_allo)),cost_sav)
    
    allocation_tab=data.frame(Measurement,Values)
    
  })
  #people to move to equal da allocation table
  
  output$people_table=renderTable({
    if (input$delivery_group=="All"){
      weekly_av_dg_table=filter(weekly_av_max_delivery_group(), Building==input$building_2)
    }
    else {
      weekly_av_dg_table=filter(weekly_av_max_delivery_group(), Building==input$building_2, Delivery_Group==input$delivery_group)
    }
    
    m=round((weekly_av_max_delivery_group()$av_sum_max_users_floor[1])- av_max_delivery_group()$av_sum_max_users_floor[1],0)
    tus=round((weekly_av_max_delivery_group()$av_sum_max_users_floor[1])- av_max_delivery_group()$av_sum_max_users_floor[2],0)
    w=round((weekly_av_max_delivery_group()$av_sum_max_users_floor[1])- av_max_delivery_group()$av_sum_max_users_floor[3],0)
    thr=round((weekly_av_max_delivery_group()$av_sum_max_users_floor[1])- av_max_delivery_group()$av_sum_max_users_floor[4],0)
    f=round((weekly_av_max_delivery_group()$av_sum_max_users_floor[1])- av_max_delivery_group()$av_sum_max_users_floor[5],0)
    
    Days=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
    Values=c(m,tus,w,thr,f)
    peop_tab=data.frame(Days,Values)
    peop_tab$Values=round((peop_tab$Values),2)
    peop_tab
    
  })
  
  
  ###### hourly plot ####### Excludes Sat & Sun and hours between 23:00 and 5:00 inlcusive
  
  #filter initial DF and count users
  hourly_allo=df%>%
    group_by(Usage_DateTime,Building,Delivery_Group,Floor)%>%
    summarise(users=n())
  
  #get date,hour and day data
  hourly_allo=hourly_allo%>%
    mutate(hour=hour(Usage_DateTime),Date=date(Usage_DateTime),day=wday(Usage_DateTime,label=T))
  
  #filter table to exlcude Sat and Sun
  hourly_wkday=filter(hourly_allo, day !="Sat" & day !="Sun")
  
  #filter table to only include 6am to 10pm
  hourly_hour=filter(hourly_wkday, hour>=6 & hour<=22) 
  
  #observe from building list
  observe({
    build3=unique(hourly_hour$Building)
    updateSelectInput(session, "building_3",choices=c(build3))
    
  })
  #observe from DG list
  observe({
    dg_list_2=unique(hourly_hour$Delivery_Group)
    updateSelectInput(session, "delivery_group_2",choices=c("All",dg_list_2))
    
  })
  #filter table to include which building & DG selected
  dg_df_2=reactive({
    
    
    if (input$delivery_group_2=="All"){
      dg_df_data_2=filter(hourly_hour, Building==input$building_3)
    } 
    else{
      dg_df_data_2=filter(hourly_hour,Building==input$building_3,Delivery_Group==input$delivery_group_2)
      
    }
  })
  
  #filter table for dates
  dg_dates_2= reactive({
    dg_dates_df_2=dg_df_2()[ dg_df_2()$Date>=(input$dateRange_3[1])& dg_df_2()$Date<=(input$dateRange_3[2]),]
  }) 
  
  #summarise the data to get maximum number of people on each floor on each date
  max_delivery_group_2=reactive({
    if (input$delivery_group_2=="All"){
      max_dg_2=dg_dates_2()%>%
        group_by(Building,Delivery_Group,Date,day,hour)%>%
        summarise(max_users_floor=max(users))
    }
    else{
      max_dg_2=dg_dates_2()%>%
        group_by(Building,Delivery_Group,Date,day,hour)%>%
        summarise(max_users_floor=max(users))
    }  
  })
  
  #sum up number of people per building on any day for each hourly
  sum_max_delivery_group_2=reactive({
    if (input$delivery_group_2=="All"){
      sum_max_dg_2=max_delivery_group_2()%>%
        group_by(Building,Date,day,hour)%>%
        summarise(sum_max_users_floor=sum(max_users_floor))
    }
    else{
      sum_max_dg_2=max_delivery_group_2()%>%
        group_by(Building,Delivery_Group,Date,day,hour)%>%
        summarise(sum_max_users_floor=sum(max_users_floor))
    }
    
  })
  
  #hourly average
  av_max_delivery_group_2=reactive({
    if (input$delivery_group_2=="All"){
      av_max_dg_2=sum_max_delivery_group_2()%>%
        group_by(Building,hour)%>%
        summarise(av_sum_max_users_floor=mean(sum_max_users_floor))
    }
    else {
      av_max_dg_2=sum_max_delivery_group_2()%>%
        group_by(Building, Delivery_Group,hour)%>%
        summarise(av_sum_max_users_floor=mean(sum_max_users_floor))
    }
  })
  
  #plot for each hour per building & DG
  output$hourly_dg=renderPlot({
    from_3=as.Date(input$dateRange_3[1],"%Y%m%d")
    from_3=format(from_3,format="%d/%m/%y")
    
    to_3=as.Date(input$dateRange_3[2],"%Y%m%d")
    to_3=format(to_3,format="%d/%m/%y")
    
    graph_title_3=paste(av_max_delivery_group_2()$Building, from_3,"to",to_3)
    
    p3=ggplot(data=av_max_delivery_group_2(), aes(x=hour, y=av_sum_max_users_floor))+
      geom_line(color="deepskyblue3",size=1)+
      geom_point(color="deepskyblue3")+
      theme_bw()+
      labs(title=graph_title_3,x="Time of day", y="People")+
      scale_y_continuous(expand =c(0,0),
                         limits=c(0,(max(av_max_delivery_group_2()$av_sum_max_users_floor)*1.2)))+
      scale_x_continuous(breaks=seq((min(av_max_delivery_group_2()$hour)),(max(av_max_delivery_group_2()$hour)),1))
    
    
    
    p3
  })
  
} 

# Run the application 
shinyApp(ui = ui, server = server)

