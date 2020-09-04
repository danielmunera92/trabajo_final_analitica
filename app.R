# Librerias ---------------------------------------------------------------
library(shiny)
library(leaflet)
library(shinydashboard)
library(dplyr)
library(lubridate)
library(tidyverse)
library(billboarder)
library(radarchart)
library(DT)
library(highcharter)
library(plotly)
library(xts)
# Datos -------------------------------------------------------------------
dirGen <- getwd()
dirDataShiny <- paste(dirGen,"data",sep = "/")
dirHelperFunctions <- paste(dirGen,"Helper_Functions.R",sep = "/")
source(dirHelperFunctions)
lector_rds(dirDataShiny)

comunas <- unique(acc_filt_final$COMUNA)

clusters <- unique(df_clust$clusters)
color_manual_clust <- c("#0A5EB7","#0CB70A",
                        "#E5E21B","#ED3A28",
                        "#F29E24")
color <- colorFactor(color_manual_clust, clusters)
# UI ----------------------------------------------------------------------

header <- dashboardHeader(
  title = "Accidentalidad Vehicular en Medellin",
  #title = span(img(src = "un_1.png", height = 35), "'Accidentalidad Vehicular en la Ciudad de Medellín'"),
  titleWidth = 600,
  # task list for status of data processing
  dropdownMenuOutput('task_menu'))

# Siderbar
siderbar <-   dashboardSidebar(
  sidebarMenu( 
    id = "menu_tabs",
    menuItem("Mapa",tabName = "mapa_1", icon = icon("globe-americas")),
    menuItem("Analisis Exploratorio",tabName = "explor_a",icon = icon("chart-bar")),
    menuItem("Mod",tabName = "mod_expl",icon = icon("atom"))
  )
)


body <- dashboardBody(
# Configuracion General ---------------------------------------------------
  tags$style("
              body {
    -moz-transform: scale(1, 1); /* Moz-browsers */
    zoom: 1; /* Other non-webkit browsers */
    zoom: 100%; /* Webkit browsers */}
              "),
  
  tabItems(

# Tab_1 -------------------------------------------------------------------


    
    tabItem(
      tabName = "mapa_1",
      fluidRow(
        
        # box( 
        #   width =  3,solidHeader = TRUE, background = "blue",
        #   dateRangeInput("dateRange",
        #                  h5("Select mapping date"),
        #                  label = 'Seleccione la fecha de análisis',
        #                  start = fa_ini, end = fa_fin)),
        valueBoxOutput("hood",width =  2),
        valueBoxOutput("mt",width =  2),
        valueBoxOutput("hrd",width = 2),
        valueBoxOutput("sld",width = 2),
        valueBoxOutput("to_acc",width = 4)
        
      ),
      fluidRow(
        box( width= 7,solidHeader = TRUE,status = "primary", title = "Mapa de Accidentalidad en la Ciudad de Medellin",
             leafletOutput("mede_map",height = 500),
             #absolutePanel(id = "time_plot" , class = "panel panel-default",fixed = TRUE,
             #draggable = TRUE,top = 60,left = 100,right = "auto",bottom = "auto",
             #billboarderOutput(outputId = "daily_acc",height =  200))),
        ),
        box(width = 5, solidHeader = FALSE,status = "primary", title = "Resumen del Barrio" , 
            div(DT::dataTableOutput("resumen_tbl"), 
                style = "font-size: 98%; width: 100%; height: 50%")
        ),
        box(width = 5 , solidHeader = FALSE , status = "primary", title = "Resumen del Cluster",
            div(DT::dataTableOutput("resumen_clt") ) )
        
      ),
      
      fluidRow(
        
        tabBox( title = "Comportamiento en el Tiempo ",
                id = "tabset1",height = "350px",
        tabPanel(width = 4 ,billboarderOutput(outputId = "daily_acc",height =  250),
                 title = "Accidentalidad Diaria",icon = icon("chart-line")),
        tabPanel(width = 4,billboarderOutput(outputId = "daily_acc_type",height = 250),
                 title = "Accidentalidad Diaria por Tipo de Accidente",icon("chart-area"))
        ),
        tabBox(title = "Caracteristicas de los CLusters",
               id = "tabset1",height = "350px",
               tabPanel(title = "Radar Mensual",icon = icon("chart-pie"),
                        chartJSRadarOutput("rdr_mensual",height = 200),width = 4),
               tabPanel(title = "Radar Anual",icon = icon("chart-pie"),
                        chartJSRadarOutput("rdr_an",height = 200),width = 4)
          
          
        )
        )
      ),

# Tab_2 - Explorador ------------------------------------------------------
    tabItem(
      tabName = "explor_a",
      fluidRow(
        box( 
        width = 3,
         title = "Seleccione Rango de Fecha ", 
         status = "primary",solidHeader = F,
         dateRangeInput("dateRange","Fechas: ", 
                        start = "2014-01-01",
                        end = "2014-12-31",
                        min = "2014-01-01",
                        max = "2018-12-31",
                        format = "yy/mm/dd",
                        language = "es") , 
        background = "blue"
          
        ),
        box(
          width = 3, 
          title = "Seleccione la Comuna" ,
          status = "primary",solidHeader = T,
          selectInput("comun_in","Comuna : ",choices = 
                        comunas,selected = "BELÉN")
          
        )
        
      ),
      
      fluidRow(
        tabBox( title = "Comportamiento en el Tiempo ",
                id = "tabset2",height = "350px",
                tabPanel(width = 4 ,
                         billboarderOutput(outputId = "daily_acc_filt",height = 300),
                         title = "Accidentalidad Diaria",icon = icon("chart-line")),
                tabPanel(width = 4,
                         billboarderOutput(outputId = "daily_acc_type_filt",height = 300),
                         title = "Accidentalidad Diaria por Tipo de Accidente",
                         icon("chart-area"))
        ),
        box(title = "Accidentalidad por Tipo" , 
            width = 6 , solidHeader = T,status = "primary",
            highchartOutput("treemap_acc",height = 340)),
        box(title = "Distribucion de Accidentes por Barrio" , 
            width = 6 , solidHeader = T,status = "primary",
            plotlyOutput("violinplot",height = 350)
            ),
        
        box(title = "Mapa de Calor Mes/Dia de la semana" , 
            width = 6 , solidHeader = T,status = "primary",
            highchartOutput("heatmap_acc",height = 340))
        
      )
      
    ),

# Tab_3 -------------------------------------------------------------------
    tabItem(
      tabName = "mod_expl", 
      fluidRow(
        box(title = "Temporalidad",status = "primary",width = 3,
             solidHeader = TRUE,
      radioButtons("tempora", label = "Seleccione la ventana de tiempo en la que quiere predecir:  ",
                   choices = c("Diaria" = "daily", 
                     "Semana" = "weekly",
                     "Mensual" = "monthly")
      ), 
      dateRangeInput("dateRangemod","Periodo de Tiempo a Predecir : ", 
                     start = "2019-01-01",
                     end = "2019-12-31",
                     min = "2019-01-01",
                     max = "2030-01-01",
                     format = "yy/mm/dd",
                     language = "es")
        )
      ),
      fluidRow(
        box(title  = "Tendencia de Accidentes Predichos" , status = "primary", 
            solidHeader = T , 
            billboarderOutput("tende_plot",height = "450px",width = "600px")),
        box(title = "Resultados Prediccion",status = "primary",solidHeader = T,
            div(DT::dataTableOutput("tabla_pred",height = 400) ) 
          
        )
        
      )
    )
  )

# End ---------------------------------------------------------------------

)



ui <- dashboardPage(title = "AcciVehicular",
                    skin ="blue",
                    header,
                    siderbar,
                    body)

# SERVER ------------------------------------------------------------------
server <- function(input,output,session){
  

# Primer Tab --------------------------------------------------------------

  
  output$mede_map <- renderLeaflet({
    
    leaflet(df_clust) %>%
      setView(lng = -75.56933, lat = 6.247268,zoom = 12.5) %>%
      addProviderTiles("CartoDB.Positron", 
                       options = providerTileOptions(noWrap = TRUE))%>% 
      addCircleMarkers(
        lng=~long,
        lat=~lat,
        layerId = ~BARRIO,
        radius=~ PromGen/4,
        stroke=FALSE,
        fillOpacity=0.4,
        color=~color(clusters),
        popup=~paste(
          "<b>", BARRIO ,"</b><br/>",
          "<b>", clusters, "</b><br/>",
          "Promedio de Accidentes Mensual : ", as.character(round(ProMonthAcc)), "<br/>"
        )
        
      ) 
    #%>%
      # addLegend(
      #   "bottomleft", # Legend position
      #   pal=color, # color palette
      #   values=~clusters, # legend values
      #   opacity = 2,
      #   title="Clusters"
      # )
    
    
  })
  
  observeEvent(input$mede_map_marker_click,{
    
    p <- input$mede_map_marker_click
    barrio <- p$id
    print(barrio)
    
    # ValueBox ---------------------------------------------------------------
    
    #GRAVEDAD POR BARRIO
    
    df_gravedad <-  dfOutGravHood %>%  filter(BARRIO == barrio)
    total_acc <- sum(df_gravedad$TotalAcc)
    
    
    output$hood <- renderValueBox({
      
      valueBox(tags$p('BARRIO',style = "font-size : 80%"),
               barrio, 
               color  = "blue"
      )
      
      
    })
    output$mt <- renderValueBox({
      
      val_to <- df_gravedad[df_gravedad$GRAVEDAD == "MUERTO",][3] %>% deframe()
      valFormat <- format(val_to,big.mark = ",", digits = 0)
      propo <- round(val_to / total_acc,3) * 100 
      mess <- paste0("       ",valFormat," (",propo,"%",")")
      
      valueBox(tags$p('Muertos',style = "font-size : 80%"),
               mess,
               icon = icon("skull-crossbones"), 
               color  = "blue",
               width = "3"
      )
      
      
      
    })
    output$hrd <- renderValueBox({
      val_to <- df_gravedad[df_gravedad$GRAVEDAD == "HERIDO",][3] %>%deframe() 
      valFormat <- format(val_to,big.mark = ",", digits = 0)
      propo <- round(val_to / total_acc,3) * 100
      mess <- paste0("       ",valFormat," (",propo,"%",")")
      
      valueBox( tags$p('Heridos',style = "font-size: 80%"),
                mess,
                icon =icon("user-injured"),
                color ="blue"
      )
      
      
      
    })
    output$sld <- renderValueBox({
      val_to <- df_gravedad[df_gravedad$GRAVEDAD == "SD",][3] %>% deframe() 
      valFormat <- format(val_to,big.mark = ",", digits = 3)
      
      propo <- round(val_to / total_acc,3) * 100
      mess <- paste0(" ",valFormat," (",propo,"%",")")
      
      valueBox( tags$p('SoloDanos',style = "font-size: 80%"),
                mess,
                icon = icon("car-crash"),
                color = "blue"
      )
      
      
      
    })
    output$to_acc <- renderValueBox({
      total_format <- total_acc %>%  format(big.mark = ",", digits = 0)
      valueBox( tags$p('Accidentes Totales',style = "font-size: 80%"),
                total_format ,
                color = "blue")
    })
    
    # Tabla -------------------------------------------------------------------
    output$resumen_tbl <- renderDataTable({
      TotalMostComBarr <- totalMostCommonDf %>% 
        filter(BARRIO == barrio)
      
      df_trans <- data.frame(t(as.matrix(TotalMostComBarr)))
      
      names(df_trans) <- "Resumen"
      
      DT::datatable(
        df_trans,
        style = 'bootstrap',
        rownames = TRUE,
        options = list(pageLength = 6,
                       info = FALSE,dom = "t",
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                         "}"))
      )
      
      

       
      
      
      
      
      
      
    })
    
    output$resumen_clt <- renderDataTable({
      
      clust_barr <- unique(df_clust[df_clust$BARRIO == barrio,"clusters"])[1]
      
      select_vars <- c("ProMonthAcc","PromYearAcc","Periocidad_name","Riesgo")
      
      Prom_Gen <- df_clust[df_clust$BARRIO == barrio,"PromGen"]
      info_met <- medoids_pam_fit[medoids_pam_fit$Cluster == clust_barr,
                             select_vars]
      
      info_met <- info_met %>% 
        mutate(ProMonthAcc = round(ProMonthAcc), 
               PromYearAcc = round(PromYearAcc),
               ScoreAccidentalidad = round(Prom_Gen))
      
      names(info_met) <- c("Promedio de Accidentes Mensuales " ,
                           "Promedio de Accidentes Anuales ",
                           "Frecuencia de Accidentes",
                           "Riesgo del Barrio ",
                           "Score de Riesgo ")
      
      
      df_trans <- data.frame(t(as.matrix(info_met)))
      names(df_trans) <- "Resumen Cluster"
      
      DT::datatable(
        df_trans,
        style = 'bootstrap',
        rownames = TRUE,
        options = list(pageLength = 6,
                       info = FALSE,dom = "t",
                       initComplete = JS(
                         "function(settings, json) {",
                         "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                         "}"))
      )
    })
    
    
    
    # Graficos Series de Tiempo -----------------------------------------------
    
    
    df_daily_cum_total <- df_daily_data %>% 
      filter(BARRIO == barrio)%>%
      group_by(FECHA) %>% 
      summarize(TotalAcc = sum(acc_total)) 
    
    df_sp_data <- dfSpData %>% filter(BARRIO == barrio) %>% select(-BARRIO)
    
    output$daily_acc <- renderBillboarder({
      
      billboarder() %>% 
        bb_linechart(df_daily_cum_total ,color = "499DE8",
                     type = "area") %>% 
        bb_y_grid(show = TRUE)%>% 
        bb_x_axis(tick = list(format = "%Y-%m",fit = FALSE)) %>% 
        bb_labs( caption = "Data source: RTE (https://medata.gov.co)")
    })
    output$daily_acc_type <- renderBillboarder({
      
      billboarder() %>%
        bb_linechart(
          data = df_sp_data,
          type = "area",
          
        ) %>%
        bb_x_axis(tick = list(format = "%Y-%m",fit = TRUE)) %>%
        #bb_x_grid(show = TRUE) %>%
        bb_colors_manual("AccidentePeaton" = "#238443" ,
                         "AccidenteVial" = "#FEB24C" ,
                         opacity = 0.4) %>% 
        bb_y_axis(min = 0, padding = 0) %>%  
        bb_legend(position = "inset", inset = list(anchor = "top-right")) %>% 
        bb_labs(y = "Total de Accidentes por Día",
                caption = "Recurso de los datos: RTE (https://medata.gov.co")
    })

    
    # Gráficos de Rdar --------------------------------------------------------
    
    output$rdr_mensual <- renderChartJSRadar({
      
      df_rada_month <- df_radar %>%  filter(Tipo == "Mes") %>% select(-Tipo)
      
      labs <- unique(df_rada_month$Caracteristica)
      
      scores <- list(
        "Muy Baja Accidentalidad" = df_rada_month[df_rada_month$Riesgo =="Muy Baja Accidentalidad","Valor"],
        "Baja Accidentalidad" = df_rada_month[df_rada_month$Riesgo =="Baja Accidentalidad","Valor"],
        "Accidentalidad Media" = df_rada_month[df_rada_month$Riesgo =="Accidentalidad Media","Valor"],
        "Alta Accidentalidad" = df_rada_month[df_rada_month$Riesgo =="Alta Accidentalidad","Valor"],
        "Muy Alta Accidentalidad" = df_rada_month[df_rada_month$Riesgo =="Muy Alta Accidentalidad","Valor"]
      )
      
      chartJSRadar(scores =scores,
                   labs = labs,
                   showToolTipLabel = T)
      
    })
    
    output$rdr_an <- renderChartJSRadar({
      
      
      df_rada_year <- df_radar %>%  filter(Tipo == "Year") %>% select(-Tipo)
      
      labs <- unique(df_rada_year$Caracteristica)
      
      scores <- list(
        "Muy Baja Accidentalidad" = df_rada_year[df_rada_year$Riesgo =="Muy Baja Accidentalidad","Valor"],
        "Baja Accidentalidad" = df_rada_year[df_rada_year$Riesgo =="Baja Accidentalidad","Valor"],
        "Accidentalidad Media" = df_rada_year[df_rada_year$Riesgo =="Accidentalidad Media","Valor"],
        "Alta Accidentalidad" = df_rada_year[df_rada_year$Riesgo =="Alta Accidentalidad","Valor"],
        "Muy Alta Accidentalidad" = df_rada_year[df_rada_year$Riesgo =="Muy Alta Accidentalidad","Valor"]
      )
      
      chartJSRadar(scores =scores,
                   labs = labs,
                   showToolTipLabel = T)
    })

  })
  
  # Reactive Circle Maps
  # observe({
  #   leafletProxy("mede_map") %>% clearPopups()
  #   event <- input$map_shape_click
  #   
  #   if (is.null(event))
  #     return()
  #   print(event)
  # })
  
  
  # daily_data_acum <- reactive({
  #   
  #   
  #   fa_ini <- ymd(input$dateRange[1])
  #   fa_fin <- ymd(input$dateRange[2])
  #   
  # 
  #   
  # })
  
  
  
  # output$daily_acc <- renderBillboarder({
  #   
  #   billboarder() %>% 
  #     bb_linechart(daily_data_acum() ,color = "499DE8",
  #                  type = "area") %>% 
  #     bb_y_grid(show = TRUE)%>% 
  #     bb_x_axis(tick = list(format = "%Y-%m",fit = FALSE)) %>% 
  #     bb_legend(show = FALSE) %>%
  #     bb_labs( caption = "Data source: RTE (https://medata.gov.co)")
  # })
  # 
  # output$daily_acc_type <- renderBillboarder({
  # 
  #   billboarder() %>%
  #     bb_linechart(
  #       data = daily_acc_type_df() ,
  #       type = "area",
  # 
  #     ) %>%
  #     bb_x_axis(tick = list(format = "%Y-%m",fit = FALSE)) %>%
  #     bb_x_grid(show = TRUE) %>%
  #     bb_x_grid(show = TRUE) %>%
  #     bb_colors_manual(opacity = 0.7) %>% 
  #     bb_subchart(show = TRUE, size = list(height = 30)) %>%
  #     bb_labs(y = "Total de Accidentes por Día",
  #             caption = "Recurso de los datos: RTE (https://medata.gov.co")
  # })
  
  
  
  
# Segunda Tab -------------------------------------------------------------

  
  # Data Diario/Daily
  df_daily_reactive <-reactive({
      
      comuna <- input$comun_in
      fa_ini <- ymd(input$dateRange[1])
      fa_fin <- ymd(input$dateRange[2])
      
      df_daily_cum_total <- dfSpDataCo %>%
        filter(between(FECHA,fa_ini,fa_fin),COMUNA == comuna) %>%
        group_by(FECHA) %>% 
        summarize(TotalAcc = AccidentePeaton+AccidenteVial) 
      
      df_daily_cum_total
      
      
            })
  df_daily_type_reactive <- reactive({
    
    comuna <- input$comun_in
    fa_ini <- ymd(input$dateRange[1])
    fa_fin <- ymd(input$dateRange[2])

    
    df_sp_data <- dfSpDataCo %>% filter(between(FECHA,fa_ini,fa_fin),
                                        COMUNA == comuna)  %>%
      group_by(FECHA) %>% 
      summarize(TotalAccPeaton = sum(AccidentePeaton),
                TotalAccVial = sum(AccidenteVial) )
    
    df_sp_data
  })
  df_treemap_reactive <- reactive({
    
    comuna <- input$comun_in
    fa_ini <- ymd(input$dateRange[1])
    fa_fin <- ymd(input$dateRange[2])
    
    comuna_acc_to <- acc_filt_final %>% 
      mutate(FECHA_filt =ymd(format(ymd_hms(FECHA),"%Y/%m/%d"))) %>% 
      filter(between(FECHA_filt,fa_ini,fa_fin)) %>% 
      filter(COMUNA == comuna) %>% 
      group_by(Letalidad) %>% 
      summarize(ToAcc = sum(accidente)) %>% ungroup() %>% 
      mutate(Total = sum(ToAcc),
             propor = paste0(round(ToAcc/Total,3) * 100,"%"),
              Letalidad = 
               case_when(Letalidad == "AccLeveVial" ~ paste0("Accidente Level Vial ","(",propor,")"),
                         Letalidad == "AccNoGraveVial" ~ paste0("Accidente No Grave Vial","(",propor,")"), 
                         Letalidad == "AccLevePeaton" ~ paste0("Accidente Level Peaton","(",propor,")"),
                         Letalidad == "AccLeveVial" ~ paste0("Accidente Level Vial","(",propor,")"),
                         Letalidad == "AccLetalVial" ~ paste0("Accidente Letal Vial","(",propor,")"),
                         Letalidad == "AccLetalPeaton" ~ paste0("Accidente Letal Peaton","(",propor,")"),
                         TRUE ~ Letalidad))
    
    comuna_acc_to
    
  })
  df_heatmap_reactive <- reactive({

    
    comuna <- input$comun_in
    fa_ini <- ymd(input$dateRange[1])
    fa_fin <- ymd(input$dateRange[2])
    
    Mes_es <- c("ENERO","FEBRERO","MARZO","ABRIL",
                "MAYO","JUNIO","JULIO","AGOSTO",
                "SEPTIEMBRE","OCTUBRE","NOVIEMBRE","DICIEMBRE")
    
    dia_semana <- c("LUNES","MARTES","MIERCOLES","JUEVES","VIERNES",
                    "SABADO","DOMINGO")
    
    
    mes_day_<- acc_filt_final %>% 
      mutate(FECHA_filt =ymd(format(ymd_hms(FECHA),"%Y/%m/%d")),
             MES_NAME = Mes_es[MES]) %>% 
      filter(between(FECHA_filt,fa_ini,fa_fin)) %>% 
      filter(COMUNA == comuna) %>% 
      group_by(MES_NAME,MES,DIA_NOMBRE) %>% 
      summarize(Total_acc = sum(accidente)) %>% 
      ungroup() %>% 
      arrange(MES) %>% 
      select(MES_NAME,DIA_NOMBRE,Total_acc)
    
    
    mes_day_
    
  })
  df_violin_reactive <- reactive({
    
    comuna <- input$comun_in
    fa_ini <- ymd(input$dateRange[1])
    fa_fin <- ymd(input$dateRange[2])
    
    
    violin_df<- acc_filt_final %>% 
      mutate(FECHA_filt =ymd(format(ymd_hms(FECHA),"%Y/%m/%d"))) %>% 
      filter(COMUNA == comuna)  %>% 
      filter(between(FECHA_filt,fa_ini,fa_fin)) %>% 
      group_by(FECHA_filt,BARRIO) %>% 
      summarize(TotalAcc = sum(accidente)) %>% 
      filter(TotalAcc >= 1)
    
    violin_df
    
  })
  
  
  output$daily_acc_filt <- renderBillboarder({
    
    billboarder() %>% 
      bb_linechart(df_daily_reactive() ,color = "499DE8",
                   type = "area") %>% 
      bb_y_grid(show = TRUE)%>% 
      bb_x_axis(tick = list(format = "%Y-%m",fit = FALSE)) %>% 
      bb_labs( caption = "Data source: RTE (https://medata.gov.co)")
  })
  output$daily_acc_type_filt <- renderBillboarder({
    
    billboarder() %>%
      bb_linechart(
        data = df_daily_type_reactive(),
        type = "area",
        
      ) %>%
      bb_x_axis(tick = list(format = "%Y-%m",fit = TRUE)) %>%
      #bb_x_grid(show = TRUE) %>%
      bb_colors_manual("AccidentePeaton" = "#238443" ,
                       "AccidenteVial" = "#FEB24C" ,
                       opacity = 0.4) %>% 
      bb_y_axis(min = 0, padding = 0) %>%  
      bb_legend(position = "inset", inset = list(anchor = "top-right")) %>% 
      bb_labs(y = "Total de Accidentes por Dia",
              caption = "Recurso de los datos: RTE (https://medata.gov.co")
  })  
  output$treemap_acc <- renderHighchart({
    
    hchart(df_treemap_reactive(), "treemap",
           hcaes(x = Letalidad, value = ToAcc,
                 color = ToAcc)) 
  })
  output$heatmap_acc <- renderHighchart({
    
    Mes_es <- c("ENERO","FEBRERO","MARZO","ABRIL",
                "MAYO","JUNIO","JULIO","AGOSTO",
                "SEPTIEMBRE","OCTUBRE","NOVIEMBRE","DICIEMBRE")
    dia_semana <- c("LUNES","MARTES","MIERCOLES","JUEVES","VIERNES",
                    "SABADO","DOMINGO")
    
    hchart(df_heatmap_reactive(), "heatmap", 
           hcaes(x = DIA_NOMBRE, 
                 y = MES_NAME,
                 value = Total_acc),
           name = "Accidentes Totales") %>% 
      hc_add_theme(hc_theme_google()) %>% 
      hc_yAxis(categories = Mes_es , 
               title = list(text = "MES")) %>% 
      hc_xAxis(categories = dia_semana , 
               title = list(text = "DIA DE LA SEMANA"))
  })
  output$violinplot <- renderPlotly({
      
     df_violin_reactive() %>% 
      plot_ly(
        x = ~BARRIO,
        y = ~TotalAcc , 
        split = ~BARRIO,
        type = "violin",
        box = list(
          visible =T
        ),
        meanline = list(
          visible = T
        )
      ) %>% 
      layout(
        xaxis = list(title = "BARRIO"), 
        yaxis = list(title = "Accidentes Totales", zeroline = F)
        
      )
      
    
  })
  
  
# Tercera Tab -------------------------------------------------------------

data_mod_temp <- reactive({
  fa_ini <- ymd(input$dateRangemod[1])
  fa_fin <- ymd(input$dateRangemod[2])
  temp <- input$tempora
  
  df_select <- create_seq_date(fa_ini,fa_fin,temp)
  
  df_select
  
})
output$tabla_pred <- renderDataTable({
  data_mod_temp()
})
output$tende_plot <- renderBillboarder({
  

  billboarder() %>%
    bb_linechart(
      data = data_mod_temp(),
      type = "area",
      
    ) %>%
    bb_x_axis(tick = list(format = "%Y-%m",fit = TRUE)) %>%
    #bb_x_grid(show = TRUE) %>%
    bb_colors_manual("AccidentePeaton" = "#238443" ,
                     "AccidenteVial" = "#FEB24C" ,
                     opacity = 0.4) %>% 
    bb_y_axis(min = 0, padding = 0) %>%  
    bb_legend(position = "inset", inset = list(anchor = "top-right")) %>% 
    bb_labs(y = "Total de Accidentes por Dia",
            caption = "Prediccion del Modelo de Accidentalidad por clase ")
  
})


}
# RunApp ------------------------------------------------------------------
shinyApp(ui,server)

