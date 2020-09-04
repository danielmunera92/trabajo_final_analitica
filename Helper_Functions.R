lector_rds <- 
  function(dirRDS)
{

    rds_files <- list.files(path = dirRDS,pattern = ".rds")
    
    for( file in rds_files)
    {
      
      complete_file_dir <- paste(dirRDS,file,sep = "/")
      file_name <- strsplit(file,"[.]")[[1]][1]
      assign(file_name,readRDS(complete_file_dir),envir = .GlobalEnv)
      
    }
    
  
  
  
  }
create_seq_date <- function(fecha_ini,fecha_fin,per)
{
  
  fecha_ini <- ymd(fecha_ini)
  fecha_fin <- ymd(fecha_fin)
  
  
  if (per == "daily"){
    seq_dt <- seq(from = fecha_ini,to = fecha_fin,by = "day")
    df_seq <- data.frame(Fech = seq_dt)
    
    df_seq <- df_seq %>% 
      mutate(MES = month(Fech),
             DIA = day(Fech), 
             DIA_SEMANA = weekdays(Fech), 
             SEMANA_Y = week(Fech),
             SEMANA_M = ceiling(day(Fech)/7),
             FIN_SEMANA = case_when(DIA_SEMANA %in% c("viernes","sabádo","domingo") ~ "FN",
                                    TRUE ~ "NFN"))
    
    cols_daily_selec <- c("Fech","clase","Acc_total")
    df_inner <- df_seq %>% 
      inner_join(df_mod_daily,by = c("MES","DIA")) %>% 
      group_by(Fech,DIA,MES,clase) %>% 
      summarize(Acc_total = ceiling(mean(Acc_PredDi))) %>% 
      ungroup() %>% select(all_of(cols_daily_selec))
    
   
    df_preed <- df_inner
    names(df_preed) <- c("Fecha","ClaseAccidente","Accidentes")
    df_preed <- df_preed %>%  spread(key = ClaseAccidente,value = Accidentes)
  }
  if (per == "weekly"){
    
    seq_dt <- seq(from = fecha_ini,to = fecha_fin,by = "week")
    df_seq <- data.frame(Fech = seq_dt)
    
    df_seq <- df_seq %>% 
      mutate(MES = month(Fech),
             SEMANA_Y = week(Fech),
             SEMANA_MES = ceiling(day(Fech)/7))
    
    df_preed <- df_seq %>% 
      inner_join(df_mod_week,by = c("SEMANA_MES","SEMANA_Y","MES"))
    cols_weekly_selec <- c("Fech","clase","PromPred")
    df_preed <- df_preed %>%  select(all_of(cols_weekly_selec))
    names(df_preed) <- c("Fecha","ClaseAccidente","Accidentes")
    df_preed$Accidentes <- ceiling(df_preed$Accidentes)
    df_preed <- df_preed %>%  spread(key = ClaseAccidente,value = Accidentes)
  }
  if (per == "monthly"){
    seq_dt <- seq(from = fecha_ini,to = fecha_fin,by = "month")
    df_seq <- data.frame(Fech = seq_dt)
    df_seq <- df_seq %>% 
      mutate(MES = month(Fech))
    df_preed <- df_seq %>% 
      inner_join(df_mod_month,by = "MES")
    
    cols_mont_selec <- c("Fech","clase","PredProm")
   df_preed <- df_preed %>%  select(all_of(cols_mont_selec))
   names(df_preed) <- c("Fecha","ClaseAccidente","Accidentes")
   df_preed$Accidentes <- ceiling(df_preed$Accidentes)
   df_preed <- df_preed %>%  spread(key = ClaseAccidente,value = Accidentes)
    
  }
  
  
  return(df_preed)
}

