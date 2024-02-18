# global.R script

#install library if necessary and load them
pacman::p_load("rio","janitor", "tidyverse", "lubridate", "shiny", "skimr")

# import raw data (change it to link !!!!!!!)
linelist_raw <- import("https://figshare.com/ndownloader/files/35249488")

################################################################################
# data cleaning 
linelist <- linelist_raw %>%
  
  # clean columns names(remove space, replace uppercase letters to lowercase letters)
  clean_names() %>%
  
  # change column types
  mutate(vl = as.numeric(vl),
         cd4 = as.numeric(cd4),
         base_drug_combo = as_factor(base_drug_combo),
         timepoints = as.integer(timepoints),
         gender = as_factor(gender),
         ethnic = as_factor(ethnic)) 
  

df_sub <- linelist %>%
  group_by(timepoints,gender,ethnic,base_drug_combo) %>%
  summarise(m_vl = mean(vl),
            m_cd4 = mean(cd4)) 
  

################################################################################
# define plotting function
p <- function(data, g  ,e , d) {
  
  #set columns to filter in a plot
  if (!("All" %in% g)) {            
    data <- data %>%
      filter(gender %in% g)
  }
  
  if (!("All" %in% e)) {            
    data <- data %>%
      filter(ethnic %in% e)
  }
  
  if (!("All" %in% d)) {            
    data <- data %>%
      filter(base_drug_combo %in% d)
  }


  # plot
  ggplot(data) +
    geom_smooth(aes(x = timepoints, y = m_vl), size=.5, span=.1, colour= "#4DB6D0", se=FALSE) +
    geom_point(aes(x = timepoints, y = m_vl), size=.2, colour ="#4DB6D0") +
    geom_smooth(aes(x = timepoints, y = m_cd4*10), size=.5, span=.1, colour = "#D9717D", se=FALSE) +
    geom_point(aes(x = timepoints, y = m_cd4*10), size=.2, colour ="#D9717D") +
    scale_x_discrete(name = "Timepoints", limits =1:60, breaks = c(5,10,20,40,60)) +
    scale_y_continuous(name = "Average viral load (copies/mL)",
                       sec.axis = sec_axis(~ ./10, name = "Average count of CD4 (cells/μL)")) +
    labs(caption = "Source: Synthetic ART-in-HIV dataset") +
    theme_light() +
    theme(axis.title.y.left = element_text(colour = "#4DB6D0", angle = 270),
          axis.title.y.right = element_text(colour = "#D9717D"),
          legend.position = "none") 
    

      
  
}



