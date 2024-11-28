# --------------------------------------------------------------------------------------------------
# This code produces suitability maps for the best siting of sand dams in Angolan region of Namibe #
# --------------------------------------------------------------------------------------------------

# Packages installation and activation
library(sp)
library(raster)
library(rivernet)
# library(whitebox)
# library(gdalUtils)
library(dplyr)
library(nat)
library(sf)
library(dplyr)
library(ggplot2)
library(stats)

# Clearing workspace
rm(list=ls(all=TRUE)) ## Clear Environment
graphics.off()
# -------------------------------------------------------------------------------------
# Setting working directories
Database_folder = "~/Documents/UNIFI/Lab_papers/0_Sand_dams_database/DATA_Sand_dams/Database/0_FINAL_DATABASE_April2024/"
Global_spatial_data_SD = "~/Documents/Data/Global_spatial_data/Global_Data_Sand_Dams/"
Admin_boundaries = "~/Documents/Data/Administrative_boundaries/"

# Elevation_Folder = "~/Documents/PhD_project/Data/Global_Land_Data/Elevation/"
# Biophysical_data = "~/Documents/UNIFI/TransAgua/Doc_TransAgua/Dati/Biophysical_suitability/"

Results = "~/Documents/UNIFI/Lab_papers/Sand_dams_database/Results/"

# # projection variables
wgs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


# # -------------------------------------------------------------------------------------------------
# Sand dams Database
# # -------------------------------------------------------------------------------------------------
setwd(Database_folder)

All_case_studies.table<- read.csv("Sand_dam_database_final_May15_2024.csv", header = T, sep = ";")
All_case_studies.table$Dam.lenght..m. <- as.numeric(as.character(All_case_studies.table$Dam.lenght..m.))
All_case_studies.table$Throwback..m. <- as.numeric(as.character(All_case_studies.table$Throwback..m.))
All_case_studies.table$Average.river.width.upstream.of.the.dam..m. <- as.numeric(as.character(All_case_studies.table$Average.river.width.upstream.of.the.dam..m.))
All_case_studies.table$LAT <- as.numeric(as.character(All_case_studies.table$LAT))
All_case_studies.table$LON <- as.numeric(as.character(All_case_studies.table$LON))
All_case_studies.table$Construction_Date <- as.numeric(as.character(All_case_studies.table$Construction_Date))

# #Filtering sand dams
# sand_dams_database <- water_point_database %>%
#   dplyr::select(row_id, clean_adm1, water_source_category, Install_Year, Functionality, lat_lon_deg) %>%
#   dplyr::filter(grepl("Sand", water_source_category))
# 
# sand_dams_Kenya <- sand_dams_database  %>% 
#   dplyr::filter(clean_adm1=="Kenya")


# Plotting descriptive database features
# Boxplot for dams characteristics, including length, throwback and stream width
Data_norm <- dplyr::filter(All_case_studies.table, (Average.river.width.upstream.of.the.dam..m.<100|is.na(Average.river.width.upstream.of.the.dam..m.)))
Data_norm <- dplyr::filter(All_case_studies.table, (Dam.lenght..m.<130|is.na(Dam.lenght..m.)))

# REMOVE sand dams from China outliers
Data_norm <- All_case_studies.table
dams_length <- Data_norm$Dam.lenght..m.
throwback <- Data_norm$Throwback..m.
stream_width <- Data_norm$Average.river.width.upstream.of.the.dam..m.

median_dams_length <- median(dams_length, na.rm=T)
median_throwback <- median(throwback, na.rm=T)
median_stream_width <- median(stream_width, na.rm=T)

boxplot(dams_length,stream_width,
        main = "Sand dams characteristics",
        # at = c(1,2,4,5),
        names = c("Dams length", "stream width"),
        las = 2,
        col = c("brown","red"),
        border = "black",
        horizontal = TRUE,
        notch = TRUE
        # outline=FALSE
)

boxplot(throwback,
        col = "grey",
        border = "black",
        horizontal = TRUE,
        notch = TRUE
        # outline=FALSE
) 

dams_GIS<-SpatialPointsDataFrame(All_case_studies.table[,12:11],
                                 All_case_studies.table, proj4string = CRS(as.character(wgs)))
sf::st_write(st_as_sf(dams_GIS), ".", "Spatial_GSDD", driver="ESRI Shapefile")

# Global Administrative boundaries
setwd(Admin_boundaries)
Countries_admin0_name <- "Global_National_Admin_Boundaries.gpkg"
Global_admin0<-st_read(Countries_admin0_name)
plot(Global_admin0)
plot(dams_GIS, col="red")#, add=T)

# # Plot cumulata sand dams all'anno
dt1<-All_case_studies.table %>%
  group_by(Construction_Date) %>%
  summarize(n_SD_anno=n()) %>%
  mutate(cumulata=cumsum(n_SD_anno))

p <- ggplot(dt1, aes(x=Construction_Date, y=cumulata)) +
  # geom_line() +
  geom_line(color="steelblue") + 
  geom_point() +
  xlab("") +
  theme_bw()

  # theme(axis.text.x=element_text(angle=60, hjust=1)) 
p

# -------------------------------------------------------------------------------------------------
# adding the storage area
Data_norm$Storage_area <- Data_norm$Throwback..m.*Data_norm$Average.river.width.upstream.of.the.dam..m.
plot(Data_norm$Dam.lenght..m., Data_norm$Storage_area)

quartiles <- quantile(Data_norm$Throwback..m., probs=c(.25, .75), na.rm = T)
IQR <- IQR(Data_norm$Throwback..m., na.rm = T)

Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 

data_no_outlier <- subset(Data_norm, Data_norm$Throwback..m. > Lower & Data_norm$Throwback..m. < Upper)
plot(data_no_outlier$Dam.lenght..m., data_no_outlier$Storage_area)
my_data <- data_no_outlier


data_test<- subset(Data_norm, Data_norm$Conductivity..µS.cm. != "")



# # CORRELATION ANALYSIS

library("ggpubr")
ggscatter(my_data, x = "Dam.lenght..m.", y = "Storage_area", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Sand dams' crest lenght (m)", ylab = "Sand storage area (m2)")


# Shapiro-Wilk normality test for mpg
shapiro.test(my_data$Dam.lenght..m.) # => p = 0.1229
# Shapiro-Wilk normality test for wt
shapiro.test(my_data$Storage_area) # => p = 0.09

res <- cor.test(my_data$Dam.lenght..m., my_data$Storage_area, 
                method = "pearson")
res

#fit linear regression model using data frame
# model.lm <- lm(Storage_area ~ Dam.lenght..m., data = data_no_outlier)

plot(model.lm, add=TRUE)
#interpolate y value based on x value of 13
y_new = approx(df$x, df$y, xout=13)

#view interpolated y value
y_new



# -------------------------------------------------------------------------------------------------
# Table dams per country and median characteristics
Data_norm_table <- Data_norm %>%
  group_by(Country) %>% 
  summarize(n_dams=n(),Median_length=mean(Dam.lenght..m.,na.rm=T), sd_length=sd(Dam.lenght..m.,na.rm=T),
            Median_stream=mean(Average.river.width.upstream.of.the.dam..m.,na.rm=T), sd_stream=sd(Average.river.width.upstream.of.the.dam..m.,na.rm=T),
            Median_throwback=mean(Throwback..m.,na.rm=T), sd_throwback=sd(Throwback..m.,na.rm=T))

# Write data to csv files:  
# decimal point = "." and value separators = comma (",")
setwd(Database_folder)
write.csv(Data_norm_table, file = "Country_statistics.csv")
# -------------------------------------------------------------------------------------------------
# Spatial data extraction
# -------------------------------------------------------------------------------------


# PASTE FROM WOCAT CODE

### extracting information for the case studies --------------------------------------------------------
# extracting values of SOCIO-ECONOMIC parameters for case studies --------------------------------------
setwd(Global_spatial_data_SD)

ras_data = list.files()
num_ras<-length(ras_data)

Ras_stack <- stack()
for (i in 1:num_ras){
  raster_Epar<-raster(ras_data[i])
  name_Epar <- names(raster_Epar)
  print(paste0(name_Epar," - ",res(raster_Epar)))
  
  projection(raster_Epar) <- CRS(crs_WGS84)
  
  cs_Epar<-extract(raster_Epar, case_studies)
  All_case_studies.table$name_Epar<-cs_Epar
  
  colnames(All_case_studies.table)[colnames(All_case_studies.table)=="name_Epar"] <- name_Epar # Rename column with Epar name
  
  Ras_stack <- stack(Ras_stack, raster_Epar)
  rm(raster_Epar)
}
setwd(WOCAT_data)
# save(Ras_stack,file="Ras_stack_4Nov.Rdata")
# load("Ras_stack_4Nov.Rdata")
# save(All_case_studies.table,file="All_case_studies.table_AllPar_4Nov.Rdata")
# load("All_case_studies.table_AllPar_4Nov.Rdata")










