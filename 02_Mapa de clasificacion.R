
##############################################################
#                 Clase 02: Introduccion a datos Raster en R #
#                     Año: 2022                              #
#                       Gorky Florez Castillo                #
#                  Parte 2: Datos raster                     #
##############################################################

#Librerias----------------------------------------------------
library(sf)
library(ggplot2)
library(tidyverse)
library(raster)
library(ggspatial)
library(cptcity)
library(leaflet)
library(leafem)
library(leaflet.extras)
library(grid)
library(RStoolbox)

Clas <- raster("Raster/Clas_2022_02_28_21_55_12.tif")
Chonta = st_read("SHP/Chonta.geojson")

ggplot()+
  geom_sf(data = Chonta)

leaflet(Chonta) %>% 
  addTiles() %>% 
  addPolygons()

plot(Clas)

Clas_d     <- crop(Clas, Chonta)
Clas_ds    <- Clas_d<- mask(Clas_d , Chonta)
plot(Clas_ds)

Clas.pa        <-  rasterToPoints(Clas_ds)
Clas.pa_a      <-  data.frame(Clas.pa)

cols <-c("#008B00", # Bosque
         "#008B00", # Suelo desnudo
         "#8B5A2B", # Tierra estéril
         "#9ACD32", # Bosque secundario
         "#FFFF00"  # Agricultura
         )

SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Peru          <- getData('GADM', country='Peru', level=0) %>% st_as_sf()

Per           <- getData('GADM', country='Peru', level=2) %>% st_as_sf()
MDD           <- subset(Per, NAME_1  == "Madre de Dios")


ggplot()+
  geom_sf(data = Tambopata)

leaflet(Tambopata) %>% 
  addTiles() %>% 
  addPolygons()

Dist_MDD = st_read("SHP/MDD.geojson")  %>% st_as_sf()
Dist_MD  <- st_transform(Dist_MDD ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Tambopata <- subset(Dist_MD, distrito_ == "Tambopata")

library(elevatr)
elev = get_elev_raster(Chonta, z=12)
Poligo_alt    <- crop(elev, Chonta)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Chonta)
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)
colores = c("#015B26", "#3F8433", 
            "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")


Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")
library(ggnewscale) 
ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6, 
              show.legend = T)+
  scale_fill_gradientn(colours = colores, 
                       name='Elevacion \n(msnm)') +
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  theme_bw()+
  theme( panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))


SurA= ggplot()+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.01)+
  geom_sf(data = Peru , fill="gray", color="black")+
  geom_sf(data = MDD, fill="black", color="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -60, y = 10, hjust = 0, vjust = 1, 
           label = "a) Sur America",size = 3, family="serif", color = 
             "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -80, y = -40, hjust = 0, vjust = 1, 
           label = "Pacific ocean",size = 3, family="serif", color = 
             "black",  fontface="italic", angle=90)+
  annotate(geom = "text", x = -55, y = -50, hjust = 0, vjust = 1, 
           label = "Atlantic ocean",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -70, y = -10, hjust = 0, vjust = 1, 
           label = "Peru",size = 3, family="serif", color = 
             "black",  fontface="italic")
SurA

MDD_GG=ggplot()+
  geom_sf(data = MDD, fill=NA, color="black")+
  geom_sf(data = SurAmeric, fill=NA, color="black", size=0.01)+
  geom_sf(data = Peru , fill=NA, color="black", size=0.01)+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  geom_sf(data = MDD, fill=NA, color="black")+
  geom_sf(data = Tambopata, fill="gray", color="black")+
  geom_sf(data = Chonta, fill="black", color="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  coord_sf(xlim = c(-72.40404, -68.65311), ylim = c(-13.7 ,-9.879849)) +
  annotate(geom = "text", x = -70.5, y = -10, hjust = 0, vjust = 1, 
           label = "b) Departamento de \nEstudio",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -71, y = -13.4, hjust = 0, vjust = 1, 
           label = "Madre de Dios",size = 4, family="serif", color = 
             "black",  fontface="italic")


Clasi_map= ggplot()+
  geom_raster(data = Clas.pa_a , aes(x,y,fill =Clas_2022_02_28_21_55_12)) + 
  scale_fill_gradientn(colours = cols, name="Leyenda",
                       labels = c("[Bosque] ","[Suelo desnudo]", 
                                  "[Tierra estéril]", "[Bosque secundario]", "[Agricultura]"),
                       breaks = c(1,2,3,4,5))+
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  theme_bw()+
  theme(
    legend.position = c(0.75,0.15),
    legend.key.size = unit(0.4, "cm"), 
    legend.key.width = unit(0.9,"cm"),
    panel.grid.major = element_blank(),
    legend.text =element_text(size=8, family="serif"),
    legend.title = element_text(size=8, family="serif"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face="bold", color="black"),
    panel.border = element_rect(size = 1, color="black"),
    axis.text.x  = element_text(face="bold", color="black", size=8),
    axis.text.y  = element_text(angle = 90,face="bold", color="black", size=8),
    strip.text=element_text(family='Anton', face='bold', size=14, hjust=0, color='white'),
    strip.background=element_rect(fill='black'))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(title = '', fill = '',  x = '', y = '') 

Clasi_map


colnames(Clas.pa_a) = c("x", "y", "Clas")

Clas.pa_a %>%
  subset(Clas <= 5 & Clas> 0) ->clase_des

clase_des

col = c("#FFFF00", #Agricultura 
        "#008B00", #Bosque 
        "#d4a373", #Suelo desnudo
        "#9ACD32",  #Bosque secundario
        "#8B5A2B" )#Tierra esteril


sum = clase_des %>%
  group_by(Clas) %>%
  summarise(count =n())%>%
  ungroup()

sum = mutate(sum, meters =count * 900, has= meters / 1000000)
sum = dplyr::select(sum, Clas, has)

sum = mutate(sum, Categoria =c("Tierra esteril",      #Tierra esteril
                               "Bosque secundario", #Bosque secundario
                               "Agricultura ", #Agricultura 
                               "Bosque ",  #Bosque 
                               "Suelo desnudo"))        #Suelo desnudo
summ <- sum %>%
  mutate(Categoria= fct_reorder(Categoria, has, .desc = TRUE))

Esta_2008= ggplot(data = summ, aes(x=Categoria, y=has, fill=Categoria))+
  scale_fill_manual(values = col)+
  geom_col()+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        legend.position = c(0.75, 0.75),
        legend.text =element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif"),
        axis.title.x = element_text(color="black", size=11,family="serif",face="bold"),
        axis.title.y = element_text(color="black", size=11,family="serif",face="bold"),
        axis.text.y  = element_text(color="black", size=10, family="serif",face="bold", angle=90),
        axis.text.x  = element_text(color="black", size=10, family="serif",face="bold", angle = 25, hjust=1),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.6,"cm"), #ancho de cuadrados de referencia 
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 11))+
  
  geom_text(aes(label=round(has,2), y = 100), position = position_dodge(0.90), size=4, angle=90)+
  labs(x= "",
       y= "Kilometros (km2)")
Esta_2008

library(ggplot2)
library(ggrepel)
library(tidyverse)
library(dplyr)
df2  = dplyr::select(sum, Categoria  , has)
df2$Porcentaje =  df2$has*100/3273.5

df <- data.frame(has = df2$Porcentaje,
                 Categoria = df2$Categoria)

df3 <- df %>% 
  mutate(csum = rev(cumsum(rev(has))), 
         pos = has/2 + lead(csum, 1),
         pos = if_else(is.na(pos), has/2, pos))

co = c("#8B5A2B", #Tierra esteril
        "#9ACD32", #Bosque secundario
        "#FFFF00", #Agricultura
        "#008B00",  #Bosque secundario
        "#d4a373" )#Suelo desnudo

G2Pastel= ggplot(df3, aes(x = "" , y = has, fill = fct_inorder(Categoria))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_manual(values = co)+
  geom_label_repel(data = df3,
                   aes(y = pos, label = paste0(round(has,2), "%")),
                   size = 3, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Categorias")) +
  theme_void()+
  theme(legend.text =element_text(size=8, family="serif"),
        legend.title = element_text(size=8, family="serif"))
G2Pastel

# Mapa final
library(cowplot)
Final=ggdraw() +
  coord_equal(xlim = c(0, 22.86), ylim = c(0, 17.78), expand = FALSE) +
  draw_plot(Clasi_map, width = 12, height = 12,x = -0.5, y = -0.5)+
  draw_plot(SurA, width = 7, height = 7,x = -1.3, y = 10.8)+
  draw_plot(MDD_GG, width = 7.5, height = 7,x = 4.1, y = 10.8)+
  draw_plot(G2Pastel, width = 7.5, height = 7,x = 12, y = 10.8)+
  draw_plot(Esta_2008, width = 10, height = 10,x = 12, y = -0.5)+
  theme(panel.background = element_rect(fill = "white"))

Final
ggsave(plot=Final,"Mapa/Mapa de clasifica1.png",units = "cm",width = 22.86, #alto
       height = 17.78, #ancho
       dpi=1200)





























































