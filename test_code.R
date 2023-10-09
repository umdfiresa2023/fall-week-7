install.packages("terra")
install.packages("tidyverse")

library("terra")
library("tidyverse")
library("readxl")

Africa_1997_2023_Sep22 <- 
  read_excel("G:/Shared drives/2023 FIRE-SA/FALL OUTPUT/Mining/DATA/Africa_1997-2023_Sep22.xlsx")

table(Africa_1997_2023_Sep22$COUNTRY)

acl<-Africa_1997_2023_Sep22 %>%
  filter(COUNTRY=="Democratic Republic of Congo")

names(acl)

acl_points<-vect(acl, 
               geom=c("LONGITUDE", "LATITUDE"), #identify the X and Y columns
               crs="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")

plot(acl_points)

plot(acl_points, "EVENT_TYPE", col=rainbow(6), cex=0.1)
plot(permits, add=TRUE)

permits<-vect("G:/Shared drives/2023 FIRE-SA/FALL OUTPUT/Mining/DATA/Democratic_Republic_of_the_Congo_mining_permits/Democratic_Republic_of_the_Congo_mining_permits.shp")
pdf_do<-as.Date(permits$date_do, "%Y/%m/%d")
pdf_de<-as.Date(permits$date_de1, "%Y/%m/%d")
plot(permits)

plot(acl_points, "EVENT_TYPE", col=rainbow(6))
plot(permits, add=TRUE)

#get permit centroid
pc<-centroids(permits)
plot(pc, cex=0.1)

pcb<-buffer(pc, width=10000, capstyle="round")
plot(acl_points, "EVENT_TYPE", col=rainbow(6), cex=0.1, reset=TRUE)
plot(pcb, add=TRUE)

install.packages("remotes")
library("remotes")
remotes::install_github("rstudio/leaflet")
plet(pcb) %>%
points(acl_points)

plot(pcb[7170])
points(acl_points)

#intersect permit buffers with violence
i<-terra::relate(pcb[7170], acl_points, relation="intersects")
dim(i)
sum(i)
vi<-as.vector(i)
pcb7170<-acl[vi,]

pcb7170_2<-pcb7170 %>%
  mutate(date=as.Date(EVENT_DATE, "%Y-%m-%d")) %>%
  mutate(p_active=ifelse(date>pdf_do[7170] & date<pdf_de[7170],1,0))
