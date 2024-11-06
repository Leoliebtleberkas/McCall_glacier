#load packages -----
library(foehnix)
library(readxl)
library(zoo)
library(ncdf4)

#specify UTC as time
Sys.setenv(TZ = "UTC")

#load data ------
excel_file <- "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/foehnix_input_finalversion.xlsx"
col_types <- c("date", rep("numeric", 62 - 1))
foehn_jjmc <- read_excel(excel_file, col_types = col_types)


#select time -----
foehn_jjmc$utc_time = as.POSIXct(foehn_jjmc$utc_time, format = "%Y-%m-%d %H:%M:%S", tz = 'UTC')


#only consider data after 17.08.2008 ------
target_datetime <- as.POSIXct("2008-08-17 00:00:00", format = "%Y-%m-%d %H:%M:%S") 

# Select rows after the target_datetime using logical indexing
foehn_jjmc08 <- foehn_jjmc[foehn_jjmc$utc_time > target_datetime, ]


#convert into zoo object ------
jjmc_zoo <- zoo(foehn_jjmc08[,-1], order.by = foehn_jjmc08$utc_time)

#set up the foehnix model for AHAB -----
#filter
filter1_ahab = list(`winddir_crest800` = c(90, 270), `winddir_ahab` = c(90, 270))
filter1_era5 = list(`winddir_crest800` = c(90, 270), `winddir_mccall800`= c(90, 270))
filter2_ahab = list(`winddir_crest800` = c(90, 270), `winddir_ahab` = c(80, 280))
#filter2_era5 = list(`winddir_crest800` = c(110, 250),`winddir_mccall800`= c(110, 250))

#model
mod_tdiffahab_filter1 = foehnix(`windspeed_ahab`~`Tdiff_ahab_crest800`+`RH_ahab`, 
                                data = jjmc_zoo, maxit = 200, filter = filter1_ahab)
mod_tdiffahab_filter2 = foehnix(`windspeed_ahab`~`Tdiff_ahab_crest800`+`RH_ahab`, 
                                data = jjmc_zoo, maxit = 200, filter = filter2_ahab)
mod_tdiffera5_filter1 = foehnix(`windspeed_ahab`~`Tdiff_ahab_crest800`+`RH_ahab`, 
                                data = jjmc_zoo, maxit = 300, filter = filter1_era5)
#mod_tdiffera5_filter2 = foehnix(`windspeed_mccall800`~`Tdiff_jjmc_crest800`, 
 #                               data = jjmc_zoo, maxit = 200, filter = filter2_era5)

#summary filter 1
summary(mod_tdiffahab_filter1)
summary(mod_tdiffahab_filter1)$separation
#plots
pdf(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/ahab_era5_filter/ahab_tdiffera5crest_90_270_hist.pdf", 
    width = 10, height = 5)
#png(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/prob_filter_combined/era5_mccall/mccall_tdiffjjmc_90_270_hist.png", 
#    width = 600, height = 350)
#dev.new()
plot(mod_tdiffahab_filter1, which = "hist")
title(xlab = expression(y: ff[AHAB]))#"y: wind speed [m/s]")
#title(sub = "AHAB, tdiffahab_era5, filter 90-270")
#title(sub = "ERA5, tdiffjjmc-era5crest800, filter 90-270")
dev.off()

pdf(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/ahab_era5_filter/ahab_tdiffera5crest_90_270_post.pdf", 
    width = 10, height = 5)
#png(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/prob_filter_combined/era5_mccall/mccall_tdiffjjmc_90_270_post.png", 
#width = 600, height = 350)
#dev.new()
plot(mod_tdiffahab_filter1, which = "posterior", breaks = seq(0, 1, by = 0.05))
#title(sub = "AHAB, tdiffahab_era5, filter 90-270")
#title(sub = "ERA5, tdiffjjmc-era5crest800, filter 90-270")
dev.off()

#Hovmöller diagram
pdf(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/ahab_era5_filter/ahab_tdiffera5crest_90_270_hovmoller.pdf", 
    width = 10, height = 5)
#png(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/prob_filter_combined/era5_mccall/mccall_tdiffjjmc_90_270_hovmoller.png", 
#    width = 700, height = 350)
image(mod_tdiffahab_filter1, deltad = 10L, deltat = 2*3600, contours = TRUE,
      contour.col = "white", lwd = 2, labcex = 1.5,
      col = colorspace::sequential_hcl(51, "Purple-Yellow", rev = TRUE),
      zlim = c(0, 0.5))
#title(main = "AHAB, filter [90, 270]", adj = 0.9)
#title(main = "ERA5, filter [90, 270]", adj = 0.9)
dev.off()


#summary filter 2
summary(mod_tdiffahab_filter2)
summary(mod_tdiffahab_filter2)$separation
#plots
pdf(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/ahab_era5_filter/ahab_tdiffera5crest_80_280_hist.pdf", 
    width = 10, height = 5)
#png(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/prob_filter_combined/era5_mccall/mccall_tdiffjjmc_110_250_hist.png", 
#    width = 600, height = 350)
#dev.new()
plot(mod_tdiffahab_filter2, which = "hist")
title(xlab = expression(y: ff[AHAB]))
#title(sub = "ERA5, tdiffjjmc-era5crest800, filter 110-250")
dev.off()

pdf(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/ahab_era5_filter/ahab_tdiffera5crest_80_280_post.pdf", 
    width = 10, height = 5)
#png(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/prob_filter_combined/era5_mccall/mccall_tdiffjjmc_110_250_post.png", 
 #   width = 600, height = 350)
#dev.new()
plot(mod_tdiffahab_filter2, which = "posterior", breaks = seq(0, 1, by = 0.05))
#title = "p_diff gaussian"
#title(sub = "AHAB, tdiffahab_era5, filter 80_280")
#title(sub = "ERA5, tdiffjjmc-era5crest800, filter 110-250")
dev.off()

#Hovmöller diagram
pdf(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/ahab_era5_filter/ahab_tdiffera5crest_80_280_hovmoller.png", 
    width = 10, height = 5)
#png(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/prob_filter_combined/era5_mccall/mccall_tdiffjjmc_110_250_hovmoller.png", 
 #   width = 700, height = 350)
image(mod_tdiffahab_filter2, deltad = 10L, deltat = 2*3600, contours = TRUE,
      contour.col = "white", lwd = 2, labcex = 1.5,
      col = colorspace::sequential_hcl(51, "Purple-Yellow", rev = TRUE),
      zlim = c(0, 0.5))
#title(main = "AHAB, filter [80, 280]", adj = 0.9)
#title(main = "ERA5, filter [110, 250]", adj = 0.9)
dev.off()

#summary filter era5
summary(mod_tdiffera5_filter1)
summary(mod_tdiffera5_filter1)$separation
#plots
pdf(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/era5_era5_filter/ahab_tdiffera5crest_90_270_hist.pdf", 
    width = 10, height = 5)
#png(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/prob_filter_combined/era5_mccall/mccall_tdiffjjmc_110_250_hist.png", 
#    width = 600, height = 350)
#dev.new()
plot(mod_tdiffera5_filter1, which = "hist")
title(xlab = expression(y: ff[AHAB]))
#title(sub = "ERA5, tdiffjjmc-era5crest800, filter 110-250")
dev.off()

pdf(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/era5_era5_filter/ahab_tdiffera5crest_90_270_post.pdf", 
    width = 10, height = 5)
#png(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/prob_filter_combined/era5_mccall/mccall_tdiffjjmc_110_250_post.png", 
#   width = 600, height = 350)
#dev.new()
plot(mod_tdiffera5_filter1, which = "posterior", breaks = seq(0, 1, by = 0.05))
#title = "p_diff gaussian"
#title(sub = "AHAB, tdiffahab_era5, filter 80_280")
#title(sub = "ERA5, tdiffjjmc-era5crest800, filter 110-250")
dev.off()

#Hovmöller diagram
pdf(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/era5_era5_filter/ahab_tdiffera5crest_90_270_hovmoller.pdf", 
    width = 10, height = 5)
#png(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/prob_filter_combined/era5_mccall/mccall_tdiffjjmc_110_250_hovmoller.png", 
#   width = 700, height = 350)
image(mod_tdiffera5_filter1, deltad = 10L, deltat = 2*3600, contours = TRUE,
      contour.col = "white", lwd = 2, labcex = 1.5,
      col = colorspace::sequential_hcl(51, "Purple-Yellow", rev = TRUE),
      zlim = c(0, 0.5))
#title(main = "AHAB, filter [80, 280]", adj = 0.9)
#title(main = "ERA5, filter [110, 250]", adj = 0.9)
dev.off()


#add probabilities to foehn_jjmc_08 -----
jjmc_zoo$prob_combined_filter1 <- pmax(fitted(mod_tdiffahab_filter1), fitted(mod_tdiffera5_filter1))
jjmc_zoo$prob_combined_filter2 <- pmax(fitted(mod_tdiffahab_filter2), fitted(mod_tdiffera5_filter2))

jjmc_zoo$prob_ahab_filter1 <- fitted(mod_tdiffahab_filter1)
#jjmc_zoo$prob_ahab_filter1 <- pmax(fitted(mod_tdiffahab_filter1), fitted(mod_tdiffera5_filter1))
#jjmc_zoo$prob_ahab_filter2 <- pmax(fitted(mod_tdiffahab_filter2), fitted(mod_tdiffera5_filter2))
jjmc_zoo$prob_ahab_filter2 <- fitted(mod_tdiffahab_filter2)
jjmc_zoo$prob_ahab_filterera5 <- fitted(mod_tdiffera5_filter1)



#set up the foehnix model for JJMC ------
#filter
filter1 = list(`3m_Winddir_jjmc` = c(110, 250), `winddir_crest800` = c(90, 270),
               `prob_ahab_filter1`= c(0.5, 1.01), `winddir_ahab`= c(90, 270)) 
filter2 = list(`3m_Winddir_jjmc` = c(110, 250),`winddir_crest800` = c(90, 270), 
               `prob_ahab_filter2`= c(0.5, 1.01), `winddir_ahab` = c(80, 280)) 
filterera5 = list(`3m_Winddir_jjmc` = c(110, 250),`winddir_crest800` = c(90, 270), 
               `prob_ahab_filterera5`= c(0.5, 1.01), `winddir_mccall800` = c(90, 270)) 

#model
mod_tdiffjjmc_filter1 = foehnix(`3m_Windspeed_jjmc`~ `Tdiff_jjmc_ahab`+`2m_RH_jjmc`, 
                                data = jjmc_zoo, maxit = 700, filter = filter1)
#mod_tdiffjjmc_filter1 = foehnix(`3m_Windspeed_jjmc`~ `Tdiff_jjmc_ahab`, 
 #                               data = jjmc_zoo, maxit = 700, filter = filter1)
mod_tdiffjjmc_filter2 = foehnix(`3m_Windspeed_jjmc`~`Tdiff_jjmc_ahab`+`2m_RH_jjmc`, 
                                data = jjmc_zoo, maxit = 600, filter = filter2)
#mod_tdiffjjmc_filter2 = foehnix(`3m_Windspeed_jjmc`~`Tdiff_jjmc_ahab`, 
 #                               data = jjmc_zoo, maxit = 600, filter = filter2)
mod_tdiffjjmc_filterera5 = foehnix(`3m_Windspeed_jjmc`~ `Tdiff_jjmc_ahab`+`2m_RH_jjmc`, 
                                data = jjmc_zoo, maxit = 700, filter = filterera5)

#summary model filter 1
summary(mod_tdiffjjmc_filter1)
summary(mod_tdiffjjmc_filter1)$separation
coef(mod_tdiffjjmc_filter1)
#plots filter 1
pdf(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/ahab_era5_filter/tdiff_ahab_jjmc_90_270_hist.pdf", 
    width = 10, height = 5)
#dev.new()
plot(mod_tdiffjjmc_filter1, which = "hist")
title(xlab = expression(y: ff[JJMC]))
dev.off()

pdf(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/ahab_era5_filter/tdiff_ahab_jjmc_90_270_post.pdf", 
    width = 10, height = 5)
#dev.new()
plot(mod_tdiffjjmc_filter1, which = "posterior", breaks = seq(0, 1, by = 0.05))
#title = "p_diff gaussian"
#title(sub = "JJMC wind, tdiff AHAB+RH, filter 90-270")
dev.off()

#Hovmöller diagram
pdf(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/ahab_era5_filter/tdiff_ahab_jjmc_90_270_hovmoller.pdf", 
    width = 10, height = 5)
image(mod_tdiffjjmc_filter1, deltad = 10L, deltat = 2*3600, contours = TRUE,
      contour.col = "white", lwd = 2, labcex = 1.5,
      col = colorspace::sequential_hcl(51, "Purple-Yellow", rev = TRUE),
      zlim = c(0, 0.5))
#title(main = "JJMC, filter [90, 270]", adj = 0.9)
dev.off()


#summary model filter 2
summary(mod_tdiffjjmc_filter2)
summary(mod_tdiffjjmc_filter2)$separation
coef(mod_tdiffjjmc_filter2)
#plots filter 2
pdf(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/ahab_era5_filter/tdiff_ahab_jjmc_80_280_hist.pdf", 
    width = 10, height = 5)
#dev.new()
plot(mod_tdiffjjmc_filter2, which = "hist")
title(xlab = expression(y: ff[JJMC]))
dev.off()

pdf(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/ahab_era5_filter/tdiff_ahab_jjmc_80_280_post.pdf", 
    width = 10, height = 5)
#dev.new()
plot(mod_tdiffjjmc_filter2, which = "posterior", breaks = seq(0, 1, by = 0.05))
#title = "p_diff gaussian"
#title(sub = "JJMC wind, tdiff ahab +RH, filter 80-280")
dev.off()

#Hovmöller diagram
pdf(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/ahab_era5_filter/tdiff_ahab_jjmc_80_280_hovmoller.pdf", 
    width = 10, height = 5)
image(mod_tdiffjjmc_filter2, deltad = 10L, deltat = 2*3600, contours = TRUE,
      contour.col = "white", lwd = 2, labcex = 1.5,
      col = colorspace::sequential_hcl(51, "Purple-Yellow", rev = TRUE),
      zlim = c(0, 0.5))
#title(main = "JJMC, filter [80, 280]", adj = 0.9)
dev.off()

#summary model filter era5
summary(mod_tdiffjjmc_filterera5)
summary(mod_tdiffjjmc_filterera5)$separation
coef(mod_tdiffjjmc_filterera5)
#plots filter 2
pdf(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/era5_era5_filter/tdiff_ahab_jjmc_90_270_hist.pdf", 
    width = 10, height = 5)
#dev.new()
plot(mod_tdiffjjmc_filterera5, which = "hist")
title(xlab = expression(y: ff[JJMC]))
dev.off()

pdf(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/era5_era5_filter/tdiff_ahab_jjmc_90_270_post.pdf", 
    width = 10, height = 5)
#dev.new()
plot(mod_tdiffjjmc_filterera5, which = "posterior", breaks = seq(0, 1, by = 0.05))
#title = "p_diff gaussian"
#title(sub = "JJMC wind, tdiff ahab +RH, filter 80-280")
dev.off()

#Hovmöller diagram
pdf(file = "C:/Users/leopo/Master/Thesis/Code/Rfoehnix/final_version/R_output/era5_era5_filter/tdiff_ahab_jjmc_90_270_hovmoller.pdf", 
    width = 10, height = 5)
image(mod_tdiffjjmc_filterera5, deltad = 10L, deltat = 2*3600, contours = TRUE,
      contour.col = "white", lwd = 2, labcex = 1.5,
      col = colorspace::sequential_hcl(51, "Purple-Yellow", rev = TRUE),
      zlim = c(0, 0.5))
#title(main = "JJMC, filter [80, 280]", adj = 0.9)
dev.off()
