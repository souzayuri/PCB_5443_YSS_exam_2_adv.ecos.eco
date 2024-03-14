# packages ----------------------------------------------------------------

rm(list=ls())

if(!require(tidyverse)) install.packages("tidyverse", dependencies = TRUE)
if(!require(quantmod)) install.packages("quantmod", dependencies = TRUE)
if(!require(xts)) install.packages("xts", dependencies = TRUE)
if(!require(emmeans)) install.packages("emmeans", dependencies = TRUE)
if(!require(ggthemes)) install.packages("ggthemes", dependencies = TRUE)
if(!require(timetk)) remotes::install_github("business-science/timetk")
if(!require(ggpubr)) install.packages("ggpubr", dependencies = TRUE)

#################### Abundance ###################  -------------------------------------------------------------


# Data --------------------------------------------------------------------

data_biota <- readr::read_csv("00_data/life_form_2020_for_abudance_recruitment_mortality.csv") |> 
  dplyr::select(c(1:3,5:23)) |>
  tidyr::pivot_longer(cols = 4:22, names_to = "Date", values_to = "abnd") |> 
  dplyr::mutate(Date = recode(Date, 
                                  "T0" = "2009-06-01", "T006" = "2010-01-01", 
                                  "T012" = "2010-06-01", "T018" = "2011-01-01",
                                  "T024" = "2011-06-01", "T030" = "2012-01-01",
                                  "T036" = "2012-06-01", "T042" = "2013-01-01",
                                  "T048" = "2013-06-01", "T054" = "2014-01-01",
                                  "T060" = "2014-06-01", "T066" = "2015-01-01", 
                                  "T072" = "2015-06-01", "T078" = "2016-01-01",
                                  "T084" = "2016-06-01", "T090" = "2017-01-01",
                                  "T096" = "2017-06-01", "T102" = "2018-01-01",
                                  "T108" = "2018-06-01")) |> 
  dplyr::group_by(Treatment, Date) |> 
  dplyr::summarise(abnd = sum(abnd)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(Date = as.POSIXct(paste(Date, "-01", "%Y-%m-%d", tz = "UTC")))


# Open --------------------------------------------------------------------


all_open <- data_biota |> 
  dplyr::filter(Treatment == "open")
all_open

all_open_ts <- all_open |> 
  timetk::tk_xts()
all_open_ts

str(all_open_ts)
tzone(all_open_ts)
nmonths(all_open_ts)
nyears(all_open_ts)

chartSeries(all_open_ts,theme="white")

chartSeries(all_open_ts,up.col='blue',dn.col='red',
            subset='2010::2017-06',theme="white")

# didn't change from 1 to 10
all_open_pks.1 <- findPeaks(all_open_ts, thresh=1)
plot(all_open_ts[all_open_pks.1])


all_open_peaks1 <- as.data.frame(all_open_ts[all_open_pks.1]) |> 
  rownames_to_column("Dates") |> 
  dplyr::mutate(Treatment = "Open")
all_open_peaks1

#how many peaks per year?
peak_per_y_open <- length(all_open_peaks1$abnd)/nyears(all_open_ts)
peak_per_y_open

#average peak magnitude (in number of individuals per year)
all_peak_mean_open <- mean(all_open_peaks1$abnd)
all_peak_mean_open

#peak standard deviation
all_peak_sd_open <- sd(all_open_peaks1$abnd)
all_peak_sd_open

#peak CV
all_peak_CV_open <- all_peak_sd_open/all_peak_mean_open
all_peak_CV_open

all_open_summary <- cbind("Peaks per year:",peak_per_y_open, 
                 "average peak magnitude:", all_peak_mean_open, 
                 "peak standard deviation:", all_peak_sd_open,
                 "peak CV:", all_peak_CV_open)
all_open_summary


all_open_peaks1$year<-as.numeric(format(as.POSIXct(all_open_peaks1$Dates),"%Y"))
all_open_peaks1

all_year_open <- min(as.numeric(format(as.POSIXct(all_open$Date),"%Y"))):max(as.numeric(format(as.POSIXct(all_open$Date),"%Y")))
all_year_open
all_years_open <- as.data.frame(all_year_open)
all_years_open

all_peak.sum_open <- all_open_peaks1 %>% group_by(year) %>% summarise(mean.peak=mean(abnd), count=n())
all_peak.sum_open
all_peak.number_open <- merge(all_years_open, all_peak.sum_open, by.x="all_year_open", by.y="year", all.x=TRUE)
all_peak.number_open
all_peak.number_open[is.na(all_peak.number_open)] <- 0 
all_peak.number_open

all_peak.number.lm_open <- lm(scale(count)~scale(all_year_open),data=all_peak.number_open)
all_lmsum.number_open <- summary(all_peak.number.lm_open)
all_lmsum.number_open
all_peak.number.slope_open <- all_peak.number.lm_open$coefficients[2]
all_peak.number.slope_open
all_peak.number.p_open <- all_lmsum.number_open$coefficients[2,4]
all_peak.number.p_open

all_peak.magnitude.lm_open <- lm(scale(abnd)~ scale(year), data = all_open_peaks1)
all_lmsum.mag_open <- summary(all_peak.magnitude.lm_open)
all_lmsum.mag_open
all_peak.magnitude.slope_open <- all_peak.magnitude.lm_open$coefficients[2]
all_peak.magnitude.slope_open
all_peak.magnitude.p_open <- all_lmsum.mag_open$coefficients[2,4]
all_peak.magnitude.p_open



# Closed ------------------------------------------------------------------


all_closed <- data_biota |> 
  dplyr::filter(Treatment == "closed")
all_closed

all_closed_ts <- all_closed |> 
  timetk::tk_xts()
all_closed_ts

str(all_closed_ts)
tzone(all_closed_ts)
nmonths(all_closed_ts)
nyears(all_closed_ts)

chartSeries(all_closed_ts,theme="white")

chartSeries(all_closed_ts,up.col='blue',dn.col='red',
            subset='2010::2017-06',theme="white")

# didnt change from 1 to 45
all_closed_pks.1 <- findPeaks(all_closed_ts, thresh=1)
plot(all_closed_ts[all_closed_pks.1])


all_closed_peaks1 <- as.data.frame(all_closed_ts[all_closed_pks.1]) |> 
  rownames_to_column("Dates") |> 
  dplyr::mutate(Treatment = "Closed")
all_closed_peaks1

#how many peaks per year?
peak_per_y_closed <- length(all_closed_peaks1$abnd)/nyears(all_closed_ts)
peak_per_y_closed

#average peak magnitude (in number of individuals per year)
all_peak_mean_closed <- mean(all_closed_peaks1$abnd)
all_peak_mean_closed

#peak standard deviation
all_peak_sd_closed <- sd(all_closed_peaks1$abnd)
all_peak_sd_closed

#peak CV
all_peak_CV_closed <- all_peak_sd_closed/all_peak_mean_closed
all_peak_CV_closed

all_closed_summary <- cbind("Peaks per year:",peak_per_y_closed, 
                          "average peak magnitude:", all_peak_mean_closed, 
                          "peak standard deviation:", all_peak_sd_closed,
                          "peak CV:", all_peak_CV_closed)
all_closed_summary


all_closed_peaks1$year<-as.numeric(format(as.POSIXct(all_closed_peaks1$Dates),"%Y"))
all_closed_peaks1

all_year_closed <- min(as.numeric(format(as.POSIXct(all_closed$Date),"%Y"))):max(as.numeric(format(as.POSIXct(all_closed$Date),"%Y")))
all_year_closed
all_years_closed <- as.data.frame(all_year_closed)
all_years_closed

all_peak.sum_closed <- all_closed_peaks1 %>% group_by(year) %>% summarise(mean.peak=mean(abnd), count=n())
all_peak.sum_closed
all_peak.number_closed <- merge(all_years_closed, all_peak.sum_closed, by.x="all_year_closed", by.y="year", all.x=TRUE)
all_peak.number_closed
all_peak.number_closed[is.na(all_peak.number_closed)] <- 0 
all_peak.number_closed

all_peak.number.lm_closed <- lm(scale(count)~scale(all_year_closed),data=all_peak.number_closed)
all_lmsum.number_closed <- summary(all_peak.number.lm_closed)
all_lmsum.number_closed
all_peak.number.slope_closed <- all_peak.number.lm_closed$coefficients[2]
all_peak.number.slope_closed
all_peak.number.p_closed <- all_lmsum.number_closed$coefficients[2,4]
all_peak.number.p_closed

all_peak.magnitude.lm_closed <- lm(scale(abnd)~ scale(year), data = all_closed_peaks1)
all_lmsum.mag_closed <- summary(all_peak.magnitude.lm_closed)
all_lmsum.mag_closed
all_peak.magnitude.slope_closed <- all_peak.magnitude.lm_closed$coefficients[2]
all_peak.magnitude.slope_closed
all_peak.magnitude.p_closed <- all_lmsum.mag_closed$coefficients[2,4]
all_peak.magnitude.p_closed



# Graph -------------------------------------------------------------------

dev.off()

all_biota <- data_biota |> 
  dplyr::mutate(Treatment = stringr::str_to_title(Treatment),
                Treatment = fct_relevel(Treatment, c("Open", "Closed"))) 
all_biota

all_biota_graph <- ggplot(all_biota, aes(x = Date, 
                          y = abnd, 
                          linetype = Treatment, 
                          color = Treatment,
                          fill = Treatment)) +
  geom_line(size = 1.5) +
  scale_color_manual(values=c("purple", "grey20")) +
  geom_point(data = all_open_peaks1, aes(x = as.POSIXct(Dates), y = abnd), 
             colour='black', fill = "grey60", size=6, shape = 21) +
  geom_point(data = all_closed_peaks1, aes(x = as.POSIXct(Dates), y = abnd), 
             colour='black', fill = "grey30", size=6, shape = 24) +
  labs(x = "Year", y = "Abundance") +
  theme_bw() +
  theme(axis.text.x= element_text(angle= 0, size = 14, vjust = 0.2),
        axis.text.y= element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12)) 
all_biota_graph



# Open 
all_peak.number.graph_open <- ggplot(all_peak.number_open, aes(x = all_year_open, y = count)) + 
  geom_point(color='darkslategrey',size=4) +
  geom_smooth(method = "lm",  se = TRUE, color="darkslategrey") +
  labs(x = "Year", y = "Number of peaks", title = "Open") +
  theme_bw() +
  theme(axis.text.x= element_text(angle= 0, size = 14, vjust = 0.2),
        axis.text.y= element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12)) 
all_peak.number.graph_open


all_peak.mag.graph_open <- ggplot(data = all_open_peaks1, aes(x = year, y = abnd)) + 
  geom_point(color = 'darkslategrey', size=4) +
  geom_smooth(method = "lm",  se = TRUE, color="darkslategrey")+
  labs(x = "Year", y = "Number of Individuals", title = "Average peak magnitude (Open)") +
  theme_bw() +
  theme(axis.text.x= element_text(angle= 0, size = 14, vjust = 0.2),
        axis.text.y= element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 16),
        strip.text = element_text(size = 12)) 
all_peak.mag.graph_open



# Closed

all_peak.number.graph_closed <- ggplot(all_peak.number_closed, aes(x = all_year_closed, y = count)) + 
  geom_point(color='darkslategrey',size=4) +
  geom_smooth(method = "lm",  se = TRUE, color="darkslategrey") +
  labs(x = "Year", y = "Number of peaks", title = "Closed") +
  theme_bw() +
  theme(axis.text.x= element_text(angle= 0, size = 14, vjust = 0.2),
        axis.text.y= element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12)) 
all_peak.number.graph_closed


all_peak.mag.graph_closed <- ggplot(data = all_closed_peaks1, aes(x = year, y = abnd)) + 
  geom_point(color = 'darkslategrey', size=4) +
  geom_smooth(method = "lm",  se = TRUE, color="darkslategrey")+
  labs(x = "Year", y = "Number of Individuals", title = "Average peak magnitude (Closed)") +
  theme_bw() +
  theme(axis.text.x= element_text(angle= 0, size = 14, vjust = 0.2),
        axis.text.y= element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12)) 
all_peak.mag.graph_closed

all_graphs <- ggpubr::ggarrange(all_biota_graph,
                                ggarrange(
                                all_peak.number.graph_open, 
                                all_peak.mag.graph_open, 
                                all_peak.number.graph_closed,
                                all_peak.mag.graph_closed, 
                                ncol = 2, 
                                labels = c("B", "C", "D", "E")), 
                                nrow = 3, 
                                labels = "A")
all_graphs


all_biota_peaks <- ggarrange(all_peak.number.graph_open,
                        all_peak.mag.graph_open, 
                        all_peak.number.graph_closed,
                        all_peak.mag.graph_closed, ncol=2, nrow=2, labels = c("B", "C","D","E"))

ggarrange(all_biota_graph, all_biota_peaks, ncol=1, nrow=2, labels = "A")

ggsave("02_figures/20240313_YSS_biota_abnd.png", h = 35, w = 30, units = "cm", dpi = 500)


#################### Recruitment ###################  -------------------------------------------------------------
# Data --------------------------------------------------------------------

data_biota <- readr::read_csv("00_data/life_form_2020_for_abudance_recruitment_mortality.csv") |> 
  dplyr::select(c(1:3,25,26)) |> 
  dplyr::mutate(abund = 1) |> 
  dplyr::filter(!Trecruit == "120") |> 
  dplyr::filter(!Trecruit == "0") |> 
  dplyr::filter(!Tdeath == "120") |> 
  dplyr::filter(!Tdeath == "alive") |> 
  dplyr::mutate(Trecruit = recode(Trecruit, 
                                  "6" = "2010-01-01", "12" = "2010-06-01", 
                                  "18" = "2011-01-01", "24" = "2011-06-01",
                                  "30" = "2012-01-01", "36" = "2012-06-01",
                                  "42" = "2013-01-01", "48" = "2013-06-01",
                                  "54" = "2014-01-01", "60" = "2014-06-01",
                                  "66" = "2015-01-01", "72" = "2015-06-01", 
                                  "78" = "2016-01-01", "84" = "2016-06-01",
                                  "90" = "2017-01-01", "96" = "2017-06-01",
                                  "108" = "2018-01-01"),
                Tdeath = recode(Tdeath, 
                                "6" = "2010-01-01", "12" = "2010-06-01", 
                                "18" = "2011-01-01", "24" = "2011-06-01",
                                "30" = "2012-01-01", "36" = "2012-06-01",
                                "42" = "2013-01-01", "48" = "2013-06-01",
                                "54" = "2014-01-01", "60" = "2014-06-01",
                                "66" = "2015-01-01", "72" = "2015-06-01", 
                                "78" = "2016-01-01", "84" = "2016-06-01",
                                "90" = "2017-01-01", "96" = "2017-06-01",
                                "108" = "2018-01-01")) |> 
  dplyr::mutate(Tdeath_date = as.POSIXct(paste(Tdeath, "-01", "%Y-%m-%d", tz = "UTC")),
                Trecruit_date = as.POSIXct(paste(Trecruit, "-01", "%Y-%m-%d", tz = "UTC"))) |> 
  dplyr::select(!c(4,5))


# Open --------------------------------------------------------------------


recruit_open <- data_biota |> 
  dplyr::filter(Treatment == "open") |> 
  dplyr::group_by(Treatment, Trecruit_date) |> 
  dplyr::summarise(n = sum(abund)) |> 
  dplyr::ungroup() 
recruit_open

recruit_open_ts <- recruit_open |> 
  timetk::tk_xts()
recruit_open_ts

str(recruit_open_ts)
tzone(recruit_open_ts)
nmonths(recruit_open_ts)
nyears(recruit_open_ts)

chartSeries(recruit_open_ts,theme="white")

chartSeries(recruit_open_ts,up.col='blue',dn.col='red',
            subset='2010::2017-06',theme="white")


recruit_open_pks.5 <- findPeaks(recruit_open_ts, thresh=5)
plot(recruit_open_ts[recruit_open_pks.5])


recruit_open_peaks5 <- as.data.frame(recruit_open_ts[recruit_open_pks.5]) |> 
  rownames_to_column("Dates") |> 
  dplyr::mutate(Treatment = "Open")


#how many peaks per year?
peak_per_y_open <- length(recruit_open_peaks5$n)/nyears(recruit_open_ts)
peak_per_y_open

#average peak magnitude (in number of individuals per year)
recruit_peak_mean_open <- mean(recruit_open_peaks5$n)
recruit_peak_mean_open

#peak standard deviation
recruit_peak_sd_open <- sd(recruit_open_peaks5$n)
recruit_peak_sd_open

#peak CV
recruit_peak_CV_open <- recruit_peak_sd_open/recruit_peak_mean_open
recruit_peak_CV_open

recruit_open_summary <- cbind("Peaks per year:",peak_per_y_open, 
                              "average peak magnitude:", recruit_peak_mean_open, 
                              "peak standard deviation:", recruit_peak_sd_open,
                              "peak CV:", recruit_peak_CV_open)
recruit_open_summary


recruit_open_peaks5$year<-as.numeric(format(as.POSIXct(recruit_open_peaks5$Dates),"%Y"))
recruit_open_peaks5

recruit_year_open <- min(as.numeric(format(as.POSIXct(recruit_open$Trecruit_date),"%Y"))):max(as.numeric(format(as.POSIXct(recruit_open$Trecruit_date),"%Y")))
recruit_year_open
recruit_years_open <- as.data.frame(recruit_year_open)
recruit_years_open

recruit_peak.sum_open <- recruit_open_peaks5 %>% group_by(year) %>% summarise(mean.peak=mean(n), count=n())
recruit_peak.sum_open
recruit_peak.number_open <- merge(recruit_years_open, recruit_peak.sum_open, by.x="year", by.y="year", all.x=TRUE)
recruit_peak.number_open
recruit_peak.number_open[is.na(recruit_peak.number_open)] <- 0 
recruit_peak.number_open

recruit_peak.number.lm_open <- lm(scale(count)~scale(year),data=recruit_peak.number_open)
recruit_lmsum.number_open <- summary(recruit_peak.number.lm_open)
recruit_lmsum.number_open
recruit_peak.number.slope_open <- recruit_peak.number.lm_open$coefficients[2]
recruit_peak.number.slope_open
recruit_peak.number.p_open <- recruit_lmsum.number_open$coefficients[2,4]
recruit_peak.number.p_open

recruit_peak.magnitude.lm_open <- lm(scale(n)~ scale(year), data = recruit_open_peaks5)
recruit_lmsum.mag_open <- summary(recruit_peak.magnitude.lm_open)
recruit_lmsum.mag_open
recruit_peak.magnitude.slope_open <- recruit_peak.magnitude.lm_open$coefficients[2]
recruit_peak.magnitude.slope_open
recruit_peak.magnitude.p_open <- recruit_lmsum.mag_open$coefficients[2,4]
recruit_peak.magnitude.p_open



# Closed ------------------------------------------------------------------



recruit_closed <- data_biota |> 
  dplyr::filter(Treatment == "closed") |> 
  dplyr::group_by(Treatment, Trecruit_date) |> 
  dplyr::summarise(n = sum(abund)) |> 
  dplyr::ungroup() 
recruit_closed

recruit_closed_ts <- recruit_closed |> 
  timetk::tk_xts()
recruit_closed_ts

str(recruit_closed_ts)
tzone(recruit_closed_ts)
nmonths(recruit_closed_ts)
nyears(recruit_closed_ts)

chartSeries(recruit_closed_ts,theme="white")

chartSeries(recruit_closed_ts,up.col='blue',dn.col='red',
            subset='2010::2017-06',theme="white")

# from 1 to 7 it doens't change
recruit_closed_pks.5 <- findPeaks(recruit_closed_ts, thresh=5)
plot(recruit_closed_ts[recruit_closed_pks.5])


recruit_closed_peaks5 <- as.data.frame(recruit_closed_ts[recruit_closed_pks.5]) |> 
  rownames_to_column("Dates") |> 
  dplyr::mutate(Treatment = "Closed")

#how many peaks per year?
peak_per_y_closed <-length(recruit_closed_peaks5$n)/nyears(recruit_closed_ts)
peak_per_y_closed

#average peak magnitude (in number of individuals per year)
recruit_peak_mean_closed <- mean(recruit_closed_peaks5$n)
recruit_peak_mean_closed

#peak standard deviation
recruit_peak_sd_closed <- sd(recruit_closed_peaks5$n)
recruit_peak_sd_closed

#peak CV
recruit_peak_CV_closed <- recruit_peak_sd_closed/recruit_peak_mean_closed
recruit_peak_CV_closed

recruit_closed_summary <- cbind("Peaks per year:",peak_per_y, 
                                "average peak magnitude:", recruit_peak_mean_closed, 
                                "peak standard deviation:", recruit_peak_sd_closed,
                                "peak CV:", recruit_peak_CV_closed)
recruit_closed_summary


recruit_closed_peaks5$year<-as.numeric(format(as.POSIXct(recruit_closed_peaks5$Dates),"%Y"))
recruit_closed_peaks5

recruit_year_closed <- min(as.numeric(format(as.POSIXct(recruit_closed$Trecruit_date),"%Y"))):max(as.numeric(format(as.POSIXct(recruit_closed$Trecruit_date),"%Y")))
recruit_year_closed
recruit_years_closed <- as.data.frame(recruit_year_closed)
recruit_years_closed

recruit_peak.sum_closed <- recruit_closed_peaks5 %>% group_by(year) %>% summarise(mean.peak=mean(n), count=n())
recruit_peak.sum_closed
recruit_peak.number_closed <- merge(recruit_years_closed, recruit_peak.sum_closed, by.x="year", by.y="year", all.x=TRUE)
recruit_peak.number_closed
recruit_peak.number_closed[is.na(recruit_peak.number_closed)] <- 0 
recruit_peak.number_closed

recruit_peak.number.lm_closed <- lm(scale(count)~scale(year),data=recruit_peak.number_closed)
recruit_lmsum.number_closed <- summary(recruit_peak.number.lm_closed)
recruit_lmsum.number_closed
recruit_peak.number.slope_closed <- recruit_peak.number.lm_closed$coefficients[2]
recruit_peak.number.slope_closed
recruit_peak.number.p_closed <- recruit_lmsum.number_closed$coefficients[2,4]
recruit_peak.number.p_closed


recruit_peak.magnitude.lm_closed <- lm(scale(n)~ scale(year), data = recruit_closed_peaks5)
recruit_lmsum.mag_closed <- summary(recruit_peak.magnitude.lm_closed)
recruit_lmsum.mag_closed
recruit_peak.magnitude.slope_closed <- recruit_peak.magnitude.lm_closed$coefficients[2]
recruit_peak.magnitude.slope_closed
recruit_peak.magnitude.p_closed <- recruit_lmsum.mag_closed$coefficients[2,4]
recruit_peak.magnitude.p_closed





# Graph -------------------------------------------------------------------

dev.off()

recruit_biota <- data_biota |> 
  dplyr::group_by(Treatment, Trecruit_date) |> 
  dplyr::summarise(n = sum(abund)) |> 
  dplyr::ungroup() |> 
  dplyr::mutate(Treatment = stringr::str_to_title(Treatment),
                Treatment = fct_relevel(Treatment, c("Open", "Closed"))) 
recruit_biota

ggplot(recruit_biota, aes(x = Trecruit_date, 
                          y = n, 
                          linetype = Treatment, 
                          color = Treatment,
                          fill = Treatment)) +
  geom_line(size = 1.5) +
  scale_color_manual(values=c("purple", "grey20")) +
  geom_point(data=recruit_open_peaks5,aes(x = as.POSIXct(Dates), y = n), 
             colour='black', fill = "grey60", size=6, shape = 21) +
  geom_point(data=recruit_closed_peaks5,aes(x = as.POSIXct(Dates), y = n), 
             colour='black', fill = "grey30", size=6, shape = 24) +
  labs(x = "Dates", y = "Plant Recruitment") +
  theme_bw() +
  theme(axis.text.x= element_text(angle= 0, size = 14, vjust = 0.2),
        axis.text.y= element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12)) 


# Open 
recruit_peak.number.graph_open <- ggplot(recruit_peak.number_open, aes(x = year, y = count)) + 
  geom_point(color='darkslategrey',size=4) +
  geom_smooth(method = "lm",  se = TRUE, color="darkslategrey") +
  labs(x = "Dates", y = "Number of peaks", title = "Open") +
  theme_bw() +
  theme(axis.text.x= element_text(angle= 0, size = 14, vjust = 0.2),
        axis.text.y= element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12)) 
recruit_peak.number.graph_open


recruit_peak.mag.graph_open <- ggplot(data = recruit_open_peaks5, aes(x = year, y = n)) + 
  geom_point(color = 'darkslategrey', size=4) +
  geom_smooth(method = "lm",  se = TRUE, color="darkslategrey")+
  labs(x = "Year", y = "Average peak magnitude (# individuals)", title = "Open") +
  theme_bw() +
  theme(axis.text.x= element_text(angle= 0, size = 14, vjust = 0.2),
        axis.text.y= element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12)) 
recruit_peak.mag.graph_open

# Closed

recruit_peak.number.graph_closed <- ggplot(recruit_peak.number_closed, aes(x = year, y = count)) + 
  geom_point(color='darkslategrey',size=4) +
  geom_smooth(method = "lm",  se = TRUE, color="darkslategrey") +
  labs(x = "Dates", y = "Number of peaks", title = "Closed") +
  theme_bw() +
  theme(axis.text.x= element_text(angle= 0, size = 14, vjust = 0.2),
        axis.text.y= element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12)) 
recruit_peak.number.graph_closed


recruit_peak.mag.graph_closed <- ggplot(data = recruit_closed_peaks5, aes(x = year, y = n)) + 
  geom_point(color = 'darkslategrey', size=4) +
  geom_smooth(method = "lm",  se = TRUE, color="darkslategrey")+
  labs(x = "Year", y = "Average peak magnitude (# individuals)", title = "Closed") +
  theme_bw() +
  theme(axis.text.x= element_text(angle= 0, size = 14, vjust = 0.2),
        axis.text.y= element_text(size = 14),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12)) 
recruit_peak.mag.graph_closed

#################### Mortality ###################  -------------------------------------------------------------