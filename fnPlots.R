# All plot functions

plotWidth= 12
plotHeight = 7 # for single graph: 6 (= 2 times 3) + 1
plotHeightLong = 10 # for multiple graphs: 9 (= 3 times 3) + 1

#' Plot the trend in number of cases by totals over time
#' needs data from cases2.txt
plotTotalCasesOverTime <- function(dat, lastDateCases, lastMaxPositiveCases, logScale = TRUE) {
  
  if(logScale)
    logWarning = " (log scale!)"
  else
    logWarning = ""
  
  cols2pivot <- colnames(dat)
  # Not displaying negative tests anymore
  #cols2pivot[2:7] <- c("Negative", "Positive", "Deaths", "Probable Deaths", "Hospitalizations", "Released")
  cols2pivot[3:7] <- c("Positive", "Deaths", "Probable Deaths", "Hospitalizations", "Released")
  colnames(dat) <- cols2pivot
  cols2pivot <- cols2pivot[3:7] # we don't need "Date"
  
  dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "Total", values_to = "Tests") # don't drop NAs because Negative tests came back on 3/28
  dt$Date <- as.Date(sprintf("%d", dt$Date), "%y%m%d")
  
  p <- ggplot(dt, aes(x = Date, y = Tests, group = Total)) +
    # log scale and 33% daily as in FT graph
    {if(logScale) scale_y_log10()} +
    {if(logScale) annotation_logticks()} +
    geom_abline(intercept = 1, slope = 33) +
    geom_line(aes(color = Total), lwd = 1) +
    geom_point(aes(color = Total, shape = Total)) +
    theme_linedraw() +
    labs(title = "Evolution of Coronavirus testing in Maryland, USA (2020)",
         x = "Date",
         y = paste("Cumulative tests counts", logWarning),
         caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org/mdcovid19/ ; last update:", format(Sys.Date(), "%b %d, %Y"))) +
    # manually place the arrow and label highlighting the last data
    annotate("segment", x = as.Date(lastDateCases) - 3.5, y = lastMaxPositiveCases,
             xend = as.Date(lastDateCases) - 2, yend = lastMaxPositiveCases,
             size = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
    annotate("text", label = paste("Last number of\ncases:", format(lastMaxPositiveCases, scientific = FALSE, big.mark = ",")),
             x = as.Date(lastDateCases) - 6, y = lastMaxPositiveCases + 1000,
             size = 3, fontface = "italic")
  
  ggsave("figures/MD-COVID19-cases.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")
  
  return(p)
}

#' Plot the trends in number of DAILY cases over time + percent positive/negative over time
#' needs data from cases2.txt
plotDailyCasesOverTime <- function(dat, logScale = FALSE) {
  
  if(logScale)
    logWarning = " (log scale!)"
  else
    logWarning = ""
  
  # compute delta per day
  dat <- dat %>%
    mutate(deltaNegative = Negative - lag(Negative, default = 0))
  dat <- dat %>%
    mutate(deltaPositive = Positive - lag(Positive, default = 0))
  dat <- dat %>%
    mutate(deltaDeaths = Deaths - lag(Deaths, default = 0))
  dat <- dat %>%
    mutate(deltaPDeaths = PDeaths - lag(PDeaths, default = 0))
  dat <- dat %>%
    mutate(deltaHospitalizations = Hospitalizations - lag(Hospitalizations, default = 0))
  dat <- dat %>%
    mutate(deltaReleased = Released - lag(Released, default = 0))
  
  dat <- dat %>% mutate (pcNeg = deltaNegative / (deltaPositive + deltaNegative) * 100)
  dat <- dat %>% mutate (pcPos = deltaPositive / (deltaPositive + deltaNegative) * 100)
  
  lastDateCases <- as.Date(sprintf("%d", max(dat$Date, na.rm = TRUE)), "%y%m%d")
  lastPositivePC <- dat$pcPos[nrow(dat)]
  
  cols2pivot <- colnames(dat)
  cols2pivot[2:10] <- c("TotalNegative", "TotalPositive", "TotalDeaths", "TotalPDeaths", "TotalHospitalizations", "TotalReleased", "XCurrentlyHospitalized", "XAcuteCare", "XIntensiveCare")
  cols2pivot[11:15] <- c("Negative", "Positive", "Deaths", "Probable Deaths", "Hospitalizations", "Released")
  colnames(dat) <- cols2pivot
  cols2pivot <- cols2pivot[12:15] # we don't need "Date" - nor Negative
  
  dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "Delta", values_to = "DailyVariation") # don't drop NAs because Negative tests came back on 3/28
  dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")
  
  p <- ggplot(dt, aes(x = Date, y = DailyVariation, group = Delta)) +
    {if(logScale) scale_y_log10()} +
    {if(logScale) annotation_logticks()} +
    geom_line(aes(color = Delta), lwd = 1) +
    geom_point(aes(color = Delta, shape = Delta)) +
    theme_linedraw() +
    labs(title = "Daily variation of Coronavirus cases in Maryland, USA (2020)",
         x = "Date",
         y = paste("Daily variation (# of new cases)", logWarning)
         )
  
  #ggsave("figures/MD-COVID19-cases-daily.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")
  
  # Calculate and display the second graph
  
  cols2pivot <- colnames(dat)
  cols2pivot[17:18] <- c("% negative", "% positive")
  colnames(dat) <- cols2pivot
  cols2pivot <- cols2pivot[c(17:18)]
  
  dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "Tests", values_to = "Percent") # don't drop NAs because Negative tests came back on 3/28
  dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")
  
  q <- ggplot(dt, aes(x = Date, y = Percent, group = Tests)) +
    {if(logScale) scale_y_log10()} +
    {if(logScale) annotation_logticks()} +
    geom_area(aes(fill= Tests), position = 'stack') +
    scale_fill_manual(values = c("#999999", "#E69F00")) +
    theme_linedraw() +
    # Annotation for broadening tests
    annotate("segment", x = as.Date("200519", "%y%m%d"), y = 25,
             xend = as.Date("200519", "%y%m%d"), yend = 0,
             size = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
    annotate("text", label = "Testing broadening\nMay 19, 2020",
             x = as.Date("200519", "%y%m%d"), y = 33,
             size = 4, fontface = "italic") +
    # manually place the arrow and label highlighting the last data
    annotate("segment", x = as.Date(lastDateCases) - 3.5, y = 40,
             xend = as.Date(lastDateCases), yend = lastPositivePC + 2,
             size = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
    annotate("text", label = paste("Last percentage of\npositive tests:", sprintf("%.2f%%", lastPositivePC)),
             x = as.Date(lastDateCases) - 6.5, y = 50,
             size = 3, fontface = "italic") +
    labs(title = "Daily proportion of Coronavirus test results in Maryland, USA (2020)",
         x = "Date",
         y = paste("Daily percentage (% of total # of tests)", logWarning),
         caption = paste("Percentage of unique tests: MD Health Department doesn't provide repetitive tests\nExplanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
  
  #ggsave("figures/MD-COVID19-tests-daily.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")
  
  #multiplot(p, q, cols = 1)
  r <- ggarrange(p, q, heights = c(1, 1), 
                 ncol = 1, nrow = 2, align = "v")
  
  ggsave("figures/MD-COVID19-cases-daily.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")
  
  return(r)
}

#' Plot the trends in number of DAILY number of tests over time + percent positive/negative over time
#' needs data from cases2.txt
plotDailyTestsOverTime <- function(dat) {
 
  # compute delta per day
  dat <- dat %>% mutate(deltaNegative = Negative - lag(Negative, default = 0))
  dat <- dat %>% mutate(deltaPositive = Positive - lag(Positive, default = 0))
  dat <- dat %>% mutate(deltaTotal = deltaPositive + deltaNegative)
  dat <- dat %>% mutate(pcPos = deltaPositive / deltaTotal * 100)
  
  dt <- subset(dat, select = c("Date", "deltaTotal", "pcPos")) # we just need those
  colsName <- colnames(dt)
  colsName <- c("Date", "NewTests", "DailyPositive")
  colnames(dt) <- colsName
  
  dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")
  
  p <- ggplot(dt, aes(x = Date, y = NewTests)) +
    geom_line() +
    geom_smooth(mapping = aes(x = Date, y = NewTests)) +
    theme_linedraw() +
    # Annotation for broadening tests
    annotate("segment", x = as.Date("200519", "%y%m%d"), y = 1700,
             xend = as.Date("200519", "%y%m%d"), yend = 0,
             size = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
    annotate("text", label = "Testing broadening\nMay 19, 2020",
             x = as.Date("200519", "%y%m%d"), y = 2500,
             size = 4, fontface = "italic") +
    labs(title = "Daily number of Coronavirus tests reported in Maryland, USA (2020)",
         x = "Date",
         y = paste("Daily number of tests")
    )

  # Calculate and display the second graph
  
  q <- ggplot(dt, aes(x = Date, y = DailyPositive)) +
    geom_line() +
    geom_smooth(mapping = aes(x = Date, y = DailyPositive)) +
    theme_linedraw() +
    # Annotation for Stage 1
    annotate("segment", x = as.Date("200515", "%y%m%d"), y = 2,
             xend = as.Date("200515", "%y%m%d"), yend = 0,
             size = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
    annotate("text", label = "Stage 1",
             x = as.Date("200515", "%y%m%d"), y = 4,
             size = 4, fontface = "italic") +
    # Annotation for broadening tests
    annotate("segment", x = as.Date("200519", "%y%m%d"), y = 4,
             xend = as.Date("200519", "%y%m%d"), yend = 0,
             size = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
    annotate("text", label = "Testing broadening\nMay 19, 2020",
             x = as.Date("200519", "%y%m%d"), y = 7,
             size = 4, fontface = "italic") +
    # Annotation for Stage 1
    annotate("segment", x = as.Date("200605", "%y%m%d"), y = 2,
             xend = as.Date("200605", "%y%m%d"), yend = 0,
             size = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
    annotate("text", label = "Stage 2",
             x = as.Date("200605", "%y%m%d"), y = 4,
             size = 4, fontface = "italic") +
    labs(title = "Daily percentage of positive Coronavirus tests reported",
         x = "Date",
         y = paste("Daily % of positive tests"),
         caption = paste("Percentage of unique tests: MD Health Department doesn't provide repetitive tests\nExplanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y"))
    )

  #multiplot(p, q, cols = 1)
  r <- ggarrange(p, q, heights = c(1, 1), 
                 ncol = 1, nrow = 2, align = "v")
  
  ggsave("figures/MD-COVID19-tests-daily.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")
  
  return(r)
}

#' Plot the trend in currently hospitalized patients
#' needs data from cases2.txt as of April 21, 2020
plotCurrentlySickPatients <- function(dat, logScale = FALSE) {
  
  if(logScale)
    logWarning = " (log scale!)"
  else
    logWarning = ""
  
  # Keeping for historical reasons: this is how I calculated CSP up to April 20:
  # compute currently sick patients (CSP) = hospitalizations - released (each day)
  #dat$CSP <- dat$Hospitalizations - dat$Released
  
  dt <- subset(dat, select = c("Date", "CurrentlyHospitalized", "AcuteCare", "IntensiveCare")) # we just need those
  cols2pivot <- colnames(dt)
  cols2pivot <- c("Date", "Cur. hospit.", "Acute Care", "ICU")
  colnames(dt) <- cols2pivot
  cols2pivot <- cols2pivot[2:4] # we don't need "Date"
  dt$Date <- as.Date(sprintf("%d",dat$Date), "%y%m%d")
  
  dt <- pivot_longer(data = dt, cols = cols2pivot, names_to = "HospitalizationType", values_to = "Count")
  
  p <- ggplot(dt, aes(x = Date, y = Count, group = HospitalizationType)) +
    {if(logScale) scale_y_log10()} +
    {if(logScale) annotation_logticks()} +
    geom_line(aes(color = HospitalizationType), lwd = 1) +
    geom_point(aes(color = HospitalizationType, shape = HospitalizationType)) +
    theme_linedraw() +
    labs(title = "Daily number of patients in hospitalization due to Coronavirus in Maryland, USA (2020)",
         x = "Date",
         y = paste("# of patients hospitalized on each day", logWarning),
         caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org/mdcovid19/ ; last update:", format(Sys.Date(), "%b %d, %Y")))

  ggsave("figures/MD-COVID19-cases-CSP.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")
  
  return(p)
}

#' Plot the new dayly hospitalized patients
#' needs data from cases2.txt as of April 21, 2020
plotNewSickPatients <- function(dat) {
  
  dt <- subset(dat, select = c("Date", "CurrentlyHospitalized", "AcuteCare", "IntensiveCare")) # we just need those
  
  dt <- dt %>% mutate(dailyHospitalized = CurrentlyHospitalized - lag(CurrentlyHospitalized, default = 0))
  dt <- dt %>% mutate(dailyAcuteCare = AcuteCare - lag(AcuteCare, default = 0))
  dt <- dt %>% mutate(dailyIntensiveCare = IntensiveCare - lag(IntensiveCare, default = 0))

  cols2pivot <- colnames(dt)
  cols2pivot <- c("Date", "TotalHospit", "TotalAcuteCare", "TotalICU", "Total Hospit.", "Acute Care", "ICU")
  colnames(dt) <- cols2pivot
  cols2pivot <- cols2pivot[5:7] # we don't need "Date"
  dt$Date <- as.Date(sprintf("%d",dat$Date), "%y%m%d")
  
  dt <- pivot_longer(data = dt, cols = cols2pivot, names_to = "Type", values_to = "Daily")
  
  p <- ggplot(dt, aes(x = Date, y = Daily, group = Type)) +
    geom_line(aes(color = Type), lwd = 1) +
    geom_point(aes(color = Type, shape = Type)) +
    theme_linedraw() +
    labs(title = "Daily number of new patients in hospitalization due to Coronavirus in Maryland, USA (2020)",
         x = "Date",
         y = "# of new patients hospitalized on each day",
         caption = paste("Negative values: more patients were discharged than admitted on that day\nData from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org/mdcovid19/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
  
  ggsave("figures/MD-COVID19-cases-CSPDaily.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")
  
  return(p)
}

#' Plot the total number of tests made & the positive ratio over time
#' needs data from cases2.txt
plotPositiveTestPc <- function(dat, lastDateCases, lastMaxPositiveCases) {
  datTT <- dat[1:3]
  datTT$TotalTests <- datTT$Negative + datTT$Positive
  datTT$PositivePC <- datTT$Positive / datTT$TotalTests * 100
  
  lastMaxTotalCases = max(datTT$TotalTests, na.rm = TRUE)
  lastPositivePC = if (is.na(datTT$PositivePC[nrow(datTT)])) (datTT$PositivePC[nrow(datTT) - 1]) else (datTT$PositivePC[nrow(datTT)]) # needed because sometimes last line is NA
  
  datTT$Date <- as.Date(sprintf("%d", datTT$Date), "%y%m%d")
  
  p <- ggplot(datTT, aes(x = Date, y = TotalTests)) +
    geom_line(lwd = 1) +
    geom_point() +
    theme_linedraw() +
    labs(title = "Cumulative number of Coronavirus tests received in Maryland, USA (2020)",
         x = " ",
         y = "Cumulative number of tests") +
    # manually place the arrow and label highlighting the last data
    annotate("segment", x = as.Date(lastDateCases) - 3.5, y = lastMaxTotalCases,
             xend = as.Date(lastDateCases) - 2, yend = lastMaxTotalCases,
             size = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
    annotate("text", label = paste("Last total number\nof tests:", format(lastMaxTotalCases, scientific = FALSE, big.mark = ",")),
             x = as.Date(lastDateCases) - 6, y = lastMaxTotalCases + 1000,
             size = 3, fontface = "italic")
  
  q <- ggplot(datTT, aes(x = Date, y = PositivePC)) +
    geom_line(lwd = 1) +
    geom_point() +
    theme_linedraw() +
    labs(title = "Proportion of cumulative positive Coronavirus tests in Maryland, USA (2020)",
         x = "Date",
         y = "Percentage of cumulative positive tests (%)",
         caption = paste("Percentage of unique tests: MD Health Department doesn't provide repetitive tests\nExplanations at https://jepoirrier.org/mdcovid19/ ; data from https://coronavirus.maryland.gov/ ; last update:", format(Sys.Date(), "%b %d, %Y"))) +
    # manually place the arrow and label highlighting the last data
    annotate("segment", x = as.Date(lastDateCases) - 3.5, y = lastPositivePC,
             xend = as.Date(lastDateCases) - 2, yend = lastPositivePC,
             size = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
    annotate("text", label = paste("Last percentage of\npositive tests:", sprintf("%.2f%%", lastPositivePC)),
             x = as.Date(lastDateCases) - 6.5, y = lastPositivePC,
             size = 3, fontface = "italic")
  
  #multiplot(p, q, cols = 1)
  r <- ggarrange(p, q, heights = c(1, 1), 
                 ncol = 1, nrow = 2, align = "v")
  
  ggsave("figures/MD-COVID19-tests-total.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")
  
  return(r)
}

# Not used anymore (can be deleted later)
# Still useful if the API isn't being updated
plotDeathsOverTime <- function(dat) {
  dt <- subset(dat, select = c("Date", "Deaths"))
  dt <- dt %>% mutate(dailyDeaths = Deaths - lag(Deaths, default = 0))
  dt$Date <- as.Date(sprintf("%d", dt$Date), "%y%m%d")
  
  lastCumulativeDeathsN <- dt$Deaths[length(dt[,2])]
  lastCumulativeDeathsDate <- max(dt$Date, na.rm = TRUE)
  peakDeathsN <- max(dt$dailyDeaths, na.rm = TRUE)
  peakDeathsDate <- tail(dt$Date[dt$dailyDeaths == peakDeathsN], 1)
  
  p <- ggplot(dt, aes(x = Date, y = Deaths)) +
    geom_line(lwd = 1) +
    geom_point() +
    theme_linedraw() +
    labs(title = "Cumulative number of Coronavirus deaths reported in Maryland, USA (2020)",
         x = " ",
         y = "Cumulative number of deaths") +
    # manually place the arrow and label highlighting the last data
    annotate("segment", x = as.Date(lastCumulativeDeathsDate) - 3.5, y = lastCumulativeDeathsN,
             xend = as.Date(lastCumulativeDeathsDate) - 2, yend = lastCumulativeDeathsN,
             size = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
    annotate("text", label = paste("Last total number\nof deaths:", format(lastCumulativeDeathsN, scientific = FALSE, big.mark = ",")),
             x = as.Date(lastCumulativeDeathsDate) - 9, y = lastCumulativeDeathsN + 5,
             size = 3, fontface = "italic")
  
  q <- ggplot(dt, aes(x = Date, y = dailyDeaths)) +
    geom_line(lwd = 1) +
    geom_point() +
    geom_smooth() +
    theme_linedraw() +
    labs(title = "Daily new Coronavirus deaths reported in Maryland, USA (2020)",
         x = "Date",
         y = "Daily new deaths",
         caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org/mdcovid19/ ; last update:", format(Sys.Date(), "%b %d, %Y"))) +
    # manually place the arrow and label highlighting the last data
    annotate("segment", x = as.Date(peakDeathsDate) - 3.5, y = peakDeathsN,
             xend = as.Date(peakDeathsDate) - 2, yend = peakDeathsN,
             size = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
    annotate("text", label = paste("Maximum of daily # deaths:", sprintf("%.0f", peakDeathsN)),
             x = as.Date(peakDeathsDate) - 12, y = peakDeathsN,
             size = 3, fontface = "italic")
  
  #multiplot(p, q, cols = 1)
  r <- ggarrange(p, q, heights = c(1, 1), 
                 ncol = 1, nrow = 2, align = "v")
  
  ggsave("figures/MD-COVID19-deaths.png", plot = r, device = "png", width = plotWidth, height = plotHeightLong, units = "in")
  
  return(r)
}

#' Plot the trend in number of confirmed cases and deaths in Congregate Facility Settings
#' needs data from nursing.txt
plotNursingCasesOverTime <- function(dat, logScale = FALSE) {
  
  if(logScale)
    logWarning = " (log scale!)"
  else
    logWarning = ""
  
  cols2pivot <- colnames(dat)
  cols2pivot[2:5] <- c("Staff Cases", "Staff Deaths", "Resident Cases", "Resident Deaths")
  colnames(dat) <- cols2pivot
  cols2pivot <- cols2pivot[2:5] # we don't need "Date"
  
  dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "Total", values_to = "Number") # don't drop NAs because Negative tests came back on 3/28
  dt$Date <- as.Date(sprintf("%d", dt$Date), "%y%m%d")
  
  p <- ggplot(dt, aes(x = Date, y = Number, group = Total)) +
    # log scale and 33% daily as in FT graph
    {if(logScale) scale_y_log10()} +
    {if(logScale) annotation_logticks()} +
    geom_line(aes(color = Total), lwd = 1) +
    geom_point(aes(color = Total, shape = Total)) +
    theme_linedraw() +
    labs(title = "Evolution of Coronavirus cases and deaths in Congregate Facility Settings in Maryland, USA (2020)",
         x = "Date",
         y = paste("Cumulative numbers", logWarning),
         caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org/mdcovid19/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
  
  ggsave("figures/MD-COVID19-nursing.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")
  
  return(p)
}

#' Plot the latest cumulative % of case/deaths in Congregate Facility Settings vs. Total cases/deaths
#' needs data from cases2.txt (-> dat1) and nursing.txt (-> dat2)
plotPcNursingCases <- function(dat1, dat2) {
  
  dat2$NursingCases <- dat2$StaffCases + dat2$ResidentCases
  dat2$NursingDeaths <- dat2$StaffDeaths + dat2$ResidentDeaths
  
  datCasesWNursing <- merge(dat1, dat2, by = "Date")
  
  datCasesWNursing$PCNursingCases <- datCasesWNursing$NursingCases / datCasesWNursing$Positive * 100
  datCasesWNursing$PCNursingDeaths <- datCasesWNursing$NursingDeaths / datCasesWNursing$Deaths * 100
  
  datCasesWNursing <- tail(datCasesWNursing, n = 1)
  datCasesWNursing <- subset(datCasesWNursing, select = c("Date", "PCNursingCases", "PCNursingDeaths"))
  datCasesWNursing$Date <- as.Date(sprintf("%d",datCasesWNursing$Date), "%y%m%d")
  
  cols2pivot <- colnames(datCasesWNursing)
  cols2pivot <- c("Date", "Nursing cases", "Nursing deaths")
  colnames(datCasesWNursing) <- cols2pivot
  cols2pivot <- cols2pivot[2:3] # we don't need "Date"
  dt <- pivot_longer(data = datCasesWNursing, cols = cols2pivot, names_to = "Type", values_to = "Percentage")
  
  p <- ggplot(dt, aes(x = Type, y = Percentage)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    theme_linedraw() +
    geom_text(aes(label=format(round(Percentage,2), scientific = FALSE, big.mark = ",")), vjust=1.6, color="white", size=3.5) +
    labs(title = "Latest cumulative proportions in nursing home due to Coronavirus in Maryland, USA (2020)",
         x = " ",
         y = paste("Percentage in nursing home (compared to total in MD)"),
         caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org/mdcovid19/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
  
  ggsave("figures/MD-COVID19-nursing-pc.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")
  
}

#' Plot the trend in number of cases in counties over time
#' needs data from counties.txt
plotCountyCasesOverTime <- function(dat) {
  
  cols2pivot <- colnames(dat)
  cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"
  
  dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "County", values_to = "Tests", values_drop_na = TRUE)
  dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")
  
  p <- ggplot(dt, aes(x = Date, y = Tests, group = County)) +
    geom_line(aes(color = County), lwd = 1) +
    geom_point(aes(color = County, shape = County)) +
    theme_linedraw() +
    #gghighlight(County == "Charles") +
    labs(title = "Evolution of COVID-19 testing in Maryland counties, USA (2020)",
         x = "Date",
         y = "Cumulative cases",
         caption = paste("DnA = Data not Available\nData from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org/mdcovid19/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
  
  ggsave("figures/MD-COVID19-counties-cases.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")
  
  return(p)
}

#' Plot the trend in number of deaths in counties over time
#' needs data from counties-death.txt
plotCountyDeathsOverTime <- function(dat) {
  
  cols2pivot <- colnames(dat)
  cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"
  
  dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "County", values_to = "Deaths", values_drop_na = TRUE)
  dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")
  
  p <- ggplot(dt, aes(x = Date, y = Deaths, group = County)) +
    geom_line(aes(color = County), lwd = 1) +
    geom_point(aes(color = County, shape = County)) +
    theme_linedraw() +
    #gghighlight(County == "Charles") +
    labs(title = "Evolution of COVID-19-confirmed deaths in Maryland counties, USA (2020)",
         x = "Date",
         y = "Cumulative number of deaths",
         caption = paste("DnA = Data not Available\nData from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org/mdcovid19/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
  
  ggsave("figures/MD-COVID19-counties-deaths.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")
  
  return(p)
}

#' Plot the trends in number of cases by age group over time
#' needs data from byage3.txt
#' REPLACED by src/age-analysis.R
# plotAgeGroupsCases <- function(dat) {
# 
#   dat$Date <- as.Date(sprintf("%d",dat$Date), "%y%m%d")
#   
#   p <- ggplot(subset(dat, CountType == "PosTests"), aes(x = Date, y = Count, group = AgeGroup)) +
#     geom_line(aes(color = AgeGroup), lwd = 1) +
#     geom_point(aes(color = AgeGroup, shape = AgeGroup)) +
#     theme_linedraw() +
#     labs(title = "Evolution of Coronavirus cases by age group in Maryland, USA (2020)",
#          x = "Date",
#          y = "Cumulative cases",
#          caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org/mdcovid19/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
# 
#   ggsave("figures/MD-COVID19-age-cases.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")
#   
#   return(p)
# }

#' Plot the trends in number of deaths by age group over time
#' needs data from byage3.txt
plotAgeGroupsDeaths <- function(dat) {
  
  dat$Date <- as.Date(sprintf("%d",dat$Date), "%y%m%d")
  
  p <- ggplot(subset(dat, CountType == "Deaths"), aes(x = Date, y = Count, group = AgeGroup)) +
    geom_line(aes(color = AgeGroup), lwd = 1) +
    geom_point(aes(color = AgeGroup, shape = AgeGroup)) +
    theme_linedraw() +
    labs(title = "Evolution of Coronavirus deaths by age group in Maryland, USA (2020)",
         x = "Date",
         y = "Cumulative number of deaths",
         caption = paste("DnA = Data not Available\nData from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org/mdcovid19/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
  
  ggsave("figures/MD-COVID19-age-deaths.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")
  
  return(p)
}

#' Plot the latest sectional data # cases and deaths by age group
#' needs data from byage2.txt
#' REPLACED by src/age-analysis.R
# plotAgeGroupsSection <- function(dat) {
#   
#   dat$Date <- as.Date(sprintf("%d",dat$Date), "%y%m%d")
#   
#   p <- ggplot(dat, aes(x = AgeGroup, y = Count, fill = CountType)) +
#     geom_bar(stat = "identity", position = position_dodge()) +
#     scale_fill_manual(name = "Legend", labels = c("Confirmed deaths", "Probable deaths", "Cases"), values = c("#000000", "#999999", "steelblue")) +
#     theme_linedraw() +
#     #geom_text(aes(label=Count), vjust=1.6, color="white", size=3.5)+
#     labs(title = "Age distribution of all Coronavirus cases in Maryland, USA (2020)",
#          color = "Legend", 
#          x = "Age groups (years)",
#          y = "All cases",
#          caption = paste("DnA: Data not Available\nData from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org/mdcovid19/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
# 
#   ggsave("figures/MD-COVID19-age-grp.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")
#   
#   return(p)
# }

#' Plot the latest sectional data for # cases by gender
#' needs data from bygender.txt
plotGenderSection <- function(dat) {
  
  dat$Date <- as.Date(sprintf("%d",dat$Date), "%y%m%d")
  maxDate = max(dat$Date)
  
  dat$Gender[dat$Gender == "DFemale"] <- "Female death"
  dat$Gender[dat$Gender == "DMale"] <- "Male death"
  dat$Gender[dat$Gender == "PDFemale"] <- "Female probable death"
  dat$Gender[dat$Gender == "PDMale"] <- "Male probable death"
  
  p <- ggplot(dat[dat$Date == maxDate,], aes(x = Gender, y = Counts)) +
    geom_bar(stat="identity") +
    theme_linedraw() +
    geom_text(aes(label=format(Counts, scientific = FALSE, big.mark = ",")), vjust=1.6, color="white", size=3.5) +
    labs(title = "Gender distribution of all Coronavirus cases in Maryland, USA (2020)",
         x = "Gender",
         y = "Cumulative count",
         caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org/mdcovid19/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
  
  ggsave("figures/MD-COVID19-gender-grp.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")
  
  return(p)
}

#' Plot the trend in number of cases by race over time
#' needs data from byrace.txt -- NOT USED UNLESS API IS DOWN
plotRaceCasesOverTime <- function(dat) {
  
  limitCasesCol <- 7 # just cases, no deaths, no probable deaths --> TODO
  dat <- dat[1:limitCasesCol]
  
  cols2pivot <- colnames(dat)
  cols2pivot <- c("Date", "African American", "Asian", "White", "Hispanic", "Other", "N.A.")
  colnames(dat) <- cols2pivot
  cols2pivot <- cols2pivot[2:limitCasesCol] # we don't need "Date"
  
  #cols2pivot <- colnames(dat)
  dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "Races", values_to = "Tests", values_drop_na = TRUE)
  dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")
  
  p <- ggplot(dt, aes(x = Date, y = Tests, group = Races)) +
    geom_line(aes(color = Races), lwd = 1) +
    geom_point(aes(color = Races, shape = Races)) +
    theme_linedraw() +
    labs(title = "Evolution of Coronavirus cases by race in Maryland, USA (2020)",
         x = "Date",
         y = "Cumulative cases",
         caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org/mdcovid19/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
  
  ggsave("figures/MD-COVID19-race-cases.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")
  
  return(p)
} 

#' Plot the trend in number of deaths by race over time
#' needs data from byrace.txt -- NOT USED UNLESS API IS DOWN
plotRaceDeathsOverTime <- function(dat) {
  
  cols2keep <- c(1, 8:13)
  dat <- dat[, cols2keep]
  
  cols2pivot <- colnames(dat)
  cols2pivot <- c("Date", "African American", "Asian", "White", "Hispanic", "Other", "N.A.")
  colnames(dat) <- cols2pivot
  cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"
  
  #cols2pivot <- colnames(dat)
  dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "Races", values_to = "Deaths", values_drop_na = TRUE)
  dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")
  
  p <- ggplot(dt, aes(x = Date, y = Deaths, group = Races)) +
    geom_line(aes(color = Races), lwd = 1) +
    geom_point(aes(color = Races, shape = Races)) +
    theme_linedraw() +
    labs(title = "Evolution of Coronavirus deaths by race in Maryland, USA (2020)",
         x = "Date",
         y = "Cumulative number of deaths",
         caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org/mdcovid19/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
  
  ggsave("figures/MD-COVID19-race-deaths.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")
  
  return(p)
}

#' Plot the latest sectional data for # cases by race
#' needs data from byrace.txt -- NOT USED UNLESS API IS DOWN
plotRaceSection <- function(dat) {
  
  limitCasesCol <- 7
  dat <- dat[1:limitCasesCol] # TODO add deaths
  
  cols2pivot <- colnames(dat)
  cols2pivot <- c("Date", "African American", "Asian", "White", "Hispanic", "Other", "N.A.")
  colnames(dat) <- cols2pivot
  cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"
  
  dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "Races", values_to = "Tests", values_drop_na = TRUE)
  dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")
  dt <- dt[(nrow(dt)-5):(nrow(dt)),]
  dt$Races <- factor(dt$Races, levels = c("African American", "White", "Hispanic", "Asian", "Other", "N.A."))
  
  p <- ggplot(dt, aes(x = Races, y = Tests)) +
    geom_bar(stat="identity", fill="orange") +
    theme_linedraw() +
    geom_text(aes(label=Tests), vjust=1.6, color="black", size=3.5) +
    labs(title = "Race distribution of Coronavirus cases in Maryland, USA (2020)",
         x = "Races",
         y = "Total cases",
         caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org/mdcovid19/ ; last update:", format(Sys.Date(), "%b %d, %Y")))
  
  ggsave("figures/MD-COVID19-race-grp.png", plot = p, device = "png", width = plotWidth, height = plotHeight, units = "in")
  
  return(p)
}
