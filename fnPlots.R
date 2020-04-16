# All plot functions

#' Plot the trend in number of cases by totals over time
#' needs data from cases2.txt
plotTotalCasesOverTime <- function(dat, lastDateCases, lastMaxPositiveCases, logScale = TRUE) {
  
  if(logScale)
    logWarning = " (log scale!)"
  else
    logWarning = ""
  
  cols2pivot <- colnames(dat)
  cols2pivot[2:7] <- c("Negative", "Positive", "Deaths", "Probable Deaths", "Hospitalizations", "Released")
  colnames(dat) <- cols2pivot
  cols2pivot <- cols2pivot[2:7] # we don't need "Date"
  
  dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "Total", values_to = "Tests") # don't drop NAs because Negative tests came back on 3/28
  dt$Date <- as.Date(sprintf("%d", dt$Date), "%y%m%d")
  
  p <- ggplot(dt, aes(x = Date, y = Tests, group = Total)) +
    # log scale and 33% daily as in FT graph
    {if(logScale) scale_y_log10()} +
    {if(logScale) annotation_logticks()} +
    geom_abline(intercept = 1, slope = 33) +
    geom_line(aes(color = Total)) +
    geom_point(aes(color = Total, shape = Total)) +
    labs(title = "Evolution of Coronavirus testing in Maryland, USA (2020)",
         x = "Date",
         y = paste("Tests counts", logWarning),
         caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y"))) +
    # manually place the arrow and label highlighting the last data
    annotate("segment", x = as.Date(lastDateCases) - 3.5, y = lastMaxPositiveCases,
             xend = as.Date(lastDateCases) - 2, yend = lastMaxPositiveCases,
             size = 0.5, arrow = arrow(length = unit(.2, "cm"))) +
    annotate("text", label = paste("Last number of\ncases:", format(lastMaxPositiveCases, scientific = FALSE, big.mark = ",")),
             x = as.Date(lastDateCases) - 6, y = lastMaxPositiveCases + 1000,
             size = 3, fontface = "italic")
  
  ggsave("figures/MD-coronavirus-cases.png", plot = p, device = "png", width = 3840/300, height = 2160/300, units = "in")
  
  return(p)
}

#' Plot the trends in number of DAILY cases over time
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
  
  cols2pivot <- colnames(dat)
  cols2pivot[2:7] <- c("TotalNegative", "TotalPositive", "TotalDeaths", "TotalPDeaths", "TotalHospitalizations", "TotalReleased")
  cols2pivot[8:12] <- c("Negative", "Positive", "Deaths", "Probable Deaths", "Hospitalizations", "Released")
  colnames(dat) <- cols2pivot
  cols2pivot <- cols2pivot[8:12] # we don't need "Date"
  
  dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "Delta", values_to = "DailyVariation") # don't drop NAs because Negative tests came back on 3/28
  dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")
  
  p <- ggplot(dt, aes(x = Date, y = DailyVariation, group = Delta)) +
    {if(logScale) scale_y_log10()} +
    {if(logScale) annotation_logticks()} +
    geom_line(aes(color = Delta)) +
    geom_point(aes(color = Delta, shape = Delta)) +
    labs(title = "Daily variation of Coronavirus cases in Maryland, USA (2020)",
         x = "Date",
         y = paste("Daily variation (# of cases)", logWarning),
         caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y")))
  
  ggsave("figures/MD-coronavirus-cases-daily.png", plot = p, device = "png", width = 3840/300, height = 2160/300, units = "in")
  
  return(p)
}

#' Plot the approximate trend in currently sick patients
#' This is calculated by removing "released" patients from "hospitalized" patients
#' (only an approximation because of lag, data not available since the beginning, etc.)
plotCurrentlySickPatients <- function(dat, logScale = FALSE) {
  
  if(logScale)
    logWarning = " (log scale!)"
  else
    logWarning = ""
  
  # compute currently sick patients (CSP) = hospitalizations - released (each day)
  dat$CSP <- dat$Hospitalizations - dat$Released
  dat$Date <- as.Date(sprintf("%d",dat$Date), "%y%m%d")
  
  dt <- subset(dat, select = c("Date", "CSP")) # we just need those 2
  
  p <- ggplot(dt, aes(x = Date, y = CSP)) +
    {if(logScale) scale_y_log10()} +
    {if(logScale) annotation_logticks()} +
    geom_line() +
    geom_point() +
    labs(title = "Daily *approximate* number of patients hospitalized due to Coronavirus in Maryland, USA (2020)",
         x = "Date",
         y = paste("Approx. # of patients hospitalized on each day", logWarning),
         caption = paste("Note: # patients currently hospitalized = # patients hospitalized - # patients released\nData from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y")))
  
  ggsave("figures/MD-coronavirus-cases-CSP.png", plot = p, device = "png", width = 3840/300, height = 2160/300, units = "in")
  
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
    geom_line() +
    geom_point() +
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
    geom_line() +
    geom_point() +
    labs(title = "Proportion of positive Coronavirus tests in Maryland, USA (2020)",
         x = "Date",
         y = "Percentage of positive tests (%)",
         caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y"))) +
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
  
  ggsave("figures/MD-coronavirus-total-tests.png", plot = r, device = "png", width = 3840/300, height = 2160/300, units = "in")
  
  return(r)
}

#' Plot the trend in number of cases in counties over time
#' needs data from counties.txt
plotCountyCasesOverTime <- function(dat) {
  
  cols2pivot <- colnames(dat)
  cols2pivot <- cols2pivot[2:length(cols2pivot)] # we don't need "Date"
  
  dt <- pivot_longer(data = dat, cols = cols2pivot, names_to = "County", values_to = "Tests", values_drop_na = TRUE)
  dt$Date <- as.Date(sprintf("%d",dt$Date), "%y%m%d")
  
  p <- ggplot(dt, aes(x = Date, y = Tests, group = County)) +
    geom_line(aes(color = County)) +
    geom_point(aes(color = County, shape = County)) +
    labs(title = "Evolution of COVID-19 testing in Maryland counties, USA (2020)",
         x = "Date",
         y = "Cases",
         caption = paste("DnA = Data not Available\nData from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y")))
  
  ggsave("figures/MD-coronavirus-counties.png", plot = p, device = "png", width = 3840/300, height = 2160/300, units = "in")
  
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
    geom_line(aes(color = County)) +
    geom_point(aes(color = County, shape = County)) +
    labs(title = "Evolution of COVID-19-confirmed deaths in Maryland counties, USA (2020)",
         x = "Date",
         y = "Number of deaths",
         caption = paste("DnA = Data not Available\nData from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y")))
  
  ggsave("figures/MD-coronavirus-counties-deaths.png", plot = p, device = "png", width = 3840/300, height = 2160/300, units = "in")
  
  return(p)
}

#' Plot the trends in number of cases by age group over time
#' needs data from byage3.txt
plotAgeGroupsCases <- function(dat) {

  dat$Date <- as.Date(sprintf("%d",dat$Date), "%y%m%d")
  
  p <- ggplot(subset(dat, CountType == "PosTests"), aes(x = Date, y = Count, group = AgeGroup)) +
    geom_line(aes(color = AgeGroup)) +
    geom_point(aes(color = AgeGroup, shape = AgeGroup)) +
    labs(title = "Evolution of Coronavirus cases by age group in Maryland, USA (2020)",
         x = "Date",
         y = "Cases",
         caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y")))

  ggsave("figures/MD-coronavirus-byage.png", plot = p, device = "png", width = 3840/300, height = 2160/300, units = "in")
  
  return(p)
}

#' Plot the trends in number of deaths by age group over time
#' needs data from byage3.txt
plotAgeGroupsDeaths <- function(dat) {
  
  dat$Date <- as.Date(sprintf("%d",dat$Date), "%y%m%d")
  
  p <- ggplot(subset(dat, CountType == "Deaths"), aes(x = Date, y = Count, group = AgeGroup)) +
    geom_line(aes(color = AgeGroup)) +
    geom_point(aes(color = AgeGroup, shape = AgeGroup)) +
    labs(title = "Evolution of Coronavirus deaths by age group in Maryland, USA (2020)",
         x = "Date",
         y = "Total number of deaths",
         caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y")))
  
  ggsave("figures/MD-coronavirus-byage-deaths.png", plot = p, device = "png", width = 3840/300, height = 2160/300, units = "in")
  
  return(p)
}

#' Plot the latest sectional data # cases and deaths by age group
#' needs data from byage2.txt
plotAgeGroupsSection <- function(dat) {
  
  dat$Date <- as.Date(sprintf("%d",dat$Date), "%y%m%d")
  
  p <- ggplot(dat, aes(x = AgeGroup, y = Count, fill = CountType)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_fill_manual(name = "Legend", labels = c("Confirmed deaths", "Probable deaths", "Cases"), values = c("#000000", "#999999", "steelblue")) +
    theme_minimal() +
    #geom_text(aes(label=Count), vjust=1.6, color="white", size=3.5)+
    labs(title = "Age distribution of all Coronavirus cases in Maryland, USA (2020)",
         color = "Legend", 
         x = "Age groups (years)",
         y = "Number",
         caption = paste("DnA: Data not Available\nData from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y")))

  ggsave("figures/MD-coronavirus-byage-grp.png", plot = p, device = "png", width = 3840/300, height = 2160/300, units = "in")
  
  return(p)
}

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
    theme_minimal() +
    geom_text(aes(label=format(Counts, scientific = FALSE, big.mark = ",")), vjust=1.6, color="white", size=3.5) +
    labs(title = "Gender distribution of all Coronavirus cases in Maryland, USA (2020)",
         x = "Gender",
         y = "Total count",
         caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y")))
  
  ggsave("figures/MD-coronavirus-bygender-grp.png", plot = p, device = "png", width = 3840/300, height = 2160/300, units = "in")
  
  return(p)
}

#' Plot the trend in number of cases by race over time
#' needs data from byrace.txt
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
    geom_line(aes(color = Races)) +
    geom_point(aes(color = Races, shape = Races)) +
    labs(title = "Evolution of Coronavirus cases by race in Maryland, USA (2020)",
         x = "Date",
         y = "Cases",
         caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y")))
  
  ggsave("figures/MD-coronavirus-byrace.png", plot = p, device = "png", width = 3840/300, height = 2160/300, units = "in")
  
  return(p)
}

#' Plot the latest sectional data for # cases by race
#' needs data from byrace.txt
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
    theme_minimal() +
    geom_text(aes(label=Tests), vjust=1.6, color="black", size=3.5) +
    labs(title = "Race distribution of Coronavirus cases in Maryland, USA (2020)",
         x = "Races",
         y = "Cases",
         caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y")))
  
  ggsave("figures/MD-coronavirus-byrace-grp.png", plot = p, device = "png", width = 3840/300, height = 2160/300, units = "in")
  
  return(p)
}

#' Plot the trend in number of cases by zip code over time
#' needs data from ZIP.txt
plotZipCasesOverTime <- function(dat, zip2highlight = 21215) {
  dat$Date <- as.Date(sprintf("%d", dat$Date), "%y%m%d")
  dat$ZipCode <- as.factor(as.character(dat$ZipCode))
  #dat <- subset(dat, max(dat$Cases) > 100)
  
  p <- ggplot(dat, aes(x = Date, y = Cases, group = ZipCode)) +
    geom_line(aes(color = ZipCode)) +
    gghighlight(ZipCode == zip2highlight) +
    theme(legend.position = "none") +
    labs(title = "Evolution of Coronavirus positive cases by zip codes in Maryland, USA (2020)",
         x = "Date",
         y = "Positive tests counts",
         caption = paste("Data from https://coronavirus.maryland.gov/ ; explanations at https://jepoirrier.org ; last update:", format(Sys.Date(), "%b %d, %Y")))
  p
  ggsave("figures/MD-coronavirus-byZip.png", plot = p, device = "png", width = 3840/300, height = 2160/300, units = "in")
  
  return(p)
}