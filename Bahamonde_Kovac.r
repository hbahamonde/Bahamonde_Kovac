cat("\014")
rm(list=ls())
graphics.off()


##############################
# Yearly weight data for weight matrix
##############################
# cat("\014")
# rm(list=ls())

# loads pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

p_load(foreign)
bilateral.d <- read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/Dyadic_COW_4.0.csv") 

# Generating another column with the absolute market trade distance between the two countries
options(scipen=1000000) 
bilateral.d$trade = as.numeric(round(abs(bilateral.d$flow1-bilateral.d$flow2), 4))

# keeping columns i'll need
bilateral.d <- bilateral.d[c("year", "importer1", "importer2", "trade")]


# Others adjustments PENDING

# 1. Rename "West Germany" to "Germany" only for 1946-1989 period.
# bilateral.d$importer1[bilateral.d$importer1 == "German Federal Republic"] <- "Germany"
# bilateral.d$importer2[bilateral.d$importer2 == "German Federal Republic"] <- "Germany"

trade = bilateral.d


# Loop to Generate Independent XLSX Files (one per year) contanining the square country matrices.
p_load(dplyr)
p_load(igraph)

setwd("~/RU/research/Bahamonde_Kovac/matrix")

sequence <- seq.int(min(trade$year),max(trade$year),1)

for ( i in sequence){
        filtracion <- filter(trade,year==i)
        d <- data.frame(filtracion$importer1,filtracion$importer2)
        g <- graph_from_data_frame(d, directed = FALSE, vertices = NULL)
        E(g)$weight <- filtracion$trade
        ad_matrix <- get.adjacency(g, attr="weight", sparse = F)
        file_name <- paste(c("trade_year_",i,".csv"), collapse = "")
        print(i)
        write.csv(ad_matrix,file_name)
}

########################################################
# Merge Trade-COW y National Material Capabilities-COW
########################################################

cat("\014")
rm(list=ls())
graphics.off()

## ---- data:loadings ----
# loads pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# load National Material Capabilities-COW
p_load(foreign)
mnc.d <- read.dta("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/NMC_5_0.dta") 

# load Trade-COW
p_load(foreign)
trade.d <- read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/National_COW_4.0.csv") 

# merge
cow.d = merge(mnc.d, trade.d, by=c("ccode","year"))

# subset variables
cow.d <- cow.d[c("ccode", "year", "milex", "milper", "irst", "pec")]

# Format Country Name
country = data.frame(
        statename = trade.d$statename,
        ccode = trade.d$ccode,
        year = trade.d$year
)
cow.d = merge(cow.d, country, by = c("ccode", "year"))
cow.d$statename <- as.character(cow.d$statename)


# rename time-ID and panel-ID vars.
colnames(cow.d)[which(names(cow.d) == "statename")] <- "ID"
colnames(cow.d)[which(names(cow.d) == "year")] <- "Time"


# reorder time-ID and panel-ID vars.
cow.d = subset(cow.d,  select=c("ID", "Time","milper", "irst")) # complete var names: "statename", "year", "milex", "milper", "irst", "pec"
cow.d$ID <- gsub(' ', '.', cow.d$ID) # replace blank space with dot "" just like in full.info.countries.first.time.span
cow.d$ID <- gsub('-', '.', cow.d$ID) # replace blank space with dot "" just like in full.info.countries.first.time.span

################################################################################################################
## First Period: 1871 to 1913
################################################################################################################

# Filter complete obs by year; here it's where I split the datasets
cow.d.1 <- subset(cow.d, Time >= 1871 & Time <= 1913)

# Drop NAs
cow.d.1$milper[cow.d.1$milper == -9] <- 0
cow.d.1$irst[cow.d.1$irst ==  -9] <- 0

# Filter countries for which we have complete data
p_load(dplyr)
cow.d.1 = cow.d.1 %>% dplyr::group_by(ID) %>% filter(milper != 0 & irst != 0)

# Drop countries for which we don't have the complete series
full.info.countries.first.time.span = unique(cow.d.1[cow.d.1$ID %in% names(which(table(cow.d.1$ID) == as.numeric(length(1871:1913)))), ]$ID) # Countries for which we have complete rows

# Filtering complete obs by country name
cow.d.1 = data.frame(cow.d.1[cow.d.1$ID %in% full.info.countries.first.time.span,])

# Reformat time variable
cow.d.1$Time = as.character(cow.d.1$Time)
cow.d.1$Time = as.Date(cow.d.1$Time,"%Y")
cow.d.1$Time <- as.POSIXct(cow.d.1$Time, origin="1871", tz = "GMT",  tryFormats ="%Y", optional = T)

# Sort df by country name and time
cow.d.1 = cow.d.1[with(cow.d.1, order(ID, Time)),]
rownames(cow.d.1) <- NULL

## Checking if panels are balanced
# p_load(plm)
# plm::is.pbalanced(cow.d.1)    


# Plot the data
# Pending


# Import CVSs
y.1871 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1871.csv"))
y.1872 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1872.csv"))
y.1873 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1873.csv"))
y.1874 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1874.csv"))
y.1875 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1875.csv"))
y.1876 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1876.csv"))
y.1877 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1877.csv"))
y.1878 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1878.csv"))
y.1879 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1879.csv"))
y.1880 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1880.csv"))
y.1881 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1881.csv"))
y.1882 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1882.csv"))
y.1883 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1883.csv"))
y.1884 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1884.csv"))
y.1885 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1885.csv"))
y.1886 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1886.csv"))
y.1887 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1887.csv"))
y.1888 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1888.csv"))
y.1889 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1889.csv"))
y.1890 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1890.csv"))
y.1891 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1891.csv"))
y.1892 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1892.csv"))
y.1893 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1893.csv"))
y.1894 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1894.csv"))
y.1895 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1895.csv"))
y.1896 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1896.csv"))
y.1897 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1897.csv"))
y.1898 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1898.csv"))
y.1899 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1899.csv"))
y.1900 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1900.csv"))
y.1901 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1901.csv"))
y.1902 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1902.csv"))
y.1903 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1903.csv"))
y.1904 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1904.csv"))
y.1905 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1905.csv"))
y.1906 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1906.csv"))
y.1907 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1907.csv"))
y.1908 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1908.csv"))
y.1909 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1909.csv"))
y.1910 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1910.csv"))
y.1911 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1911.csv"))
y.1912 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1912.csv"))
y.1913 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1913.csv"))


# Standarize country names, select columns, then rows, and then delete X column
p_load(dplyr)
# test = y.1871
# test$X <- gsub(' ', '.', test$X);test$X <- gsub('-', '.', test$X);test = test %>% select(c(X, full.info.countries.first.time.span));test = test[test$X %in% full.info.countries.first.time.span,];test$X <- NULL;rownames(test) <- NULL 


y.1871$X <- gsub(' ', '.', y.1871$X);y.1871$X <- gsub('-', '.', y.1871$X);y.1871 = y.1871 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1871 = y.1871[y.1871$X %in% full.info.countries.first.time.span,];y.1871$X <- NULL;rownames(y.1871) <- NULL 
y.1872$X <- gsub(' ', '.', y.1872$X);y.1872$X <- gsub('-', '.', y.1872$X);y.1872 = y.1872 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1872 = y.1872[y.1872$X %in% full.info.countries.first.time.span,];y.1872$X <- NULL;rownames(y.1872) <- NULL 
y.1873$X <- gsub(' ', '.', y.1873$X);y.1873$X <- gsub('-', '.', y.1873$X);y.1873 = y.1873 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1873 = y.1873[y.1873$X %in% full.info.countries.first.time.span,];y.1873$X <- NULL;rownames(y.1873) <- NULL 
y.1874$X <- gsub(' ', '.', y.1874$X);y.1874$X <- gsub('-', '.', y.1874$X);y.1874 = y.1874 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1874 = y.1874[y.1874$X %in% full.info.countries.first.time.span,];y.1874$X <- NULL;rownames(y.1874) <- NULL 
y.1875$X <- gsub(' ', '.', y.1875$X);y.1875$X <- gsub('-', '.', y.1875$X);y.1875 = y.1875 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1875 = y.1875[y.1875$X %in% full.info.countries.first.time.span,];y.1875$X <- NULL;rownames(y.1875) <- NULL 
y.1876$X <- gsub(' ', '.', y.1876$X);y.1876$X <- gsub('-', '.', y.1876$X);y.1876 = y.1876 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1876 = y.1876[y.1876$X %in% full.info.countries.first.time.span,];y.1876$X <- NULL;rownames(y.1876) <- NULL 
y.1877$X <- gsub(' ', '.', y.1877$X);y.1877$X <- gsub('-', '.', y.1877$X);y.1877 = y.1877 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1877 = y.1877[y.1877$X %in% full.info.countries.first.time.span,];y.1877$X <- NULL;rownames(y.1877) <- NULL 
y.1878$X <- gsub(' ', '.', y.1878$X);y.1878$X <- gsub('-', '.', y.1878$X);y.1878 = y.1878 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1878 = y.1878[y.1878$X %in% full.info.countries.first.time.span,];y.1878$X <- NULL;rownames(y.1878) <- NULL 
y.1879$X <- gsub(' ', '.', y.1879$X);y.1879$X <- gsub('-', '.', y.1879$X);y.1879 = y.1879 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1879 = y.1879[y.1879$X %in% full.info.countries.first.time.span,];y.1879$X <- NULL;rownames(y.1879) <- NULL 
y.1880$X <- gsub(' ', '.', y.1880$X);y.1880$X <- gsub('-', '.', y.1880$X);y.1880 = y.1880 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1880 = y.1880[y.1880$X %in% full.info.countries.first.time.span,];y.1880$X <- NULL;rownames(y.1880) <- NULL 
y.1881$X <- gsub(' ', '.', y.1881$X);y.1881$X <- gsub('-', '.', y.1881$X);y.1881 = y.1881 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1881 = y.1881[y.1881$X %in% full.info.countries.first.time.span,];y.1881$X <- NULL;rownames(y.1881) <- NULL 
y.1882$X <- gsub(' ', '.', y.1882$X);y.1882$X <- gsub('-', '.', y.1882$X);y.1882 = y.1882 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1882 = y.1882[y.1882$X %in% full.info.countries.first.time.span,];y.1882$X <- NULL;rownames(y.1882) <- NULL 
y.1883$X <- gsub(' ', '.', y.1883$X);y.1883$X <- gsub('-', '.', y.1883$X);y.1883 = y.1883 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1883 = y.1883[y.1883$X %in% full.info.countries.first.time.span,];y.1883$X <- NULL;rownames(y.1883) <- NULL 
y.1884$X <- gsub(' ', '.', y.1884$X);y.1884$X <- gsub('-', '.', y.1884$X);y.1884 = y.1884 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1884 = y.1884[y.1884$X %in% full.info.countries.first.time.span,];y.1884$X <- NULL;rownames(y.1884) <- NULL 
y.1885$X <- gsub(' ', '.', y.1885$X);y.1885$X <- gsub('-', '.', y.1885$X);y.1885 = y.1885 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1885 = y.1885[y.1885$X %in% full.info.countries.first.time.span,];y.1885$X <- NULL;rownames(y.1885) <- NULL 
y.1886$X <- gsub(' ', '.', y.1886$X);y.1886$X <- gsub('-', '.', y.1886$X);y.1886 = y.1886 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1886 = y.1886[y.1886$X %in% full.info.countries.first.time.span,];y.1886$X <- NULL;rownames(y.1886) <- NULL 
y.1887$X <- gsub(' ', '.', y.1887$X);y.1887$X <- gsub('-', '.', y.1887$X);y.1887 = y.1887 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1887 = y.1887[y.1887$X %in% full.info.countries.first.time.span,];y.1887$X <- NULL;rownames(y.1887) <- NULL 
y.1888$X <- gsub(' ', '.', y.1888$X);y.1888$X <- gsub('-', '.', y.1888$X);y.1888 = y.1888 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1888 = y.1888[y.1888$X %in% full.info.countries.first.time.span,];y.1888$X <- NULL;rownames(y.1888) <- NULL 
y.1889$X <- gsub(' ', '.', y.1889$X);y.1889$X <- gsub('-', '.', y.1889$X);y.1889 = y.1889 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1889 = y.1889[y.1889$X %in% full.info.countries.first.time.span,];y.1889$X <- NULL;rownames(y.1889) <- NULL 
y.1890$X <- gsub(' ', '.', y.1890$X);y.1890$X <- gsub('-', '.', y.1890$X);y.1890 = y.1890 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1890 = y.1890[y.1890$X %in% full.info.countries.first.time.span,];y.1890$X <- NULL;rownames(y.1890) <- NULL 
y.1891$X <- gsub(' ', '.', y.1891$X);y.1891$X <- gsub('-', '.', y.1891$X);y.1891 = y.1891 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1891 = y.1891[y.1891$X %in% full.info.countries.first.time.span,];y.1891$X <- NULL;rownames(y.1891) <- NULL 
y.1892$X <- gsub(' ', '.', y.1892$X);y.1892$X <- gsub('-', '.', y.1892$X);y.1892 = y.1892 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1892 = y.1892[y.1892$X %in% full.info.countries.first.time.span,];y.1892$X <- NULL;rownames(y.1892) <- NULL 
y.1893$X <- gsub(' ', '.', y.1893$X);y.1893$X <- gsub('-', '.', y.1893$X);y.1893 = y.1893 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1893 = y.1893[y.1893$X %in% full.info.countries.first.time.span,];y.1893$X <- NULL;rownames(y.1893) <- NULL 
y.1894$X <- gsub(' ', '.', y.1894$X);y.1894$X <- gsub('-', '.', y.1894$X);y.1894 = y.1894 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1894 = y.1894[y.1894$X %in% full.info.countries.first.time.span,];y.1894$X <- NULL;rownames(y.1894) <- NULL 
y.1895$X <- gsub(' ', '.', y.1895$X);y.1895$X <- gsub('-', '.', y.1895$X);y.1895 = y.1895 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1895 = y.1895[y.1895$X %in% full.info.countries.first.time.span,];y.1895$X <- NULL;rownames(y.1895) <- NULL 
y.1896$X <- gsub(' ', '.', y.1896$X);y.1896$X <- gsub('-', '.', y.1896$X);y.1896 = y.1896 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1896 = y.1896[y.1896$X %in% full.info.countries.first.time.span,];y.1896$X <- NULL;rownames(y.1896) <- NULL 
y.1897$X <- gsub(' ', '.', y.1897$X);y.1897$X <- gsub('-', '.', y.1897$X);y.1897 = y.1897 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1897 = y.1897[y.1897$X %in% full.info.countries.first.time.span,];y.1897$X <- NULL;rownames(y.1897) <- NULL 
y.1898$X <- gsub(' ', '.', y.1898$X);y.1898$X <- gsub('-', '.', y.1898$X);y.1898 = y.1898 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1898 = y.1898[y.1898$X %in% full.info.countries.first.time.span,];y.1898$X <- NULL;rownames(y.1898) <- NULL 
y.1899$X <- gsub(' ', '.', y.1899$X);y.1899$X <- gsub('-', '.', y.1899$X);y.1899 = y.1899 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1899 = y.1899[y.1899$X %in% full.info.countries.first.time.span,];y.1899$X <- NULL;rownames(y.1899) <- NULL 
y.1900$X <- gsub(' ', '.', y.1900$X);y.1900$X <- gsub('-', '.', y.1900$X);y.1900 = y.1900 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1900 = y.1900[y.1900$X %in% full.info.countries.first.time.span,];y.1900$X <- NULL;rownames(y.1900) <- NULL 
y.1901$X <- gsub(' ', '.', y.1901$X);y.1901$X <- gsub('-', '.', y.1901$X);y.1901 = y.1901 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1901 = y.1901[y.1901$X %in% full.info.countries.first.time.span,];y.1901$X <- NULL;rownames(y.1901) <- NULL 
y.1902$X <- gsub(' ', '.', y.1902$X);y.1902$X <- gsub('-', '.', y.1902$X);y.1902 = y.1902 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1902 = y.1902[y.1902$X %in% full.info.countries.first.time.span,];y.1902$X <- NULL;rownames(y.1902) <- NULL 
y.1903$X <- gsub(' ', '.', y.1903$X);y.1903$X <- gsub('-', '.', y.1903$X);y.1903 = y.1903 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1903 = y.1903[y.1903$X %in% full.info.countries.first.time.span,];y.1903$X <- NULL;rownames(y.1903) <- NULL 
y.1904$X <- gsub(' ', '.', y.1904$X);y.1904$X <- gsub('-', '.', y.1904$X);y.1904 = y.1904 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1904 = y.1904[y.1904$X %in% full.info.countries.first.time.span,];y.1904$X <- NULL;rownames(y.1904) <- NULL 
y.1905$X <- gsub(' ', '.', y.1905$X);y.1905$X <- gsub('-', '.', y.1905$X);y.1905 = y.1905 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1905 = y.1905[y.1905$X %in% full.info.countries.first.time.span,];y.1905$X <- NULL;rownames(y.1905) <- NULL 
y.1906$X <- gsub(' ', '.', y.1906$X);y.1906$X <- gsub('-', '.', y.1906$X);y.1906 = y.1906 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1906 = y.1906[y.1906$X %in% full.info.countries.first.time.span,];y.1906$X <- NULL;rownames(y.1906) <- NULL 
y.1907$X <- gsub(' ', '.', y.1907$X);y.1907$X <- gsub('-', '.', y.1907$X);y.1907 = y.1907 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1907 = y.1907[y.1907$X %in% full.info.countries.first.time.span,];y.1907$X <- NULL;rownames(y.1907) <- NULL 
y.1908$X <- gsub(' ', '.', y.1908$X);y.1908$X <- gsub('-', '.', y.1908$X);y.1908 = y.1908 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1908 = y.1908[y.1908$X %in% full.info.countries.first.time.span,];y.1908$X <- NULL;rownames(y.1908) <- NULL 
y.1909$X <- gsub(' ', '.', y.1909$X);y.1909$X <- gsub('-', '.', y.1909$X);y.1909 = y.1909 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1909 = y.1909[y.1909$X %in% full.info.countries.first.time.span,];y.1909$X <- NULL;rownames(y.1909) <- NULL 
y.1910$X <- gsub(' ', '.', y.1910$X);y.1910$X <- gsub('-', '.', y.1910$X);y.1910 = y.1910 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1910 = y.1910[y.1910$X %in% full.info.countries.first.time.span,];y.1910$X <- NULL;rownames(y.1910) <- NULL 
y.1911$X <- gsub(' ', '.', y.1911$X);y.1911$X <- gsub('-', '.', y.1911$X);y.1911 = y.1911 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1911 = y.1911[y.1911$X %in% full.info.countries.first.time.span,];y.1911$X <- NULL;rownames(y.1911) <- NULL 
y.1912$X <- gsub(' ', '.', y.1912$X);y.1912$X <- gsub('-', '.', y.1912$X);y.1912 = y.1912 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1912 = y.1912[y.1912$X %in% full.info.countries.first.time.span,];y.1912$X <- NULL;rownames(y.1912) <- NULL 
y.1913$X <- gsub(' ', '.', y.1913$X);y.1913$X <- gsub('-', '.', y.1913$X);y.1913 = y.1913 %>% dplyr::select(c(X, full.info.countries.first.time.span));y.1913 = y.1913[y.1913$X %in% full.info.countries.first.time.span,];y.1913$X <- NULL;rownames(y.1913) <- NULL 



# Building WM for the first period
wm.1 = list(as.matrix(y.1871), as.matrix(y.1872), as.matrix(y.1873), as.matrix(y.1874), as.matrix(y.1875), as.matrix(y.1876), as.matrix(y.1877), as.matrix(y.1878), as.matrix(y.1879), as.matrix(y.1880), as.matrix(y.1881), as.matrix(y.1882), as.matrix(y.1883), as.matrix(y.1884), as.matrix(y.1885), as.matrix(y.1886), as.matrix(y.1887), as.matrix(y.1888), as.matrix(y.1889), as.matrix(y.1890), as.matrix(y.1891), as.matrix(y.1892), as.matrix(y.1893), as.matrix(y.1894), as.matrix(y.1895), as.matrix(y.1896), as.matrix(y.1897), as.matrix(y.1898), as.matrix(y.1899), as.matrix(y.1900), as.matrix(y.1901), as.matrix(y.1902), as.matrix(y.1903), as.matrix(y.1904), as.matrix(y.1905), as.matrix(y.1906), as.matrix(y.1907), as.matrix(y.1908), as.matrix(y.1909), as.matrix(y.1910), as.matrix(y.1911), as.matrix(y.1912), as.matrix(y.1913))



################################################################################################################
## Second Period: 1955 - 2014 (Minor Countries)
################################################################################################################

# Filter complete obs by year; here it's where I split the datasets
cow.d.2 <- subset(cow.d, Time >= 1955 & Time <= 2014) # 2014

# Drop NAs
cow.d.2$milper[cow.d.2$milper == -9] <- NA
cow.d.2$irst[cow.d.2$irst ==  -9] <- NA

# Filter countries for which we have complete data
cow.d.2 = cow.d.2 %>% group_by(ID) %>% filter(milper != 0 & irst != 0)

# Drop countries for which we don't have the complete series
full.info.countries.second.time.span = unique(cow.d.2[cow.d.2$ID %in% names(which(table(cow.d.2$ID) == max(table(cow.d.2$ID)))), ]$ID) # Countries for which we have complete rows

# Exclude Luxembourg (it's got too many similar values year after year)
#full.info.countries.second.time.span <- full.info.countries.second.time.span[full.info.countries.second.time.span != "Luxembourg"]
#full.info.countries.second.time.span <- full.info.countries.second.time.span[full.info.countries.second.time.span != "Taiwan"]
#full.info.countries.second.time.span <- full.info.countries.second.time.span[full.info.countries.second.time.span != "North.Korea"]

full.info.countries.second.time.span = full.info.countries.second.time.span[c(-5, -21, -29)] # ALL COUNTRIES EXCEPT: 5. US, 21. Russia, 29. China # WORKING!

# test = max(table(cow.d.2$ID)) ; View(test) # Germanies appear 36 times. Consider do GVAR for both of them.

# Filtering complete obs by country name
cow.d.2 = data.frame(cow.d.2[cow.d.2$ID %in% full.info.countries.second.time.span,])

# TEST: introduce stochastic noise ## Truncated normal with mean 0
p_load(truncnorm)
set.seed(2020); cow.d.2$milper = round(cow.d.2$milper + rtruncnorm(n=nrow(cow.d.2), a=-3, b=3, mean=0, sd=1), 2) # Mean 0
set.seed(2019); cow.d.2$irst = round(cow.d.2$irst + rtruncnorm(n=nrow(cow.d.2), a=-3, b=3, mean=0, sd=1), 2) # Mean 0

# Reformat time variable
cow.d.2$Time = as.character(cow.d.2$Time)
cow.d.2$Time = as.Date(cow.d.2$Time,"%Y")
cow.d.2$Time <- as.POSIXct(cow.d.2$Time, origin=min(cow.d.2$Time), tz = "GMT",  tryFormats ="%Y", optional = T)

# Sort df by country name and time
cow.d.2 = cow.d.2[with(cow.d.2, order(ID, Time)),]
rownames(cow.d.2) <- NULL

## Checking if panels are balanced
# p_load(plm)
# plm::is.pbalanced(cow.d.2)    


# Plot the data
# Pending


# Import CVSs
y.1955 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1955.csv"))
y.1956 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1956.csv"))
y.1957 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1957.csv"))
y.1958 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1958.csv"))
y.1959 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1959.csv"))
y.1960 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1960.csv"))
y.1961 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1961.csv"))
y.1962 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1962.csv"))
y.1963 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1963.csv"))
y.1964 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1964.csv"))
y.1965 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1965.csv"))
y.1966 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1966.csv"))
y.1967 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1967.csv"))
y.1968 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1968.csv"))
y.1969 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1969.csv"))
y.1970 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1970.csv"))
y.1971 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1971.csv"))
y.1972 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1972.csv"))
y.1973 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1973.csv"))
y.1974 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1974.csv"))
y.1975 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1975.csv"))
y.1976 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1976.csv"))
y.1977 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1977.csv"))
y.1978 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1978.csv"))
y.1979 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1979.csv"))
y.1980 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1980.csv"))
y.1981 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1981.csv"))
y.1982 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1982.csv"))
y.1983 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1983.csv"))
y.1984 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1984.csv"))
y.1985 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1985.csv"))
y.1986 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1986.csv"))
y.1987 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1987.csv"))
y.1988 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1988.csv"))
y.1989 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1989.csv"))
y.1990 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1990.csv"))
y.1991 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1991.csv"))
y.1992 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1992.csv"))
y.1993 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1993.csv"))
y.1994 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1994.csv"))
y.1995 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1995.csv"))
y.1996 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1996.csv"))
y.1997 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1997.csv"))
y.1998 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1998.csv"))
y.1999 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1999.csv"))
y.2000 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2000.csv"))
y.2001 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2001.csv"))
y.2002 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2002.csv"))
y.2003 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2003.csv"))
y.2004 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2004.csv"))
y.2005 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2005.csv"))
y.2006 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2006.csv"))
y.2007 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2007.csv"))
y.2008 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2008.csv"))
y.2009 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2009.csv"))
y.2010 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2010.csv"))
y.2011 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2011.csv"))
y.2012 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2012.csv"))
y.2013 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2013.csv"))
y.2014 = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2014.csv"))


# Standarize country names, select columns, then rows, and then delete X column
p_load(dplyr)

y.1955$X <- gsub(' ', '.', y.1955$X);y.1955$X <- gsub('-', '.', y.1955$X);y.1955 = y.1955[y.1955$X %in% full.info.countries.second.time.span,];y.1955 = y.1955 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1955 <- y.1955[order(y.1955$X),];y.1955$X <- NULL;rownames(y.1955) <- NULL 
y.1956$X <- gsub(' ', '.', y.1956$X);y.1956$X <- gsub('-', '.', y.1956$X);y.1956 = y.1956[y.1956$X %in% full.info.countries.second.time.span,];y.1956 = y.1956 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1956 <- y.1956[order(y.1956$X),];y.1956$X <- NULL;rownames(y.1956) <- NULL 
y.1957$X <- gsub(' ', '.', y.1957$X);y.1957$X <- gsub('-', '.', y.1957$X);y.1957 = y.1957[y.1957$X %in% full.info.countries.second.time.span,];y.1957 = y.1957 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1957 <- y.1957[order(y.1957$X),];y.1957$X <- NULL;rownames(y.1957) <- NULL 
y.1958$X <- gsub(' ', '.', y.1958$X);y.1958$X <- gsub('-', '.', y.1958$X);y.1958 = y.1958[y.1958$X %in% full.info.countries.second.time.span,];y.1958 = y.1958 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1958 <- y.1958[order(y.1958$X),];y.1958$X <- NULL;rownames(y.1958) <- NULL 
y.1959$X <- gsub(' ', '.', y.1959$X);y.1959$X <- gsub('-', '.', y.1959$X);y.1959 = y.1959[y.1959$X %in% full.info.countries.second.time.span,];y.1959 = y.1959 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1959 <- y.1959[order(y.1959$X),];y.1959$X <- NULL;rownames(y.1959) <- NULL 
y.1960$X <- gsub(' ', '.', y.1960$X);y.1960$X <- gsub('-', '.', y.1960$X);y.1960 = y.1960[y.1960$X %in% full.info.countries.second.time.span,];y.1960 = y.1960 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1960 <- y.1960[order(y.1960$X),];y.1960$X <- NULL;rownames(y.1960) <- NULL 
y.1961$X <- gsub(' ', '.', y.1961$X);y.1961$X <- gsub('-', '.', y.1961$X);y.1961 = y.1961[y.1961$X %in% full.info.countries.second.time.span,];y.1961 = y.1961 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1961 <- y.1961[order(y.1961$X),];y.1961$X <- NULL;rownames(y.1961) <- NULL 
y.1962$X <- gsub(' ', '.', y.1962$X);y.1962$X <- gsub('-', '.', y.1962$X);y.1962 = y.1962[y.1962$X %in% full.info.countries.second.time.span,];y.1962 = y.1962 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1962 <- y.1962[order(y.1962$X),];y.1962$X <- NULL;rownames(y.1962) <- NULL 
y.1963$X <- gsub(' ', '.', y.1963$X);y.1963$X <- gsub('-', '.', y.1963$X);y.1963 = y.1963[y.1963$X %in% full.info.countries.second.time.span,];y.1963 = y.1963 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1963 <- y.1963[order(y.1963$X),];y.1963$X <- NULL;rownames(y.1963) <- NULL 
y.1964$X <- gsub(' ', '.', y.1964$X);y.1964$X <- gsub('-', '.', y.1964$X);y.1964 = y.1964[y.1964$X %in% full.info.countries.second.time.span,];y.1964 = y.1964 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1964 <- y.1964[order(y.1964$X),];y.1964$X <- NULL;rownames(y.1964) <- NULL 
y.1965$X <- gsub(' ', '.', y.1965$X);y.1965$X <- gsub('-', '.', y.1965$X);y.1965 = y.1965[y.1965$X %in% full.info.countries.second.time.span,];y.1965 = y.1965 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1965 <- y.1965[order(y.1965$X),];y.1965$X <- NULL;rownames(y.1965) <- NULL 
y.1966$X <- gsub(' ', '.', y.1966$X);y.1966$X <- gsub('-', '.', y.1966$X);y.1966 = y.1966[y.1966$X %in% full.info.countries.second.time.span,];y.1966 = y.1966 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1966 <- y.1966[order(y.1966$X),];y.1966$X <- NULL;rownames(y.1966) <- NULL 
y.1967$X <- gsub(' ', '.', y.1967$X);y.1967$X <- gsub('-', '.', y.1967$X);y.1967 = y.1967[y.1967$X %in% full.info.countries.second.time.span,];y.1967 = y.1967 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1967 <- y.1967[order(y.1967$X),];y.1967$X <- NULL;rownames(y.1967) <- NULL 
y.1968$X <- gsub(' ', '.', y.1968$X);y.1968$X <- gsub('-', '.', y.1968$X);y.1968 = y.1968[y.1968$X %in% full.info.countries.second.time.span,];y.1968 = y.1968 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1968 <- y.1968[order(y.1968$X),];y.1968$X <- NULL;rownames(y.1968) <- NULL 
y.1969$X <- gsub(' ', '.', y.1969$X);y.1969$X <- gsub('-', '.', y.1969$X);y.1969 = y.1969[y.1969$X %in% full.info.countries.second.time.span,];y.1969 = y.1969 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1969 <- y.1969[order(y.1969$X),];y.1969$X <- NULL;rownames(y.1969) <- NULL 
y.1970$X <- gsub(' ', '.', y.1970$X);y.1970$X <- gsub('-', '.', y.1970$X);y.1970 = y.1970[y.1970$X %in% full.info.countries.second.time.span,];y.1970 = y.1970 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1970 <- y.1970[order(y.1970$X),];y.1970$X <- NULL;rownames(y.1970) <- NULL 
y.1971$X <- gsub(' ', '.', y.1971$X);y.1971$X <- gsub('-', '.', y.1971$X);y.1971 = y.1971[y.1971$X %in% full.info.countries.second.time.span,];y.1971 = y.1971 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1971 <- y.1971[order(y.1971$X),];y.1971$X <- NULL;rownames(y.1971) <- NULL 
y.1972$X <- gsub(' ', '.', y.1972$X);y.1972$X <- gsub('-', '.', y.1972$X);y.1972 = y.1972[y.1972$X %in% full.info.countries.second.time.span,];y.1972 = y.1972 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1972 <- y.1972[order(y.1972$X),];y.1972$X <- NULL;rownames(y.1972) <- NULL 
y.1973$X <- gsub(' ', '.', y.1973$X);y.1973$X <- gsub('-', '.', y.1973$X);y.1973 = y.1973[y.1973$X %in% full.info.countries.second.time.span,];y.1973 = y.1973 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1973 <- y.1973[order(y.1973$X),];y.1973$X <- NULL;rownames(y.1973) <- NULL 
y.1974$X <- gsub(' ', '.', y.1974$X);y.1974$X <- gsub('-', '.', y.1974$X);y.1974 = y.1974[y.1974$X %in% full.info.countries.second.time.span,];y.1974 = y.1974 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1974 <- y.1974[order(y.1974$X),];y.1974$X <- NULL;rownames(y.1974) <- NULL 
y.1975$X <- gsub(' ', '.', y.1975$X);y.1975$X <- gsub('-', '.', y.1975$X);y.1975 = y.1975[y.1975$X %in% full.info.countries.second.time.span,];y.1975 = y.1975 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1975 <- y.1975[order(y.1975$X),];y.1975$X <- NULL;rownames(y.1975) <- NULL 
y.1976$X <- gsub(' ', '.', y.1976$X);y.1976$X <- gsub('-', '.', y.1976$X);y.1976 = y.1976[y.1976$X %in% full.info.countries.second.time.span,];y.1976 = y.1976 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1976 <- y.1976[order(y.1976$X),];y.1976$X <- NULL;rownames(y.1976) <- NULL 
y.1977$X <- gsub(' ', '.', y.1977$X);y.1977$X <- gsub('-', '.', y.1977$X);y.1977 = y.1977[y.1977$X %in% full.info.countries.second.time.span,];y.1977 = y.1977 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1977 <- y.1977[order(y.1977$X),];y.1977$X <- NULL;rownames(y.1977) <- NULL 
y.1978$X <- gsub(' ', '.', y.1978$X);y.1978$X <- gsub('-', '.', y.1978$X);y.1978 = y.1978[y.1978$X %in% full.info.countries.second.time.span,];y.1978 = y.1978 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1978 <- y.1978[order(y.1978$X),];y.1978$X <- NULL;rownames(y.1978) <- NULL 
y.1979$X <- gsub(' ', '.', y.1979$X);y.1979$X <- gsub('-', '.', y.1979$X);y.1979 = y.1979[y.1979$X %in% full.info.countries.second.time.span,];y.1979 = y.1979 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1979 <- y.1979[order(y.1979$X),];y.1979$X <- NULL;rownames(y.1979) <- NULL 
y.1980$X <- gsub(' ', '.', y.1980$X);y.1980$X <- gsub('-', '.', y.1980$X);y.1980 = y.1980[y.1980$X %in% full.info.countries.second.time.span,];y.1980 = y.1980 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1980 <- y.1980[order(y.1980$X),];y.1980$X <- NULL;rownames(y.1980) <- NULL 
y.1981$X <- gsub(' ', '.', y.1981$X);y.1981$X <- gsub('-', '.', y.1981$X);y.1981 = y.1981[y.1981$X %in% full.info.countries.second.time.span,];y.1981 = y.1981 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1981 <- y.1981[order(y.1981$X),];y.1981$X <- NULL;rownames(y.1981) <- NULL 
y.1982$X <- gsub(' ', '.', y.1982$X);y.1982$X <- gsub('-', '.', y.1982$X);y.1982 = y.1982[y.1982$X %in% full.info.countries.second.time.span,];y.1982 = y.1982 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1982 <- y.1982[order(y.1982$X),];y.1982$X <- NULL;rownames(y.1982) <- NULL 
y.1983$X <- gsub(' ', '.', y.1983$X);y.1983$X <- gsub('-', '.', y.1983$X);y.1983 = y.1983[y.1983$X %in% full.info.countries.second.time.span,];y.1983 = y.1983 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1983 <- y.1983[order(y.1983$X),];y.1983$X <- NULL;rownames(y.1983) <- NULL 
y.1984$X <- gsub(' ', '.', y.1984$X);y.1984$X <- gsub('-', '.', y.1984$X);y.1984 = y.1984[y.1984$X %in% full.info.countries.second.time.span,];y.1984 = y.1984 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1984 <- y.1984[order(y.1984$X),];y.1984$X <- NULL;rownames(y.1984) <- NULL 
y.1985$X <- gsub(' ', '.', y.1985$X);y.1985$X <- gsub('-', '.', y.1985$X);y.1985 = y.1985[y.1985$X %in% full.info.countries.second.time.span,];y.1985 = y.1985 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1985 <- y.1985[order(y.1985$X),];y.1985$X <- NULL;rownames(y.1985) <- NULL 
y.1986$X <- gsub(' ', '.', y.1986$X);y.1986$X <- gsub('-', '.', y.1986$X);y.1986 = y.1986[y.1986$X %in% full.info.countries.second.time.span,];y.1986 = y.1986 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1986 <- y.1986[order(y.1986$X),];y.1986$X <- NULL;rownames(y.1986) <- NULL 
y.1987$X <- gsub(' ', '.', y.1987$X);y.1987$X <- gsub('-', '.', y.1987$X);y.1987 = y.1987[y.1987$X %in% full.info.countries.second.time.span,];y.1987 = y.1987 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1987 <- y.1987[order(y.1987$X),];y.1987$X <- NULL;rownames(y.1987) <- NULL 
y.1988$X <- gsub(' ', '.', y.1988$X);y.1988$X <- gsub('-', '.', y.1988$X);y.1988 = y.1988[y.1988$X %in% full.info.countries.second.time.span,];y.1988 = y.1988 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1988 <- y.1988[order(y.1988$X),];y.1988$X <- NULL;rownames(y.1988) <- NULL 
y.1989$X <- gsub(' ', '.', y.1989$X);y.1989$X <- gsub('-', '.', y.1989$X);y.1989 = y.1989[y.1989$X %in% full.info.countries.second.time.span,];y.1989 = y.1989 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1989 <- y.1989[order(y.1989$X),];y.1989$X <- NULL;rownames(y.1989) <- NULL 
y.1990$X <- gsub(' ', '.', y.1990$X);y.1990$X <- gsub('-', '.', y.1990$X);y.1990 = y.1990[y.1990$X %in% full.info.countries.second.time.span,];y.1990 = y.1990 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1990 <- y.1990[order(y.1990$X),];y.1990$X <- NULL;rownames(y.1990) <- NULL 
y.1991$X <- gsub(' ', '.', y.1991$X);y.1991$X <- gsub('-', '.', y.1991$X);y.1991 = y.1991[y.1991$X %in% full.info.countries.second.time.span,];y.1991 = y.1991 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1991 <- y.1991[order(y.1991$X),];y.1991$X <- NULL;rownames(y.1991) <- NULL 
y.1992$X <- gsub(' ', '.', y.1992$X);y.1992$X <- gsub('-', '.', y.1992$X);y.1992 = y.1992[y.1992$X %in% full.info.countries.second.time.span,];y.1992 = y.1992 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1992 <- y.1992[order(y.1992$X),];y.1992$X <- NULL;rownames(y.1992) <- NULL 
y.1993$X <- gsub(' ', '.', y.1993$X);y.1993$X <- gsub('-', '.', y.1993$X);y.1993 = y.1993[y.1993$X %in% full.info.countries.second.time.span,];y.1993 = y.1993 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1993 <- y.1993[order(y.1993$X),];y.1993$X <- NULL;rownames(y.1993) <- NULL 
y.1994$X <- gsub(' ', '.', y.1994$X);y.1994$X <- gsub('-', '.', y.1994$X);y.1994 = y.1994[y.1994$X %in% full.info.countries.second.time.span,];y.1994 = y.1994 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1994 <- y.1994[order(y.1994$X),];y.1994$X <- NULL;rownames(y.1994) <- NULL 
y.1995$X <- gsub(' ', '.', y.1995$X);y.1995$X <- gsub('-', '.', y.1995$X);y.1995 = y.1995[y.1995$X %in% full.info.countries.second.time.span,];y.1995 = y.1995 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1995 <- y.1995[order(y.1995$X),];y.1995$X <- NULL;rownames(y.1995) <- NULL 
y.1996$X <- gsub(' ', '.', y.1996$X);y.1996$X <- gsub('-', '.', y.1996$X);y.1996 = y.1996[y.1996$X %in% full.info.countries.second.time.span,];y.1996 = y.1996 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1996 <- y.1996[order(y.1996$X),];y.1996$X <- NULL;rownames(y.1996) <- NULL 
y.1997$X <- gsub(' ', '.', y.1997$X);y.1997$X <- gsub('-', '.', y.1997$X);y.1997 = y.1997[y.1997$X %in% full.info.countries.second.time.span,];y.1997 = y.1997 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1997 <- y.1997[order(y.1997$X),];y.1997$X <- NULL;rownames(y.1997) <- NULL 
y.1998$X <- gsub(' ', '.', y.1998$X);y.1998$X <- gsub('-', '.', y.1998$X);y.1998 = y.1998[y.1998$X %in% full.info.countries.second.time.span,];y.1998 = y.1998 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1998 <- y.1998[order(y.1998$X),];y.1998$X <- NULL;rownames(y.1998) <- NULL 
y.1999$X <- gsub(' ', '.', y.1999$X);y.1999$X <- gsub('-', '.', y.1999$X);y.1999 = y.1999[y.1999$X %in% full.info.countries.second.time.span,];y.1999 = y.1999 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.1999 <- y.1999[order(y.1999$X),];y.1999$X <- NULL;rownames(y.1999) <- NULL 
y.2000$X <- gsub(' ', '.', y.2000$X);y.2000$X <- gsub('-', '.', y.2000$X);y.2000 = y.2000[y.2000$X %in% full.info.countries.second.time.span,];y.2000 = y.2000 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2000 <- y.2000[order(y.2000$X),];y.2000$X <- NULL;rownames(y.2000) <- NULL 
y.2001$X <- gsub(' ', '.', y.2001$X);y.2001$X <- gsub('-', '.', y.2001$X);y.2001 = y.2001[y.2001$X %in% full.info.countries.second.time.span,];y.2001 = y.2001 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2001 <- y.2001[order(y.2001$X),];y.2001$X <- NULL;rownames(y.2001) <- NULL 
y.2002$X <- gsub(' ', '.', y.2002$X);y.2002$X <- gsub('-', '.', y.2002$X);y.2002 = y.2002[y.2002$X %in% full.info.countries.second.time.span,];y.2002 = y.2002 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2002 <- y.2002[order(y.2002$X),];y.2002$X <- NULL;rownames(y.2002) <- NULL 
y.2003$X <- gsub(' ', '.', y.2003$X);y.2003$X <- gsub('-', '.', y.2003$X);y.2003 = y.2003[y.2003$X %in% full.info.countries.second.time.span,];y.2003 = y.2003 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2003 <- y.2003[order(y.2003$X),];y.2003$X <- NULL;rownames(y.2003) <- NULL 
y.2004$X <- gsub(' ', '.', y.2004$X);y.2004$X <- gsub('-', '.', y.2004$X);y.2004 = y.2004[y.2004$X %in% full.info.countries.second.time.span,];y.2004 = y.2004 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2004 <- y.2004[order(y.2004$X),];y.2004$X <- NULL;rownames(y.2004) <- NULL 
y.2005$X <- gsub(' ', '.', y.2005$X);y.2005$X <- gsub('-', '.', y.2005$X);y.2005 = y.2005[y.2005$X %in% full.info.countries.second.time.span,];y.2005 = y.2005 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2005 <- y.2005[order(y.2005$X),];y.2005$X <- NULL;rownames(y.2005) <- NULL 
y.2006$X <- gsub(' ', '.', y.2006$X);y.2006$X <- gsub('-', '.', y.2006$X);y.2006 = y.2006[y.2006$X %in% full.info.countries.second.time.span,];y.2006 = y.2006 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2006 <- y.2006[order(y.2006$X),];y.2006$X <- NULL;rownames(y.2006) <- NULL 
y.2007$X <- gsub(' ', '.', y.2007$X);y.2007$X <- gsub('-', '.', y.2007$X);y.2007 = y.2007[y.2007$X %in% full.info.countries.second.time.span,];y.2007 = y.2007 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2007 <- y.2007[order(y.2007$X),];y.2007$X <- NULL;rownames(y.2007) <- NULL 
y.2008$X <- gsub(' ', '.', y.2008$X);y.2008$X <- gsub('-', '.', y.2008$X);y.2008 = y.2008[y.2008$X %in% full.info.countries.second.time.span,];y.2008 = y.2008 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2008 <- y.2008[order(y.2008$X),];y.2008$X <- NULL;rownames(y.2008) <- NULL 
y.2009$X <- gsub(' ', '.', y.2009$X);y.2009$X <- gsub('-', '.', y.2009$X);y.2009 = y.2009[y.2009$X %in% full.info.countries.second.time.span,];y.2009 = y.2009 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2009 <- y.2009[order(y.2009$X),];y.2009$X <- NULL;rownames(y.2009) <- NULL 
y.2010$X <- gsub(' ', '.', y.2010$X);y.2010$X <- gsub('-', '.', y.2010$X);y.2010 = y.2010[y.2010$X %in% full.info.countries.second.time.span,];y.2010 = y.2010 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2010 <- y.2010[order(y.2010$X),];y.2010$X <- NULL;rownames(y.2010) <- NULL 
y.2011$X <- gsub(' ', '.', y.2011$X);y.2011$X <- gsub('-', '.', y.2011$X);y.2011 = y.2011[y.2011$X %in% full.info.countries.second.time.span,];y.2011 = y.2011 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2011 <- y.2011[order(y.2011$X),];y.2011$X <- NULL;rownames(y.2011) <- NULL 
y.2012$X <- gsub(' ', '.', y.2012$X);y.2012$X <- gsub('-', '.', y.2012$X);y.2012 = y.2012[y.2012$X %in% full.info.countries.second.time.span,];y.2012 = y.2012 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2012 <- y.2012[order(y.2012$X),];y.2012$X <- NULL;rownames(y.2012) <- NULL 
y.2013$X <- gsub(' ', '.', y.2013$X);y.2013$X <- gsub('-', '.', y.2013$X);y.2013 = y.2013[y.2013$X %in% full.info.countries.second.time.span,];y.2013 = y.2013 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2013 <- y.2013[order(y.2013$X),];y.2013$X <- NULL;rownames(y.2013) <- NULL 
y.2014$X <- gsub(' ', '.', y.2014$X);y.2014$X <- gsub('-', '.', y.2014$X);y.2014 = y.2014[y.2014$X %in% full.info.countries.second.time.span,];y.2014 = y.2014 %>% dplyr::select(c(X, sort(full.info.countries.second.time.span)));y.2014 <- y.2014[order(y.2014$X),];y.2014$X <- NULL;rownames(y.2014) <- NULL 


# Building WM for the second period
wm.2 = list(as.matrix(y.1955), as.matrix(y.1956), as.matrix(y.1957), as.matrix(y.1958), as.matrix(y.1959), as.matrix(y.1960), as.matrix(y.1961), as.matrix(y.1962), as.matrix(y.1963), as.matrix(y.1964), as.matrix(y.1965), as.matrix(y.1966), as.matrix(y.1967), as.matrix(y.1968), as.matrix(y.1969), as.matrix(y.1970), as.matrix(y.1971), as.matrix(y.1972), as.matrix(y.1973), as.matrix(y.1974), as.matrix(y.1975), as.matrix(y.1976), as.matrix(y.1977), as.matrix(y.1978), as.matrix(y.1979), as.matrix(y.1980), as.matrix(y.1981), as.matrix(y.1982), as.matrix(y.1983), as.matrix(y.1984), as.matrix(y.1985), as.matrix(y.1986), as.matrix(y.1987), as.matrix(y.1988), as.matrix(y.1989), as.matrix(y.1990), as.matrix(y.1991), as.matrix(y.1992), as.matrix(y.1993), as.matrix(y.1994), as.matrix(y.1995), as.matrix(y.1996), as.matrix(y.1997), as.matrix(y.1998), as.matrix(y.1999), as.matrix(y.2000), as.matrix(y.2001), as.matrix(y.2002), as.matrix(y.2003), as.matrix(y.2004), as.matrix(y.2005), as.matrix(y.2006), as.matrix(y.2007), as.matrix(y.2008), as.matrix(y.2009), as.matrix(y.2010), as.matrix(y.2011), as.matrix(y.2012), as.matrix(y.2013), as.matrix(y.2014)
            )


################################################################################################################
## Second Period: 1955 - 2014 (Big Countries)
################################################################################################################

# Filter complete obs by year; here it's where I split the datasets
cow.d.2.B <- subset(cow.d, Time >= 1955 & Time <= 2014) # 2014

# Drop NAs
cow.d.2.B$milper[cow.d.2.B$milper == -9] <- NA
cow.d.2.B$irst[cow.d.2.B$irst ==  -9] <- NA

# Filter countries for which we have complete data
cow.d.2.B = cow.d.2.B %>% group_by(ID) %>% filter(milper != 0 & irst != 0)

# Drop countries for which we don't have the complete series
full.info.countries.second.time.span.B = unique(cow.d.2.B[cow.d.2.B$ID %in% names(which(table(cow.d.2.B$ID) == max(table(cow.d.2.B$ID)))), ]$ID) # Countries for which we have complete rows

# Exclude Luxembourg (it's got too many similar values year after year)
#full.info.countries.second.time.span.B <- full.info.countries.second.time.span.B[full.info.countries.second.time.span.B != "Luxembourg"]
#full.info.countries.second.time.span.B <- full.info.countries.second.time.span.B[full.info.countries.second.time.span.B != "Taiwan"]
#full.info.countries.second.time.span.B <- full.info.countries.second.time.span.B[full.info.countries.second.time.span.B != "North.Korea"]

full.info.countries.second.time.span.B = full.info.countries.second.time.span.B[c(5, 21, 29)] # ALL COUNTRIES EXCEPT: 5. US, 21. Russia, 29. China # WORKING!

# test = max(table(cow.d.2.B$ID)) ; View(test) # Germanies appear 36 times. Consider do GVAR for both of them.

# Filtering complete obs by country name
cow.d.2.B = data.frame(cow.d.2.B[cow.d.2.B$ID %in% full.info.countries.second.time.span.B,])

# TEST: introduce stochastic noise ## Truncated normal with mean 0
p_load(truncnorm)
set.seed(2020); cow.d.2.B$milper = round(cow.d.2.B$milper + rtruncnorm(n=nrow(cow.d.2.B), a=-3, b=3, mean=0, sd=1), 2) # Mean 0
set.seed(2019); cow.d.2.B$irst = round(cow.d.2.B$irst + rtruncnorm(n=nrow(cow.d.2.B), a=-3, b=3, mean=0, sd=1), 2) # Mean 0

# Reformat time variable
cow.d.2.B$Time = as.character(cow.d.2.B$Time)
cow.d.2.B$Time = as.Date(cow.d.2.B$Time,"%Y")
cow.d.2.B$Time <- as.POSIXct(cow.d.2.B$Time, origin=min(cow.d.2.B$Time), tz = "GMT",  tryFormats ="%Y", optional = T)

# Sort df by country name and time
cow.d.2.B = cow.d.2.B[with(cow.d.2.B, order(ID, Time)),]
rownames(cow.d.2.B) <- NULL

## Checking if panels are balanced
# p_load(plm)
# plm::is.pbalanced(cow.d.2.B)    


# Plot the data
# Pending


# Import CVSs
y.1955.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1955.csv"))
y.1956.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1956.csv"))
y.1957.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1957.csv"))
y.1958.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1958.csv"))
y.1959.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1959.csv"))
y.1960.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1960.csv"))
y.1961.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1961.csv"))
y.1962.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1962.csv"))
y.1963.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1963.csv"))
y.1964.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1964.csv"))
y.1965.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1965.csv"))
y.1966.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1966.csv"))
y.1967.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1967.csv"))
y.1968.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1968.csv"))
y.1969.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1969.csv"))
y.1970.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1970.csv"))
y.1971.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1971.csv"))
y.1972.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1972.csv"))
y.1973.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1973.csv"))
y.1974.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1974.csv"))
y.1975.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1975.csv"))
y.1976.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1976.csv"))
y.1977.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1977.csv"))
y.1978.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1978.csv"))
y.1979.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1979.csv"))
y.1980.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1980.csv"))
y.1981.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1981.csv"))
y.1982.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1982.csv"))
y.1983.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1983.csv"))
y.1984.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1984.csv"))
y.1985.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1985.csv"))
y.1986.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1986.csv"))
y.1987.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1987.csv"))
y.1988.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1988.csv"))
y.1989.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1989.csv"))
y.1990.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1990.csv"))
y.1991.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1991.csv"))
y.1992.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1992.csv"))
y.1993.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1993.csv"))
y.1994.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1994.csv"))
y.1995.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1995.csv"))
y.1996.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1996.csv"))
y.1997.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1997.csv"))
y.1998.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1998.csv"))
y.1999.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_1999.csv"))
y.2000.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2000.csv"))
y.2001.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2001.csv"))
y.2002.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2002.csv"))
y.2003.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2003.csv"))
y.2004.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2004.csv"))
y.2005.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2005.csv"))
y.2006.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2006.csv"))
y.2007.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2007.csv"))
y.2008.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2008.csv"))
y.2009.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2009.csv"))
y.2010.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2010.csv"))
y.2011.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2011.csv"))
y.2012.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2012.csv"))
y.2013.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2013.csv"))
y.2014.b = data.frame(read.csv("/Users/hectorbahamonde/RU/research/Bahamonde_Kovac/matrix/trade_year_2014.csv"))


# Standarize country names, select columns, then rows, and then delete X column
p_load(dplyr)

y.1955.b$X <- gsub(' ', '.', y.1955.b$X);y.1955.b$X <- gsub('-', '.', y.1955.b$X);y.1955.b = y.1955.b[y.1955.b$X %in% full.info.countries.second.time.span.B,];y.1955.b = y.1955.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1955.b <- y.1955.b[order(y.1955.b$X),];y.1955.b$X <- NULL;rownames(y.1955.b) <- NULL 
y.1956.b$X <- gsub(' ', '.', y.1956.b$X);y.1956.b$X <- gsub('-', '.', y.1956.b$X);y.1956.b = y.1956.b[y.1956.b$X %in% full.info.countries.second.time.span.B,];y.1956.b = y.1956.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1956.b <- y.1956.b[order(y.1956.b$X),];y.1956.b$X <- NULL;rownames(y.1956.b) <- NULL 
y.1957.b$X <- gsub(' ', '.', y.1957.b$X);y.1957.b$X <- gsub('-', '.', y.1957.b$X);y.1957.b = y.1957.b[y.1957.b$X %in% full.info.countries.second.time.span.B,];y.1957.b = y.1957.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1957.b <- y.1957.b[order(y.1957.b$X),];y.1957.b$X <- NULL;rownames(y.1957.b) <- NULL 
y.1958.b$X <- gsub(' ', '.', y.1958.b$X);y.1958.b$X <- gsub('-', '.', y.1958.b$X);y.1958.b = y.1958.b[y.1958.b$X %in% full.info.countries.second.time.span.B,];y.1958.b = y.1958.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1958.b <- y.1958.b[order(y.1958.b$X),];y.1958.b$X <- NULL;rownames(y.1958.b) <- NULL 
y.1959.b$X <- gsub(' ', '.', y.1959.b$X);y.1959.b$X <- gsub('-', '.', y.1959.b$X);y.1959.b = y.1959.b[y.1959.b$X %in% full.info.countries.second.time.span.B,];y.1959.b = y.1959.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1959.b <- y.1959.b[order(y.1959.b$X),];y.1959.b$X <- NULL;rownames(y.1959.b) <- NULL 
y.1960.b$X <- gsub(' ', '.', y.1960.b$X);y.1960.b$X <- gsub('-', '.', y.1960.b$X);y.1960.b = y.1960.b[y.1960.b$X %in% full.info.countries.second.time.span.B,];y.1960.b = y.1960.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1960.b <- y.1960.b[order(y.1960.b$X),];y.1960.b$X <- NULL;rownames(y.1960.b) <- NULL 
y.1961.b$X <- gsub(' ', '.', y.1961.b$X);y.1961.b$X <- gsub('-', '.', y.1961.b$X);y.1961.b = y.1961.b[y.1961.b$X %in% full.info.countries.second.time.span.B,];y.1961.b = y.1961.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1961.b <- y.1961.b[order(y.1961.b$X),];y.1961.b$X <- NULL;rownames(y.1961.b) <- NULL 
y.1962.b$X <- gsub(' ', '.', y.1962.b$X);y.1962.b$X <- gsub('-', '.', y.1962.b$X);y.1962.b = y.1962.b[y.1962.b$X %in% full.info.countries.second.time.span.B,];y.1962.b = y.1962.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1962.b <- y.1962.b[order(y.1962.b$X),];y.1962.b$X <- NULL;rownames(y.1962.b) <- NULL 
y.1963.b$X <- gsub(' ', '.', y.1963.b$X);y.1963.b$X <- gsub('-', '.', y.1963.b$X);y.1963.b = y.1963.b[y.1963.b$X %in% full.info.countries.second.time.span.B,];y.1963.b = y.1963.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1963.b <- y.1963.b[order(y.1963.b$X),];y.1963.b$X <- NULL;rownames(y.1963.b) <- NULL 
y.1964.b$X <- gsub(' ', '.', y.1964.b$X);y.1964.b$X <- gsub('-', '.', y.1964.b$X);y.1964.b = y.1964.b[y.1964.b$X %in% full.info.countries.second.time.span.B,];y.1964.b = y.1964.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1964.b <- y.1964.b[order(y.1964.b$X),];y.1964.b$X <- NULL;rownames(y.1964.b) <- NULL 
y.1965.b$X <- gsub(' ', '.', y.1965.b$X);y.1965.b$X <- gsub('-', '.', y.1965.b$X);y.1965.b = y.1965.b[y.1965.b$X %in% full.info.countries.second.time.span.B,];y.1965.b = y.1965.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1965.b <- y.1965.b[order(y.1965.b$X),];y.1965.b$X <- NULL;rownames(y.1965.b) <- NULL 
y.1966.b$X <- gsub(' ', '.', y.1966.b$X);y.1966.b$X <- gsub('-', '.', y.1966.b$X);y.1966.b = y.1966.b[y.1966.b$X %in% full.info.countries.second.time.span.B,];y.1966.b = y.1966.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1966.b <- y.1966.b[order(y.1966.b$X),];y.1966.b$X <- NULL;rownames(y.1966.b) <- NULL 
y.1967.b$X <- gsub(' ', '.', y.1967.b$X);y.1967.b$X <- gsub('-', '.', y.1967.b$X);y.1967.b = y.1967.b[y.1967.b$X %in% full.info.countries.second.time.span.B,];y.1967.b = y.1967.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1967.b <- y.1967.b[order(y.1967.b$X),];y.1967.b$X <- NULL;rownames(y.1967.b) <- NULL 
y.1968.b$X <- gsub(' ', '.', y.1968.b$X);y.1968.b$X <- gsub('-', '.', y.1968.b$X);y.1968.b = y.1968.b[y.1968.b$X %in% full.info.countries.second.time.span.B,];y.1968.b = y.1968.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1968.b <- y.1968.b[order(y.1968.b$X),];y.1968.b$X <- NULL;rownames(y.1968.b) <- NULL 
y.1969.b$X <- gsub(' ', '.', y.1969.b$X);y.1969.b$X <- gsub('-', '.', y.1969.b$X);y.1969.b = y.1969.b[y.1969.b$X %in% full.info.countries.second.time.span.B,];y.1969.b = y.1969.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1969.b <- y.1969.b[order(y.1969.b$X),];y.1969.b$X <- NULL;rownames(y.1969.b) <- NULL 
y.1970.b$X <- gsub(' ', '.', y.1970.b$X);y.1970.b$X <- gsub('-', '.', y.1970.b$X);y.1970.b = y.1970.b[y.1970.b$X %in% full.info.countries.second.time.span.B,];y.1970.b = y.1970.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1970.b <- y.1970.b[order(y.1970.b$X),];y.1970.b$X <- NULL;rownames(y.1970.b) <- NULL 
y.1971.b$X <- gsub(' ', '.', y.1971.b$X);y.1971.b$X <- gsub('-', '.', y.1971.b$X);y.1971.b = y.1971.b[y.1971.b$X %in% full.info.countries.second.time.span.B,];y.1971.b = y.1971.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1971.b <- y.1971.b[order(y.1971.b$X),];y.1971.b$X <- NULL;rownames(y.1971.b) <- NULL 
y.1972.b$X <- gsub(' ', '.', y.1972.b$X);y.1972.b$X <- gsub('-', '.', y.1972.b$X);y.1972.b = y.1972.b[y.1972.b$X %in% full.info.countries.second.time.span.B,];y.1972.b = y.1972.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1972.b <- y.1972.b[order(y.1972.b$X),];y.1972.b$X <- NULL;rownames(y.1972.b) <- NULL 
y.1973.b$X <- gsub(' ', '.', y.1973.b$X);y.1973.b$X <- gsub('-', '.', y.1973.b$X);y.1973.b = y.1973.b[y.1973.b$X %in% full.info.countries.second.time.span.B,];y.1973.b = y.1973.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1973.b <- y.1973.b[order(y.1973.b$X),];y.1973.b$X <- NULL;rownames(y.1973.b) <- NULL 
y.1974.b$X <- gsub(' ', '.', y.1974.b$X);y.1974.b$X <- gsub('-', '.', y.1974.b$X);y.1974.b = y.1974.b[y.1974.b$X %in% full.info.countries.second.time.span.B,];y.1974.b = y.1974.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1974.b <- y.1974.b[order(y.1974.b$X),];y.1974.b$X <- NULL;rownames(y.1974.b) <- NULL 
y.1975.b$X <- gsub(' ', '.', y.1975.b$X);y.1975.b$X <- gsub('-', '.', y.1975.b$X);y.1975.b = y.1975.b[y.1975.b$X %in% full.info.countries.second.time.span.B,];y.1975.b = y.1975.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1975.b <- y.1975.b[order(y.1975.b$X),];y.1975.b$X <- NULL;rownames(y.1975.b) <- NULL 
y.1976.b$X <- gsub(' ', '.', y.1976.b$X);y.1976.b$X <- gsub('-', '.', y.1976.b$X);y.1976.b = y.1976.b[y.1976.b$X %in% full.info.countries.second.time.span.B,];y.1976.b = y.1976.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1976.b <- y.1976.b[order(y.1976.b$X),];y.1976.b$X <- NULL;rownames(y.1976.b) <- NULL 
y.1977.b$X <- gsub(' ', '.', y.1977.b$X);y.1977.b$X <- gsub('-', '.', y.1977.b$X);y.1977.b = y.1977.b[y.1977.b$X %in% full.info.countries.second.time.span.B,];y.1977.b = y.1977.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1977.b <- y.1977.b[order(y.1977.b$X),];y.1977.b$X <- NULL;rownames(y.1977.b) <- NULL 
y.1978.b$X <- gsub(' ', '.', y.1978.b$X);y.1978.b$X <- gsub('-', '.', y.1978.b$X);y.1978.b = y.1978.b[y.1978.b$X %in% full.info.countries.second.time.span.B,];y.1978.b = y.1978.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1978.b <- y.1978.b[order(y.1978.b$X),];y.1978.b$X <- NULL;rownames(y.1978.b) <- NULL 
y.1979.b$X <- gsub(' ', '.', y.1979.b$X);y.1979.b$X <- gsub('-', '.', y.1979.b$X);y.1979.b = y.1979.b[y.1979.b$X %in% full.info.countries.second.time.span.B,];y.1979.b = y.1979.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1979.b <- y.1979.b[order(y.1979.b$X),];y.1979.b$X <- NULL;rownames(y.1979.b) <- NULL 
y.1980.b$X <- gsub(' ', '.', y.1980.b$X);y.1980.b$X <- gsub('-', '.', y.1980.b$X);y.1980.b = y.1980.b[y.1980.b$X %in% full.info.countries.second.time.span.B,];y.1980.b = y.1980.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1980.b <- y.1980.b[order(y.1980.b$X),];y.1980.b$X <- NULL;rownames(y.1980.b) <- NULL 
y.1981.b$X <- gsub(' ', '.', y.1981.b$X);y.1981.b$X <- gsub('-', '.', y.1981.b$X);y.1981.b = y.1981.b[y.1981.b$X %in% full.info.countries.second.time.span.B,];y.1981.b = y.1981.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1981.b <- y.1981.b[order(y.1981.b$X),];y.1981.b$X <- NULL;rownames(y.1981.b) <- NULL 
y.1982.b$X <- gsub(' ', '.', y.1982.b$X);y.1982.b$X <- gsub('-', '.', y.1982.b$X);y.1982.b = y.1982.b[y.1982.b$X %in% full.info.countries.second.time.span.B,];y.1982.b = y.1982.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1982.b <- y.1982.b[order(y.1982.b$X),];y.1982.b$X <- NULL;rownames(y.1982.b) <- NULL 
y.1983.b$X <- gsub(' ', '.', y.1983.b$X);y.1983.b$X <- gsub('-', '.', y.1983.b$X);y.1983.b = y.1983.b[y.1983.b$X %in% full.info.countries.second.time.span.B,];y.1983.b = y.1983.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1983.b <- y.1983.b[order(y.1983.b$X),];y.1983.b$X <- NULL;rownames(y.1983.b) <- NULL 
y.1984.b$X <- gsub(' ', '.', y.1984.b$X);y.1984.b$X <- gsub('-', '.', y.1984.b$X);y.1984.b = y.1984.b[y.1984.b$X %in% full.info.countries.second.time.span.B,];y.1984.b = y.1984.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1984.b <- y.1984.b[order(y.1984.b$X),];y.1984.b$X <- NULL;rownames(y.1984.b) <- NULL 
y.1985.b$X <- gsub(' ', '.', y.1985.b$X);y.1985.b$X <- gsub('-', '.', y.1985.b$X);y.1985.b = y.1985.b[y.1985.b$X %in% full.info.countries.second.time.span.B,];y.1985.b = y.1985.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1985.b <- y.1985.b[order(y.1985.b$X),];y.1985.b$X <- NULL;rownames(y.1985.b) <- NULL 
y.1986.b$X <- gsub(' ', '.', y.1986.b$X);y.1986.b$X <- gsub('-', '.', y.1986.b$X);y.1986.b = y.1986.b[y.1986.b$X %in% full.info.countries.second.time.span.B,];y.1986.b = y.1986.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1986.b <- y.1986.b[order(y.1986.b$X),];y.1986.b$X <- NULL;rownames(y.1986.b) <- NULL 
y.1987.b$X <- gsub(' ', '.', y.1987.b$X);y.1987.b$X <- gsub('-', '.', y.1987.b$X);y.1987.b = y.1987.b[y.1987.b$X %in% full.info.countries.second.time.span.B,];y.1987.b = y.1987.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1987.b <- y.1987.b[order(y.1987.b$X),];y.1987.b$X <- NULL;rownames(y.1987.b) <- NULL 
y.1988.b$X <- gsub(' ', '.', y.1988.b$X);y.1988.b$X <- gsub('-', '.', y.1988.b$X);y.1988.b = y.1988.b[y.1988.b$X %in% full.info.countries.second.time.span.B,];y.1988.b = y.1988.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1988.b <- y.1988.b[order(y.1988.b$X),];y.1988.b$X <- NULL;rownames(y.1988.b) <- NULL 
y.1989.b$X <- gsub(' ', '.', y.1989.b$X);y.1989.b$X <- gsub('-', '.', y.1989.b$X);y.1989.b = y.1989.b[y.1989.b$X %in% full.info.countries.second.time.span.B,];y.1989.b = y.1989.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1989.b <- y.1989.b[order(y.1989.b$X),];y.1989.b$X <- NULL;rownames(y.1989.b) <- NULL 
y.1990.b$X <- gsub(' ', '.', y.1990.b$X);y.1990.b$X <- gsub('-', '.', y.1990.b$X);y.1990.b = y.1990.b[y.1990.b$X %in% full.info.countries.second.time.span.B,];y.1990.b = y.1990.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1990.b <- y.1990.b[order(y.1990.b$X),];y.1990.b$X <- NULL;rownames(y.1990.b) <- NULL 
y.1991.b$X <- gsub(' ', '.', y.1991.b$X);y.1991.b$X <- gsub('-', '.', y.1991.b$X);y.1991.b = y.1991.b[y.1991.b$X %in% full.info.countries.second.time.span.B,];y.1991.b = y.1991.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1991.b <- y.1991.b[order(y.1991.b$X),];y.1991.b$X <- NULL;rownames(y.1991.b) <- NULL 
y.1992.b$X <- gsub(' ', '.', y.1992.b$X);y.1992.b$X <- gsub('-', '.', y.1992.b$X);y.1992.b = y.1992.b[y.1992.b$X %in% full.info.countries.second.time.span.B,];y.1992.b = y.1992.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1992.b <- y.1992.b[order(y.1992.b$X),];y.1992.b$X <- NULL;rownames(y.1992.b) <- NULL 
y.1993.b$X <- gsub(' ', '.', y.1993.b$X);y.1993.b$X <- gsub('-', '.', y.1993.b$X);y.1993.b = y.1993.b[y.1993.b$X %in% full.info.countries.second.time.span.B,];y.1993.b = y.1993.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1993.b <- y.1993.b[order(y.1993.b$X),];y.1993.b$X <- NULL;rownames(y.1993.b) <- NULL 
y.1994.b$X <- gsub(' ', '.', y.1994.b$X);y.1994.b$X <- gsub('-', '.', y.1994.b$X);y.1994.b = y.1994.b[y.1994.b$X %in% full.info.countries.second.time.span.B,];y.1994.b = y.1994.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1994.b <- y.1994.b[order(y.1994.b$X),];y.1994.b$X <- NULL;rownames(y.1994.b) <- NULL 
y.1995.b$X <- gsub(' ', '.', y.1995.b$X);y.1995.b$X <- gsub('-', '.', y.1995.b$X);y.1995.b = y.1995.b[y.1995.b$X %in% full.info.countries.second.time.span.B,];y.1995.b = y.1995.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1995.b <- y.1995.b[order(y.1995.b$X),];y.1995.b$X <- NULL;rownames(y.1995.b) <- NULL 
y.1996.b$X <- gsub(' ', '.', y.1996.b$X);y.1996.b$X <- gsub('-', '.', y.1996.b$X);y.1996.b = y.1996.b[y.1996.b$X %in% full.info.countries.second.time.span.B,];y.1996.b = y.1996.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1996.b <- y.1996.b[order(y.1996.b$X),];y.1996.b$X <- NULL;rownames(y.1996.b) <- NULL 
y.1997.b$X <- gsub(' ', '.', y.1997.b$X);y.1997.b$X <- gsub('-', '.', y.1997.b$X);y.1997.b = y.1997.b[y.1997.b$X %in% full.info.countries.second.time.span.B,];y.1997.b = y.1997.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1997.b <- y.1997.b[order(y.1997.b$X),];y.1997.b$X <- NULL;rownames(y.1997.b) <- NULL 
y.1998.b$X <- gsub(' ', '.', y.1998.b$X);y.1998.b$X <- gsub('-', '.', y.1998.b$X);y.1998.b = y.1998.b[y.1998.b$X %in% full.info.countries.second.time.span.B,];y.1998.b = y.1998.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1998.b <- y.1998.b[order(y.1998.b$X),];y.1998.b$X <- NULL;rownames(y.1998.b) <- NULL 
y.1999.b$X <- gsub(' ', '.', y.1999.b$X);y.1999.b$X <- gsub('-', '.', y.1999.b$X);y.1999.b = y.1999.b[y.1999.b$X %in% full.info.countries.second.time.span.B,];y.1999.b = y.1999.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.1999.b <- y.1999.b[order(y.1999.b$X),];y.1999.b$X <- NULL;rownames(y.1999.b) <- NULL 
y.2000.b$X <- gsub(' ', '.', y.2000.b$X);y.2000.b$X <- gsub('-', '.', y.2000.b$X);y.2000.b = y.2000.b[y.2000.b$X %in% full.info.countries.second.time.span.B,];y.2000.b = y.2000.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2000.b <- y.2000.b[order(y.2000.b$X),];y.2000.b$X <- NULL;rownames(y.2000.b) <- NULL 
y.2001.b$X <- gsub(' ', '.', y.2001.b$X);y.2001.b$X <- gsub('-', '.', y.2001.b$X);y.2001.b = y.2001.b[y.2001.b$X %in% full.info.countries.second.time.span.B,];y.2001.b = y.2001.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2001.b <- y.2001.b[order(y.2001.b$X),];y.2001.b$X <- NULL;rownames(y.2001.b) <- NULL 
y.2002.b$X <- gsub(' ', '.', y.2002.b$X);y.2002.b$X <- gsub('-', '.', y.2002.b$X);y.2002.b = y.2002.b[y.2002.b$X %in% full.info.countries.second.time.span.B,];y.2002.b = y.2002.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2002.b <- y.2002.b[order(y.2002.b$X),];y.2002.b$X <- NULL;rownames(y.2002.b) <- NULL 
y.2003.b$X <- gsub(' ', '.', y.2003.b$X);y.2003.b$X <- gsub('-', '.', y.2003.b$X);y.2003.b = y.2003.b[y.2003.b$X %in% full.info.countries.second.time.span.B,];y.2003.b = y.2003.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2003.b <- y.2003.b[order(y.2003.b$X),];y.2003.b$X <- NULL;rownames(y.2003.b) <- NULL 
y.2004.b$X <- gsub(' ', '.', y.2004.b$X);y.2004.b$X <- gsub('-', '.', y.2004.b$X);y.2004.b = y.2004.b[y.2004.b$X %in% full.info.countries.second.time.span.B,];y.2004.b = y.2004.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2004.b <- y.2004.b[order(y.2004.b$X),];y.2004.b$X <- NULL;rownames(y.2004.b) <- NULL 
y.2005.b$X <- gsub(' ', '.', y.2005.b$X);y.2005.b$X <- gsub('-', '.', y.2005.b$X);y.2005.b = y.2005.b[y.2005.b$X %in% full.info.countries.second.time.span.B,];y.2005.b = y.2005.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2005.b <- y.2005.b[order(y.2005.b$X),];y.2005.b$X <- NULL;rownames(y.2005.b) <- NULL 
y.2006.b$X <- gsub(' ', '.', y.2006.b$X);y.2006.b$X <- gsub('-', '.', y.2006.b$X);y.2006.b = y.2006.b[y.2006.b$X %in% full.info.countries.second.time.span.B,];y.2006.b = y.2006.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2006.b <- y.2006.b[order(y.2006.b$X),];y.2006.b$X <- NULL;rownames(y.2006.b) <- NULL 
y.2007.b$X <- gsub(' ', '.', y.2007.b$X);y.2007.b$X <- gsub('-', '.', y.2007.b$X);y.2007.b = y.2007.b[y.2007.b$X %in% full.info.countries.second.time.span.B,];y.2007.b = y.2007.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2007.b <- y.2007.b[order(y.2007.b$X),];y.2007.b$X <- NULL;rownames(y.2007.b) <- NULL 
y.2008.b$X <- gsub(' ', '.', y.2008.b$X);y.2008.b$X <- gsub('-', '.', y.2008.b$X);y.2008.b = y.2008.b[y.2008.b$X %in% full.info.countries.second.time.span.B,];y.2008.b = y.2008.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2008.b <- y.2008.b[order(y.2008.b$X),];y.2008.b$X <- NULL;rownames(y.2008.b) <- NULL 
y.2009.b$X <- gsub(' ', '.', y.2009.b$X);y.2009.b$X <- gsub('-', '.', y.2009.b$X);y.2009.b = y.2009.b[y.2009.b$X %in% full.info.countries.second.time.span.B,];y.2009.b = y.2009.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2009.b <- y.2009.b[order(y.2009.b$X),];y.2009.b$X <- NULL;rownames(y.2009.b) <- NULL 
y.2010.b$X <- gsub(' ', '.', y.2010.b$X);y.2010.b$X <- gsub('-', '.', y.2010.b$X);y.2010.b = y.2010.b[y.2010.b$X %in% full.info.countries.second.time.span.B,];y.2010.b = y.2010.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2010.b <- y.2010.b[order(y.2010.b$X),];y.2010.b$X <- NULL;rownames(y.2010.b) <- NULL 
y.2011.b$X <- gsub(' ', '.', y.2011.b$X);y.2011.b$X <- gsub('-', '.', y.2011.b$X);y.2011.b = y.2011.b[y.2011.b$X %in% full.info.countries.second.time.span.B,];y.2011.b = y.2011.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2011.b <- y.2011.b[order(y.2011.b$X),];y.2011.b$X <- NULL;rownames(y.2011.b) <- NULL 
y.2012.b$X <- gsub(' ', '.', y.2012.b$X);y.2012.b$X <- gsub('-', '.', y.2012.b$X);y.2012.b = y.2012.b[y.2012.b$X %in% full.info.countries.second.time.span.B,];y.2012.b = y.2012.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2012.b <- y.2012.b[order(y.2012.b$X),];y.2012.b$X <- NULL;rownames(y.2012.b) <- NULL 
y.2013.b$X <- gsub(' ', '.', y.2013.b$X);y.2013.b$X <- gsub('-', '.', y.2013.b$X);y.2013.b = y.2013.b[y.2013.b$X %in% full.info.countries.second.time.span.B,];y.2013.b = y.2013.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2013.b <- y.2013.b[order(y.2013.b$X),];y.2013.b$X <- NULL;rownames(y.2013.b) <- NULL 
y.2014.b$X <- gsub(' ', '.', y.2014.b$X);y.2014.b$X <- gsub('-', '.', y.2014.b$X);y.2014.b = y.2014.b[y.2014.b$X %in% full.info.countries.second.time.span.B,];y.2014.b = y.2014.b %>% dplyr::select(c(X, sort(full.info.countries.second.time.span.B)));y.2014.b <- y.2014.b[order(y.2014.b$X),];y.2014.b$X <- NULL;rownames(y.2014.b) <- NULL 


# Building WM for the second period
wm.2.b = list(as.matrix(y.1955.b), as.matrix(y.1956.b), as.matrix(y.1957.b), as.matrix(y.1958.b), as.matrix(y.1959.b), as.matrix(y.1960.b), as.matrix(y.1961.b), as.matrix(y.1962.b), as.matrix(y.1963.b), as.matrix(y.1964.b), as.matrix(y.1965.b), as.matrix(y.1966.b), as.matrix(y.1967.b), as.matrix(y.1968.b), as.matrix(y.1969.b), as.matrix(y.1970.b), as.matrix(y.1971.b), as.matrix(y.1972.b), as.matrix(y.1973.b), as.matrix(y.1974.b), as.matrix(y.1975.b), as.matrix(y.1976.b), as.matrix(y.1977.b), as.matrix(y.1978.b), as.matrix(y.1979.b), as.matrix(y.1980.b), as.matrix(y.1981.b), as.matrix(y.1982.b), as.matrix(y.1983.b), as.matrix(y.1984.b), as.matrix(y.1985.b), as.matrix(y.1986.b), as.matrix(y.1987.b), as.matrix(y.1988.b), as.matrix(y.1989.b), as.matrix(y.1990.b), as.matrix(y.1991.b), as.matrix(y.1992.b), as.matrix(y.1993.b), as.matrix(y.1994.b), as.matrix(y.1995.b), as.matrix(y.1996.b), as.matrix(y.1997.b), as.matrix(y.1998.b), as.matrix(y.1999.b), as.matrix(y.2000.b), as.matrix(y.2001.b), as.matrix(y.2002.b), as.matrix(y.2003.b), as.matrix(y.2004.b), as.matrix(y.2005.b), as.matrix(y.2006.b), as.matrix(y.2007.b), as.matrix(y.2008.b), as.matrix(y.2009.b), as.matrix(y.2010.b), as.matrix(y.2011.b), as.matrix(y.2012.b), as.matrix(y.2013.b), as.matrix(y.2014.b)
)
## ----


########################################################
# GVARX
########################################################

# Present results like Table 4.5 in Box-Steffensmeier2014a, 121.

# GVARX
# https://www.rdocumentation.org/packages/GVARX/versions/1.3
# http://web.ntnu.edu.tw/~tsungwu/R_DevOps/GVAR/R_DevOps_GVAR_English.html
# data("tradeweightx")

# GVAR is developed by Pesaran2004

# Be adviced, this function only computes Granger causality tests for 
# BIVARIATE specifications. Will compute different models for both var types.

## Look at the pr(F-statistic)
# "the F test has the greatest power to determine the joint statistical significance of the coefficients on the lags of the variable hypothesized to Granger cause another variable. The null of no Granger causality is equivalent to the hypothesis that all these coefficients are jointly zero." Box-Steffensmeier2014a, p. 112
# F test tests if pi parameters ar jointly zero (Freeman1983, 333).
# "tests for the joint significance of the estimated regression parameters, the ir,j's, were based on F statistic" (Freeman1983, 346)
# https://stats.stackexchange.com/questions/131261/granger-causality-interpretation-using-r/132527


########################################################
# First Period
########################################################

## ---- gvar:model:first:period ----

p_load(GVARX)

p.1=2 # The number of lag for Xt matrix
FLag.1=2 # The number of lag for foreign variables in country-specific VAR
lag.max.1=5 # The maximal number of lag for estimating country-specific VAR
type.1="both" # Model specificaiton for VAR. As in package vars, we have four selection: "none","const","trend", "both".
ic.1="AIC" # Information criteria for optimal lag.As in package vars, we have four selection: "AIC", "HQ", "SC", and "FPE".

options(scipen=9999999)
mainOUTPUT.1 = GVECMest(
        data = cow.d.1,
        p = p.1,
        FLag = FLag.1,
        lag.max = lag.max.1,
        type = type.1,
        ic = ic.1,
        weight.matrix=wm.1)

# Store values
# summary(mainOUTPUT.1$gvecm[[9]])

## P-values
p.1.Austria.Hungary.pvalue.1 = round(0.01748, 3)
p.1.Austria.Hungary.pvalue.2 = round(0.2383, 3)

p.1.Belgium.pvalue.1 = round(0.001246 , 3)
p.1.Belgium.pvalue.2 = round(0.0167, 3)

p.1.France.pvalue.1 = round(0.257, 3)
p.1.France.pvalue.2 = round(0.09469, 3)

p.1.Germany.pvalue.1 = round(0.003133 , 3)
p.1.Germany.pvalue.2 = round(0.02249, 3)

p.1.Italy.pvalue.1 = round(0.003514, 3)
p.1.Italy.pvalue.2 = round(0.000335, 3)

p.1.Russia.pvalue.1 = round(0.000002816, 3)
p.1.Russia.pvalue.2 = round(0.0339, 3)

p.1.Spain.pvalue.1 = round(0.1263, 3)
p.1.Spain.pvalue.2 = round(0.2142, 3)

p.1.United.Kingdom.pvalue.1 = round(0.02331, 3)
p.1.United.Kingdom.pvalue.2 = round(0.2845, 3)

p.1.United.States.pvalue.1 = round(0.04415, 3)
p.1.United.States.pvalue.2 = round(0.000161, 3)


### Austria.Hungary
## Ftests
p.1.Austria.Hungary.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.milper.fstatistic.value"]), 3)
p.1.Austria.Hungary.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.1.Austria.Hungary.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.1.Austria.Hungary.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.1.Austria.Hungary.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.milper.adj.r.squared"]), 3) 
# Rsq 2
p.1.Austria.Hungary.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.irst.adj.r.squared"]), 3) 
# Lags
p.1.Austria.Hungary.lags = mainOUTPUT.1$lagmatrix$lags[1] # Lag for the first country 

### Belgium
## Ftests
p.1.Belgium.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[2]])))["varresult.Belgium.milper.fstatistic.value"]), 3)
p.1.Belgium.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[2]])))["varresult.Belgium.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.1.Belgium.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[2]])))["varresult.Belgium.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[2]])))["varresult.Belgium.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.1.Belgium.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[2]])))["varresult.Belgium.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[2]])))["varresult.Belgium.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.1.Belgium.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[2]])))["varresult.Belgium.milper.adj.r.squared"]), 3) 
# Rsq 2
p.1.Belgium.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[2]])))["varresult.Belgium.irst.adj.r.squared"]), 3) 
# Lags
p.1.Belgium.lags = mainOUTPUT.1$lagmatrix$lags[2] # Lag for the first country 

### France
## Ftests
p.1.France.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[3]])))["varresult.France.milper.fstatistic.value"]), 3)
p.1.France.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[3]])))["varresult.France.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.1.France.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[3]])))["varresult.France.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[3]])))["varresult.France.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.1.France.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[3]])))["varresult.France.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[3]])))["varresult.France.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.1.France.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[3]])))["varresult.France.milper.adj.r.squared"]), 3) 
# Rsq 2
p.1.France.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[3]])))["varresult.France.irst.adj.r.squared"]), 3) 
# Lags
p.1.France.lags = mainOUTPUT.1$lagmatrix$lags[3] # Lag for the first country 

### Germany
## Ftests
p.1.Germany.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[4]])))["varresult.Germany.milper.fstatistic.value"]), 3)
p.1.Germany.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[4]])))["varresult.Germany.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.1.Germany.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[4]])))["varresult.Germany.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[4]])))["varresult.Germany.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.1.Germany.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[4]])))["varresult.Germany.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[4]])))["varresult.Germany.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.1.Germany.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[4]])))["varresult.Germany.milper.adj.r.squared"]), 3) 
# Rsq 2
p.1.Germany.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[4]])))["varresult.Germany.irst.adj.r.squared"]), 3) 
# Lags
p.1.Germany.lags = mainOUTPUT.1$lagmatrix$lags[4] # Lag for the first country 

### Italy
## Ftests
p.1.Italy.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[5]])))["varresult.Italy.milper.fstatistic.value"]), 3)
p.1.Italy.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[5]])))["varresult.Italy.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.1.Italy.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[5]])))["varresult.Italy.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[5]])))["varresult.Italy.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.1.Italy.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[5]])))["varresult.Italy.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[5]])))["varresult.Italy.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.1.Italy.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[5]])))["varresult.Italy.milper.adj.r.squared"]), 3) 
# Rsq 2
p.1.Italy.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[5]])))["varresult.Italy.irst.adj.r.squared"]), 3) 
# Lags
p.1.Italy.lags = mainOUTPUT.1$lagmatrix$lags[5] # Lag for the first country 

### Russia
## Ftests
p.1.Russia.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[6]])))["varresult.Russia.milper.fstatistic.value"]), 3)
p.1.Russia.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[6]])))["varresult.Russia.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.1.Russia.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[6]])))["varresult.Russia.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[6]])))["varresult.Russia.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.1.Russia.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[6]])))["varresult.Russia.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[6]])))["varresult.Russia.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.1.Russia.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[6]])))["varresult.Russia.milper.adj.r.squared"]), 3) 
# Rsq 2
p.1.Russia.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[6]])))["varresult.Russia.irst.adj.r.squared"]), 3) 
# Lags
p.1.Russia.lags = mainOUTPUT.1$lagmatrix$lags[6] # Lag for the first country 

### Spain
## Ftests
p.1.Spain.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[7]])))["varresult.Spain.milper.fstatistic.value"]), 3)
p.1.Spain.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[7]])))["varresult.Spain.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.1.Spain.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[7]])))["varresult.Spain.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[7]])))["varresult.Spain.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.1.Spain.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[7]])))["varresult.Spain.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[7]])))["varresult.Spain.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.1.Spain.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[7]])))["varresult.Spain.milper.adj.r.squared"]), 3) 
# Rsq 2
p.1.Spain.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[7]])))["varresult.Spain.irst.adj.r.squared"]), 3) 
# Lags
p.1.Spain.lags = mainOUTPUT.1$lagmatrix$lags[7] # Lag for the first country 

### United.Kingdom
## Ftests
p.1.United.Kingdom.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[8]])))["varresult.United.Kingdom.milper.fstatistic.value"]), 3)
p.1.United.Kingdom.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[8]])))["varresult.United.Kingdom.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.1.United.Kingdom.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[8]])))["varresult.United.Kingdom.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[8]])))["varresult.United.Kingdom.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.1.United.Kingdom.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[8]])))["varresult.United.Kingdom.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[8]])))["varresult.United.Kingdom.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.1.United.Kingdom.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[8]])))["varresult.United.Kingdom.milper.adj.r.squared"]), 3) 
# Rsq 2
p.1.United.Kingdom.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[8]])))["varresult.United.Kingdom.irst.adj.r.squared"]), 3) 
# Lags
p.1.United.Kingdom.lags = mainOUTPUT.1$lagmatrix$lags[8] # Lag for the first country 

### United.States
## Ftests
p.1.United.States.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[9]])))["varresult.United.States.of.America.milper.fstatistic.value"]), 3)
p.1.United.States.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[9]])))["varresult.United.States.of.America.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.1.United.States.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[9]])))["varresult.United.States.of.America.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[9]])))["varresult.United.States.of.America.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.1.United.States.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.1$gvecm[[9]])))["varresult.United.States.of.America.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.1$gvecm[[9]])))["varresult.United.States.of.America.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.1.United.States.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[9]])))["varresult.United.States.of.America.milper.adj.r.squared"]), 3) 
# Rsq 2
p.1.United.States.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.1$gvecm[[9]])))["varresult.United.States.of.America.irst.adj.r.squared"]), 3) 
# Lags
p.1.United.States.lags = mainOUTPUT.1$lagmatrix$lags[9] # Lag for the first country 

## ----


summary(mainOUTPUT.1$gvecm[[1]]) # Austria.Hungary 
summary(mainOUTPUT.1$gvecm[[2]]) # Belgium 
summary(mainOUTPUT.1$gvecm[[3]]) # France 
summary(mainOUTPUT.1$gvecm[[4]]) # Germany 
summary(mainOUTPUT.1$gvecm[[5]]) # Italy 
summary(mainOUTPUT.1$gvecm[[6]]) # Russia 
summary(mainOUTPUT.1$gvecm[[7]]) # Spain 
summary(mainOUTPUT.1$gvecm[[8]]) # United Kingdom  
summary(mainOUTPUT.1$gvecm[[9]]) # United States


### Others
# unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))[1] # VD (1)
# unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))[2] # VD (2)
# unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.milper.aliased.const"] # Constant (1)
# unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.milper.aliased.trend"] # Trend (1)
# unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.irst.aliased.const"] # Constant (2)
# unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["varresult.Austria.Hungary.irst.aliased.trend"] # Trend (2) 
# unlist(list(summary(mainOUTPUT.1$gvecm[[1]])))["obs"] # Obs (N)








## List of Possible Pendings

### 1. Compare the dependency of residuals in VAR and GVAR.

# Compare the dependency of residuals in VAR and GVAR.
cor2_avg = averageCORgvecm(out=mainOUTPUT.1)
as.matrix((cor2_avg$vecmRSDcor)[[1]])
as.matrix((cor2_avg$vecmRSDcor)[[2]])


## https://www.rdocumentation.org/packages/GVARX/versions/1.3/topics/averageCORgvecm
data.frame(cor2_avg$vecmRSDcor) # Average residual correlations of country-specific VECM
data.frame(cor2_avg$gvecmRSDcor) # Average residual correlations of country-specific VECM augmented by foreign variables(GVECM)

### 2. Estimate Country-Specific Johansen Test Results In A Global VECM Setting
p.1=2
FLag.1=2
type="const"
ic="SC"
weight.matrix=tradeweight1

mainOUT.JO.1 = GVECM.jo(
        data = cow.d.1,
        p = p.1,
        FLag = FLag.1, 
        weight.matrix = wm.1
        )

mainOUT.JO.1$JO.test

########################################################
# Second Period (Small Countries)
########################################################

## ---- gvar:model:second:period ----

p_load(GVARX)

p.2=2 # The number of lag for Xt matrix
FLag.2=2 # The number of lag for foreign variables in country-specific VAR
lag.max.2=5 # The maximal number of lag for estimating country-specific VAR
type.2="none" # Model specificaiton for VAR. As in package vars, we have four selection: "none","const","trend", "both".
ic.2="AIC" # Information criteria for optimal lag.As in package vars, we have four selection: "AIC", "HQ", "SC", and "FPE".

options(scipen=9999999)
mainOUTPUT.2 = GVECMest(
        data = cow.d.2,
        p = p.2,
        FLag = FLag.2,
        lag.max = lag.max.2,
        type = type.2,
        ic = ic.2,
        weight.matrix=wm.2)

# Storing Values

## P-values
# summary(mainOUTPUT.2$gvecm[[1]]) # Argentina 
p.2.Argentina.pvalue.1 = round(0.568, 3)
p.2.Argentina.pvalue.2 = round(0.0001175, 3)

# summary(mainOUTPUT.2$gvecm[[2]]) # Australia 
p.2.Australia.pvalue.1 = round(0.6493, 3)
p.2.Australia.pvalue.2 = round(0.00001349, 3)

# summary(mainOUTPUT.2$gvecm[[3]]) # Austria 
p.2.Austria.pvalue.1 = round(0.7261, 3)
p.2.Austria.pvalue.2 = round(0.02396, 3)

# summary(mainOUTPUT.2$gvecm[[4]]) # Belgium 
p.2.Belgium.pvalue.1 = round(0.002088, 3)
p.2.Belgium.pvalue.2 = round(0.0049, 3)

# summary(mainOUTPUT.2$gvecm[[5]]) # Brazil 
p.2.Brazil.pvalue.1 = round(0.9103, 3)
p.2.Brazil.pvalue.2 = round(0.0000004244, 3)

# summary(mainOUTPUT.2$gvecm[[6]]) # Bulgaria 
p.2.Bulgaria.pvalue.1 = round(0.5803, 3)
p.2.Bulgaria.pvalue.2 = round(0.00001425, 3)

# summary(mainOUTPUT.2$gvecm[[7]]) # Canada 
p.2.Canada.pvalue.1 = round(0.2202, 3)
p.2.Canada.pvalue.2 = round(0.01886, 3)

# summary(mainOUTPUT.2$gvecm[[8]]) # Chile 
p.2.Chile.pvalue.1 = round(0.9693, 3)
p.2.Chile.pvalue.2 = round(0.00001052, 3)

# summary(mainOUTPUT.2$gvecm[[9]]) # Colombia 
p.2.Colombia.pvalue.1 = round(0.0008545, 3)
p.2.Colombia.pvalue.2 = round(0.004422, 3)

# summary(mainOUTPUT.2$gvecm[[10]]) # Egypt 
p.2.Egypt.pvalue.1 = round(0.9546, 3)
p.2.Egypt.pvalue.2 = round(0.0133, 3)

# summary(mainOUTPUT.2$gvecm[[11]]) # Finland 
p.2.Finland.pvalue.1 = round(0.0932, 3)
p.2.Finland.pvalue.2 = round(0.0000006223, 3)

# summary(mainOUTPUT.2$gvecm[[12]]) # France 
p.2.France.pvalue.1 = round(0.3429, 3)
p.2.France.pvalue.2 = round(0.1157, 3)

# summary(mainOUTPUT.2$gvecm[[13]]) # Greece 
p.2.Greece.pvalue.1 = round(0.02343, 3)
p.2.Greece.pvalue.2 = round(0.006374, 3)

# summary(mainOUTPUT.2$gvecm[[14]]) # Hungary 
p.2.Hungary.pvalue.1 = round(0.01072, 3)
p.2.Hungary.pvalue.2 = round(0.001666, 3)

# summary(mainOUTPUT.2$gvecm[[15]]) # India 
p.2.India.pvalue.1 = round(0.8474, 3)
p.2.India.pvalue.2 = round(0.00000006172, 3)

# summary(mainOUTPUT.2$gvecm[[16]]) # Israel 
p.2.Israel.pvalue.1 = round(0.6225, 3)
p.2.Israel.pvalue.2 = round(0.588, 3)

# summary(mainOUTPUT.2$gvecm[[17]]) # Italy 
p.2.Italy.pvalue.1 = round(0.1688, 3)
p.2.Italy.pvalue.2 = round(0.0005577, 3)

# summary(mainOUTPUT.2$gvecm[[18]]) # Japan 
p.2.Japan.pvalue.1 = round(0.01465, 3)
p.2.Japan.pvalue.2 = round(0.0005064, 3)

# summary(mainOUTPUT.2$gvecm[[19]]) # Luxembourg 
p.2.Luxembourg.pvalue.1 = round(0.003431, 3)
p.2.Luxembourg.pvalue.2 = round(0.541, 3)

# summary(mainOUTPUT.2$gvecm[[20]]) # Mexico 
p.2.Mexico.pvalue.1 = round(0.2625, 3)
p.2.Mexico.pvalue.2 = round(0.0000008429, 3)

# summary(mainOUTPUT.2$gvecm[[21]]) # Netherlands 
p.2.Netherlands.pvalue.1 = round(0.001368, 3)
p.2.Netherlands.pvalue.2 = round(0.001453, 3)

# summary(mainOUTPUT.2$gvecm[[22]]) # North.Korea 
p.2.North.Korea.pvalue.1 = round(0.006032, 3)
p.2.North.Korea.pvalue.2 = round(0.01334, 3)

# summary(mainOUTPUT.2$gvecm[[23]]) # Norway 
p.2.Norway.pvalue.1 = round(0.1784, 3)
p.2.Norway.pvalue.2 = round(0.7044, 3)

# summary(mainOUTPUT.2$gvecm[[24]]) # Poland 
p.2.Poland.pvalue.1 = round(0.734, 3)
p.2.Poland.pvalue.2 = round(0.005378, 3)

# summary(mainOUTPUT.2$gvecm[[25]]) # Portugal 
p.2.Portugal.pvalue.1 = round(0.4042, 3)
p.2.Portugal.pvalue.2 = round(0.004184, 3)

# summary(mainOUTPUT.2$gvecm[[26]]) # Romania 
p.2.Romania.pvalue.1 = round(0.4644, 3)
p.2.Romania.pvalue.2 = round(0.0007191, 3)

# summary(mainOUTPUT.2$gvecm[[27]]) # South.Africa 
p.2.South.Africa.pvalue.1 = round(0.9124, 3)
p.2.South.Africa.pvalue.2 = round(0.547, 3)

# summary(mainOUTPUT.2$gvecm[[28]]) # South.Korea 
p.2.South.Korea.pvalue.1 = round(0.9238, 3)
p.2.South.Korea.pvalue.2 = round(0.000003068, 3)

# summary(mainOUTPUT.2$gvecm[[29]]) # Spain 
p.2.Spain.pvalue.1 = round(0.0005994, 3)
p.2.Spain.pvalue.2 = round(0.002212, 3)

# summary(mainOUTPUT.2$gvecm[[30]]) # Taiwan 
p.2.Taiwan.pvalue.1 = round(0.06379, 3)
p.2.Taiwan.pvalue.2 = round(0.00002633, 3)

# summary(mainOUTPUT.2$gvecm[[31]]) # Turkey 
p.2.Turkey.pvalue.1 = round(0.1567, 3)
p.2.Turkey.pvalue.2 = round(0.00000000000092, 3)

# summary(mainOUTPUT.2$gvecm[[32]]) # United.Kingdom 
p.2.United.Kingdom.pvalue.1 = round(0.00000000001582, 3)
p.2.United.Kingdom.pvalue.2 = round(0.4036, 3)


### Argentina
## Ftests
p.2.Argentina.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[1]])))["varresult.Argentina.milper.fstatistic.value"]), 3)
p.2.Argentina.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[1]])))["varresult.Argentina.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Argentina.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[1]])))["varresult.Argentina.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[1]])))["varresult.Argentina.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Argentina.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[1]])))["varresult.Argentina.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[1]])))["varresult.Argentina.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Argentina.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[1]])))["varresult.Argentina.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Argentina.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[1]])))["varresult.Argentina.irst.adj.r.squared"]), 3) 
# Lags
p.2.Argentina.lags = mainOUTPUT.2$lagmatrix$lags[1] # Lag for the first country 


### Australia
## Ftests
p.2.Australia.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[2]])))["varresult.Australia.milper.fstatistic.value"]), 3)
p.2.Australia.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[2]])))["varresult.Australia.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Australia.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[2]])))["varresult.Australia.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[2]])))["varresult.Australia.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Australia.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[2]])))["varresult.Australia.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[2]])))["varresult.Australia.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Australia.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[2]])))["varresult.Australia.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Australia.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[2]])))["varresult.Australia.irst.adj.r.squared"]), 3) 
# Lags
p.2.Australia.lags = mainOUTPUT.2$lagmatrix$lags[2] # Lag for the first country 


### Austria
## Ftests
p.2.Austria.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[3]])))["varresult.Austria.milper.fstatistic.value"]), 3)
p.2.Austria.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[3]])))["varresult.Austria.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Austria.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[3]])))["varresult.Austria.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[3]])))["varresult.Austria.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Austria.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[3]])))["varresult.Austria.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[3]])))["varresult.Austria.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Austria.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[3]])))["varresult.Austria.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Austria.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[3]])))["varresult.Austria.irst.adj.r.squared"]), 3) 
# Lags
p.2.Austria.lags = mainOUTPUT.2$lagmatrix$lags[3] # Lag for the first country 

### Belgium
## Ftests
p.2.Belgium.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[4]])))["varresult.Belgium.milper.fstatistic.value"]), 3)
p.2.Belgium.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[4]])))["varresult.Belgium.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Belgium.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[4]])))["varresult.Belgium.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[4]])))["varresult.Belgium.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Belgium.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[4]])))["varresult.Belgium.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[4]])))["varresult.Belgium.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Belgium.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[4]])))["varresult.Belgium.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Belgium.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[4]])))["varresult.Belgium.irst.adj.r.squared"]), 3) 
# Lags
p.2.Belgium.lags = mainOUTPUT.2$lagmatrix$lags[4] # Lag for the first country 


### Brazil
## Ftests
p.2.Brazil.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[5]])))["varresult.Brazil.milper.fstatistic.value"]), 3)
p.2.Brazil.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[5]])))["varresult.Brazil.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Brazil.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[5]])))["varresult.Brazil.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[5]])))["varresult.Brazil.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Brazil.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[5]])))["varresult.Brazil.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[5]])))["varresult.Brazil.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Brazil.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[5]])))["varresult.Brazil.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Brazil.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[5]])))["varresult.Brazil.irst.adj.r.squared"]), 3) 
# Lags
p.2.Brazil.lags = mainOUTPUT.2$lagmatrix$lags[5] # Lag for the first country 

### Bulgaria
## Ftests
p.2.Bulgaria.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[6]])))["varresult.Bulgaria.milper.fstatistic.value"]), 3)
p.2.Bulgaria.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[6]])))["varresult.Bulgaria.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Bulgaria.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[6]])))["varresult.Bulgaria.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[6]])))["varresult.Bulgaria.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Bulgaria.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[6]])))["varresult.Bulgaria.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[6]])))["varresult.Bulgaria.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Bulgaria.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[6]])))["varresult.Bulgaria.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Bulgaria.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[6]])))["varresult.Bulgaria.irst.adj.r.squared"]), 3) 
# Lags
p.2.Bulgaria.lags = mainOUTPUT.2$lagmatrix$lags[6] # Lag for the first country 

### Canada
## Ftests
p.2.Canada.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[7]])))["varresult.Canada.milper.fstatistic.value"]), 3)
p.2.Canada.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[7]])))["varresult.Canada.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Canada.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[7]])))["varresult.Canada.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[7]])))["varresult.Canada.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Canada.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[7]])))["varresult.Canada.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[7]])))["varresult.Canada.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Canada.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[7]])))["varresult.Canada.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Canada.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[7]])))["varresult.Canada.irst.adj.r.squared"]), 3) 
# Lags
p.2.Canada.lags = mainOUTPUT.2$lagmatrix$lags[7] # Lag for the first country 

### Chile
## Ftests
p.2.Chile.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[8]])))["varresult.Chile.milper.fstatistic.value"]), 3)
p.2.Chile.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[8]])))["varresult.Chile.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Chile.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[8]])))["varresult.Chile.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[8]])))["varresult.Chile.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Chile.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[8]])))["varresult.Chile.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[8]])))["varresult.Chile.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Chile.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[8]])))["varresult.Chile.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Chile.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[8]])))["varresult.Chile.irst.adj.r.squared"]), 3) 
# Lags
p.2.Chile.lags = mainOUTPUT.2$lagmatrix$lags[8] # Lag for the first country 


### Colombia
## Ftests
p.2.Colombia.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[9]])))["varresult.Colombia.milper.fstatistic.value"]), 3)
p.2.Colombia.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[9]])))["varresult.Colombia.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Colombia.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[9]])))["varresult.Colombia.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[9]])))["varresult.Colombia.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Colombia.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[9]])))["varresult.Colombia.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[9]])))["varresult.Colombia.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Colombia.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[9]])))["varresult.Colombia.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Colombia.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[9]])))["varresult.Colombia.irst.adj.r.squared"]), 3) 
# Lags
p.2.Colombia.lags = mainOUTPUT.2$lagmatrix$lags[9] # Lag for the first country 

### Egypt
## Ftests
p.2.Egypt.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[10]])))["varresult.Egypt.milper.fstatistic.value"]), 3)
p.2.Egypt.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[10]])))["varresult.Egypt.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Egypt.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[10]])))["varresult.Egypt.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[10]])))["varresult.Egypt.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Egypt.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[10]])))["varresult.Egypt.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[10]])))["varresult.Egypt.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Egypt.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[10]])))["varresult.Egypt.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Egypt.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[10]])))["varresult.Egypt.irst.adj.r.squared"]), 3) 
# Lags
p.2.Egypt.lags = mainOUTPUT.2$lagmatrix$lags[10] # Lag for the first country 

### Finland
## Ftests
p.2.Finland.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[11]])))["varresult.Finland.milper.fstatistic.value"]), 3)
p.2.Finland.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[11]])))["varresult.Finland.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Finland.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[11]])))["varresult.Finland.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[11]])))["varresult.Finland.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Finland.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[11]])))["varresult.Finland.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[11]])))["varresult.Finland.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Finland.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[11]])))["varresult.Finland.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Finland.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[11]])))["varresult.Finland.irst.adj.r.squared"]), 3) 
# Lags
p.2.Finland.lags = mainOUTPUT.2$lagmatrix$lags[11] # Lag for the first country 

### France
## Ftests
p.2.France.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[12]])))["varresult.France.milper.fstatistic.value"]), 3)
p.2.France.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[12]])))["varresult.France.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.France.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[12]])))["varresult.France.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[12]])))["varresult.France.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.France.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[12]])))["varresult.France.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[12]])))["varresult.France.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.France.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[12]])))["varresult.France.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.France.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[12]])))["varresult.France.irst.adj.r.squared"]), 3) 
# Lags
p.2.France.lags = mainOUTPUT.2$lagmatrix$lags[12] # Lag for the first country 

### Greece
## Ftests
p.2.Greece.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[13]])))["varresult.Greece.milper.fstatistic.value"]), 3)
p.2.Greece.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[13]])))["varresult.Greece.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Greece.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[13]])))["varresult.Greece.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[13]])))["varresult.Greece.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Greece.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[13]])))["varresult.Greece.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[13]])))["varresult.Greece.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Greece.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[13]])))["varresult.Greece.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Greece.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[13]])))["varresult.Greece.irst.adj.r.squared"]), 3) 
# Lags
p.2.Greece.lags = mainOUTPUT.2$lagmatrix$lags[13] # Lag for the first country 


### Hungary
## Ftests
p.2.Hungary.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[14]])))["varresult.Hungary.milper.fstatistic.value"]), 3)
p.2.Hungary.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[14]])))["varresult.Hungary.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Hungary.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[14]])))["varresult.Hungary.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[14]])))["varresult.Hungary.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Hungary.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[14]])))["varresult.Hungary.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[14]])))["varresult.Hungary.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Hungary.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[14]])))["varresult.Hungary.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Hungary.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[14]])))["varresult.Hungary.irst.adj.r.squared"]), 3) 
# Lags
p.2.Hungary.lags = mainOUTPUT.2$lagmatrix$lags[14] # Lag for the first country 


### India
## Ftests
p.2.India.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[15]])))["varresult.India.milper.fstatistic.value"]), 3)
p.2.India.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[15]])))["varresult.India.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.India.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[15]])))["varresult.India.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[15]])))["varresult.India.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.India.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[15]])))["varresult.India.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[15]])))["varresult.India.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.India.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[15]])))["varresult.India.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.India.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[15]])))["varresult.India.irst.adj.r.squared"]), 3) 
# Lags
p.2.India.lags = mainOUTPUT.2$lagmatrix$lags[15] # Lag for the first country 


### Israel
## Ftests
p.2.Israel.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[16]])))["varresult.Israel.milper.fstatistic.value"]), 3)
p.2.Israel.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[16]])))["varresult.Israel.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Israel.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[16]])))["varresult.Israel.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[16]])))["varresult.Israel.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Israel.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[16]])))["varresult.Israel.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[16]])))["varresult.Israel.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Israel.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[16]])))["varresult.Israel.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Israel.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[16]])))["varresult.Israel.irst.adj.r.squared"]), 3) 
# Lags
p.2.Israel.lags = mainOUTPUT.2$lagmatrix$lags[16] # Lag for the first country 


### Italy
## Ftests
p.2.Italy.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[17]])))["varresult.Italy.milper.fstatistic.value"]), 3)
p.2.Italy.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[17]])))["varresult.Italy.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Italy.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[17]])))["varresult.Italy.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[17]])))["varresult.Italy.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Italy.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[17]])))["varresult.Italy.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[17]])))["varresult.Italy.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Italy.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[17]])))["varresult.Italy.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Italy.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[17]])))["varresult.Italy.irst.adj.r.squared"]), 3) 
# Lags
p.2.Italy.lags = mainOUTPUT.2$lagmatrix$lags[17] # Lag for the first country 

### Japan
## Ftests
p.2.Japan.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[18]])))["varresult.Japan.milper.fstatistic.value"]), 3)
p.2.Japan.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[18]])))["varresult.Japan.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Japan.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[18]])))["varresult.Japan.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[18]])))["varresult.Japan.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Japan.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[18]])))["varresult.Japan.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[18]])))["varresult.Japan.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Japan.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[18]])))["varresult.Japan.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Japan.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[18]])))["varresult.Japan.irst.adj.r.squared"]), 3) 
# Lags
p.2.Japan.lags = mainOUTPUT.2$lagmatrix$lags[18] # Lag for the first country 

### Luxembourg
## Ftests
p.2.Luxembourg.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[19]])))["varresult.Luxembourg.milper.fstatistic.value"]), 3)
p.2.Luxembourg.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[19]])))["varresult.Luxembourg.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Luxembourg.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[19]])))["varresult.Luxembourg.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[19]])))["varresult.Luxembourg.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Luxembourg.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[19]])))["varresult.Luxembourg.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[19]])))["varresult.Luxembourg.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Luxembourg.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[19]])))["varresult.Luxembourg.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Luxembourg.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[19]])))["varresult.Luxembourg.irst.adj.r.squared"]), 3) 
# Lags
p.2.Luxembourg.lags = mainOUTPUT.2$lagmatrix$lags[19] # Lag for the first country 


### Mexico
## Ftests
p.2.Mexico.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[20]])))["varresult.Mexico.milper.fstatistic.value"]), 3)
p.2.Mexico.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[20]])))["varresult.Mexico.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Mexico.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[20]])))["varresult.Mexico.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[20]])))["varresult.Mexico.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Mexico.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[20]])))["varresult.Mexico.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[20]])))["varresult.Mexico.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Mexico.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[20]])))["varresult.Mexico.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Mexico.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[20]])))["varresult.Mexico.irst.adj.r.squared"]), 3) 
# Lags
p.2.Mexico.lags = mainOUTPUT.2$lagmatrix$lags[20] # Lag for the first country 

### Netherlands
## Ftests
p.2.Netherlands.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[21]])))["varresult.Netherlands.milper.fstatistic.value"]), 3)
p.2.Netherlands.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[21]])))["varresult.Netherlands.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Netherlands.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[21]])))["varresult.Netherlands.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[21]])))["varresult.Netherlands.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Netherlands.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[21]])))["varresult.Netherlands.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[21]])))["varresult.Netherlands.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Netherlands.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[21]])))["varresult.Netherlands.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Netherlands.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[21]])))["varresult.Netherlands.irst.adj.r.squared"]), 3) 
# Lags
p.2.Netherlands.lags = mainOUTPUT.2$lagmatrix$lags[21] # Lag for the first country 


### North.Korea
## Ftests
p.2.North.Korea.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[22]])))["varresult.North.Korea.milper.fstatistic.value"]), 3)
p.2.North.Korea.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[22]])))["varresult.North.Korea.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.North.Korea.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[22]])))["varresult.North.Korea.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[22]])))["varresult.North.Korea.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.North.Korea.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[22]])))["varresult.North.Korea.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[22]])))["varresult.North.Korea.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.North.Korea.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[22]])))["varresult.North.Korea.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.North.Korea.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[22]])))["varresult.North.Korea.irst.adj.r.squared"]), 3) 
# Lags
p.2.North.Korea.lags = mainOUTPUT.2$lagmatrix$lags[22] # Lag for the first country 

### Norway
## Ftests
p.2.Norway.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[23]])))["varresult.Norway.milper.fstatistic.value"]), 3)
p.2.Norway.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[23]])))["varresult.Norway.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Norway.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[23]])))["varresult.Norway.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[23]])))["varresult.Norway.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Norway.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[23]])))["varresult.Norway.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[23]])))["varresult.Norway.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Norway.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[23]])))["varresult.Norway.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Norway.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[23]])))["varresult.Norway.irst.adj.r.squared"]), 3) 
# Lags
p.2.Norway.lags = mainOUTPUT.2$lagmatrix$lags[23] # Lag for the first country 

### Poland
## Ftests
p.2.Poland.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[24]])))["varresult.Poland.milper.fstatistic.value"]), 3)
p.2.Poland.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[24]])))["varresult.Poland.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Poland.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[24]])))["varresult.Poland.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[24]])))["varresult.Poland.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Poland.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[24]])))["varresult.Poland.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[24]])))["varresult.Poland.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Poland.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[24]])))["varresult.Poland.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Poland.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[24]])))["varresult.Poland.irst.adj.r.squared"]), 3) 
# Lags
p.2.Poland.lags = mainOUTPUT.2$lagmatrix$lags[24] # Lag for the first country 

### Portugal
## Ftests
p.2.Portugal.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[25]])))["varresult.Portugal.milper.fstatistic.value"]), 3)
p.2.Portugal.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[25]])))["varresult.Portugal.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Portugal.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[25]])))["varresult.Portugal.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[25]])))["varresult.Portugal.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Portugal.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[25]])))["varresult.Portugal.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[25]])))["varresult.Portugal.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Portugal.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[25]])))["varresult.Portugal.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Portugal.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[25]])))["varresult.Portugal.irst.adj.r.squared"]), 3) 
# Lags
p.2.Portugal.lags = mainOUTPUT.2$lagmatrix$lags[25] # Lag for the first country 

### Romania
## Ftests
p.2.Romania.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[26]])))["varresult.Romania.milper.fstatistic.value"]), 3)
p.2.Romania.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[26]])))["varresult.Romania.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Romania.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[26]])))["varresult.Romania.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[26]])))["varresult.Romania.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Romania.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[26]])))["varresult.Romania.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[26]])))["varresult.Romania.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Romania.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[26]])))["varresult.Romania.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Romania.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[26]])))["varresult.Romania.irst.adj.r.squared"]), 3) 
# Lags
p.2.Romania.lags = mainOUTPUT.2$lagmatrix$lags[26] # Lag for the first country 


### South.Africa
## Ftests
p.2.South.Africa.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[27]])))["varresult.South.Africa.milper.fstatistic.value"]), 3)
p.2.South.Africa.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[27]])))["varresult.South.Africa.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.South.Africa.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[27]])))["varresult.South.Africa.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[27]])))["varresult.South.Africa.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.South.Africa.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[27]])))["varresult.South.Africa.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[27]])))["varresult.South.Africa.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.South.Africa.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[27]])))["varresult.South.Africa.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.South.Africa.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[27]])))["varresult.South.Africa.irst.adj.r.squared"]), 3) 
# Lags
p.2.South.Africa.lags = mainOUTPUT.2$lagmatrix$lags[27] # Lag for the first country 



### South.Korea
## Ftests
p.2.South.Korea.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[28]])))["varresult.South.Korea.milper.fstatistic.value"]), 3)
p.2.South.Korea.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[28]])))["varresult.South.Korea.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.South.Korea.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[28]])))["varresult.South.Korea.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[28]])))["varresult.South.Korea.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.South.Korea.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[28]])))["varresult.South.Korea.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[28]])))["varresult.South.Korea.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.South.Korea.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[28]])))["varresult.South.Korea.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.South.Korea.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[28]])))["varresult.South.Korea.irst.adj.r.squared"]), 3) 
# Lags
p.2.South.Korea.lags = mainOUTPUT.2$lagmatrix$lags[28] # Lag for the first country 


### Spain
## Ftests
p.2.Spain.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[29]])))["varresult.Spain.milper.fstatistic.value"]), 3)
p.2.Spain.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[29]])))["varresult.Spain.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Spain.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[29]])))["varresult.Spain.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[29]])))["varresult.Spain.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Spain.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[29]])))["varresult.Spain.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[29]])))["varresult.Spain.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Spain.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[29]])))["varresult.Spain.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Spain.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[29]])))["varresult.Spain.irst.adj.r.squared"]), 3) 
# Lags
p.2.Spain.lags = mainOUTPUT.2$lagmatrix$lags[29] # Lag for the first country 


### Taiwan
## Ftests
p.2.Taiwan.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[30]])))["varresult.Taiwan.milper.fstatistic.value"]), 3)
p.2.Taiwan.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[30]])))["varresult.Taiwan.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Taiwan.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[30]])))["varresult.Taiwan.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[30]])))["varresult.Taiwan.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Taiwan.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[30]])))["varresult.Taiwan.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[30]])))["varresult.Taiwan.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Taiwan.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[30]])))["varresult.Taiwan.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Taiwan.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[30]])))["varresult.Taiwan.irst.adj.r.squared"]), 3) 
# Lags
p.2.Taiwan.lags = mainOUTPUT.2$lagmatrix$lags[30] # Lag for the first country 

### Turkey
## Ftests
p.2.Turkey.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[31]])))["varresult.Turkey.milper.fstatistic.value"]), 3)
p.2.Turkey.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[31]])))["varresult.Turkey.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Turkey.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[31]])))["varresult.Turkey.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[31]])))["varresult.Turkey.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Turkey.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[31]])))["varresult.Turkey.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[31]])))["varresult.Turkey.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Turkey.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[31]])))["varresult.Turkey.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Turkey.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[31]])))["varresult.Turkey.irst.adj.r.squared"]), 3) 
# Lags
p.2.Turkey.lags = mainOUTPUT.2$lagmatrix$lags[31] # Lag for the first country 

### United.Kingdom
## Ftests
p.2.United.Kingdom.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[32]])))["varresult.United.Kingdom.milper.fstatistic.value"]), 3)
p.2.United.Kingdom.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[32]])))["varresult.United.Kingdom.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.United.Kingdom.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[32]])))["varresult.United.Kingdom.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[32]])))["varresult.United.Kingdom.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.United.Kingdom.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2$gvecm[[32]])))["varresult.United.Kingdom.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2$gvecm[[32]])))["varresult.United.Kingdom.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.United.Kingdom.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[32]])))["varresult.United.Kingdom.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.United.Kingdom.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2$gvecm[[32]])))["varresult.United.Kingdom.irst.adj.r.squared"]), 3) 
# Lags
p.2.United.Kingdom.lags = mainOUTPUT.2$lagmatrix$lags[32] # Lag for the first country 
## ----

summary(mainOUTPUT.2$gvecm[[1]]) # Argentina 
summary(mainOUTPUT.2$gvecm[[2]]) # Australia 
summary(mainOUTPUT.2$gvecm[[3]]) # Austria 
summary(mainOUTPUT.2$gvecm[[4]]) # Belgium 
summary(mainOUTPUT.2$gvecm[[5]]) # Brazil 
summary(mainOUTPUT.2$gvecm[[6]]) # Bulgaria 
summary(mainOUTPUT.2$gvecm[[7]]) # Canada 
summary(mainOUTPUT.2$gvecm[[8]]) # Chile 
summary(mainOUTPUT.2$gvecm[[9]]) # Colombia 
summary(mainOUTPUT.2$gvecm[[10]]) # Egypt 
summary(mainOUTPUT.2$gvecm[[11]]) # Finland 
summary(mainOUTPUT.2$gvecm[[12]]) # France 
summary(mainOUTPUT.2$gvecm[[13]]) # Greece 
summary(mainOUTPUT.2$gvecm[[14]]) # Hungary 
summary(mainOUTPUT.2$gvecm[[15]]) # India 
summary(mainOUTPUT.2$gvecm[[16]]) # Israel 
summary(mainOUTPUT.2$gvecm[[17]]) # Italy 
summary(mainOUTPUT.2$gvecm[[18]]) # Japan 
summary(mainOUTPUT.2$gvecm[[19]]) # Luxembourg 
summary(mainOUTPUT.2$gvecm[[20]]) # Mexico 
summary(mainOUTPUT.2$gvecm[[21]]) # Netherlands 
summary(mainOUTPUT.2$gvecm[[22]]) # North.Korea 
summary(mainOUTPUT.2$gvecm[[23]]) # Norway 
summary(mainOUTPUT.2$gvecm[[24]]) # Poland 
summary(mainOUTPUT.2$gvecm[[25]]) # Portugal 
summary(mainOUTPUT.2$gvecm[[26]]) # Romania 
summary(mainOUTPUT.2$gvecm[[27]]) # South.Africa 
summary(mainOUTPUT.2$gvecm[[28]]) # South.Korea 
summary(mainOUTPUT.2$gvecm[[29]]) # Spain 
summary(mainOUTPUT.2$gvecm[[30]]) # Taiwan 
summary(mainOUTPUT.2$gvecm[[31]]) # Turkey 
summary(mainOUTPUT.2$gvecm[[32]]) # United.Kingdom 


########################################################
# Second Period (Big Countries)
########################################################

## ---- gvar:model:second:period:b ----

p_load(GVARX)

p.2.b=2 # The number of lag for Xt matrix
FLag.2.b=2 # The number of lag for foreign variables in country-specific VAR
lag.max.2.b=5 # The maximal number of lag for estimating country-specific VAR
type.2.b="none" # Model specificaiton for VAR. As in package vars, we have four selection: "none","const","trend", "both".
ic.2.b="AIC" # Information criteria for optimal lag.As in package vars, we have four selection: "AIC", "HQ", "SC", and "FPE".

options(scipen=9999999)
mainOUTPUT.2.b = GVECMest(
        data = cow.d.2.B,
        p = p.2.b,
        FLag = FLag.2.b,
        lag.max = lag.max.2.b,
        type = type.2.b,
        ic = ic.2.b,
        weight.matrix=wm.2.b)

# Storing Values

# summary(mainOUTPUT.2$gvecm[[9]]) # China 
p.2.China.pvalue.1 = round(0.9962, 3)
p.2.China.pvalue.2 = round(0.00000000000000022, 3)

# summary(mainOUTPUT.2$gvecm[[28]]) # Russia 
p.2.Russia.pvalue.1 = round(0.003474, 3)
p.2.Russia.pvalue.2 = round(0.03106, 3)

# summary(mainOUTPUT.2$gvecm[[35]]) # United.States.of.America 
p.2.United.States.of.America.pvalue.1 = round(0.1001, 3)
p.2.United.States.of.America.pvalue.2 = round(0.06328, 3)


### China
## Ftests
p.2.China.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[1]])))["varresult.China.milper.fstatistic.value"]), 3)
p.2.China.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[1]])))["varresult.China.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.China.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[1]])))["varresult.China.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[1]])))["varresult.China.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.China.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[1]])))["varresult.China.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[1]])))["varresult.China.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.China.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[1]])))["varresult.China.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.China.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[1]])))["varresult.China.irst.adj.r.squared"]), 3) 
# Lags
p.2.China.lags = mainOUTPUT.2.b$lagmatrix$lags[1] # Lag for the first country 



### Russia
## Ftests
p.2.Russia.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[2]])))["varresult.Russia.milper.fstatistic.value"]), 3)
p.2.Russia.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[2]])))["varresult.Russia.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.Russia.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[2]])))["varresult.Russia.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[2]])))["varresult.Russia.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.Russia.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[2]])))["varresult.Russia.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[2]])))["varresult.Russia.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.Russia.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[2]])))["varresult.Russia.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.Russia.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[2]])))["varresult.Russia.irst.adj.r.squared"]), 3) 
# Lags
p.2.Russia.lags = mainOUTPUT.2.b$lagmatrix$lags[2] # Lag for the first country 


### United.States.of.America
## Ftests
p.2.United.States.of.America.ftest.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[3]])))["varresult.United.States.of.America.milper.fstatistic.value"]), 3)
p.2.United.States.of.America.ftest.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[3]])))["varresult.United.States.of.America.irst.fstatistic.value"]), 3)
## DF Num, Dem (1)
p.2.United.States.of.America.df.num.dem.1 = paste(
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[3]])))["varresult.United.States.of.America.milper.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[3]])))["varresult.United.States.of.America.milper.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
## DF Num, Dem (2)
p.2.United.States.of.America.df.num.dem.2 = paste(
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[3]])))["varresult.United.States.of.America.irst.fstatistic.numdf"], # DF-Num (1),
        unlist(list(summary(mainOUTPUT.2.b$gvecm[[3]])))["varresult.United.States.of.America.irst.fstatistic.dendf"], # DF-Den (1)
        sep = ",")
# Rsq 1
p.2.United.States.of.America.rsq.1 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[3]])))["varresult.United.States.of.America.milper.adj.r.squared"]), 3) 
# Rsq 2
p.2.United.States.of.America.rsq.2 = round(as.numeric(unlist(list(summary(mainOUTPUT.2.b$gvecm[[3]])))["varresult.United.States.of.America.irst.adj.r.squared"]), 3) 
# Lags
p.2.United.States.of.America.lags = mainOUTPUT.2.b$lagmatrix$lags[3] # Lag for the first country 

## ----

summary(mainOUTPUT.2$gvecm[[1]]) # China 
summary(mainOUTPUT.2$gvecm[[2]]) # Russia 
summary(mainOUTPUT.2$gvecm[[3]]) # United.States.of.America 









############################################
# Checking Panel stationarity First Period
############################################

# "the GVAR methodology can be applied to stationary and/or integrated variables" Dees2007, p. 10
# Granger models can be estimated with non-stationary data (Freeman1983, 334).

## tests in "plm" package assume "the series under scrutiny are cross-sectionally independent"
## tests in "punitroots" assume "cross-dependence across the panel units", which is what we believe
## Kleiber2011, p. 2


### Package
## Dependencies
# install.packages("fBasics")
# install.packages("fUnitRoots")
# install.packages("CADFtest")
## Install package of interest
# install.packages("punitroots")
library(punitroots)

# Need to melt data first to make them look like these data("OECDunemp")
p_load(dplyr)

cow.d.1.irst = select(cow.d.1, "ID", "irst", "Time")
cow.d.1.irst = data.frame(t(reshape(cow.d.1.irst, idvar = "ID", timevar = "Time", direction = "wide")))
rownames(cow.d.1.irst) <- NULL
colnames(cow.d.1.irst) <- c(as.character(lapply(cow.d.1.irst[1,] , as.character)))
cow.d.1.irst = data.frame(cow.d.1.irst[-c(1), ])
cow.d.1.irst <- mutate_all(cow.d.1.irst, function(x) as.numeric(as.character(x)))

cow.d.1.milper = select(cow.d.1, "ID", "milper", "Time")
cow.d.1.milper = data.frame(t(reshape(cow.d.1.milper, idvar = "ID", timevar = "Time", direction = "wide")))
rownames(cow.d.1.milper) <- NULL
colnames(cow.d.1.milper) <- c(as.character(lapply(cow.d.1.milper[1,] , as.character)))
cow.d.1.milper = data.frame(cow.d.1.milper[-c(1), ])
cow.d.1.milper <- mutate_all(cow.d.1.milper, function(x) as.numeric(as.character(x)))

## ?pCADFtest
# This function implements the panel Covariate Augmented Dickey-Fuller (pCADF) test developed in Costantini and Lupi (2012). 
# The panel unit root tests proposed in Choi (2001) and in Demetrescu et al. (2006) can also be performed using this function.
cow.d.1.irst.station = pCADFtest(Y=cow.d.1.irst, type = "drift", criterion = "AIC")
cow.d.1.milper.station = pCADFtest(Y = cow.d.1.milper, type = "drift", criterion = "AIC")

summary(cow.d.1.irst.station)
# 1. The line Correction for cross-correlation: TRUE states that cross-dependence has been detected and Hartung’s correction has been used in the combination of the p values as sug- gested in Demetrescu et al. (2006). Kleiber2011, 10
# 2. unit root = NULL. Do we have enough to reject the null? p-value of 1.00000 indicates that we have non-stationarity.


summary(cow.d.1.milper.station)
# 1. The line Correction for cross-correlation: TRUE states that cross-dependence has been detected and Hartung’s correction has been used in the combination of the p values as sug- gested in Demetrescu et al. (2006). Kleiber2011, 10
# 2. unit root = NULL. Do we have enough to reject the null? p-value of 0.07191166 indicates that we have non-stationarity.


#############################
# First Differencing the series
#############################

cow.d.1.irst.diff = data.frame(diff(as.matrix(cow.d.1.irst)))
cow.d.1.milper.diff = data.frame(diff(as.matrix(cow.d.1.milper)))

#############################
# Re-testing for stationarity
#############################

options(scipen=9999999)

cow.d.1.irst.diff.station = pCADFtest(Y=cow.d.1.irst.diff, type = "drift", criterion = "AIC") ; summary(cow.d.1.irst.diff.station)
cow.d.1.milper.diff.station = pCADFtest(Y=cow.d.1.milper.diff, type = "drift", criterion = "AIC") ; summary(cow.d.1.milper.diff.station)

# Really small p-values: series are stationary once they're first differenced


#############################
# Test Lag Structure
#############################

# Pending
## Test for stability
### This is neccesary to set "p" below.

# Include in the paper a brief but detailed discussion, like FN # 31 in Box-Steffensmeier2014a, p. 120.


# check lags per country; build df's, one per country
country.1.t1 = data.frame(milper = cow.d.1[cow.d.1$ID == full.info.countries.first.time.span[1], ]$milper, irst = cow.d.1[cow.d.1$ID == full.info.countries.first.time.span[1], ]$irst, row.names = NULL)

# package to test lag lenght
p_load(vars)

lag.max.lag.lenght = 5
test.restrictions.lag.lenght = "both" # both = test for both constant and trend


test = VARselect(country.1.t1, lag.max = lag.max.lag.lenght, type=test.restrictions.lag.lenght) 
test$selection # "selection" = Vector with the optimal lag number according to each criterium




################################################################################################################
#### SECOND TIME PERIOD
################################################################################################################











################
#### ABSTRACT
################

## ---- abstract ----
fileConn <- file ("abstract.txt")
writeLines(paste("This is the abstract."), fileConn)
close(fileConn)
## ----




