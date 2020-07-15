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
cow.d.1 = cow.d.1 %>% group_by(ID) %>% filter(milper != 0 & irst != 0)

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


# 1. Sort column X (with countries) and Delete column X
y.1871 = y.1871[order(y.1871$X), ] ; y.1871$X <- NULL
y.1872 = y.1872[order(y.1872$X), ] ; y.1872$X <- NULL
y.1873 = y.1873[order(y.1873$X), ] ; y.1873$X <- NULL
y.1874 = y.1874[order(y.1874$X), ] ; y.1874$X <- NULL
y.1875 = y.1875[order(y.1875$X), ] ; y.1875$X <- NULL
y.1876 = y.1876[order(y.1876$X), ] ; y.1876$X <- NULL
y.1877 = y.1877[order(y.1877$X), ] ; y.1877$X <- NULL
y.1878 = y.1878[order(y.1878$X), ] ; y.1878$X <- NULL
y.1879 = y.1879[order(y.1879$X), ] ; y.1879$X <- NULL
y.1880 = y.1880[order(y.1880$X), ] ; y.1880$X <- NULL
y.1881 = y.1881[order(y.1881$X), ] ; y.1881$X <- NULL
y.1882 = y.1882[order(y.1882$X), ] ; y.1882$X <- NULL
y.1883 = y.1883[order(y.1883$X), ] ; y.1883$X <- NULL
y.1884 = y.1884[order(y.1884$X), ] ; y.1884$X <- NULL
y.1885 = y.1885[order(y.1885$X), ] ; y.1885$X <- NULL
y.1886 = y.1886[order(y.1886$X), ] ; y.1886$X <- NULL
y.1887 = y.1887[order(y.1887$X), ] ; y.1887$X <- NULL
y.1888 = y.1888[order(y.1888$X), ] ; y.1888$X <- NULL
y.1889 = y.1889[order(y.1889$X), ] ; y.1889$X <- NULL
y.1890 = y.1890[order(y.1890$X), ] ; y.1890$X <- NULL
y.1891 = y.1891[order(y.1891$X), ] ; y.1891$X <- NULL
y.1892 = y.1892[order(y.1892$X), ] ; y.1892$X <- NULL
y.1893 = y.1893[order(y.1893$X), ] ; y.1893$X <- NULL
y.1894 = y.1894[order(y.1894$X), ] ; y.1894$X <- NULL
y.1895 = y.1895[order(y.1895$X), ] ; y.1895$X <- NULL
y.1896 = y.1896[order(y.1896$X), ] ; y.1896$X <- NULL
y.1897 = y.1897[order(y.1897$X), ] ; y.1897$X <- NULL
y.1898 = y.1898[order(y.1898$X), ] ; y.1898$X <- NULL
y.1899 = y.1899[order(y.1899$X), ] ; y.1899$X <- NULL
y.1900 = y.1900[order(y.1900$X), ] ; y.1900$X <- NULL
y.1901 = y.1901[order(y.1901$X), ] ; y.1901$X <- NULL
y.1902 = y.1902[order(y.1902$X), ] ; y.1902$X <- NULL
y.1903 = y.1903[order(y.1903$X), ] ; y.1903$X <- NULL
y.1904 = y.1904[order(y.1904$X), ] ; y.1904$X <- NULL
y.1905 = y.1905[order(y.1905$X), ] ; y.1905$X <- NULL
y.1906 = y.1906[order(y.1906$X), ] ; y.1906$X <- NULL
y.1907 = y.1907[order(y.1907$X), ] ; y.1907$X <- NULL
y.1908 = y.1908[order(y.1908$X), ] ; y.1908$X <- NULL
y.1909 = y.1909[order(y.1909$X), ] ; y.1909$X <- NULL
y.1910 = y.1910[order(y.1910$X), ] ; y.1910$X <- NULL
y.1911 = y.1911[order(y.1911$X), ] ; y.1911$X <- NULL
y.1912 = y.1912[order(y.1912$X), ] ; y.1912$X <- NULL
y.1913 = y.1913[order(y.1913$X), ] ; y.1913$X <- NULL

# 2. Sort column names and delete columns (countries) for which we have complete missingness
y.1871 = y.1871[ , order(names(y.1871))] # ; y.1871 = y.1871[ , colSums(y.1871 != 0) > 0]
y.1872 = y.1872[ , order(names(y.1872))] # ; y.1872 = y.1872[ , colSums(y.1872 != 0) > 0]
y.1873 = y.1873[ , order(names(y.1873))] # ; y.1873 = y.1873[ , colSums(y.1873 != 0) > 0]
y.1874 = y.1874[ , order(names(y.1874))] # ; y.1874 = y.1874[ , colSums(y.1874 != 0) > 0]
y.1875 = y.1875[ , order(names(y.1875))] # ; y.1875 = y.1875[ , colSums(y.1875 != 0) > 0]
y.1876 = y.1876[ , order(names(y.1876))] # ; y.1876 = y.1876[ , colSums(y.1876 != 0) > 0]
y.1877 = y.1877[ , order(names(y.1877))] # ; y.1877 = y.1877[ , colSums(y.1877 != 0) > 0]
y.1878 = y.1878[ , order(names(y.1878))] # ; y.1878 = y.1878[ , colSums(y.1878 != 0) > 0]
y.1879 = y.1879[ , order(names(y.1879))] # ; y.1879 = y.1879[ , colSums(y.1879 != 0) > 0]
y.1880 = y.1880[ , order(names(y.1880))] # ; y.1880 = y.1880[ , colSums(y.1880 != 0) > 0]
y.1881 = y.1881[ , order(names(y.1881))] # ; y.1881 = y.1881[ , colSums(y.1881 != 0) > 0]
y.1882 = y.1882[ , order(names(y.1882))] # ; y.1882 = y.1882[ , colSums(y.1882 != 0) > 0]
y.1883 = y.1883[ , order(names(y.1883))] # ; y.1883 = y.1883[ , colSums(y.1883 != 0) > 0]
y.1884 = y.1884[ , order(names(y.1884))] # ; y.1884 = y.1884[ , colSums(y.1884 != 0) > 0]
y.1885 = y.1885[ , order(names(y.1885))] # ; y.1885 = y.1885[ , colSums(y.1885 != 0) > 0]
y.1886 = y.1886[ , order(names(y.1886))] # ; y.1886 = y.1886[ , colSums(y.1886 != 0) > 0]
y.1887 = y.1887[ , order(names(y.1887))] # ; y.1887 = y.1887[ , colSums(y.1887 != 0) > 0]
y.1888 = y.1888[ , order(names(y.1888))] # ; y.1888 = y.1888[ , colSums(y.1888 != 0) > 0]
y.1889 = y.1889[ , order(names(y.1889))] # ; y.1889 = y.1889[ , colSums(y.1889 != 0) > 0]
y.1890 = y.1890[ , order(names(y.1890))] # ; y.1890 = y.1890[ , colSums(y.1890 != 0) > 0]
y.1891 = y.1891[ , order(names(y.1891))] # ; y.1891 = y.1891[ , colSums(y.1891 != 0) > 0]
y.1892 = y.1892[ , order(names(y.1892))] # ; y.1892 = y.1892[ , colSums(y.1892 != 0) > 0]
y.1893 = y.1893[ , order(names(y.1893))] # ; y.1893 = y.1893[ , colSums(y.1893 != 0) > 0]
y.1894 = y.1894[ , order(names(y.1894))] # ; y.1894 = y.1894[ , colSums(y.1894 != 0) > 0]
y.1895 = y.1895[ , order(names(y.1895))] # ; y.1895 = y.1895[ , colSums(y.1895 != 0) > 0]
y.1896 = y.1896[ , order(names(y.1896))] # ; y.1896 = y.1896[ , colSums(y.1896 != 0) > 0]
y.1897 = y.1897[ , order(names(y.1897))] # ; y.1897 = y.1897[ , colSums(y.1897 != 0) > 0]
y.1898 = y.1898[ , order(names(y.1898))] # ; y.1898 = y.1898[ , colSums(y.1898 != 0) > 0]
y.1899 = y.1899[ , order(names(y.1899))] # ; y.1899 = y.1899[ , colSums(y.1899 != 0) > 0]
y.1900 = y.1900[ , order(names(y.1900))] # ; y.1900 = y.1900[ , colSums(y.1900 != 0) > 0]
y.1901 = y.1901[ , order(names(y.1901))] # ; y.1901 = y.1901[ , colSums(y.1901 != 0) > 0]
y.1902 = y.1902[ , order(names(y.1902))] # ; y.1902 = y.1902[ , colSums(y.1902 != 0) > 0]
y.1903 = y.1903[ , order(names(y.1903))] # ; y.1903 = y.1903[ , colSums(y.1903 != 0) > 0]
y.1904 = y.1904[ , order(names(y.1904))] # ; y.1904 = y.1904[ , colSums(y.1904 != 0) > 0]
y.1905 = y.1905[ , order(names(y.1905))] # ; y.1905 = y.1905[ , colSums(y.1905 != 0) > 0]
y.1906 = y.1906[ , order(names(y.1906))] # ; y.1906 = y.1906[ , colSums(y.1906 != 0) > 0]
y.1907 = y.1907[ , order(names(y.1907))] # ; y.1907 = y.1907[ , colSums(y.1907 != 0) > 0]
y.1908 = y.1908[ , order(names(y.1908))] # ; y.1908 = y.1908[ , colSums(y.1908 != 0) > 0]
y.1909 = y.1909[ , order(names(y.1909))] # ; y.1909 = y.1909[ , colSums(y.1909 != 0) > 0]
y.1910 = y.1910[ , order(names(y.1910))] # ; y.1910 = y.1910[ , colSums(y.1910 != 0) > 0]
y.1911 = y.1911[ , order(names(y.1911))] # ; y.1911 = y.1911[ , colSums(y.1911 != 0) > 0]
y.1912 = y.1912[ , order(names(y.1912))] # ; y.1912 = y.1912[ , colSums(y.1912 != 0) > 0]
y.1913 = y.1913[ , order(names(y.1913))] # ; y.1913 = y.1913[ , colSums(y.1913 != 0) > 0]


## Keeping Columns for which we have complete information
p_load(dplyr)
y.1871 = dplyr::select(y.1871, full.info.countries.first.time.span)
y.1872 = dplyr::select(y.1872, full.info.countries.first.time.span)
y.1873 = dplyr::select(y.1873, full.info.countries.first.time.span)
y.1874 = dplyr::select(y.1874, full.info.countries.first.time.span)
y.1875 = dplyr::select(y.1875, full.info.countries.first.time.span)
y.1876 = dplyr::select(y.1876, full.info.countries.first.time.span)
y.1877 = dplyr::select(y.1877, full.info.countries.first.time.span)
y.1878 = dplyr::select(y.1878, full.info.countries.first.time.span)
y.1879 = dplyr::select(y.1879, full.info.countries.first.time.span)
y.1880 = dplyr::select(y.1880, full.info.countries.first.time.span)
y.1881 = dplyr::select(y.1881, full.info.countries.first.time.span)
y.1882 = dplyr::select(y.1882, full.info.countries.first.time.span)
y.1883 = dplyr::select(y.1883, full.info.countries.first.time.span)
y.1884 = dplyr::select(y.1884, full.info.countries.first.time.span)
y.1885 = dplyr::select(y.1885, full.info.countries.first.time.span)
y.1886 = dplyr::select(y.1886, full.info.countries.first.time.span)
y.1887 = dplyr::select(y.1887, full.info.countries.first.time.span)
y.1888 = dplyr::select(y.1888, full.info.countries.first.time.span)
y.1889 = dplyr::select(y.1889, full.info.countries.first.time.span)
y.1890 = dplyr::select(y.1890, full.info.countries.first.time.span)
y.1891 = dplyr::select(y.1891, full.info.countries.first.time.span)
y.1892 = dplyr::select(y.1892, full.info.countries.first.time.span)
y.1893 = dplyr::select(y.1893, full.info.countries.first.time.span)
y.1894 = dplyr::select(y.1894, full.info.countries.first.time.span)
y.1895 = dplyr::select(y.1895, full.info.countries.first.time.span)
y.1896 = dplyr::select(y.1896, full.info.countries.first.time.span)
y.1897 = dplyr::select(y.1897, full.info.countries.first.time.span)
y.1898 = dplyr::select(y.1898, full.info.countries.first.time.span)
y.1899 = dplyr::select(y.1899, full.info.countries.first.time.span)
y.1900 = dplyr::select(y.1900, full.info.countries.first.time.span)
y.1901 = dplyr::select(y.1901, full.info.countries.first.time.span)
y.1902 = dplyr::select(y.1902, full.info.countries.first.time.span)
y.1903 = dplyr::select(y.1903, full.info.countries.first.time.span)
y.1904 = dplyr::select(y.1904, full.info.countries.first.time.span)
y.1905 = dplyr::select(y.1905, full.info.countries.first.time.span)
y.1906 = dplyr::select(y.1906, full.info.countries.first.time.span)
y.1907 = dplyr::select(y.1907, full.info.countries.first.time.span)
y.1908 = dplyr::select(y.1908, full.info.countries.first.time.span)
y.1909 = dplyr::select(y.1909, full.info.countries.first.time.span)
y.1910 = dplyr::select(y.1910, full.info.countries.first.time.span)
y.1911 = dplyr::select(y.1911, full.info.countries.first.time.span)
y.1912 = dplyr::select(y.1912, full.info.countries.first.time.span)
y.1913 = dplyr::select(y.1913, full.info.countries.first.time.span)

## Keeping Rows for which we have complete information
y.1871 = y.1871[1:length(full.info.countries.first.time.span), ]
y.1872 = y.1872[1:length(full.info.countries.first.time.span), ]
y.1873 = y.1873[1:length(full.info.countries.first.time.span), ]
y.1874 = y.1874[1:length(full.info.countries.first.time.span), ]
y.1875 = y.1875[1:length(full.info.countries.first.time.span), ]
y.1876 = y.1876[1:length(full.info.countries.first.time.span), ]
y.1877 = y.1877[1:length(full.info.countries.first.time.span), ]
y.1878 = y.1878[1:length(full.info.countries.first.time.span), ]
y.1879 = y.1879[1:length(full.info.countries.first.time.span), ]
y.1880 = y.1880[1:length(full.info.countries.first.time.span), ]
y.1881 = y.1881[1:length(full.info.countries.first.time.span), ]
y.1882 = y.1882[1:length(full.info.countries.first.time.span), ]
y.1883 = y.1883[1:length(full.info.countries.first.time.span), ]
y.1884 = y.1884[1:length(full.info.countries.first.time.span), ]
y.1885 = y.1885[1:length(full.info.countries.first.time.span), ]
y.1886 = y.1886[1:length(full.info.countries.first.time.span), ]
y.1887 = y.1887[1:length(full.info.countries.first.time.span), ]
y.1888 = y.1888[1:length(full.info.countries.first.time.span), ]
y.1889 = y.1889[1:length(full.info.countries.first.time.span), ]
y.1890 = y.1890[1:length(full.info.countries.first.time.span), ]
y.1891 = y.1891[1:length(full.info.countries.first.time.span), ]
y.1892 = y.1892[1:length(full.info.countries.first.time.span), ]
y.1893 = y.1893[1:length(full.info.countries.first.time.span), ]
y.1894 = y.1894[1:length(full.info.countries.first.time.span), ]
y.1895 = y.1895[1:length(full.info.countries.first.time.span), ]
y.1896 = y.1896[1:length(full.info.countries.first.time.span), ]
y.1897 = y.1897[1:length(full.info.countries.first.time.span), ]
y.1898 = y.1898[1:length(full.info.countries.first.time.span), ]
y.1899 = y.1899[1:length(full.info.countries.first.time.span), ]
y.1900 = y.1900[1:length(full.info.countries.first.time.span), ]
y.1901 = y.1901[1:length(full.info.countries.first.time.span), ]
y.1902 = y.1902[1:length(full.info.countries.first.time.span), ]
y.1903 = y.1903[1:length(full.info.countries.first.time.span), ]
y.1904 = y.1904[1:length(full.info.countries.first.time.span), ]
y.1905 = y.1905[1:length(full.info.countries.first.time.span), ]
y.1906 = y.1906[1:length(full.info.countries.first.time.span), ]
y.1907 = y.1907[1:length(full.info.countries.first.time.span), ]
y.1908 = y.1908[1:length(full.info.countries.first.time.span), ]
y.1909 = y.1909[1:length(full.info.countries.first.time.span), ]
y.1910 = y.1910[1:length(full.info.countries.first.time.span), ]
y.1911 = y.1911[1:length(full.info.countries.first.time.span), ]
y.1912 = y.1912[1:length(full.info.countries.first.time.span), ]
y.1913 = y.1913[1:length(full.info.countries.first.time.span), ]


# Resetting Rownames
rownames(y.1871) <- NULL
rownames(y.1872) <- NULL
rownames(y.1873) <- NULL
rownames(y.1874) <- NULL
rownames(y.1875) <- NULL
rownames(y.1876) <- NULL
rownames(y.1877) <- NULL
rownames(y.1878) <- NULL
rownames(y.1879) <- NULL
rownames(y.1880) <- NULL
rownames(y.1881) <- NULL
rownames(y.1882) <- NULL
rownames(y.1883) <- NULL
rownames(y.1884) <- NULL
rownames(y.1885) <- NULL
rownames(y.1886) <- NULL
rownames(y.1887) <- NULL
rownames(y.1888) <- NULL
rownames(y.1889) <- NULL
rownames(y.1890) <- NULL
rownames(y.1891) <- NULL
rownames(y.1892) <- NULL
rownames(y.1893) <- NULL
rownames(y.1894) <- NULL
rownames(y.1895) <- NULL
rownames(y.1896) <- NULL
rownames(y.1897) <- NULL
rownames(y.1898) <- NULL
rownames(y.1899) <- NULL
rownames(y.1900) <- NULL
rownames(y.1901) <- NULL
rownames(y.1902) <- NULL
rownames(y.1903) <- NULL
rownames(y.1904) <- NULL
rownames(y.1905) <- NULL
rownames(y.1906) <- NULL
rownames(y.1907) <- NULL
rownames(y.1908) <- NULL
rownames(y.1909) <- NULL
rownames(y.1910) <- NULL
rownames(y.1911) <- NULL
rownames(y.1912) <- NULL
rownames(y.1913) <- NULL

# Building WM for the first period
wm.1 = list(as.matrix(y.1871), as.matrix(y.1872), as.matrix(y.1873), as.matrix(y.1874), as.matrix(y.1875), as.matrix(y.1876), as.matrix(y.1877), as.matrix(y.1878), as.matrix(y.1879), as.matrix(y.1880), as.matrix(y.1881), as.matrix(y.1882), as.matrix(y.1883), as.matrix(y.1884), as.matrix(y.1885), as.matrix(y.1886), as.matrix(y.1887), as.matrix(y.1888), as.matrix(y.1889), as.matrix(y.1890), as.matrix(y.1891), as.matrix(y.1892), as.matrix(y.1893), as.matrix(y.1894), as.matrix(y.1895), as.matrix(y.1896), as.matrix(y.1897), as.matrix(y.1898), as.matrix(y.1899), as.matrix(y.1900), as.matrix(y.1901), as.matrix(y.1902), as.matrix(y.1903), as.matrix(y.1904), as.matrix(y.1905), as.matrix(y.1906), as.matrix(y.1907), as.matrix(y.1908), as.matrix(y.1909), as.matrix(y.1910), as.matrix(y.1911), as.matrix(y.1912), as.matrix(y.1913))



################################################################################################################
## Second Period: 1955 - 2012
################################################################################################################

# Filter complete obs by year; here it's where I split the datasets
cow.d.2 <- subset(cow.d, Time >= 1955 & Time <= 2014)

# Drop NAs
cow.d.2$milper[cow.d.2$milper == -9] <- 0
cow.d.2$irst[cow.d.2$irst ==  -9] <- 0

# Filter countries for which we have complete data
cow.d.2 = cow.d.2 %>% group_by(ID) %>% filter(milper != 0 & irst != 0)

# Drop countries for which we don't have the complete series
full.info.countries.second.time.span = unique(cow.d.2[cow.d.2$ID %in% names(which(table(cow.d.2$ID) == max(table(cow.d.2$ID)))), ]$ID) # Countries for which we have complete rows

# Exclude Luxembourg (it's got too many similar values year after year)
full.info.countries.second.time.span <- full.info.countries.second.time.span[full.info.countries.second.time.span != "Luxembourg"]

# test = max(table(cow.d.2$ID)) ; View(test) # Germanies appear 36 times. Consider do GVAR for both of them.

# Filtering complete obs by country name
cow.d.2 = data.frame(cow.d.2[cow.d.2$ID %in% full.info.countries.second.time.span,])

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

# 1. Sort column X (with countries) and Delete column X
y.1955 = y.1955[order(y.1955$X), ] ; y.1955$X <- NULL
y.1956 = y.1956[order(y.1956$X), ] ; y.1956$X <- NULL
y.1957 = y.1957[order(y.1957$X), ] ; y.1957$X <- NULL
y.1958 = y.1958[order(y.1958$X), ] ; y.1958$X <- NULL
y.1959 = y.1959[order(y.1959$X), ] ; y.1959$X <- NULL
y.1960 = y.1960[order(y.1960$X), ] ; y.1960$X <- NULL
y.1961 = y.1961[order(y.1961$X), ] ; y.1961$X <- NULL
y.1962 = y.1962[order(y.1962$X), ] ; y.1962$X <- NULL
y.1963 = y.1963[order(y.1963$X), ] ; y.1963$X <- NULL
y.1964 = y.1964[order(y.1964$X), ] ; y.1964$X <- NULL
y.1965 = y.1965[order(y.1965$X), ] ; y.1965$X <- NULL
y.1966 = y.1966[order(y.1966$X), ] ; y.1966$X <- NULL
y.1967 = y.1967[order(y.1967$X), ] ; y.1967$X <- NULL
y.1968 = y.1968[order(y.1968$X), ] ; y.1968$X <- NULL
y.1969 = y.1969[order(y.1969$X), ] ; y.1969$X <- NULL
y.1970 = y.1970[order(y.1970$X), ] ; y.1970$X <- NULL
y.1971 = y.1971[order(y.1971$X), ] ; y.1971$X <- NULL
y.1972 = y.1972[order(y.1972$X), ] ; y.1972$X <- NULL
y.1973 = y.1973[order(y.1973$X), ] ; y.1973$X <- NULL
y.1974 = y.1974[order(y.1974$X), ] ; y.1974$X <- NULL
y.1975 = y.1975[order(y.1975$X), ] ; y.1975$X <- NULL
y.1976 = y.1976[order(y.1976$X), ] ; y.1976$X <- NULL
y.1977 = y.1977[order(y.1977$X), ] ; y.1977$X <- NULL
y.1978 = y.1978[order(y.1978$X), ] ; y.1978$X <- NULL
y.1979 = y.1979[order(y.1979$X), ] ; y.1979$X <- NULL
y.1980 = y.1980[order(y.1980$X), ] ; y.1980$X <- NULL
y.1981 = y.1981[order(y.1981$X), ] ; y.1981$X <- NULL
y.1982 = y.1982[order(y.1982$X), ] ; y.1982$X <- NULL
y.1983 = y.1983[order(y.1983$X), ] ; y.1983$X <- NULL
y.1984 = y.1984[order(y.1984$X), ] ; y.1984$X <- NULL
y.1985 = y.1985[order(y.1985$X), ] ; y.1985$X <- NULL
y.1986 = y.1986[order(y.1986$X), ] ; y.1986$X <- NULL
y.1987 = y.1987[order(y.1987$X), ] ; y.1987$X <- NULL
y.1988 = y.1988[order(y.1988$X), ] ; y.1988$X <- NULL
y.1989 = y.1989[order(y.1989$X), ] ; y.1989$X <- NULL
y.1990 = y.1990[order(y.1990$X), ] ; y.1990$X <- NULL
y.1991 = y.1991[order(y.1991$X), ] ; y.1991$X <- NULL
y.1992 = y.1992[order(y.1992$X), ] ; y.1992$X <- NULL
y.1993 = y.1993[order(y.1993$X), ] ; y.1993$X <- NULL
y.1994 = y.1994[order(y.1994$X), ] ; y.1994$X <- NULL
y.1995 = y.1995[order(y.1995$X), ] ; y.1995$X <- NULL
y.1996 = y.1996[order(y.1996$X), ] ; y.1996$X <- NULL
y.1997 = y.1997[order(y.1997$X), ] ; y.1997$X <- NULL
y.1998 = y.1998[order(y.1998$X), ] ; y.1998$X <- NULL
y.1999 = y.1999[order(y.1999$X), ] ; y.1999$X <- NULL
y.2000 = y.2000[order(y.2000$X), ] ; y.2000$X <- NULL
y.2001 = y.2001[order(y.2001$X), ] ; y.2001$X <- NULL
y.2002 = y.2002[order(y.2002$X), ] ; y.2002$X <- NULL
y.2003 = y.2003[order(y.2003$X), ] ; y.2003$X <- NULL
y.2004 = y.2004[order(y.2004$X), ] ; y.2004$X <- NULL
y.2005 = y.2005[order(y.2005$X), ] ; y.2005$X <- NULL
y.2006 = y.2006[order(y.2006$X), ] ; y.2006$X <- NULL
y.2007 = y.2007[order(y.2007$X), ] ; y.2007$X <- NULL
y.2008 = y.2008[order(y.2008$X), ] ; y.2008$X <- NULL
y.2009 = y.2009[order(y.2009$X), ] ; y.2009$X <- NULL
y.2010 = y.2010[order(y.2010$X), ] ; y.2010$X <- NULL
y.2011 = y.2011[order(y.2011$X), ] ; y.2011$X <- NULL
y.2012 = y.2012[order(y.2012$X), ] ; y.2012$X <- NULL
y.2013 = y.2013[order(y.2013$X), ] ; y.2013$X <- NULL
y.2014 = y.2014[order(y.2014$X), ] ; y.2014$X <- NULL


# 2. Sort column names and delete columns (countries) for which we have complete missingness
y.1955 = y.1955[ , order(names(y.1955))]
y.1956 = y.1956[ , order(names(y.1956))]
y.1957 = y.1957[ , order(names(y.1957))]
y.1958 = y.1958[ , order(names(y.1958))]
y.1959 = y.1959[ , order(names(y.1959))]
y.1960 = y.1960[ , order(names(y.1960))]
y.1961 = y.1961[ , order(names(y.1961))]
y.1962 = y.1962[ , order(names(y.1962))]
y.1963 = y.1963[ , order(names(y.1963))]
y.1964 = y.1964[ , order(names(y.1964))]
y.1965 = y.1965[ , order(names(y.1965))]
y.1966 = y.1966[ , order(names(y.1966))]
y.1967 = y.1967[ , order(names(y.1967))]
y.1968 = y.1968[ , order(names(y.1968))]
y.1969 = y.1969[ , order(names(y.1969))]
y.1970 = y.1970[ , order(names(y.1970))]
y.1971 = y.1971[ , order(names(y.1971))]
y.1972 = y.1972[ , order(names(y.1972))]
y.1973 = y.1973[ , order(names(y.1973))]
y.1974 = y.1974[ , order(names(y.1974))]
y.1975 = y.1975[ , order(names(y.1975))]
y.1976 = y.1976[ , order(names(y.1976))]
y.1977 = y.1977[ , order(names(y.1977))]
y.1978 = y.1978[ , order(names(y.1978))]
y.1979 = y.1979[ , order(names(y.1979))]
y.1980 = y.1980[ , order(names(y.1980))]
y.1981 = y.1981[ , order(names(y.1981))]
y.1982 = y.1982[ , order(names(y.1982))]
y.1983 = y.1983[ , order(names(y.1983))]
y.1984 = y.1984[ , order(names(y.1984))]
y.1985 = y.1985[ , order(names(y.1985))]
y.1986 = y.1986[ , order(names(y.1986))]
y.1987 = y.1987[ , order(names(y.1987))]
y.1988 = y.1988[ , order(names(y.1988))]
y.1989 = y.1989[ , order(names(y.1989))]
y.1990 = y.1990[ , order(names(y.1990))]
y.1991 = y.1991[ , order(names(y.1991))]
y.1992 = y.1992[ , order(names(y.1992))]
y.1993 = y.1993[ , order(names(y.1993))]
y.1994 = y.1994[ , order(names(y.1994))]
y.1995 = y.1995[ , order(names(y.1995))]
y.1996 = y.1996[ , order(names(y.1996))]
y.1997 = y.1997[ , order(names(y.1997))]
y.1998 = y.1998[ , order(names(y.1998))]
y.1999 = y.1999[ , order(names(y.1999))]
y.2000 = y.2000[ , order(names(y.2000))]
y.2001 = y.2001[ , order(names(y.2001))]
y.2002 = y.2002[ , order(names(y.2002))]
y.2003 = y.2003[ , order(names(y.2003))]
y.2004 = y.2004[ , order(names(y.2004))]
y.2005 = y.2005[ , order(names(y.2005))]
y.2006 = y.2006[ , order(names(y.2006))]
y.2007 = y.2007[ , order(names(y.2007))]
y.2008 = y.2008[ , order(names(y.2008))]
y.2009 = y.2009[ , order(names(y.2009))]
y.2010 = y.2010[ , order(names(y.2010))]
y.2011 = y.2011[ , order(names(y.2011))]
y.2012 = y.2012[ , order(names(y.2012))]
y.2013 = y.2013[ , order(names(y.2013))]
y.2014 = y.2014[ , order(names(y.2014))]

## Keeping Columns for which we have complete information
p_load(dplyr)
y.1955 = dplyr::select(y.1955, full.info.countries.second.time.span)
y.1956 = dplyr::select(y.1956, full.info.countries.second.time.span)
y.1957 = dplyr::select(y.1957, full.info.countries.second.time.span)
y.1958 = dplyr::select(y.1958, full.info.countries.second.time.span)
y.1959 = dplyr::select(y.1959, full.info.countries.second.time.span)
y.1960 = dplyr::select(y.1960, full.info.countries.second.time.span)
y.1961 = dplyr::select(y.1961, full.info.countries.second.time.span)
y.1962 = dplyr::select(y.1962, full.info.countries.second.time.span)
y.1963 = dplyr::select(y.1963, full.info.countries.second.time.span)
y.1964 = dplyr::select(y.1964, full.info.countries.second.time.span)
y.1965 = dplyr::select(y.1965, full.info.countries.second.time.span)
y.1966 = dplyr::select(y.1966, full.info.countries.second.time.span)
y.1967 = dplyr::select(y.1967, full.info.countries.second.time.span)
y.1968 = dplyr::select(y.1968, full.info.countries.second.time.span)
y.1969 = dplyr::select(y.1969, full.info.countries.second.time.span)
y.1970 = dplyr::select(y.1970, full.info.countries.second.time.span)
y.1971 = dplyr::select(y.1971, full.info.countries.second.time.span)
y.1972 = dplyr::select(y.1972, full.info.countries.second.time.span)
y.1973 = dplyr::select(y.1973, full.info.countries.second.time.span)
y.1974 = dplyr::select(y.1974, full.info.countries.second.time.span)
y.1975 = dplyr::select(y.1975, full.info.countries.second.time.span)
y.1976 = dplyr::select(y.1976, full.info.countries.second.time.span)
y.1977 = dplyr::select(y.1977, full.info.countries.second.time.span)
y.1978 = dplyr::select(y.1978, full.info.countries.second.time.span)
y.1979 = dplyr::select(y.1979, full.info.countries.second.time.span)
y.1980 = dplyr::select(y.1980, full.info.countries.second.time.span)
y.1981 = dplyr::select(y.1981, full.info.countries.second.time.span)
y.1982 = dplyr::select(y.1982, full.info.countries.second.time.span)
y.1983 = dplyr::select(y.1983, full.info.countries.second.time.span)
y.1984 = dplyr::select(y.1984, full.info.countries.second.time.span)
y.1985 = dplyr::select(y.1985, full.info.countries.second.time.span)
y.1986 = dplyr::select(y.1986, full.info.countries.second.time.span)
y.1987 = dplyr::select(y.1987, full.info.countries.second.time.span)
y.1988 = dplyr::select(y.1988, full.info.countries.second.time.span)
y.1989 = dplyr::select(y.1989, full.info.countries.second.time.span)
y.1990 = dplyr::select(y.1990, full.info.countries.second.time.span)
y.1991 = dplyr::select(y.1991, full.info.countries.second.time.span)
y.1992 = dplyr::select(y.1992, full.info.countries.second.time.span)
y.1993 = dplyr::select(y.1993, full.info.countries.second.time.span)
y.1994 = dplyr::select(y.1994, full.info.countries.second.time.span)
y.1995 = dplyr::select(y.1995, full.info.countries.second.time.span)
y.1996 = dplyr::select(y.1996, full.info.countries.second.time.span)
y.1997 = dplyr::select(y.1997, full.info.countries.second.time.span)
y.1998 = dplyr::select(y.1998, full.info.countries.second.time.span)
y.1999 = dplyr::select(y.1999, full.info.countries.second.time.span)
y.2000 = dplyr::select(y.2000, full.info.countries.second.time.span)
y.2001 = dplyr::select(y.2001, full.info.countries.second.time.span)
y.2002 = dplyr::select(y.2002, full.info.countries.second.time.span)
y.2003 = dplyr::select(y.2003, full.info.countries.second.time.span)
y.2004 = dplyr::select(y.2004, full.info.countries.second.time.span)
y.2005 = dplyr::select(y.2005, full.info.countries.second.time.span)
y.2006 = dplyr::select(y.2006, full.info.countries.second.time.span)
y.2007 = dplyr::select(y.2007, full.info.countries.second.time.span)
y.2008 = dplyr::select(y.2008, full.info.countries.second.time.span)
y.2009 = dplyr::select(y.2009, full.info.countries.second.time.span)
y.2010 = dplyr::select(y.2010, full.info.countries.second.time.span)
y.2011 = dplyr::select(y.2011, full.info.countries.second.time.span)
y.2012 = dplyr::select(y.2012, full.info.countries.second.time.span)
y.2013 = dplyr::select(y.2013, full.info.countries.second.time.span)
y.2014 = dplyr::select(y.2014, full.info.countries.second.time.span)


## Keeping Rows for which we have complete information
y.1955 = y.1955[1:length(full.info.countries.second.time.span), ]
y.1956 = y.1956[1:length(full.info.countries.second.time.span), ]
y.1957 = y.1957[1:length(full.info.countries.second.time.span), ]
y.1958 = y.1958[1:length(full.info.countries.second.time.span), ]
y.1959 = y.1959[1:length(full.info.countries.second.time.span), ]
y.1960 = y.1960[1:length(full.info.countries.second.time.span), ]
y.1961 = y.1961[1:length(full.info.countries.second.time.span), ]
y.1962 = y.1962[1:length(full.info.countries.second.time.span), ]
y.1963 = y.1963[1:length(full.info.countries.second.time.span), ]
y.1964 = y.1964[1:length(full.info.countries.second.time.span), ]
y.1965 = y.1965[1:length(full.info.countries.second.time.span), ]
y.1966 = y.1966[1:length(full.info.countries.second.time.span), ]
y.1967 = y.1967[1:length(full.info.countries.second.time.span), ]
y.1968 = y.1968[1:length(full.info.countries.second.time.span), ]
y.1969 = y.1969[1:length(full.info.countries.second.time.span), ]
y.1970 = y.1970[1:length(full.info.countries.second.time.span), ]
y.1971 = y.1971[1:length(full.info.countries.second.time.span), ]
y.1972 = y.1972[1:length(full.info.countries.second.time.span), ]
y.1973 = y.1973[1:length(full.info.countries.second.time.span), ]
y.1974 = y.1974[1:length(full.info.countries.second.time.span), ]
y.1975 = y.1975[1:length(full.info.countries.second.time.span), ]
y.1976 = y.1976[1:length(full.info.countries.second.time.span), ]
y.1977 = y.1977[1:length(full.info.countries.second.time.span), ]
y.1978 = y.1978[1:length(full.info.countries.second.time.span), ]
y.1979 = y.1979[1:length(full.info.countries.second.time.span), ]
y.1980 = y.1980[1:length(full.info.countries.second.time.span), ]
y.1981 = y.1981[1:length(full.info.countries.second.time.span), ]
y.1982 = y.1982[1:length(full.info.countries.second.time.span), ]
y.1983 = y.1983[1:length(full.info.countries.second.time.span), ]
y.1984 = y.1984[1:length(full.info.countries.second.time.span), ]
y.1985 = y.1985[1:length(full.info.countries.second.time.span), ]
y.1986 = y.1986[1:length(full.info.countries.second.time.span), ]
y.1987 = y.1987[1:length(full.info.countries.second.time.span), ]
y.1988 = y.1988[1:length(full.info.countries.second.time.span), ]
y.1989 = y.1989[1:length(full.info.countries.second.time.span), ]
y.1990 = y.1990[1:length(full.info.countries.second.time.span), ]
y.1991 = y.1991[1:length(full.info.countries.second.time.span), ]
y.1992 = y.1992[1:length(full.info.countries.second.time.span), ]
y.1993 = y.1993[1:length(full.info.countries.second.time.span), ]
y.1994 = y.1994[1:length(full.info.countries.second.time.span), ]
y.1995 = y.1995[1:length(full.info.countries.second.time.span), ]
y.1996 = y.1996[1:length(full.info.countries.second.time.span), ]
y.1997 = y.1997[1:length(full.info.countries.second.time.span), ]
y.1998 = y.1998[1:length(full.info.countries.second.time.span), ]
y.1999 = y.1999[1:length(full.info.countries.second.time.span), ]
y.2000 = y.2000[1:length(full.info.countries.second.time.span), ]
y.2001 = y.2001[1:length(full.info.countries.second.time.span), ]
y.2002 = y.2002[1:length(full.info.countries.second.time.span), ]
y.2003 = y.2003[1:length(full.info.countries.second.time.span), ]
y.2004 = y.2004[1:length(full.info.countries.second.time.span), ]
y.2005 = y.2005[1:length(full.info.countries.second.time.span), ]
y.2006 = y.2006[1:length(full.info.countries.second.time.span), ]
y.2007 = y.2007[1:length(full.info.countries.second.time.span), ]
y.2008 = y.2008[1:length(full.info.countries.second.time.span), ]
y.2009 = y.2009[1:length(full.info.countries.second.time.span), ]
y.2010 = y.2010[1:length(full.info.countries.second.time.span), ]
y.2011 = y.2011[1:length(full.info.countries.second.time.span), ]
y.2012 = y.2012[1:length(full.info.countries.second.time.span), ]
y.2013 = y.2013[1:length(full.info.countries.second.time.span), ]
y.2014 = y.2014[1:length(full.info.countries.second.time.span), ]

# Building WM for the first period
wm.2 = list(as.matrix(y.1955), as.matrix(y.1956), as.matrix(y.1957), as.matrix(y.1958), as.matrix(y.1959), as.matrix(y.1960), as.matrix(y.1961), as.matrix(y.1962), as.matrix(y.1963), as.matrix(y.1964), as.matrix(y.1965), as.matrix(y.1966), as.matrix(y.1967), as.matrix(y.1968), as.matrix(y.1969), as.matrix(y.1970), as.matrix(y.1971), as.matrix(y.1972), as.matrix(y.1973), as.matrix(y.1974), as.matrix(y.1975), as.matrix(y.1976), as.matrix(y.1977), as.matrix(y.1978), as.matrix(y.1979), as.matrix(y.1980), as.matrix(y.1981), as.matrix(y.1982), as.matrix(y.1983), as.matrix(y.1984), as.matrix(y.1985), as.matrix(y.1986), as.matrix(y.1987), as.matrix(y.1988), as.matrix(y.1989), as.matrix(y.1990), as.matrix(y.1991), as.matrix(y.1992), as.matrix(y.1993), as.matrix(y.1994), as.matrix(y.1995), as.matrix(y.1996), as.matrix(y.1997), as.matrix(y.1998), as.matrix(y.1999), as.matrix(y.2000), as.matrix(y.2001), as.matrix(y.2002), as.matrix(y.2003), as.matrix(y.2004), as.matrix(y.2005), as.matrix(y.2006), as.matrix(y.2007), as.matrix(y.2008), as.matrix(y.2009), as.matrix(y.2010), as.matrix(y.2011), as.matrix(y.2012), as.matrix(y.2013), as.matrix(y.2014))



########################################################
# GVARX
########################################################

# Present results like Table 4.5 in Box-Steffensmeier2014a, 121.

# GVARX
# https://www.rdocumentation.org/packages/GVARX/versions/1.3
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

summary(mainOUTPUT.1$gvecm[[1]]) # Austria.Hungary 
summary(mainOUTPUT.1$gvecm[[2]]) # Belgium 
summary(mainOUTPUT.1$gvecm[[3]]) # France 
summary(mainOUTPUT.1$gvecm[[4]]) # Germany 
summary(mainOUTPUT.1$gvecm[[5]]) # Italy 
summary(mainOUTPUT.1$gvecm[[6]]) # Russia 
summary(mainOUTPUT.1$gvecm[[7]]) # Spain 
summary(mainOUTPUT.1$gvecm[[8]]) # United Kingdom  
summary(mainOUTPUT.1$gvecm[[9]]) # United States


########################################################
# Second Period
########################################################
p_load(GVARX)

p.2=3 # The number of lag for Xt matrix
FLag.2=3 # The number of lag for foreign variables in country-specific VAR
lag.max.2=3 # The maximal number of lag for estimating country-specific VAR
type.2="none" # Model specificaiton for VAR. As in package vars, we have four selection: "none","const","trend", "both".
ic.2="SC" # Information criteria for optimal lag.As in package vars, we have four selection: "AIC", "HQ", "SC", and "FPE".

options(scipen=9999999)

mainOUTPUT.2 = GVECMest(
        data = cow.d.2,
        p = p.2,
        FLag = FLag.2,
        lag.max = lag.max.2,
        type = type.2,
        ic = ic.2,
        weight.matrix=wm.2)


summary(mainOUTPUT.2$gvecm[[1]]) # Argentina 
summary(mainOUTPUT.2$gvecm[[2]]) # Australia 
summary(mainOUTPUT.2$gvecm[[3]]) # Austria 
summary(mainOUTPUT.2$gvecm[[4]]) # Belgium 
summary(mainOUTPUT.2$gvecm[[5]]) # Brazil 






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




