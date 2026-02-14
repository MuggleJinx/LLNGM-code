library(abind)
library(ncdf4)
library(mapdata)
library(splancs)

extract.summer <- function(file.name) {
  ncin <- nc_open(file.name)
  p <- ncvar_get(ncin, "pr")
  t <- ncvar_get(ncin, "time")
  T <- as.Date(t, origin = "1979-01-01", "%Y-%m-%d")
  years <- unique(as.numeric(format(T, "%Y")))
  months <- as.numeric(format(T, "%m"))
  years.save <- years
  for (i in 1:length(years)) {
    ind <-
      (as.numeric(format(T, "%Y")) == years[i]) * (months > 5) * (months < 9)
    if (sum(ind) == 0) {
      years.save <- setdiff(years.save, years[i])
    }
  }
  precip <- array(dim = c(147, 116, length(years.save)))
  for (i in 1:length(years.save)) {
    ind <-
      (as.numeric(format(T, "%Y")) == years.save[i]) * (months > 5) * (months < 9)
    precip[, , i] <- apply(p[, , ind == 1], c(1, 2), mean)
  }
  return(list(years = years.save, precip = precip))
}

#################
# Download data #
#################
base.url <- "https://www.earthsystemgrid.org/dataset/narccap.ecp2.ncep.table2/file/"
file1 <- "pr_ECP2_ncep_1979010103.nc"
file2 <- "pr_ECP2_ncep_1981010103.nc"
file3 <- "pr_ECP2_ncep_1986010103.nc"
file4 <- "pr_ECP2_ncep_1991010103.nc"
file5 <- "pr_ECP2_ncep_1996010103.nc"
file6 <- "pr_ECP2_ncep_2001010103.nc"

download.file(sprintf("%s%s", base.url, file1), file1, method = "auto", quiet = FALSE)
download.file(sprintf("%s%s", base.url, file2), file2, method = "auto", quiet = FALSE)
download.file(sprintf("%s%s", base.url, file3), file3, method = "auto", quiet = FALSE)
download.file(sprintf("%s%s", base.url, file4), file4, method = "auto", quiet = FALSE)
download.file(sprintf("%s%s", base.url, file5), file5, method = "auto", quiet = FALSE)
download.file(sprintf("%s%s", base.url, file6), file6, method = "auto", quiet = FALSE)

download.file("http://www.narccap.ucar.edu/data/table4/orog_ECP2.nc", "orog_ECP2.nc",
  method = "auto", quiet = FALSE
)

#############
# read data #
#############
ncin <- nc_open(file1)
lon <- ncvar_get(ncin, "lon") - 360
lat <- ncvar_get(ncin, "lat")
var1 <- extract.summer(file1)
var2 <- extract.summer(file2)
var3 <- extract.summer(file3)
var4 <- extract.summer(file4)
var5 <- extract.summer(file5)
var6 <- extract.summer(file6)

years <- c(
  var1$years,
  var2$year,
  var3$years,
  var4$years,
  var5$years,
  var6$years
)

precip <- abind(var1$precip,
  var2$precip,
  var3$precip,
  var4$precip,
  var5$precip,
  var6$precip,
  along = 3
)

# convert to cm/day
precip <- precip * 8640

# take cube root:
p <- precip^(1 / 3)

# standardize by pointwise means
mp <- apply(p, c(1, 2), mean)
Y <- p
for (i in 1:26) {
  Y[, , i] <- p[, , i] - mp
}

# extract data for domain of interest
usa <- map("usa")
border <- as.matrix(cbind(usa$x, usa$y))
border.points <- border[!is.na(border[, 1]), ]
i <- which(abs(diff(border.points[, 1])) > 20)
border.points <- rbind(border.points[1:i[2], ])
loc.g <- cbind(c(lon), c(lat))
xy.in <- inout(loc.g, border.points, bound = TRUE)

Y <- matrix(Y, 147 * 116, 26)
Y <- Y[xy.in == 1, ]
lat <- c(lat)
lat <- lat[xy.in == 1]
lon <- c(lon)
lon <- lon[xy.in == 1]

loc <- cbind(lon, lat)

ncalt <- nc_open("orog_ECP2.nc")
alt.lat <- ncvar_get(ncalt, varid = "lat")
alt.lon <- ncvar_get(ncalt, varid = "lon")
alt <- ncvar_get(ncalt, varid = "orog")
nc_close(ncalt)
loc.alt <- cbind(c(alt.lon), c(alt.lat))
loc.alt[, 1] <- loc.alt[, 1] - 360

save(Y, years, loc, xy.in, loc.alt, alt, loc.g, border.points, file = "precip.data2.Rdata")
