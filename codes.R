library(tidyverse)
library(benford)

dat = read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
# run on May 23, 20221 at 11 pm IST
#head(dat)

#dat = as_tibble(dat)

#unique(dat$location)

check_benford = function(country)
{
  daily_cases = dat %>% filter(location == country) %>% select(new_cases) %>% unlist() %>% as.numeric() %>% na.omit()
  bn = benford(daily_cases)
  
  mat = bn[[2]]
  
  plot(mat[,1], mat[,3], ylim = c(0, max(mat[,2], mat[,3])+5), type = "b", pch = 20, xlab = "Digit", ylab = "Frequency", main = paste0("Benford Distribution for ", country))
  lines(mat[,1], mat[,2], col = "red")
  points(mat[,1], mat[,2], col = "red", pch = 20)
  legend("topright", c("Expected", "Actual"), fill = c("black", "red"))
}



#check_benford("China")



############


# Benford RMSE

rmse = numeric()

for (i in 1:length(unique(dat$location)))
{
  country = unique(dat$location)[i]
  daily_cases = dat %>% filter(location == country) %>% select(new_cases) %>% unlist() %>% as.numeric() %>% discard(is.na)
  
  if(length(daily_cases) == 0) next;
  
  bn = benford(daily_cases)
  
  mat = bn[[2]]
  
  actual = mat[,2]
  expected = mat[,3]
  rmse[i] = sqrt(mean((actual - expected)^2, na.rm = T))
}

world1 = data.frame(location = unique(dat$location), iso = unique(dat$iso_code), rmse = rmse)


##### Map plot

library("sf")
library("rnaturalearth")
library("rnaturalearthdata")

world = ne_countries(scale = "medium", returnclass = "sf")
world1 = merge(world,world1,by.x = "iso_a3", by.y = "iso")

world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

ggplot(data = world1) +
  theme_bw() +
  geom_sf(aes(fill = rmse)) +
  geom_text(data= world_points,aes(x=X, y=Y, label=name), col = "grey", check_overlap = T, size = 1.5) +
  scale_fill_viridis_c(option = "plasma") +
  labs(x = "", y = "", fill = "RMSE", caption = "Benford analysis is used for fraud detection. Low RMSE is associated with low fraud probability.\nData from John Hopkins University (Our World in Data, May 23, 2021). Analysis and viz by Harshvardhan.", title = "Are Countries Manipulating COVID-19 Data?", subtitle = "Benford Analysis on COVID-19 Daily Cases")

ggsave("country.png")


# Saving RMSE and Country Rank
View(world1)

country_summ = data.frame("Country/Region" = world1$admin, "RMSE" = world1$rmse)
country_summ$Rank[order(country_summ$RMSE)] = 1:nrow(country_summ)
View(country_summ)

x = c(country_summ$Country.Region[which(country_summ$RMSE >20)])
cat(x, sep = ", ")

write.csv(country_summ, file = "summary_results.csv")
