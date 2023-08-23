# =========================== Import Libraries & Data ==========================
Sys.setlocale(locale = 'persian')

library(data.table)
library(ggplot2)

d = fread('iranprovs_mortality_monthly.csv', encoding = 'UTF-8')

# ============================== pre-processing ================================

d$ym_num = d$y + d$m / 12 - 1/24                  # create year-month feature


ym_num_covid = 1398 + 10/12 - 1/24                # set start date of COVID-19
ym_num_start = ym_num_covid - 6                   # set start date of task valid data

ds = d[, .(n = sum(n)), .(y, m, ym_num, prov)]    # remove age & sex features
ds = ds[ym_num > ym_num_start]                    # remove invalid data for this task


provs = unique(ds$prov)                           # provinces
months = unique(ds$m)                             # months

# ========= fit linear model and obtaining the number of excess death ==========

ds$n_predicted = 0                                # model predicted value for death
ds$upper_thresh = 0                               # upper threshold for normal death
ds$lower_thresh = 0                               # lower threshold for normal death

# fit model for each month of each province
for (this_prov in provs) {
  for (this_month in months) {
    condition = ds$prov == this_prov & ds$m == this_month
    
    this_ds = ds[condition]
    this_train_ds = this_ds[ym_num < ym_num_covid]
    
    fit = lm(n ~ ym_num, this_train_ds)
    pvalue = summary(fit)$coefficients[2,4]       # calculating p-value
    
    # choose model prediction for p-value < 0.1
    # otherwise, choose average of samples instead of model prediction
    if (pvalue < 0.1) {
      n_predicted = pmax(predict(fit ,this_ds), 0)
      # if prediction gets negative then use minimum number of death instead
      n_predicted[n_predicted == 0] = min(this_train_ds$n)
    } else {
      n_predicted = rep(mean(this_train_ds$n),length(this_ds$n))
    }
    
    sigma = sd(this_train_ds$n)                   # calculating standard-deviation
    
    ds[condition]$n_predicted = n_predicted                 # save predictions
    ds[condition]$upper_thresh = n_predicted + 2 * sigma    # set upper threshold
    ds[condition]$lower_thresh = n_predicted - 2 * sigma    # set lower threshold
  }
}

# plot one of the linear models
this_ds = ds[prov == provs[2] & m == months[8]]
ggplot(this_ds, aes(ym_num, n))+
  geom_point()+
  geom_line()+
  scale_x_continuous(breaks = 1389:1401)+
  geom_vline(xintercept = ym_num_covid, linetype = 'dashed')+
  geom_line(aes(ym_num, n_predicted), color = 'blue')+
  geom_line(aes(ym_num, upper_thresh), color = 'blue', linetype = 'dashed')+
  geom_line(aes(ym_num, lower_thresh), color = 'blue', linetype = 'dashed')


# calculating excess death according to predictions and upper thresholds of death
ds$excess_death = round((ds$n - ds$n_predicted) * (ds$n > ds$upper_thresh))

# =========================== COVID-19 excess death ============================

COVID19_ds = ds[ym_num >= ym_num_covid]         # COVID-19 data
COVID19_ds$norm_excess_death = COVID19_ds$excess_death / COVID19_ds$n_predicted * 100

# plot heat map: percentage of excess death for each month of each province
ggplot(COVID19_ds, aes(x = ym_num, y = prov)) +
  geom_raster(aes(fill = norm_excess_death)) +
  scale_fill_gradient(low = "white", high = "red")+
  labs(title = "COVID-19 Precentage of Excess Death", x = "time", y = "Province")


# Plot table: excess death for each month of each province
ggplot(COVID19_ds, aes(x = ym_num, y = prov, fill = excess_death)) + 
  geom_tile() + 
  geom_text(aes(label = round(excess_death, 0)), size = 2) + 
  scale_fill_gradient(low = "white", high = "red")+
  labs(x = "time", y = "Province", fill = "Excess Death")

# calculate the total excess death for each province
prov_excess_death = COVID19_ds[, .(excess_death=sum(excess_death)), .(y, prov)]

# plot the total excess death of provinces
ggplot(prov_excess_death, aes(x = reorder(prov, excess_death), y = excess_death, fill=factor(y))) +
  geom_bar(stat="identity")+
  labs(title = "Excess Death of Provinces (during COVID-19)", x = "Province", y = "Excess Death")+
  scale_fill_manual(values=rainbow(20))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


all_excess_death = sum(prov_excess_death$excess_death)
# سیستان و بلوچستان is 16th province in array "provs"
sistan_excess_death = sum(prov_excess_death[prov == provs[16]]$excess_death)
# یزد is 31th province in array "provs"
yazd_excess_death = sum(prov_excess_death[prov == provs[31]]$excess_death)

paste('فوت اضافه کل کشور: ', all_excess_death)
paste('فوت اضافه سیستان و بلوچستان: ', sistan_excess_death)
paste('فوت اضافه یزد: ', yazd_excess_death)


# calculate the total excess death for each province (version 2)
prov_excess_death_v2 = COVID19_ds[, .(excess_death=sum(excess_death)), .(prov)]

# plot the total excess death of provinces (version 2)
ggplot(prov_excess_death_v2, aes(x = reorder(prov, excess_death), y = excess_death)) +
  geom_bar(stat="identity", fill="orange")+
  labs(title = "Excess Death of Provinces (during COVID-19)", x = "Province", y = "Excess Death")+
  geom_text(aes(label=excess_death), vjust=1.6, color="black", size=3.5)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# =================== Best Provinces in Controlling COVID-19 ===================

# age groups: (0, 25), (25, 65), (65, 95+)

new_ds = d[, .(n = sum(n)), .(ym_num, y, m, prov, age_group)]    # remove sex feature

# "bc" stands for "before COVID19"
before_COVID19_ds = new_ds[ym_num >= ym_num_start & ym_num < ym_num_covid]
bc_young_ds = before_COVID19_ds[age_group >= "10-14" & age_group <= "20-24"]
bc_adult_ds = before_COVID19_ds[age_group > "20-24" & age_group <= "60-64"]
bc_old_ds = before_COVID19_ds[age_group > "60-64"]
bc_young_ds = bc_young_ds[, .(n=mean(n)), .(m, prov)]
bc_adult_ds = bc_adult_ds[, .(n=mean(n)), .(m, prov)]
bc_old_ds = bc_old_ds[, .(n=mean(n)), .(m, prov)]


after_COVID19_ds = new_ds[ym_num >= ym_num_covid]

young_ds = after_COVID19_ds[age_group >= "10-14" & age_group <= "20-24"]
adult_ds = after_COVID19_ds[age_group > "20-24" & age_group <= "60-64"]
old_ds = after_COVID19_ds[age_group > "60-64"]


young_ds = young_ds[, .(n=sum(n)), .(ym_num, y, m, prov)]
adult_ds = adult_ds[, .(n=sum(n)), .(ym_num, y, m, prov)]
old_ds = old_ds[, .(n=sum(n)), .(ym_num, y, m, prov)]

young_ds$n2 = 0
adult_ds$n2 = 0
old_ds$n2 = 0

# normalizing the number of deaths for each months of each province
for (this_prov in provs) {
  for (this_month in months) {
    young_ds[m == this_month & prov == this_prov]$n2 = 
      young_ds[m == this_month & prov == this_prov]$n / bc_young_ds[m == this_month & prov == this_prov]$n
    adult_ds[m == this_month & prov == this_prov]$n2 = 
      adult_ds[m == this_month & prov == this_prov]$n / bc_adult_ds[m == this_month & prov == this_prov]$n
    old_ds[m == this_month & prov == this_prov]$n2 = 
      old_ds[m == this_month & prov == this_prov]$n / bc_old_ds[m == this_month & prov == this_prov]$n
  }
}


young_ds$n_preidicted = 0
adult_ds$n_preidicted = 0
old_ds$n_preidicted = 0

slop_result = new_ds[, .(n=sum(n)), .(prov)]
slop_result$young_slop = 0
slop_result$adult_slop = 0
slop_result$old_slop = 0

for (this_prov in provs) {
  condition = young_ds$prov == this_prov
  this_young_ds = young_ds[condition]
  fit = lm(n2 ~ ym_num, this_young_ds)
  slop_result[prov == this_prov]$young_slop = summary(fit)$coefficients[2,1]
  young_ds[condition]$n_preidicted = predict(fit, this_young_ds)
  
  condition = adult_ds$prov == this_prov
  this_adult_ds = adult_ds[condition]
  fit = lm(n2 ~ ym_num, this_adult_ds)
  slop_result[prov == this_prov]$adult_slop = summary(fit)$coefficients[2,1]
  adult_ds[condition]$n_preidicted = predict(fit, this_adult_ds)
  
  condition = old_ds$prov == this_prov
  this_old_ds = old_ds[condition]
  fit = lm(n2 ~ ym_num, this_old_ds)
  slop_result[prov == this_prov]$old_slop = summary(fit)$coefficients[2,1]
  old_ds[condition]$n_preidicted = predict(fit, this_old_ds)
}

ggplot(slop_result, aes(x = reorder(prov, young_slop), y = young_slop)) +
  geom_bar(stat="identity", fill="orange")+
  labs(title="age group=(10, 25)", x = "Province", y = "Slop of Excess Death")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggplot(slop_result, aes(x = reorder(prov, adult_slop), y = adult_slop)) +
  geom_bar(stat="identity", fill="orange")+
  labs(title="age group=(25, 65)", x = "Province", y = "Slop of Excess Death")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

ggplot(slop_result, aes(x = reorder(prov, old_slop), y = old_slop)) +
  geom_bar(stat="identity", fill="orange")+
  labs(title="age group=65+", x = "Province", y = "Slop of Excess Death")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))


# =================== Best Provinces in Controlling COVID-19 ===================


bc_ds = before_COVID19_ds[age_group >= "40-44" & age_group <= "90-94"]
ac_ds = after_COVID19_ds[age_group >= "40-44" & age_group <= "90-94"]

bc_ds = bc_ds[, .(n=mean(n)), .(m, prov)]
ac_ds = ac_ds[, .(n=sum(n)), .(ym_num, y, m, prov)]

ac_ds$n2 = 0
# normalizing the number of deaths for each months of each province
for (this_prov in provs) {
  for (this_month in months) {
    ac_ds[m == this_month & prov == this_prov]$n2 = 
      ac_ds[m == this_month & prov == this_prov]$n / bc_ds[m == this_month & prov == this_prov]$n
  }
}

ac_ds$n_preidicted = 0
slop_result$ac_slop = 0
for (this_prov in provs) {
  condition = ac_ds$prov == this_prov
  this_ac_ds = ac_ds[condition]
  fit = lm(n2 ~ ym_num, this_ac_ds)
  slop_result[prov == this_prov]$ac_slop = summary(fit)$coefficients[2,1]
  ac_ds[condition]$n_preidicted = predict(fit, this_ac_ds)
}

ggplot(slop_result, aes(x = reorder(prov, ac_slop), y = ac_slop)) +
  geom_bar(stat="identity", fill="orange")+
  labs(title="age group=(40, 90)", x = "Province", y = "Slop of Excess Death")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

