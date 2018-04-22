
##WOE - Numeric Variables

n_app <- woe.binning(app_dt, 'target', app_dt, min.perc.total = 0.05, min.perc.class = 0.01, stop.limit = 0.01, event.class = 1)
n_ups <- woe.binning(ups_dt, 'target', ups_dt, min.perc.total = 0.05, min.perc.class = 0.01, stop.limit = 0.01, event.class = 1)
n_chu <- woe.binning(chu_dt, 'target', chu_dt, min.perc.total = 0.05, min.perc.class = 0.01, stop.limit = 0.01, event.class = 1)


woe.binning.table(n_app)
woe.binning.table(n_ups)
woe.binning.table(n_chu)


n_app_woe <- woe.binning.deploy(app_dt, n_app, add.woe.or.dum.var = 'woe')
n_ups_woe <- woe.binning.deploy(ups_dt, n_ups, add.woe.or.dum.var = 'woe')
n_chu_woe <- woe.binning.deploy(chu_dt, n_chu, add.woe.or.dum.var = 'woe')


##WOE - Character Variables

c_app <- woe.binning(app_fact_dt, 'target', app_fact_dt, min.perc.total = 0.05, min.perc.class = 0.01, stop.limit = 0.01, event.class = 1)
c_ups <- woe.binning(ups_fact_dt, 'target', ups_fact_dt, min.perc.total = 0.05, min.perc.class = 0.01, stop.limit = 0.01, event.class = 1)
c_chu <- woe.binning(chu_fact_dt, 'target', chu_fact_dt, min.perc.total = 0.05, min.perc.class = 0.01, stop.limit = 0.01, event.class = 1)

woe.binning.plot(c_app)
woe.binning.table(c_app)
woe.binning.table(c_ups)
woe.binning.table(c_chu)

woe.binning.table(c_chu)[1:10]

c_app_woe <- woe.binning.deploy(app_fact_dt, c_app, add.woe.or.dum.var = 'woe')
c_ups_woe <- woe.binning.deploy(ups_fact_dt, c_ups, add.woe.or.dum.var = 'woe')
c_chu_woe <- woe.binning.deploy(chu_fact_dt, c_chu, add.woe.or.dum.var = 'woe')

## CREATE FINAL MODELING DATASET
temp_n_app_woe <- dplyr::select(n_app_woe,starts_with('woe.'))
temp_c_app_woe <- dplyr::select(c_app_woe,starts_with('woe.'))
app_all_dt <- cbind(app_dt,temp_n_app_woe,temp_c_app_woe)

corr_app_all <-data.frame(round(cor(app_all_dt,app_all_dt$target),5))

temp_n_ups_woe <- dplyr::select(n_ups_woe,starts_with('woe.'))
temp_c_ups_woe <- dplyr::select(c_ups_woe,starts_with('woe.'))
ups_all_dt <- cbind(ups_dt,temp_n_ups_woe,temp_c_ups_woe)

corr_ups_all <-data.frame(round(cor(ups_all_dt,ups_all_dt$target),5))

temp_n_chu_woe <- dplyr::select(n_chu_woe,starts_with('woe.'))
temp_c_chu_woe <- dplyr::select(c_chu_woe,starts_with('woe.'))
chu_all_dt <- cbind(chu_dt,temp_n_chu_woe,temp_c_chu_woe)

corr_chu_all <-data.frame(round(cor(chu_all_dt,chu_all_dt$target),5))



