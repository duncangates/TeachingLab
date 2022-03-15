
ipg_forms_test <- readr::read_rds(here::here("Dashboards/IPGBoard/data/ipg_data.rds"))

sort(colnames(ipg_forms_test))[c(1:5, 8:33, 45:48, 53, 57:60, 67:68)] -> ipg_plot_select_names

ipg_plot_select_names %>%
  readr::write_rds(here::here("Dashboards/IPGBoard/data/ipg_plot_select_names.rds"))


sort(colnames(ipg_forms_test))[c(6:7, 34:36, 54:55, 62)] -> ipg_text_select_names

ipg_text_select_names %>%
  readr::write_rds(here::here("Dashboards/IPGBoard/data/ipg_text_select_names.rds"))
