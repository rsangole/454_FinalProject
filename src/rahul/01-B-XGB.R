# Model #1
# First test for one-vs-all for `spruce_fir` vs all

df_m1 <- df_xg %>%
    make_response_var_one_vs_all("spruce_fir")

df_m1
