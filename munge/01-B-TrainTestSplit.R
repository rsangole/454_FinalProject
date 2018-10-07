raw_data %>% tabyl(cover_type) %>% adorn_pct_formatting()
# cover_type	        n	    percent
# spruce_fir	        211840	36.5%
# lodgepole_pine	    283301	48.8%
# ponderosa_pine	    35754	6.2%
# cottonwood_Willow	    2747	0.5%
# aspen	                9493	1.6%
# douglasfir	        17367	3.0%
# krummholz	            20510	3.5%

set.seed(10)

train_index <- caret::createDataPartition(y = raw_data$cover_type,
                           p = 0.7,
                           times = 1,
                           list = F)
head(train_index)

raw_train <- raw_data[train_index,]
raw_test  <- raw_data[-train_index,]

raw_train %>% tabyl(cover_type) %>% adorn_pct_formatting()
raw_test %>% tabyl(cover_type) %>% adorn_pct_formatting()

dim(raw_train)
dim(raw_test)

save(raw_train, file = 'cache/raw_train.rdata')
save(raw_test, file = 'cache/raw_test.rdata')
save(raw_data,file = 'cache/raw_data.rdata')
