library(readxl)

Fake_Data_and_Metadata_Final_no_pass <- read_excel("~/Fake+Data+and+Metadata+-+Final+no+pass.xlsx", 
                                                   sheet = "Daily use of a WF credit card ")

twopersondf = Fake_Data_and_Metadata_Final_no_pass[Fake_Data_and_Metadata_Final_no_pass$masked_id==1 | Fake_Data_and_Metadata_Final_no_pass$masked_id==2,]



library(reshape)
reshaped_df = cast(Fake_Data_and_Metadata_Final_no_pass, masked_id ~ Des1, value = 'Payment', fun.aggregate=mean)
reshaped_df[is.na(reshaped_df)] = 0

library(randomForest)
fit <- randomForest(ALCOHOL ~ AUTO + APPLIANCES + AIRLINES,
                    data=reshaped_df, 
                    importance=TRUE, 
                    ntree=2000)
print(fit)

# Try to change to percentage
reshaped_df = reshaped_df[,-1]
rs = rowSums(reshaped_df)
reshaped_df_norm = t(apply(reshaped_df, 1, function(x) x/sum(x)))
colnames(reshaped_df_norm) = colnames(reshaped_df)
reshaped_df_norm = as.data.frame(reshaped_df_norm)

library(randomForest)
fit <- randomForest(ALCOHOL ~ AUTO + APPLIANCES + AIRLINES,
                    data=reshaped_df_norm, 
                    importance=TRUE, 
                    ntree=2000)
print(fit)

hist(reshaped_df_norm$ALCOHOL)
