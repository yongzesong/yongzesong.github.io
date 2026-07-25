# 加载必要的包
library(ggplot2)
library(dplyr)
library(reshape2)

# 选择相关的列
selected_columns <- c("P_climate", "LST_climate", "water_soil", "NDVI_plant", "soil_plant", "climate_plant", "climate_soil")
data_subset <- data[selected_columns]

# 添加一个行号作为ID
data_subset$ID <- 1:nrow(data_subset)

# 将数据转换为长格式
data_long <- melt(data_subset, id.vars = "ID", variable.name = "Variable", value.name = "Value")

# 筛选 Value 列中介于 -1 和 1 之间的值
data_filtered <- data_long %>% filter(Value >= -1 & Value <= 1)

# 绘制小提琴图，调整点的透明度、均值点位置，并使用紫色线条
ggplot(data_filtered, aes(x = Variable, y = Value)) +
  geom_jitter(width = 0.1, alpha = 0.3) +
  geom_violin(trim = FALSE, color = "purple") +  # 使用紫色线条
  stat_summary(fun = mean, geom = "point", color = "red", size = 3, position = position_nudge(y = 0.1)) +  # 调整均值点的位置
  labs(x = "Variable", y = "Local Power Value", title = "Violin Plot of Local Power Values") +
  theme_minimal()
