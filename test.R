library(ggplot2)
library(leaflet)
library(dplyr)
csv_file_path <- "F:/R code/redd_plus_vi.csv"
data <- read.csv(csv_file_path)
data <- na.omit(data)
colnames(data) <- c("ID", "VĨ ĐỘ", "KINH ĐỘ", "TỈNH", "HUYỆN", "XÃ", "TIỂU KHU", "KHOẢNH", "LÔ", "ĐỊA DANH", "CHỦ RỪNG", "LOẠI ĐẤT/LOẠI RỪNG", "NGUỒN GỐC", "NGÀY", "ƯU TIÊN", "NGƯỜI NHẬN KHOÁN", "TRẠNG THÁI KIỂM TRA THỰC ĐỊA")
data$`VĨ ĐỘ` <- as.numeric(data$`VĨ ĐỘ`)
data$`KINH ĐỘ` <- as.numeric(data$`KINH ĐỘ`)
jungle_table <- table(data$TỈNH, data$`NGUỒN GỐC`)
barplot(jungle_table, beside = TRUE, legend.text = TRUE, col = rainbow(nrow(jungle_table)), xlab = "Tỉnh", ylab = "số lượng", main = "Nguồn gốc rừng tự nhiên")
hist(data$`KINH ĐỘ`, breaks = 20, col = "blue", xlab = "KINH_ĐỘ", main = "Distribution of KINH_ĐỘ")
jungle_counts <- data %>%
  count(`LOẠI ĐẤT/LOẠI RỪNG`)
#type of jungle
jungle_percent <- jungle_counts %>%
  mutate(percent = n / sum(n))
plot <- ggplot(jungle_percent, aes(x = "", y = percent, fill = `LOẠI ĐẤT/LOẠI RỪNG`)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y") +
  labs(x = NULL, y = NULL, fill = "LOẠI ĐẤT/LOẠI RỪNG", title = "Distribution of Jungle Types") +
  scale_fill_brewer(palette = "Set3")  # Choose a color palette
plot

ggplot(data, aes(x = `VĨ ĐỘ`, y = `KINH ĐỘ`)) +
  geom_point(color = "red") +
  labs(x = "VĨ ĐỘ", y = "KINH ĐỘ", title = "Scatter Plot") +
  theme_minimal()
ggplot(data, aes(x = `TRẠNG THÁI KIỂM TRA THỰC ĐỊA`)) +
  geom_bar()

lam_dong<- data %>%
  filter(TỈNH== "Tỉnh Lâm Đồng")
nghe_an<- data %>%
  filter(TỈNH== "Tỉnh Nghệ An")
map <- leaflet()
map <- leaflet() %>%
  addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
  setView(lng= 108.2772, lat=14.0583,zoom= 5) %>%
  addCircleMarkers(lng = lam_dong$'KINH ĐỘ', lat = lam_dong$'VĨ ĐỘ',color = 'red',radius=5,label = as.character(lam_dong$`CHỦ RỪNG`),clusterOptions = markerClusterOptions(),group= "Lâm Đồng")%>%
  addCircleMarkers(lng = nghe_an$'KINH ĐỘ', lat = nghe_an$'VĨ ĐỘ',radius=5,label = as.character(nghe_an$`CHỦ RỪNG`),  clusterOptions = markerClusterOptions(),group= "Nghệ An")%>%
  addLayersControl(overlayGroups = c("Lâm Đồng","Nghệ An"), options= layersControlOptions(collapsed = FALSE))
map
