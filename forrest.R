datas <- read.csv("C:/Users/Admin/Downloads/redd_plus_vi.csv")
datas <- na.omit(data)

datas <- datas %>%
  mutate(TRẠNG.THÁI.KIỂM.TRA.THỰC.ĐỊA = recode(TRẠNG.THÁI.KIỂM.TRA.THỰC.ĐỊA, "Đã kểm tra" = "Đã kiểm tra"))
ggplot(datas, aes(x = TRẠNG.THÁI.KIỂM.TRA.THỰC.ĐỊA)) + geom_bar()


source_percent <- datas %>%
  group_by(NGUỒN.GỐC) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100)
pie_chart <- ggplot(source_percent, aes(x = "", y = percent, fill = NGUỒN.GỐC)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "NGUỒN.GỐC") +
  theme_void()+ 
  geom_text(aes(label = paste0(round(percent), "%")), position = position_stack(vjust = 0.5))
print(pie_chart)








