plot_data <- bind_rows(
  afkast_long %>%
    filter(!is.na(laane_kategori)) %>%
    group_by(laane_kategori) %>%
    summarise(vaerdi = mean(afkast, na.rm = TRUE), .groups = "drop") %>%
    mutate(variable = "Afkastningsgrad"),
  soliditet_long %>%
    filter(!is.na(laane_kategori)) %>%
    group_by(laane_kategori) %>%
    summarise(vaerdi = mean(soliditet, na.rm = TRUE), .groups = "drop") %>%
    mutate(variable = "Soliditetsgrad")
)

plot_data$laane_kategori <- factor(plot_data$laane_kategori,
                                   levels = c("Negativ", "Neutral", "Positiv"))
ggplot(plot_data,
       aes(x = variable, y = vaerdi, fill = laane_kategori)) +
  geom_col(position = position_dodge(width = 0.6), width = 0.55) +
  scale_y_continuous(
    limits = c(0, 45),
    breaks = seq(0, 45, 5),
    expand = c(0, 0)
  ) +
  labs(
    title = "Virksomheder med et højere afkast\nog en højere soliditetsgrad synes bedre om finansieringsklimaet",
    x = "",
    y = "Pct."
  ) +
  scale_fill_manual(
    values = c("Negativ" = "#BFBFBF",
               "Neutral" = "#666666",
               "Positiv" = "#009DE0"),
    labels = c(
      "Negativ" = "Meget dårlige/Dårlige",
      "Neutral" = "Neutrale",
      "Positiv" = "Gode/Meget gode"
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 11),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.margin = margin(t = -5),
    legend.spacing.y = unit(0, "pt"),
    plot.margin = margin(t = 10, r = 20, b = 10, l = 10)
  ) +
  guides(fill = guide_legend(nrow = 1))
