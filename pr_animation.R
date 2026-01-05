# R 4.4.2
require(ggplot2) # ggplot
require(gganimate) # transition_reveal, view_follow, anim_save, animate
require(gifski) # creating gif
require(magick) # image_read_svg, image_modulate
require(rsvg) # loaded by image_read_svg
require(jsonlite) # fromJSON
require(grid) # rasterGrob
require(zoo) # rollsum

pullRequests <- fromJSON("results_pull_requests.json")
pullRequests <- within(pullRequests, {
	month <- paste0(month, "-01")
	month <- as.Date(month, format="%Y-%m-%d")
	quarter <- rollmean(total, k=3, na.pad=TRUE, align="right")
})

lilogo <- image_read_svg("lichesslogowhite.svg", width=400, height=400) |>
	image_modulate(brightness=20) |>
	rasterGrob(interpolate=TRUE)

graph <- ggplot(pullRequests, aes(x=month, y=total, group=1)) +
	annotation_custom(lilogo) +
	ylab("Pull requests") +
	theme(text=element_text(size=16, colour="#EEEEEE"),
				axis.text=element_text(colour="#FFFFFF"),
				axis.title.x=element_blank(),
				panel.border=element_blank(),
				panel.background=element_rect(fill=NA),
				panel.grid.major=element_blank(),
				panel.grid.minor=element_blank(),
				plot.background=element_rect(fill="#161512", linetype=0),
				legend.position="none",
				plot.margin=unit(c(1, 1, 1, 1), "cm")) +
	scale_x_date(date_breaks="1 year", date_labels="%Y", expand=c(0, 0, 0.05, 0)) +
	geom_line(linewidth=2, alpha=.8, color="#A7711C", aes(group=1)) +
	geom_point(size=2, color="#A7711C")

graph_quarter <- graph + aes(y=quarter) + ylab("Monthly PRs (Rolling 3 Month Mean)")

anim_save("pulls_q.gif",
					animate(graph_quarter + transition_reveal(month) + view_follow(fixed_y=TRUE),
									height=500, width=800, fps=30, duration=10, end_pause=60, res=100,
									renderer=gifski_renderer()))
