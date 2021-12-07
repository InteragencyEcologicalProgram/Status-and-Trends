
#########################################################
## Overwrite standardized plot margins:

smr_theme_update <- function() {
	smonitr::smr_theme() + 
		theme(plot.margin=margin(t=3,r=2,b=1,l=1, unit="pt"))
}

#########################################################
## Automated caption and alt text:
Caption <- ggplot2::ggproto("Caption", Stat,
  required_aes = c("x","y"),
	default_aes = aes(label = stat(caption)),
	
  setup_params = function(data, params) {

		lt_avg <- mean(data$y, na.rm=TRUE)
		lt_sd <- sd(data$y, na.rm=TRUE)
		
		## Handle cases where current index is missing:
		current_ind <- which(data$x == params$report_year)
		stopifnot(length(current_ind) %in% c(0,1))
		current <- ifelse(length(current_ind) == 0, NA, data$y[current_ind])
		
		## Create figure caption:
		if(is.na(current)) {
			if(!is.na(lt_avg) && lt_avg >= 100) {
				caption_fmt <- "The long-term average is %0.0f."
			} else {
				caption_fmt <- "The long-term average is %0.1f."
			}
			caption <- sprintf(fmt=caption_fmt, lt_avg)
		} else {
		  lowerBound <- 0.9*lt_avg
		  upperBound <- 1.1*lt_avg
		  if(lowerBound <= current & current <= upperBound) {
				qualifier <- "close to"
			} else if(current < lt_avg) {
				qualifier <- "lower than"
			} else if(current > lt_avg) {
				qualifier <- "higher than"
			}
			
			caption_fmt <- "In %d, %s was %s the long-term average."												 
			caption <- sprintf(fmt=caption_fmt, params$report_year, params$stat_name, 
												 qualifier)												 
		}
		
		list(
			lt_avg = lt_avg,
			stat_name = params$stat_name,
			report_year = params$report_year, 
			caption = caption, 
			na.rm = params$na.rm
		)
  },
	
	compute_group = function(data, scales, lt_avg, stat_name, report_year, caption) {
		data.frame("x"=0, "y"=0, "caption"=caption)
  }
)

smr_caption <- function(mapping = NULL, data = NULL, geom = "text",
												position = "identity", na.rm = FALSE, show.legend = NA,
												inherit.aes = TRUE, alpha=0, ...) {
  ggplot2::layer(
    stat = Caption, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm=na.rm, alpha=0, ...)
  )
}


Alttext <- ggplot2::ggproto("Alttext", Stat,
  required_aes = c("x","y"),
	default_aes = aes(label = stat(alttext)),

	compute_group = function(data, scales, stat_name) {
		## Create figure alt text:
		alttext_fmt <- "Graph of %s from %d to %d. Values range from %0.1f to %0.1f."
		alttext <- sprintf(fmt=alttext_fmt, stat_name, 
											 min(data$x, na.rm=TRUE), max(data$x, na.rm=TRUE),
											 min(data$y, na.rm=TRUE), max(data$y, na.rm=TRUE))
		data.frame("x"=0, "y"=0, "alttext"=alttext)
  }
)

smr_alttext <- function(mapping = NULL, data = NULL, geom = "text",
												position = "identity", na.rm = FALSE, show.legend = NA,
												inherit.aes = TRUE, alpha=0, ...) {
  ggplot2::layer(
    stat = Alttext, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, alpha=alpha, ...)
  )
}


#########################################################
## Manual caption and alt text:

CaptionManual <- ggplot2::ggproto("CaptionManual", Stat,
  required_aes = c("x","y"),
	default_aes = aes(label = stat(caption)),
	
  setup_params = function(data, params) {	
		list(
			caption = params$caption, 
			na.rm = params$na.rm
		)
  },
	
	compute_panel = function(data, scales, caption) {
		data.frame("x"=0, "y"=0, "caption"=caption)
  }
)

smr_caption_manual <- function(mapping = NULL, data = NULL, geom = "text",
															 position = "identity", na.rm = FALSE, show.legend = NA,
															 inherit.aes = TRUE, alpha=0, ...) {
  ggplot2::layer(
    stat = CaptionManual, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm=na.rm, alpha=0, ...)
  )
}


AlttextManual <- ggplot2::ggproto("AlttextManual", Stat,
  required_aes = c("x","y"),
	default_aes = aes(label = stat(alttext)),

  setup_params = function(data, params) {	
		list(
			alttext = params$alttext, 
			na.rm = params$na.rm
		)
  },
	
	compute_panel = function(data, scales, alttext) {
		data.frame("x"=0, "y"=0, "alttext"=alttext)
  }
)

smr_alttext_manual <- function(mapping = NULL, data = NULL, geom = "text",
															 position = "identity", na.rm = FALSE, show.legend = NA,
															 inherit.aes = TRUE, alpha=0, ...) {
  ggplot2::layer(
    stat = AlttextManual, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, alpha=alpha, ...)
  )
}


#########################################################
## Functions to retrieve caption and alt text, and save plot to svg file.

getCaption <- function(p) {
	dat <- suppressWarnings(ggplot2::ggplot_build(p)$data)
	n <- length(dat)
	
	## Assumes caption is second to last and alttext is last.
	dat[[n-1]]$caption
}

getAlttext <- function(p) {
	dat <- suppressWarnings(ggplot2::ggplot_build(p)$data)
	n <- length(dat)
	
	## Assumes caption is second to last and alttext is last.
	dat[[n]]$alttext
}

getPlot <- function(p) {
	## Used globally: fig_root_svg
	
	file_name <- paste0(deparse(substitute(p)), ".svg")
	suppressWarnings(
		ggsave(p, file=file.path(fig_root_svg,file_name), dpi=200, units="cm", width=7, height=5.5)
	)
	knitr::include_graphics(file.path(fig_root_svg,file_name))
}

getPlotLarge <- function(p) {
	## Used globally: fig_root_svg
	
	file_name <- paste0(deparse(substitute(p)), ".svg")
	suppressWarnings(
		ggsave(p, file=file.path(fig_root_svg,file_name), dpi=300, units="cm", width=13, 
					 height=8.5)
	)
	knitr::include_graphics(file.path(fig_root_svg,file_name))
}

getPlotLarger <- function(p) {
	## Used globally: fig_root_svg
	
	file_name <- paste0(deparse(substitute(p)), ".svg")
	suppressWarnings(
		ggsave(p, file=file.path(fig_root_svg,file_name), dpi=300, units="cm", width=20, 
					 height=10)
	)
	knitr::include_graphics(file.path(fig_root_svg,file_name))
}
