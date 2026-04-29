
#' Plot Dimensionality-Reduced Item Coordinates
#'
#' Takes the output of \code{reduce_cosim_dims()} and produces interactive
#' plotly scatter plots (2D and optionally 3D) with hover-only item labels,
#' plus static ggplot/ggrepel versions (\code{*_gg} suffix).
#'
#' @param coords Data frame. Output of \code{reduce_cosim_dims()}.
#' @param colour_by Character string. Column in \code{coords} to use for
#'   colouring points (default \code{"scale"}).
#' @param label_width Integer. Maximum characters per line in hover tooltips
#'   before a line break is inserted (default 20).
#' @param show_colour Logical. Whether to show the colour legend (default \code{TRUE}).
#'   Points are always coloured by \code{colour_by}; this only toggles the legend.
#' @param colours Named or unnamed character vector of hex colour codes to use for
#'   the groups in \code{colour_by}. Defaults to a built-in palette of 20 distinct
#'   colours. Pass a named vector (e.g. \code{c(group1 = "#e63946", group2 = "#457b9d")})
#'   to assign colours to specific group labels.
#' @param save_plots Logical. If \code{TRUE}, saves each plot to disk using its
#'   list name (e.g. \code{plot_mds.png}, \code{plot_mds_3d.html}). plotly plots
#'   are saved as standalone HTML; ggplot plots are saved as PNG. Default \code{FALSE}.
#' @param plot_file_prefix Character string. Prefix prepended to each saved file
#'   name. Can include a directory path (e.g. \code{"results/run1_"}). Default
#'   \code{"./"} saves to the current working directory with no prefix.
#'   The directory portion is created if it does not exist.
#' @param plot_width,plot_height Numeric. Width and height (in inches for PNG,
#'   pixels for HTML) for saved plots. Defaults: 10 x 8 inches / 1000 x 800 px.
#'
#' @return A list with:
#'   \describe{
#'     \item{plot_mds}{Interactive plotly 2D scatter — MDS.}
#'     \item{plot_tsne}{Interactive plotly 2D scatter — t-SNE.}
#'     \item{plot_umap}{Interactive plotly 2D scatter — UMAP.}
#'     \item{plot_mds_gg}{Static ggplot with ggrepel labels — MDS.}
#'     \item{plot_tsne_gg}{Static ggplot with ggrepel labels — t-SNE.}
#'     \item{plot_umap_gg}{Static ggplot with ggrepel labels — UMAP.}
#'     \item{plot_mds_3d}{plotly 3D scatter — MDS (only when 3D coords present).}
#'     \item{plot_tsne_3d}{plotly 3D scatter — t-SNE (only when 3D coords present).}
#'     \item{plot_umap_3d}{plotly 3D scatter — UMAP (only when 3D coords present).}
#'   }
#'
#' @importFrom ggplot2 ggplot aes geom_point geom_text labs theme_minimal scale_colour_manual ggsave
#' @importFrom plotly plot_ly layout
#' @importFrom stringr str_wrap
#' @importFrom htmlwidgets saveWidget
#'
#' @examples
#' \dontrun{
#' result <- cosim_itens_scales(item_emb, scale_emb, item_text, factor_itens, factor_scale)
#' coords <- reduce_cosim_dims(result$cosim_mat, factor_scale)
#'
#' plots  <- plot_cosim_dims(coords, colour_by = "scale")
#' plots$plot_mds        # interactive plotly
#' plots$plot_mds_gg     # static ggplot with ggrepel
#'
#' # If 3D coords were computed:
#' coords3 <- reduce_cosim_dims(result$cosim_mat, factor_scale, n_dims = 3)
#' plots3  <- plot_cosim_dims(coords3, colour_by = "scale")
#' plots3$plot_mds_3d
#' }
#'
#' @export
plot_cosim_dims <- function(
    coords,
    colour_by        = "scale",
    label_width      = 20,
    show_colour      = TRUE,
    colours          = c(
      "#e63946", "#457b9d", "#2a9d8f", "#e9c46a", "#f4a261",
      "#6a4c93", "#43aa8b", "#f3722c", "#577590", "#90be6d",
      "#c77dff", "#4cc9f0", "#fb8500", "#06d6a0", "#ef476f",
      "#118ab2", "#ffd166", "#8ecae6", "#a8dadc", "#264653"
    ),
    save_plots       = FALSE,
    plot_file_prefix = "./",
    plot_width       = 10,
    plot_height      = 8) {

  has_3d <- all(c("mds_3", "tsne_3", "umap_3") %in% names(coords))

  coords$label_text <- stringr::str_wrap(coords$item_text, width = label_width)
  coords$hover_text <- gsub("\n", "<br>", coords$label_text)

  # ── plotly 2D helper ─────────────────────────────────────────────────────────
  make_plotly_2d <- function(df, x_col, y_col, title) {
    plotly::plot_ly(
      data          = df,
      x             = stats::as.formula(paste0("~", x_col)),
      y             = stats::as.formula(paste0("~", y_col)),
      color         = stats::as.formula(paste0("~", colour_by)),
      colors        = colours,
      text          = ~hover_text,
      hovertemplate = "%{text}<extra></extra>",
      type          = "scatter",
      mode          = "markers",
      marker        = list(size = 8, opacity = 0.8)
    ) |>
      plotly::layout(title = title, showlegend = show_colour)
  }

  # ── static ggplot helper (geom_text — ggrepel crashes on this system) ────────
  make_ggrepel <- function(df, x_col, y_col, title) {
    p <- ggplot2::ggplot(df, ggplot2::aes(
      x      = .data[[x_col]],
      y      = .data[[y_col]],
      colour = .data[[colour_by]],
      label  = label_text
    )) +
      ggplot2::geom_point(size = 2, alpha = 0.8) +
      ggplot2::geom_text(size = 2.5, vjust = -0.6, check_overlap = TRUE,
                         show.legend = FALSE) +
      ggplot2::scale_colour_manual(values = colours) +
      ggplot2::labs(title = title, x = x_col, y = y_col, colour = colour_by) +
      ggplot2::theme_minimal() +
      if (!show_colour) ggplot2::theme(legend.position = "none") else NULL
    p
  }

  out <- list(
    plot_mds     = make_plotly_2d(coords, "mds_1",  "mds_2",  "MDS"),
    plot_tsne    = make_plotly_2d(coords, "tsne_1", "tsne_2", "t-SNE"),
    plot_umap    = make_plotly_2d(coords, "umap_1", "umap_2", "UMAP"),
    plot_mds_gg  = make_ggrepel(coords,  "mds_1",  "mds_2",  "MDS"),
    plot_tsne_gg = make_ggrepel(coords,  "tsne_1", "tsne_2", "t-SNE"),
    plot_umap_gg = make_ggrepel(coords,  "umap_1", "umap_2", "UMAP")
  )

  # ── plotly 3D helper (only when 3D coords present) ───────────────────────────
  if (has_3d) {
    make_plotly_3d <- function(df, x_col, y_col, z_col, title) {
      plotly::plot_ly(
        data          = df,
        x             = stats::as.formula(paste0("~", x_col)),
        y             = stats::as.formula(paste0("~", y_col)),
        z             = stats::as.formula(paste0("~", z_col)),
        color         = stats::as.formula(paste0("~", colour_by)),
        colors        = colours,
        text          = ~hover_text,
        hovertemplate = "%{text}<extra></extra>",
        type          = "scatter3d",
        mode          = "markers",
        marker        = list(size = 4, opacity = 0.8)
      ) |>
        plotly::layout(title = title, showlegend = show_colour)
    }

    out$plot_mds_3d  <- make_plotly_3d(coords, "mds_1",  "mds_2",  "mds_3",  "MDS 3D")
    out$plot_tsne_3d <- make_plotly_3d(coords, "tsne_1", "tsne_2", "tsne_3", "t-SNE 3D")
    out$plot_umap_3d <- make_plotly_3d(coords, "umap_1", "umap_2", "umap_3", "UMAP 3D")
  }

  # ── Save plots to disk if requested ──────────────────────────────────────────
  if (save_plots) {
    # Ensure the directory portion of the prefix exists
    prefix_dir <- dirname(plot_file_prefix)
    if (nzchar(prefix_dir) && !dir.exists(prefix_dir))
      dir.create(prefix_dir, recursive = TRUE)

    for (nm in names(out)) {
      p <- out[[nm]]

      if (inherits(p, "plotly") || inherits(p, "htmlwidget")) {
        # Interactive plotly → standalone HTML
        file_path <- paste0(plot_file_prefix, nm, ".html")
        # saveWidget requires an absolute path; normalize
        htmlwidgets::saveWidget(
          widget        = p,
          file          = normalizePath(file_path, mustWork = FALSE),
          selfcontained = TRUE
        )
        message("Saved: ", file_path)

      } else if (inherits(p, "ggplot")) {
        # Static ggplot → PNG
        file_path <- paste0(plot_file_prefix, nm, ".png")
        ggplot2::ggsave(
          filename = file_path,
          plot     = p,
          width    = plot_width,
          height   = plot_height,
          dpi      = 150
        )
        message("Saved: ", file_path)
      }
    }
  }

  out
}
