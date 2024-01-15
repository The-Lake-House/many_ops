#!/usr/bin/env Rscript

library(data.table)
library(ggplot2)
library(gridExtra)

get_legend <- function(plot) {
    # ggplot_gtable opens a blank display device, duh
    pdf(file = NULL)
    gtable <- ggplot_gtable(ggplot_build(plot))
    dev.off()
    leg <- which(sapply(gtable[["grobs"]], `[[`, "name") == "guide-box")
    legend <- gtable[["grobs"]][[leg]]
    return(legend)
}

argv <- commandArgs(trailingOnly = TRUE)
if (length(argv) < 2) {
    stop("Usage: ./analyze INPUT_BASE_DIR OUTPUT_BASE_DIR")
}
inputBaseDir <- argv[1]
outputBaseDir <- argv[2]
dir.create(outputBaseDir, showWarnings = FALSE, recursive = TRUE)

analyses <- c("many_inserts", "many_deletes", "many_updates", "many_inserts_partitioned")

loadTrace <- function(path) {
    fread(
        path,
        col.names = c("rep", "value"),
        data.table = FALSE
    )
}

metrics <- data.frame(
    analysis = character(),
    variant = character(),
    rep = integer(),
    du = integer(),
    ls = integer(),
    trace_op = integer(),
    trace_select = integer(),
    runtime_op = integer(),
    runtime_select = integer(),
    hms = integer()
)

trace <- data.frame(
    analysis = character(),
    variant = character(),
    type = character(),
    rep = integer(),
    req = character()
)

sar <- data.frame(
    analysis = character(),
    variant = character(),
    timestamp = character(),
    cpu_percent_user = double(),
    cpu_percent_system = double(),
    cpu_percent_idle = double(),
    mem_percent_used = double(),
    io_tps = double(),
    io_rtps = double(),
    io_wtps = double(),
    disk_await = double()
)

for (analysis in analyses) {

    for (variant in list.files(paste0(inputBaseDir, "/", analysis))) {

        inputDir <- paste0(inputBaseDir, "/", analysis, "/", variant)

        du = scan(paste0(inputDir, "/du"), what = integer(), quiet = TRUE)
        ls = scan(paste0(inputDir, "/ls"), what = integer(), quiet = TRUE)
        runtime_op = scan(paste0(inputDir, "/runtime_op"), what = integer(), quiet = TRUE)
        runtime_select = scan(paste0(inputDir, "/runtime_select"), what = integer(), quiet = TRUE)
        hms = scan(paste0(inputDir, "/hms"), what = integer(), quiet = TRUE)

        trace_op_per_variant <- loadTrace(paste0(inputDir, "/trace_op"))
        trace_select_per_variant <- loadTrace(paste0(inputDir, "/trace_select"))

        metrics <- rbind(
            metrics,
            data.frame(
                analysis = analysis,
                variant = variant,
                rep = 1:length(du),
                du = du,
                ls = ls,
                trace_op = as.vector(table(trace_op_per_variant$rep)),
                trace_select = as.vector(table(trace_select_per_variant$rep)),
                runtime_op = runtime_op,
                runtime_select = runtime_select,
                hms = hms
            )
        )

        trace <- rbind(
            trace,
            data.frame(
                analysis = analysis,
                variant = variant,
                type = "op",
                rep = trace_op_per_variant[["rep"]],
                req = trace_op_per_variant[["value"]]
            )
        )

        trace <- rbind(
            trace,
            data.frame(
                analysis = analysis,
                variant = variant,
                type = "select",
                rep = trace_select_per_variant[["rep"]],
                req = trace_select_per_variant[["value"]]
            )
        )

        cpu <- fread(paste0(inputDir, "/sar_cpu"))
        mem <- fread(paste0(inputDir, "/sar_mem"))
        io <- fread(paste0(inputDir, "/sar_io"))
        disk <- fread(paste0(inputDir, "/sar_disk"))
        sar <- rbind(
            sar,
            data.frame(
                analysis = analysis,
                variant = variant,
                timestamp = cpu[["timestamp"]],
                cpu_percent_user = cpu[["%user"]],
                cpu_percent_system = cpu[["%system"]],
                cpu_percent_idle = cpu[["%idle"]],
                mem_percent_used = mem[["%memused"]],
                io_tps = io[["tps"]],
                io_rtps = io[["rtps"]],
                io_wtps = io[["wtps"]],
                disk_await = disk[["await"]]
            )
        )

    }

}

metrics[["analysis"]] <- factor(
    metrics[["analysis"]],
    levels = analyses,
    labels = c("Many Inserts", "Many Deletes", "Many Updates", "Many Inserts (Partitioned)")
)

metrics[["variant"]] <- factor(
    metrics[["variant"]],
    levels = c("hive", "hudi_cow", "hudi_mor", "iceberg_cow", "iceberg_mor", "delta_no_deletion_vectors", "delta_deletion_vectors"),
    labels = c("Hive", "Hudi (CoW)", "Hudi (MoR)", "Iceberg (CoW)", "Iceberg (MoR)", "Delta Lake (Without Deletion Vectors)", "Delta Lake (With Deletion Vectors)")
)

metrics[["du"]] <- metrics[["du"]] / 1024

sar[["analysis"]] <- factor(
    sar[["analysis"]],
    levels = analyses,
    labels = c("Many Inserts", "Many Deletes", "Many Updates", "Many Inserts (Partitioned)")
)

sar[["variant"]] <- factor(
    sar[["variant"]],
    levels = c("hive", "hudi_cow", "hudi_mor", "iceberg_cow", "iceberg_mor", "delta_no_deletion_vectors", "delta_deletion_vectors"),
    labels = c("Hive", "Hudi (CoW)", "Hudi (MoR)", "Iceberg (CoW)", "Iceberg (MoR)", "Delta Lake (Without Deletion Vectors)", "Delta Lake (With Deletion Vectors)")
)

sar[["timestamp"]] <- as.POSIXct(sar[["timestamp"]], tz = "UTC")

trace[["analysis"]] <- factor(
    trace[["analysis"]],
    levels = analyses,
    labels = c("Many Inserts", "Many Deletes", "Many Updates", "Many Inserts (Partitioned)")
)

trace[["variant"]] <- factor(
    trace[["variant"]],
    levels = c("hive", "hudi_cow", "hudi_mor", "iceberg_cow", "iceberg_mor", "delta_no_deletion_vectors", "delta_deletion_vectors"),
    labels = c("Hive", "Hudi (CoW)", "Hudi (MoR)", "Iceberg (CoW)", "Iceberg (MoR)", "Delta Lake (Without Deletion Vectors)", "Delta Lake (With Deletion Vectors)")
)

trace[["req"]] <- factor(
    trace[["req"]],
    levels = unique(trace[["req"]]),
    labels = unique(trace[["req"]])
)

for (analysis in levels(metrics[["analysis"]])) {

    metricsSubset <- metrics[metrics[["analysis"]] == analysis, ]

    analysisOutputDir <- paste0(outputBaseDir, "/", analysis)
    dir.create(analysisOutputDir, showWarnings = FALSE, recursive = TRUE)

    ggplot(metricsSubset, aes(rep, du, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Size in KiB", linetype = "Variant", color = "Variant", title = "MinIO: Total size of bucket", subtitle = analysis) + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    ggsave(paste0(analysisOutputDir, "/du.pdf"), title = paste0(analysis, ": du"))
    ggsave(paste0(analysisOutputDir, "/du.svg"))

    ggplot(metricsSubset, aes(rep, ls, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Number of files", linetype = "Variant", color = "Variant", title = "MinIO: Number of objects in bucket", subtitle = analysis) + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    ggsave(paste0(analysisOutputDir, "/ls.pdf"), title = paste0(analysis, ": ls"))
    ggsave(paste0(analysisOutputDir, "/ls.svg"))

    ggplot(metricsSubset, aes(rep, trace_op, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Number of requests", linetype = "Variant", color = "Variant", title = "MinIO: Number of S3 requests during update operation", subtitle = analysis) + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    ggsave(paste0(analysisOutputDir, "/trace_op.pdf"), title = paste0(analysis, ": trace_op"))
    ggsave(paste0(analysisOutputDir, "/trace_op.svg"))

    ggplot(metricsSubset, aes(rep, trace_select, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Number of requests", linetype = "Variant", color = "Variant", title = "MinIO: Number of S3 requests during table scan", subtitle = analysis) + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    ggsave(paste0(analysisOutputDir, "/trace_select.pdf"), title = paste0(analysis, ": trace_select"))
    ggsave(paste0(analysisOutputDir, "/trace_select.svg"))

    ggplot(metricsSubset, aes(rep, runtime_op, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Runtime [ms]", linetype = "Variant", color = "Variant", title = "Query runtime during update operation", subtitle = analysis) + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    ggsave(paste0(analysisOutputDir, "/runtime_op.pdf"), title = paste0(analysis, ": runtime_op"))
    ggsave(paste0(analysisOutputDir, "/runtime_op.svg"))

    ggplot(metricsSubset, aes(rep, runtime_select, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Runtime [ms]", linetype = "Variant", color = "Variant", title = "Query runtime during table scan", subtitle = analysis) + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    ggsave(paste0(analysisOutputDir, "/runtime_select.pdf"), title = paste0(analysis, ": runtime_select"))
    ggsave(paste0(analysisOutputDir, "/runtime_select.svg"))

    ggplot(metricsSubset, aes(rep, hms, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Size in bytes", linetype = "Variant", color = "Variant", title = "HMS: Size of Postgres dump", subtitle = analysis) + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    ggsave(paste0(analysisOutputDir, "/hms.pdf"), title = paste0(analysis, ": hms"))
    ggsave(paste0(analysisOutputDir, "/hms.svg"))

    p1 <- ggplot(metricsSubset, aes(rep, runtime_op, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Runtime [ms]", linetype = "Variant", color = "Variant", title = "a) Query runtime during update op.") + theme(legend.position = "top") + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    p2 <- ggplot(metricsSubset, aes(rep, runtime_select, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Runtime [ms]", linetype = "Variant", color = "Variant", title = "b) Query runtime during table scan") + guides(linetype = "none", color = "none") + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    p3 <- ggplot(metricsSubset, aes(rep, trace_op, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Number of requests", linetype = "Variant", color = "Variant", title = "c) Number of S3 requests during update op.") + guides(linetype = "none", color = "none") + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    p4 <- ggplot(metricsSubset, aes(rep, trace_select, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Number of requests", linetype = "Variant", color = "Variant", title = "d) Number of S3 requests during table scan") + guides(linetype = "none", color = "none") + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    p5 <- ggplot(metricsSubset, aes(rep, du, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Size in KiB", linetype = "Variant", color = "Variant", title = "e) Total size of bucket") + guides(linetype = "none", color = "none") + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    p6 <- ggplot(metricsSubset, aes(rep, ls, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Number of files", linetype = "Variant", color = "Variant", title = "f) Number of objects in bucket") + guides(linetype = "none", color = "none") + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    legend <- get_legend(p1)
    p1 <- p1 + guides(linetype = "none", color = "none")
    p <- arrangeGrob(p1, p2, p3, p4, p5, p6, legend,
        ncol = 2,
        layout_matrix = matrix(
            c(1, 2, 3, 4, 5, 6, 7, 7),
            ncol = 2,
            byrow = TRUE
        ),
        heights = c(5/16, 5/16, 5/16, 1/16)
    )
    ggsave(paste0(outputBaseDir, "/", analysis, ".pdf"), plot = p, title = analysis, width = 10, height = 15)
    ggsave(paste0(outputBaseDir, "/", analysis, ".svg"), plot = p, width = 10, height = 15)

    traceOpPlots <- list()
    traceSelectPlots <- list()

    for (variant in levels(metricsSubset[["variant"]])) {

        sarSubset <- sar[sar[["analysis"]] == analysis & sar[["variant"]] == variant, ]

        # Skip empty levels
        if (nrow(sarSubset) == 0) {
            next
        }

        variantOutputDir <- paste0(outputBaseDir, "/", analysis, "/", variant)
        dir.create(variantOutputDir, showWarnings = FALSE, recursive = TRUE)

        ggplot(sarSubset, aes(timestamp, cpu_percent_user)) + geom_point() + ylim(0, 100) + labs(x = "Time in UTC", title = "CPU: %user", subtitle = paste0(analysis, "/", variant))
        ggsave(paste0(variantOutputDir, "/sar_cpu_user.pdf"), title = paste0(analysis, "/", variant, "/cpu/%user"))
        ggsave(paste0(variantOutputDir, "/sar_cpu_user.svg"))

        ggplot(sarSubset, aes(timestamp, cpu_percent_system)) + geom_point() + ylim(0, 100) + labs(x = "Time in UTC", title = "CPU: %system", subtitle = paste0(analysis, "/", variant))
        ggsave(paste0(variantOutputDir, "/sar_cpu_system.pdf"), title = paste0(analysis, "/", variant, "/cpu/%system"))
        ggsave(paste0(variantOutputDir, "/sar_cpu_system.svg"))

        ggplot(sarSubset, aes(timestamp, cpu_percent_idle)) + geom_point() + ylim(0, 100) + labs(x = "Time in UTC", title = "CPU: %idle", subtitle = paste0(analysis, "/", variant))
        ggsave(paste0(variantOutputDir, "/sar_cpu_idle.pdf"), title = paste0(analysis, "/", variant, "/cpu/%idle"))
        ggsave(paste0(variantOutputDir, "/sar_cpu_idle.svg"))

        ggplot(sarSubset, aes(timestamp, mem_percent_used)) + geom_point() + ylim(0, 100) + labs(x = "Time in UTC", title = "RAM: %memused", subtitle = paste0(analysis, "/", variant))
        ggsave(paste0(variantOutputDir, "/sar_mem_memused.pdf"), title = paste0(analysis, "/", variant, "/mem/%memused"))
        ggsave(paste0(variantOutputDir, "/sar_mem_memused.svg"))

        ggplot(sarSubset, aes(timestamp, io_tps)) + geom_point() + ylim(0, NA) + labs(x = "Time in UTC", title = "I/O: tps", subtitle = paste0(analysis, "/", variant))
        ggsave(paste0(variantOutputDir, "/sar_io_tps.pdf"), title = paste0(analysis, "/", variant, "/io/tps"))
        ggsave(paste0(variantOutputDir, "/sar_io_tps.svg"))

        ggplot(sarSubset, aes(timestamp, io_rtps)) + geom_point() + ylim(0, NA) + labs(x = "Time in UTC", title = "I/O: rtps", subtitle = paste0(analysis, "/", variant))
        ggsave(paste0(variantOutputDir, "/sar_io_rtps.pdf"), title = paste0(analysis, "/", variant, "/io/rtps"))
        ggsave(paste0(variantOutputDir, "/sar_io_rtps.svg"))

        ggplot(sarSubset, aes(timestamp, io_wtps)) + geom_point() + ylim(0, NA) + labs(x = "Time in UTC", title = "I/O: wtps", subtitle = paste0(analysis, "/", variant))
        ggsave(paste0(variantOutputDir, "/sar_io_wtps.pdf"), title = paste0(analysis, "/", variant, "/io/wtps"))
        ggsave(paste0(variantOutputDir, "/sar_io_wtps.svg"))

        ggplot(sarSubset, aes(timestamp, disk_await)) + geom_point() + ylim(0, NA) + labs(x = "Time in UTC", title = "Disk: await", subtitle = paste0(analysis, "/", variant))
        ggsave(paste0(variantOutputDir, "/sar_disk_await.pdf"), title = paste0(analysis, "/", variant, "/disk/await"))
        ggsave(paste0(variantOutputDir, "/sar_disk_await.svg"))

        traceSubset <- trace[trace[["analysis"]] == analysis & trace[["variant"]] == variant, ]

        p_op <- ggplot(traceSubset[traceSubset[["type"]] == "op", ], aes(rep, fill = req)) + geom_bar(width = 1) + ylim(0, NA) + labs(x = "Rep", y = "Number of requests", fill = "Operation") + scale_fill_discrete(drop = FALSE)
        ggsave(paste0(variantOutputDir, "/trace_op.pdf"), plot = p_op + labs(title = "MinIO: Number of S3 requests during update operation", subtitle = paste0(analysis, ": ", variant)), title = paste0(analysis, "/", variant, "/trace_op"))
        ggsave(paste0(variantOutputDir, "/trace_op.svg"), plot = p_op + labs(title = "MinIO: Number of S3 requests during update operation", subtitle = paste0(analysis, ": ", variant)))
        traceOpPlots <- c(traceOpPlots, list(p_op + labs(title = variant)))

        p_select <- ggplot(traceSubset[traceSubset[["type"]] == "select", ], aes(rep, fill = req)) + geom_bar(width = 1) + ylim(0, NA) + labs(x = "Rep", y = "Number of requests", fill = "Operation") + scale_fill_discrete(drop = FALSE)
        ggsave(paste0(variantOutputDir, "/trace_select.pdf"), plot = p_select + labs(title = "MinIO: Number of S3 requests during table scan", subtitle = paste0(analysis, " / ", variant)), title = paste0(analysis, "/", variant, "/trace_select"))
        ggsave(paste0(variantOutputDir, "/trace_select.svg"), plot = p_select + labs(title = "MinIO: Number of S3 requests during table scan", subtitle = paste0(analysis, " / ", variant)))
        traceSelectPlots <- c(traceSelectPlots, list(p_select + labs(title = variant)))

    }

    legend <- get_legend(traceOpPlots[[1]])
    for (i in seq_along(traceOpPlots)) {
        traceOpPlots[[i]] <- traceOpPlots[[i]] + guides(fill = "none")
    }
    traceOpPlots <- c(traceOpPlots, list(legend))
    traceOpPlots <- c(traceOpPlots, list(ncol = 2))
    p_op <- do.call(arrangeGrob, traceOpPlots)
    ggsave(paste0(analysisOutputDir, "/trace_op_reqs.pdf"), plot = p_op, title = analysis, width = 10, height = 15)
    ggsave(paste0(analysisOutputDir, "/trace_op_reqs.svg"), plot = p_op, width = 10, height = 15)

    legend <- get_legend(traceSelectPlots[[1]])
    for (i in seq_along(traceSelectPlots)) {
        traceSelectPlots[[i]] <- traceSelectPlots[[i]] + guides(fill = "none")
    }
    traceSelectPlots <- c(traceSelectPlots, list(legend))
    traceSelectPlots <- c(traceSelectPlots, list(ncol = 2))
    p_select <- do.call(arrangeGrob, traceSelectPlots)
    ggsave(paste0(analysisOutputDir, "/trace_select_reqs.pdf"), plot = p_select, title = analysis, width = 10, height = 15)
    ggsave(paste0(analysisOutputDir, "/trace_select_reqs.svg"), plot = p_select, width = 10, height = 15)

}

# Per metric
ggplot(metrics, aes(rep, du, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Size in KiB", linetype = "Variant", color = "Variant", title = "MinIO: Total size of bucket") + facet_wrap(vars(analysis))
ggsave(paste0(outputBaseDir, "/du.pdf"), title = "du", width = 15, height = 10)
ggsave(paste0(outputBaseDir, "/du.svg"), width = 15, height = 10)

# Produce extra plot without Hudi
ggplot(metrics[metrics[["variant"]] != "Hudi (CoW)" & !(metrics[["analysis"]] == "Many Inserts" & metrics[["variant"]] == "Hudi (MoR)") & metrics[["analysis"]] != "Many Inserts (Partitioned)", ], aes(rep, du, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Size in KiB", linetype = "Variant", color = "Variant", title = "MinIO: Total size of bucket without Hudi (CoW)") + facet_wrap(vars(analysis), ncol = 2) + theme(legend.position = "top") + guides(linetype = guide_legend(ncol = 2), color = guide_legend(ncol = 2)) + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
ggsave(paste0(outputBaseDir, "/du_wo_hudi.pdf"), title = "du without Hudi", height = 10)
ggsave(paste0(outputBaseDir, "/du_wo_hudi.svg"), height = 10)

# Produce extra plot without Hudi and Iceberg
ggplot(metrics[!metrics[["variant"]] %in% c("Hudi (CoW)", "Hudi (MoR)", "Iceberg (MoR)", "Iceberg (CoW)") & metrics[["analysis"]] != "Many Inserts (Partitioned)", ], aes(rep, du, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Size in KiB", linetype = "Variant", color = "Variant", title = "MinIO: Total size of bucket (without Hudi and Iceberg)") + facet_wrap(vars(analysis), ncol = 2) + theme(legend.position = "top") + guides(linetype = guide_legend(ncol = 2), color = guide_legend(ncol = 2)) + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
ggsave(paste0(outputBaseDir, "/du_wo_hudi_iceberg.pdf"), title = "du without Hudi and Iceberg", height = 10)
ggsave(paste0(outputBaseDir, "/du_wo_hudi_iceberg.svg"), height = 10)

ggplot(metrics, aes(rep, ls, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Number of files", linetype = "Variant", color = "Variant", title = "MinIO: Number of objects in bucket") + facet_wrap(vars(analysis))
ggsave(paste0(outputBaseDir, "/ls.pdf"), title = "ls", width = 15, height = 10)
ggsave(paste0(outputBaseDir, "/ls.svg"), width = 15, height = 10)

ggplot(metrics, aes(rep, trace_op, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Number of requests", linetype = "Variant", color = "Variant", title = "MinIO: Number of S3 requests during update operation") + facet_wrap(vars(analysis))
ggsave(paste0(outputBaseDir, "/trace_op.pdf"), title = "trace_op", width = 15, height = 10)
ggsave(paste0(outputBaseDir, "/trace_op.svg"), width = 15, height = 10)

ggplot(metrics, aes(rep, trace_select, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Number of requests", linetype = "Variant", color = "Variant", title = "MinIO: Number of S3 requests during table scan") + facet_wrap(vars(analysis))
ggsave(paste0(outputBaseDir, "/trace_select.pdf"), title = "trace_select", width = 15, height = 10)
ggsave(paste0(outputBaseDir, "/trace_select.svg"), width = 15, height = 10)

ggplot(metrics, aes(rep, runtime_op, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Runtime [ms]", linetype = "Variant", color = "Variant", title = "Query runtime during update operation") + facet_wrap(vars(analysis))
ggsave(paste0(outputBaseDir, "/runtime_op.pdf"), title = "runtime_op", width = 15, height = 10)
ggsave(paste0(outputBaseDir, "/runtime_op.svg"), width = 15, height = 10)

ggplot(metrics, aes(rep, runtime_select, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Runtime [ms]", linetype = "Variant", color = "Variant", title = "Query runtime during table scan") + facet_wrap(vars(analysis))
ggsave(paste0(outputBaseDir, "/runtime_select.pdf"), title = "runtime_select", width = 15, height = 10)
ggsave(paste0(outputBaseDir, "/runtime_select.svg"), width = 15, height = 10)

ggplot(metrics, aes(rep, hms, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Size in bytes", linetype = "Variant", color = "Variant", title = "HMS: Size of Postgres dump") + facet_wrap(vars(analysis))
ggsave(paste0(outputBaseDir, "/hms.pdf"), title = "hms", width = 15, height = 10)
ggsave(paste0(outputBaseDir, "/hms.svg"), width = 15, height = 10)
