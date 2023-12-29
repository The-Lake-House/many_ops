#!/usr/bin/env Rscript

library(data.table)
library(ggplot2)
library(gridExtra)

get_legend <- function(plot) {
  tmp <- ggplot_gtable(ggplot_build(plot))
  leg <- which(sapply(tmp[["grobs"]], `[[`, "name") == "guide-box")
  legend <- tmp[["grobs"]][[leg]]
  return(legend)
}

argv <- commandArgs(trailingOnly = TRUE)
if (length(argv) < 3) {
    stop("Usage: ./analyze INPUT_BASE_DIR OUTPUT_BASE_DIR EXT")
}
inputBaseDir <- argv[1]
outputBaseDir <- argv[2]
extension <- argv[3]
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
    labels = c("Hive", "Hudi (CoW)", "Hudi (MoR)", "Iceberg (CoW)", "Iceberg (MoR)", "Delta (Without Deletion Vectors)", "Delta Lake (With Deletion Vectors)")
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
    labels = c("Hive", "Hudi (CoW)", "Hudi (MoR)", "Iceberg (CoW)", "Iceberg (MoR)", "Delta (Without Deletion Vectors)", "Delta Lake (With Deletion Vectors)")
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
    labels = c("Hive", "Hudi (CoW)", "Hudi (MoR)", "Iceberg (CoW)", "Iceberg (MoR)", "Delta (Without Deletion Vectors)", "Delta Lake (With Deletion Vectors)")
)

for (analysis in levels(metrics[["analysis"]])) {

    metricsSubset <- metrics[metrics[["analysis"]] == analysis, ]

    ggplot(metricsSubset, aes(rep, du, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Size in KiB", linetype = "Variant", color = "Variant", title = "MinIO: Total size of bucket", subtitle = analysis) + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    ggsave(paste0(outputBaseDir, "/", analysis, " du.", extension), title = paste0(analysis, ": du"))

    ggplot(metricsSubset, aes(rep, ls, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Number of files", linetype = "Variant", color = "Variant", title = "MinIO: Number of objects in bucket", subtitle = analysis) + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    ggsave(paste0(outputBaseDir, "/", analysis, " ls.", extension), title = paste0(analysis, ": ls"))

    ggplot(metricsSubset, aes(rep, trace_op, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Number of requests", linetype = "Variant", color = "Variant", title = "MinIO: Number of S3 requests during update operation", subtitle = analysis) + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    ggsave(paste0(outputBaseDir, "/", analysis, " trace_op.", extension), title = paste0(analysis, ": trace_op"))

    ggplot(metricsSubset, aes(rep, trace_select, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Number of requests", linetype = "Variant", color = "Variant", title = "MinIO: Number of S3 requests during table scan", subtitle = analysis) + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    ggsave(paste0(outputBaseDir, "/", analysis, " trace_select.", extension), title = paste0(analysis, ": trace_select"))

    ggplot(metricsSubset, aes(rep, runtime_op, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Runtime [ms]", linetype = "Variant", color = "Variant", title = "Query runtime during update operation", subtitle = analysis) + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    ggsave(paste0(outputBaseDir, "/", analysis, " runtime_op.", extension), title = paste0(analysis, ": runtime_op"))

    ggplot(metricsSubset, aes(rep, runtime_select, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Runtime [ms]", linetype = "Variant", color = "Variant", title = "Query runtime during table scan", subtitle = analysis) + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    ggsave(paste0(outputBaseDir, "/", analysis, " runtime_select.", extension), title = paste0(analysis, ": runtime_select"))

    ggplot(metricsSubset, aes(rep, hms, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Size in bytes", linetype = "Variant", color = "Variant", title = "HMS: Size of Postgres dump", subtitle = analysis) + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    ggsave(paste0(outputBaseDir, "/", analysis, " hms.", extension), title = paste0(analysis, ": hms"))

    p1 <- ggplot(metricsSubset, aes(rep, trace_op, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Number of requests", linetype = "Variant", color = "Variant", title = "MinIO: Number of S3 requests during update operation") + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    p2 <- ggplot(metricsSubset, aes(rep, trace_select, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Number of requests", linetype = "Variant", color = "Variant", title = "MinIO: Number of S3 requests during table scan") + guides(linetype = "none", color = "none") + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    p3 <- ggplot(metricsSubset, aes(rep, runtime_op, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Runtime [ms]", linetype = "Variant", color = "Variant", title = "Query runtime during update operation") + guides(linetype = "none", color = "none") + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    p4 <- ggplot(metricsSubset, aes(rep, runtime_select, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Runtime [ms]", linetype = "Variant", color = "Variant", title = "Query runtime during table scan") + guides(linetype = "none", color = "none") + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    p5 <- ggplot(metricsSubset, aes(rep, du, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Size in KiB", linetype = "Variant", color = "Variant", title = "MinIO: Total size of bucket") + guides(linetype = "none", color = "none") + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    p6 <- ggplot(metricsSubset, aes(rep, ls, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Number of files", linetype = "Variant", color = "Variant", title = "MinIO: Number of objects in bucket") + guides(linetype = "none", color = "none") + scale_linetype_discrete(drop = FALSE) + scale_color_discrete(drop = FALSE)
    legend <- get_legend(p1)
    p1 <- p1 + guides(linetype = "none", color = "none")
    p <- arrangeGrob(p1, p2, p3, p4, p5, p6, legend,
        ncol = 2,
        layout_matrix = matrix(
            c(1, 2, 3, 4, 5, 6, 7, 7),
            ncol = 2,
            byrow = TRUE
        )
    )
    ggsave(paste0(outputBaseDir, "/", analysis, ".", extension), plot = p, title = analysis, width = 10, height = 15)

    for (variant in levels(metricsSubset[["variant"]])) {

        outputDir <- paste0(outputBaseDir, "/", analysis, "/", variant)
        dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)

        sarSubset <- sar[sar[["analysis"]] == analysis & sar[["variant"]] == variant, ]

        ggplot(sarSubset, aes(timestamp, cpu_percent_user)) + geom_point() + ylim(0, 100) + labs(x = "Time in UTC", title = "CPU: %user", subtitle = paste0(analysis, "/", variant))
        ggsave(paste0(outputDir, "/sar_cpu_user.", extension), title = paste0(analysis, "/", variant, "/cpu/%user"))

        ggplot(sarSubset, aes(timestamp, cpu_percent_system)) + geom_point() + ylim(0, 100) + labs(x = "Time in UTC", title = "CPU: %system", subtitle = paste0(analysis, "/", variant))
        ggsave(paste0(outputDir, "/sar_cpu_system.", extension), title = paste0(analysis, "/", variant, "/cpu/%system"))

        ggplot(sarSubset, aes(timestamp, cpu_percent_idle)) + geom_point() + ylim(0, 100) + labs(x = "Time in UTC", title = "CPU: %idle", subtitle = paste0(analysis, "/", variant))
        ggsave(paste0(outputDir, "/sar_cpu_idle.", extension), title = paste0(analysis, "/", variant, "/cpu/%idle"))

        ggplot(sarSubset, aes(timestamp, mem_percent_used)) + geom_point() + ylim(0, 100) + labs(x = "Time in UTC", title = "RAM: %memused", subtitle = paste0(analysis, "/", variant))
        ggsave(paste0(outputDir, "/sar_mem_memused.", extension), title = paste0(analysis, "/", variant, "/mem/%memused"))

        ggplot(sarSubset, aes(timestamp, io_tps)) + geom_point() + ylim(0, NA) + labs(x = "Time in UTC", title = "I/O: tps", subtitle = paste0(analysis, "/", variant))
        ggsave(paste0(outputDir, "/sar_io_tps.", extension), title = paste0(analysis, "/", variant, "/io/tps"))

        ggplot(sarSubset, aes(timestamp, io_rtps)) + geom_point() + ylim(0, NA) + labs(x = "Time in UTC", title = "I/O: rtps", subtitle = paste0(analysis, "/", variant))
        ggsave(paste0(outputDir, "/sar_io_rtps.", extension), title = paste0(analysis, "/", variant, "/io/rtps"))

        ggplot(sarSubset, aes(timestamp, io_wtps)) + geom_point() + ylim(0, NA) + labs(x = "Time in UTC", title = "I/O: wtps", subtitle = paste0(analysis, "/", variant))
        ggsave(paste0(outputDir, "/sar_io_wtps.", extension), title = paste0(analysis, "/", variant, "/io/wtps"))

        ggplot(sarSubset, aes(timestamp, disk_await)) + geom_point() + ylim(0, NA) + labs(x = "Time in UTC", title = "Disk: await", subtitle = paste0(analysis, "/", variant))
        ggsave(paste0(outputDir, "/sar_disk_await.", extension), title = paste0(analysis, "/", variant, "/disk/await"))

        traceSubset <- trace[trace[["analysis"]] == analysis & trace[["variant"]] == variant, ]

        ggplot(traceSubset[traceSubset[["type"]] == "op", ], aes(rep, fill = req)) + geom_bar() + ylim(0, NA) + labs(x = "Rep", y = "Number of requests", fill = "Operation", title = "MinIO: Number of S3 requests during update operation", subtitle = paste0(analysis, ": ", variant))
        ggsave(paste0(outputDir, "/trace_op.", extension), title = paste0(analysis, "/", variant, "/trace_op"))

        ggplot(traceSubset[traceSubset[["type"]] == "select", ], aes(rep, fill = req)) + geom_bar() + ylim(0, NA) + labs(x = "Rep", y = "Number of requests", fill = "Operation", title = "MinIO: Number of S3 requests during table scan", subtitle = paste0(analysis, " / ", variant))
        ggsave(paste0(outputDir, "/trace_select.", extension), title = paste0(analysis, "/", variant, "/trace_select"))

    }

}

# Per metric
ggplot(metrics, aes(rep, du, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Size in KiB", linetype = "Variant", color = "Variant", title = "MinIO: Total size of bucket") + facet_wrap(vars(analysis))
ggsave(paste0(outputBaseDir, "/du.", extension), title = "du", width = 15, height = 10, unit = "in")

ggplot(metrics, aes(rep, ls, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Number of files", linetype = "Variant", color = "Variant", title = "MinIO: Number of objects in bucket") + facet_wrap(vars(analysis))
ggsave(paste0(outputBaseDir, "/ls.", extension), title = "ls", width = 15, height = 10, unit = "in")

ggplot(metrics, aes(rep, trace_op, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Number of requests", linetype = "Variant", color = "Variant", title = "MinIO: Number of S3 requests during update operation") + facet_wrap(vars(analysis))
ggsave(paste0(outputBaseDir, "/trace_op.", extension), title = "trace_op", width = 15, height = 10, unit = "in")

ggplot(metrics, aes(rep, trace_select, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Number of requests", linetype = "Variant", color = "Variant", title = "MinIO: Number of S3 requests during table scan") + facet_wrap(vars(analysis))
ggsave(paste0(outputBaseDir, "/trace_select.", extension), title = "trace_select", width = 15, height = 10, unit = "in")

ggplot(metrics, aes(rep, runtime_op, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Runtime [ms]", linetype = "Variant", color = "Variant", title = "Query runtime during update operation") + facet_wrap(vars(analysis))
ggsave(paste0(outputBaseDir, "/runtime_op.", extension), title = "runtime_op", width = 15, height = 10, unit = "in")

ggplot(metrics, aes(rep, runtime_select, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Runtime [ms]", linetype = "Variant", color = "Variant", title = "Query runtime during table scan") + facet_wrap(vars(analysis))
ggsave(paste0(outputBaseDir, "/runtime_select.", extension), title = "runtime_select", width = 15, height = 10, unit = "in")

ggplot(metrics, aes(rep, hms, linetype = variant, color = variant)) + geom_line() + ylim(0, NA) + labs(x = "Rep", y = "Size in bytes", linetype = "Variant", color = "Variant", title = "HMS: Size of Postgres dump") + facet_wrap(vars(analysis))
ggsave(paste0(outputBaseDir, "/hms.", extension), title = "hms", width = 15, height = 10, unit = "in")
