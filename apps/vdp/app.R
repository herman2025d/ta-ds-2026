# ==============================================================================
# PROTOTIPE SHINY VDP — CERITA DATA PARTISIPASI PENDIDIKAN ANTARPROVINSI 2015–2025
# "Sudah Sejauh Mana Anak Indonesia Bersekolah?" untuk publik awam
# Data   : data/app_VDP.rds (dibuat 03_VDP/script/03_siapkan_shiny_VDP.R)
# Paket  : shiny, bslib, ggplot2, plotly, leaflet, ggiraph — semuanya ada di webR (shinylive)
#          highcharter TIDAK dipakai: dependensinya (XML) tidak tersedia di webR; grafik dinamis
#          (range slider) dibuat dengan plotly.
# Visual : fungsi/00_visual_system.R & label_variabel_VDP.R (salinan otomatis, jangan diedit di sini)
# Jalankan lokal: shiny::runApp("D:/R/DATA SAINS (TUGAS AKHIR)/03_VDP/shiny/app")
# ==============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(plotly)
library(leaflet)

perlu <- c(file.path("fungsi", c("00_visual_system.R", "label_variabel_VDP.R", "teks_VDP.R")),
           file.path("data", "app_VDP.rds"), file.path("www", "gerak_provinsi.gif"))
if (!all(file.exists(perlu)))
  stop("File aplikasi belum lengkap (", paste(perlu[!file.exists(perlu)], collapse = ", "),
       "). Jalankan dulu 03_VDP/script/03_siapkan_shiny_VDP.R.", call. = FALSE)
for (f in perlu[1:3]) source(f, encoding = "UTF-8")
if (!exists("warna_teks") || !exists("BENTUK_PAPUA"))
  stop("fungsi/label_variabel_VDP.R masih versi lama (sebelum audit desain 17 Nov). ",
       "Jalankan ulang 03_VDP/script/03_siapkan_shiny_VDP.R.", call. = FALSE)

D    <- readRDS(file.path("data", "app_VDP.rds"))
P    <- D$panel
NAS  <- P[P$nasional, ]
PROV <- P[!P$nasional, ]
NAMA_PROV <- sort(D$provinsi$nama)
V_APS <- c("APS_07_12", "APS_13_15", "APS_16_18", "APS_19_23")
V_APM <- c("APM_SD", "APM_SMP", "APM_SM")
V_APK <- c("APK_SD", "APK_SMP", "APK_SM")
V_NET <- c("NET_KOTA", "NET_DESA", "NET_TOTAL")
FONT_WEB <- "Segoe UI, Calibri, Arial, sans-serif"
BASE_APP <- 12
TB  <- teks_bab(D)
REK <- teks_rekomendasi(D)
INDUK <- c(`93` = 91, `94` = 91, `95` = 91, `96` = 92)   # provinsi pemekaran 2022 -> induk

# ggiraph dipakai bila tersedia & berfungsi; bila tidak (mis. masalah font di peramban),
# grafik yang sama ditampilkan statis sehingga aplikasi tetap berjalan.
ADA_GIRAFE <- requireNamespace("ggiraph", quietly = TRUE) &&
  isTRUE(tryCatch({ ggiraph::girafe(ggobj = ggplot() + geom_point(aes(1, 1))); TRUE },
                  error = function(e) FALSE))

catatan <- function(...) tags$p(class = "small text-muted mb-0", ...)
istilah <- function(kata, arti) tooltip(tags$span(class = "istilah", kata), arti)
tint <- function(col, t = 0.85) grDevices::colorRampPalette(c(col, "#FFFFFF"))(101)[round(t * 100) + 1]
pct  <- function(x, d = 1) ifelse(is.na(x), "tidak ada data", paste0(fmt_angka(x, d), "%"))
nilai_prov <- function(nama, v, th) { x <- PROV[[v]][PROV$nama == nama & PROV$tahun == th]; if (length(x)) x else NA }
nilai_nas  <- function(v, th) NAS[[v]][NAS$tahun == th]

# ------------------------------------------------------------------------------
# Data peta & peringkat
# ------------------------------------------------------------------------------
# Tahun dengan cakupan lama: wilayah provinsi pemekaran diwarnai dengan angka provinsi induk
# (wilayah lama induk memang mencakup daerah itu), dan diberi keterangan.
data_peta <- function(v, th) {
  d <- PROV[PROV$tahun == th, c("kode", "nama", v, "CAKUPAN_LAMA_PEND", "CAKUPAN_LAMA_NET")]
  names(d)[3] <- "nilai"
  lama <- if (v %in% V_NET) d$CAKUPAN_LAMA_NET else d$CAKUPAN_LAMA_PEND
  d$catatan <- ""; d$salinan <- FALSE
  if (any(lama)) {
    for (k in names(INDUK)) {
      i <- d$kode == as.numeric(k); j <- d$kode == INDUK[[k]]
      d$nilai[i] <- d$nilai[j]; d$salinan[i] <- TRUE
      d$catatan[i] <- paste0("Belum menjadi provinsi sendiri; angka = ", d$nama[j], " (wilayah lama)")
    }
    d$catatan[d$kode %in% c(91, 92)] <- "Wilayah lama (sebelum pemekaran 2022), lebih luas dari sekarang"
  }
  ind <- nilai_nas(v, th)
  d$indonesia <- ind
  d$selisih <- d$nilai - ind
  d$label <- paste0("<b>", d$nama, "</b><br>",
                    ifelse(is.na(d$nilai), "Tidak ada data",
                           paste0(fmt_angka(d$nilai, 1), "% · Indonesia ", fmt_angka(ind, 1), "%")),
                    ifelse(nzchar(d$catatan), paste0("<br><i>", d$catatan, "</i>"), ""))
  d
}

peta_selisih <- function(v, th, sorot = "") {
  d <- data_peta(v, th); lim <- D$lim[[v]]
  pal <- colorNumeric(c(DIV_RENDAH, DIV_TENGAH, DIV_TINGGI), domain = c(-lim, lim), na.color = COL[["abu_muda"]])
  i <- match(D$peta$kode, d$kode)
  pilih <- nzchar(sorot) & D$peta$nama == sorot
  leaflet(options = leafletOptions(zoomSnap = 0.25, minZoom = 2.5, attributionControl = FALSE)) |>
    addPolygons(data = cbind(D$peta$lng, D$peta$lat),
                fillColor = pal(pmax(pmin(d$selisih[i], lim), -lim)), fillOpacity = 0.92,
                # audit 17 Nov: batas abu-abu (bukan putih) agar provinsi yang dekat angka Indonesia
                # (warnanya hampir putih) tetap terlihat bentuknya
                color = ifelse(pilih, COL[["hitam"]], COL[["abu"]]), weight = ifelse(pilih, 2.5, 0.5),
                label = lapply(d$label[i], htmltools::HTML),
                labelOptions = labelOptions(style = list("font-family" = FONT_WEB, "font-size" = "13px")),
                highlightOptions = highlightOptions(weight = 2, color = COL[["hitam"]], bringToFront = TRUE)) |>
    fitBounds(95, -11, 141, 6)
}

# Legenda peta di bawah peta (audit 17 Nov: legenda bawaan leaflet menutupi pulau di layar HP)
legenda_selisih <- function(lim) {
  grad <- paste0("linear-gradient(to right,", DIV_RENDAH, ",", DIV_TENGAH, ",", DIV_TINGGI, ")")
  tags$div(class = "legenda-peta",
           tags$div(class = "small fw-semibold mb-1", "Selisih dari angka Indonesia (poin persen)"),
           tags$div(style = paste0("height:12px;border-radius:2px;opacity:.92;background:", grad)),
           tags$div(class = "d-flex justify-content-between small",
                    tags$span(paste0("−", lim)), tags$span("0"), tags$span(paste0("+", lim))),
           tags$div(class = "d-flex justify-content-between small text-muted",
                    tags$span("◀ di bawah Indonesia"), tags$span("di atas Indonesia ▶")))
}

# ------------------------------------------------------------------------------
# Grafik ggplot (+ ggiraph): fungsi murni
# ------------------------------------------------------------------------------
geom_tile_i  <- function(...) if (ADA_GIRAFE) ggiraph::geom_tile_interactive(...) else geom_tile(...)
geom_point_i <- function(...) if (ADA_GIRAFE) ggiraph::geom_point_interactive(...) else geom_point(...)

# sempit = layar HP (wadah < 540 px): panel disusun 2 kolom agar kotak & teks tetap terbaca
gg_waffle <- function(prov = "", sempit = FALSE) {
  wil <- data.frame(wilayah = "Indonesia", var = V_APS, n = round(unlist(NAS[NAS$tahun == 2025, V_APS])))
  if (nzchar(prov))
    wil <- rbind(wil, data.frame(wilayah = prov, var = V_APS,
                                 n = round(unlist(PROV[PROV$nama == prov & PROV$tahun == 2025, V_APS]))))
  w <- do.call(rbind, lapply(seq_len(nrow(wil)), function(i)
    data.frame(wilayah = wil$wilayah[i], var = wil$var[i], n = wil$n[i], k = 1:100)))
  w$x <- (w$k - 1) %% 10; w$y <- (w$k - 1) %/% 10
  w$status <- factor(ifelse(w$k <= w$n, "Bersekolah", "Tidak bersekolah"), levels = names(COL_SEKOLAH))
  # di HP label panel dipendekkan ("7–12 tahun") dan nama provinsi dibungkus agar tidak terpotong
  lab_usia <- if (sempit) paste0(sub("Usia ", "", lab_pendek(V_APS)), " tahun") else paste0(lab_pendek(V_APS), " tahun")
  w$usia <- factor(lab_usia[match(w$var, V_APS)], levels = lab_usia)
  w$wilayah <- factor(w$wilayah, levels = unique(wil$wilayah))
  if (sempit) levels(w$wilayah) <- vapply(levels(w$wilayah), function(s) paste(strwrap(s, 14), collapse = "\n"), "")
  w$tip <- paste0(w$wilayah, " · ", lab_pendek(w$var), " tahun: ", w$n, " dari 100 bersekolah")
  w$grup <- paste(w$wilayah, w$var)
  lab <- unique(w[, c("wilayah", "usia", "n")])   # setelah label disesuaikan, agar cocok dengan panel
  ggplot(w, aes(x, y, fill = status)) +
    geom_tile_i(aes(tooltip = tip, data_id = grup), colour = "#FFFFFF", linewidth = 0.4, width = 0.9, height = 0.9) +
    geom_text(data = lab, aes(x = 4.5, y = 10.7, label = paste0(n, " dari 100")), inherit.aes = FALSE,
              size = 4.2, fontface = "bold", colour = COL[["hitam"]], family = FONT) +
    (if (!sempit) facet_grid(wilayah ~ usia, switch = "y")
     else if (nzchar(prov)) facet_wrap(vars(usia, wilayah), ncol = 2)
     else facet_wrap(vars(usia), ncol = 2)) +
    coord_equal(ylim = c(-0.5, 11.4), clip = "off") +
    scale_fill_manual(values = COL_SEKOLAH, name = NULL) +
    labs(x = NULL, y = NULL) +
    theme_akademik(base_size = BASE_APP, grid = "none") +
    theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.line = element_blank(),
          strip.text.x = element_text(hjust = 0.5, lineheight = 1), strip.text.y.left = element_text(angle = 0, hjust = 1),
          strip.placement = "outside", strip.clip = "off")
}

gg_peringkat <- function(v, th, sorot = "", sempit = FALSE) {
  d <- data_peta(v, th); d <- d[!d$salinan & !is.na(d$nilai), ]
  ind <- d$indonesia[1]
  d$posisi <- factor(ifelse(d$selisih < 0, "Di bawah Indonesia", "Di atas Indonesia"),
                     levels = c("Di bawah Indonesia", "Di atas Indonesia"))
  d$peringkat <- rank(-d$nilai, ties.method = "min")
  d$nama_f <- reorder(d$nama, d$nilai)
  d$tip <- paste0(d$nama, ": ", fmt_angka(d$nilai, 1), "% · peringkat ", d$peringkat, " dari ", nrow(d),
                  ifelse(nzchar(d$catatan), paste0(" (", tolower(d$catatan), ")"), ""))
  g <- ggplot(d, aes(nilai, nama_f, colour = posisi)) +
    geom_vline(xintercept = ind, colour = COL[["abu_tua"]], linewidth = 0.4) +
    geom_segment(aes(x = ind, xend = nilai, yend = nama_f), linewidth = 0.6) +
    geom_point_i(aes(tooltip = tip, data_id = nama), size = 2.6) +
    # label "Indonesia" diletakkan di sisi garis yang lebih lapang (audit 17 Nov: terpotong di HP)
    annotate("text", x = ind, y = nrow(d) + 0.9, label = paste0("Indonesia ", fmt_angka(ind, 1), "%"),
             hjust = if (ind > mean(range(d$nilai))) 1.05 else -0.05, size = 3.4, colour = COL[["abu_tua"]], family = FONT) +
    scale_colour_manual(values = c("Di bawah Indonesia" = COL[["koral"]], "Di atas Indonesia" = COL[["navy"]]),
                        name = NULL, drop = FALSE, guide = guide_legend(nrow = if (sempit) 2 else 1)) +
    scale_x_continuous(labels = function(x) paste0(x, "%"),
                       breaks = scales::breaks_pretty(if (sempit) 3 else 5)) +   # di HP angka sumbu tidak berdesakan
    scale_y_discrete(expand = expansion(add = c(0.6, 1.8))) +
    coord_cartesian(clip = "off") +
    labs(x = paste(strwrap(paste0(lab_awam(v), " (", th, ")"), if (sempit) 22 else 45), collapse = "\n"), y = NULL) +
    theme_akademik(base_size = BASE_APP, grid = "x") +
    theme(axis.text.y = element_text(size = rel(0.85)))
  if (nzchar(sorot) && sorot %in% d$nama)
    g <- g + geom_point(data = d[d$nama == sorot, ], shape = 21, size = 5, stroke = 1.2,
                        colour = COL[["hitam"]], fill = NA)
  g
}

gg_internet_sekolah <- function(sorot = "") {
  d <- PROV[PROV$tahun == 2024, ]
  d$kelompok <- factor(ifelse(d$kode >= 91, "6 provinsi Papua", "Provinsi lain"), levels = names(COL_PAPUA))
  d$tip <- paste0(d$nama, "\nMemakai internet: ", fmt_angka(d$NET_TOTAL, 1), "%\nRemaja 16–18 bersekolah: ",
                  fmt_angka(d$APS_16_18, 1), "%")
  g <- ggplot(d, aes(NET_TOTAL, APS_16_18, colour = kelompok, shape = kelompok)) +
    geom_point_i(aes(tooltip = tip, data_id = nama), size = 3) +
    scale_colour_manual(values = COL_PAPUA, name = NULL) +
    scale_shape_manual(values = BENTUK_PAPUA, name = NULL) +
    scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
    scale_y_continuous(labels = function(x) paste0(x, "%")) +
    labs(x = "Memakai internet (2024)", y = "Remaja 16–18 bersekolah") +   # ringkas: muat di HP
    theme_akademik(base_size = BASE_APP, grid = "xy")
  if (nzchar(sorot))
    g <- g + geom_point(data = d[d$nama == sorot, ], shape = 21, size = 6, stroke = 1.2,
                        colour = COL[["hitam"]], fill = NA)
  g
}

girafe_vdp <- function(g, w, h) {
  ggiraph::girafe(ggobj = g, width_svg = w, height_svg = h, options = list(
    ggiraph::opts_tooltip(css = paste0("background:#FFFFFF;color:", COL[["hitam"]], ";border:1px solid ",
                                       COL[["abu_muda"]], ";padding:6px 8px;border-radius:4px;font-family:",
                                       FONT_WEB, ";font-size:13px;"), use_fill = FALSE),
    ggiraph::opts_hover(css = "opacity:0.8;stroke-width:1.5px;"),
    ggiraph::opts_hover_inv(css = "opacity:0.35;"),
    ggiraph::opts_selection(type = "none"),
    ggiraph::opts_toolbar(saveaspng = FALSE),
    ggiraph::opts_sizing(rescale = TRUE)))
}

# ------------------------------------------------------------------------------
# Grafik plotly (dinamis/interaktif/animasi): fungsi murni
# ------------------------------------------------------------------------------
gaya_plotly <- function(p) {
  p |>
    layout(font = list(family = FONT_WEB, size = 13, color = COL[["hitam"]]), separators = ",.",
           paper_bgcolor = COL[["latar"]], plot_bgcolor = COL[["latar"]],
           hoverlabel = list(font = list(family = FONT_WEB), bgcolor = "#FFFFFF"),
           # legenda di atas area grafik (yanchor bawah): bila berbaris-baris di HP, ia mendorong
           # margin ke atas, tidak menimpa garis (audit 17 Nov)
           legend = list(orientation = "h", x = 0, xanchor = "left", y = 1.02, yanchor = "bottom"),
           margin = list(l = 55, r = 15, t = 30, b = 40)) |>
    config(displaylogo = FALSE,
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d", "toggleSpikelines",
                                      "hoverCompareCartesian", "hoverClosestCartesian"))
}
sumbu <- function(judul, ...) list(title = judul, gridcolor = COL[["grid"]], zeroline = FALSE,
                                   linecolor = COL[["abu"]], ...)
# Sumbu tahun: di HP label tiap 2 tahun agar tidak diputar tegak (audit 17 Nov)
sumbu_tahun <- function(sempit = FALSE, slider = FALSE) {
  s <- sumbu("", dtick = if (sempit) 2 else 1, tickformat = "d", tickangle = 0)
  if (slider) s$rangeslider <- list(visible = TRUE, thickness = 0.07)
  s
}
# Label langsung di ujung kanan garis (warna teks digelapkan agar kontras >= 4,5:1);
# jarak minimum antarlabel mencegah tumpang-tindih
label_ujung <- function(y, teks, warna, jarak = 6) {
  o <- order(-y); yy <- y[o]
  for (i in seq_along(yy)[-1]) yy[i] <- min(yy[i], yy[i - 1] - jarak)
  y[o] <- yy
  lapply(seq_along(y), function(i)
    list(x = 1, xref = "paper", xanchor = "left", xshift = 4, y = y[i], yref = "y", text = teks[i],
         showarrow = FALSE, font = list(color = warna_teks(warna[i]), size = 12)))
}
pandemi <- list(type = "rect", xref = "x", yref = "paper", x0 = 2019.6, x1 = 2021.4, y0 = 0, y1 = 1,
                fillcolor = COL[["grid"]], line = list(width = 0), layer = "below")

plot_tren <- function(prov = "", sempit = FALSE) {
  p <- plot_ly()
  for (v in V_APS) {
    lb <- lab_pendek(v)
    p <- add_lines(p, x = NAS$tahun, y = NAS[[v]], name = lb, legendgroup = lb,
                   line = list(color = COL_USIA[[lb]], width = if (v == "APS_16_18") 4 else 2),
                   text = paste0("Indonesia · ", lb, ": ", fmt_angka(NAS[[v]], 1), "%"), hoverinfo = "text")
    if (nzchar(prov)) {
      d <- PROV[PROV$nama == prov, ]
      p <- add_lines(p, x = d$tahun, y = d[[v]], name = paste(lb, "·", prov), legendgroup = lb, showlegend = FALSE,
                     line = list(color = COL_USIA[[lb]], width = 2, dash = "dash"),
                     text = paste0(prov, " · ", lb, ": ", pct(d[[v]]),
                                   ifelse(d$CAKUPAN_LAMA_PEND, " (wilayah lama)", "")), hoverinfo = "text")
    }
  }
  # label langsung kelompok umur (audit 17 Nov: abu tua vs teal sulit dibedakan pada buta warna merah-hijau)
  akhir <- unlist(NAS[NAS$tahun == max(NAS$tahun), V_APS])
  ujung <- label_ujung(akhir, sub("Usia ", "", lab_pendek(V_APS)), unname(COL_USIA[lab_pendek(V_APS)]))
  layout(p, xaxis = sumbu_tahun(sempit, slider = TRUE),
         yaxis = sumbu("Anak yang bersekolah", range = c(0, 102), ticksuffix = "%"),
         shapes = list(pandemi),
         annotations = c(list(list(x = 2020.5, y = 0.04, xref = "x", yref = "paper", text = "Pandemi",
                                   showarrow = FALSE, font = list(color = COL[["abu_tua"]], size = 11))), ujung)) |>
    gaya_plotly() |> layout(margin = list(r = 48), showlegend = !sempit)   # di HP cukup label langsung
}

plot_gerak <- function(sorot = "", sempit = FALSE) {
  d <- PROV[!is.na(PROV$APS_13_15) & !is.na(PROV$APS_16_18), ]
  d$kelompok <- factor(ifelse(d$kode >= 91, "6 provinsi Papua", "Provinsi lain"), levels = names(COL_PAPUA))
  d$garis <- ifelse(d$nama == sorot, 3, 0.6)
  d$simbol <- ifelse(d$kelompok == "6 provinsi Papua", "triangle-up", "circle")   # tidak hanya warna
  d$tip <- paste0("<b>", d$nama, "</b> (", d$tahun, ")<br>Usia 13–15: ", fmt_angka(d$APS_13_15, 1),
                  "%<br>Usia 16–18: ", fmt_angka(d$APS_16_18, 1), "%",
                  ifelse(d$CAKUPAN_LAMA_PEND, "<br><i>wilayah lama (sebelum pemekaran)</i>", ""))
  n <- NAS
  n$tip <- paste0("<b>Indonesia</b> (", n$tahun, ")<br>Usia 13–15: ", fmt_angka(n$APS_13_15, 1),
                  "%<br>Usia 16–18: ", fmt_angka(n$APS_16_18, 1), "%")
  rx <- range(d$APS_13_15) + c(-1.5, 1); ry <- range(d$APS_16_18) + c(-3, 3)
  p <- plot_ly()
  # satu trace per kelompok: warna dan simbol tetap per trace (pemetaan simbol plotly tertukar saat dipecah)
  for (k in names(COL_PAPUA)) {
    dk <- d[d$kelompok == k, ]
    p <- add_markers(p, data = dk, x = ~APS_13_15, y = ~APS_16_18, frame = ~tahun, ids = ~nama, split = ~kelompok,
                     text = ~tip, hoverinfo = "text",
                     marker = list(size = 12, opacity = 0.85, color = COL_PAPUA[[k]], symbol = dk$simbol[1],
                                   line = list(width = ~garis, color = COL[["hitam"]])))
  }
  p |>
    # split = nama -> legenda bernama "Indonesia" (sebelumnya tampil "trace 2")
    add_markers(data = n, x = ~APS_13_15, y = ~APS_16_18, frame = ~tahun, ids = ~nama, split = ~nama,
                text = ~tip, hoverinfo = "text",
                marker = list(symbol = "diamond", size = 16, color = COL[["hitam"]])) |>
    layout(xaxis = sumbu("Anak usia 13–15 yang bersekolah", range = rx, ticksuffix = "%"),
           yaxis = sumbu("Remaja usia 16–18 yang bersekolah", range = ry, ticksuffix = "%")) |>
    animation_opts(frame = 1000, transition = 700, redraw = FALSE) |>
    # tombol & penggeser tahun diberi ruang sendiri di bawah judul sumbu (audit 17 Nov: sempat bertumpuk)
    animation_slider(currentvalue = list(prefix = "Tahun ", xanchor = "right", font = list(color = COL[["abu_tua"]])),
                     pad = list(t = 55, l = 75), y = 0) |>
    animation_button(label = "▶ Putar", x = 0, xanchor = "left", y = 0, yanchor = "top", pad = list(t = 62)) |>
    gaya_plotly() |> layout(margin = list(b = 115))
}

plot_jarak <- function(sempit = FALSE) {
  p <- plot_ly()
  for (v in c("APS_13_15", "APS_16_18", "APS_19_23")) {
    lb <- lab_pendek(v); s <- D$sigma[D$sigma$var == v, ]
    p <- add_lines(p, x = s$tahun, y = s$sb, name = lb,
                   line = list(color = COL_USIA[[lb]], width = if (v == "APS_16_18") 4 else 2),
                   text = paste0(lb, ", ", s$tahun, ": jarak ", fmt_angka(s$sb, 2), " poin"), hoverinfo = "text")
  }
  layout(p, xaxis = sumbu_tahun(sempit), yaxis = sumbu("Jarak antarprovinsi (poin persen)", rangemode = "tozero")) |>
    gaya_plotly()
}

plot_internet <- function(prov = "", sempit = FALSE) {
  g <- D$digital
  p <- plot_ly() |>
    add_lines(x = g$tahun, y = g$desa, name = "Desa", line = list(color = COL_DAERAH[["Desa"]], width = 3),
              text = paste0("Desa ", g$tahun, ": ", fmt_angka(g$desa, 1), "%"), hoverinfo = "text") |>
    add_lines(x = g$tahun, y = g$kota, name = "Kota", fill = "tonexty", fillcolor = tint(COL[["abu_muda"]], 0.5),
              line = list(color = COL_DAERAH[["Kota"]], width = 3),
              text = paste0("Kota ", g$tahun, ": ", fmt_angka(g$kota, 1), "% · selisih ",
                            fmt_angka(g$selisih_kota_desa, 1), " poin"), hoverinfo = "text")
  if (nzchar(prov)) {
    d <- PROV[PROV$nama == prov & PROV$tahun <= 2024, ]
    for (dd in list(c("NET_KOTA", "Kota"), c("NET_DESA", "Desa"))) {
      if (all(is.na(d[[dd[1]]]))) next
      p <- add_lines(p, x = d$tahun, y = d[[dd[1]]], name = paste(dd[2], prov),
                     line = list(color = COL_DAERAH[[dd[2]]], width = 2, dash = "dash"),
                     text = paste0(prov, " · ", tolower(dd[2]), " ", d$tahun, ": ", pct(d[[dd[1]]]),
                                   ifelse(d$CAKUPAN_LAMA_NET, " (wilayah lama)", "")), hoverinfo = "text")
    }
  }
  layout(p, xaxis = sumbu_tahun(sempit, slider = TRUE),
         yaxis = sumbu("Penduduk yang memakai internet", range = c(0, 100), ticksuffix = "%")) |>
    gaya_plotly()
}

plot_jelajah <- function(v, provs, sempit = FALSE) {
  p <- plot_ly() |>
    add_lines(x = NAS$tahun, y = NAS[[v]], name = "Indonesia", line = list(color = COL[["hitam"]], width = 4),
              text = paste0("Indonesia ", NAS$tahun, ": ", pct(NAS[[v]])), hoverinfo = "text")
  # audit 17 Nov: koral & oker mirip bagi buta warna -> tiap provinsi juga beda jenis garis;
  # oker digelapkan agar garis >= 3:1 terhadap latar putih
  warna <- c(unname(COL[c("navy", "teal", "koral", "violet")]), warna_teks(COL[["oker"]], 3))
  jenis <- c("solid", "dash", "dot", "dashdot", "longdash")
  lama_col <- if (v %in% V_NET) "CAKUPAN_LAMA_NET" else "CAKUPAN_LAMA_PEND"
  for (i in seq_along(provs)) {
    d <- PROV[PROV$nama == provs[i], ]
    p <- add_lines(p, x = d$tahun, y = d[[v]], name = provs[i],
                   line = list(color = warna[i], width = 2.5, dash = jenis[i]),
                   text = paste0(provs[i], " ", d$tahun, ": ", pct(d[[v]]),
                                 ifelse(d[[lama_col]], " (wilayah lama)", "")), hoverinfo = "text")
  }
  # judul indikator ada di kepala kartu; sumbu tegak cukup satuan (judul panjang meluber di HP)
  layout(p, xaxis = sumbu_tahun(sempit, slider = TRUE),
         yaxis = sumbu("", ticksuffix = "%", rangemode = "tozero")) |>
    gaya_plotly()
}

# ------------------------------------------------------------------------------
# Antarmuka
# ------------------------------------------------------------------------------
font_app <- font_collection("Segoe UI", "Calibri", "Arial", "sans-serif")
tema <- bs_theme(version = 5, bg = COL[["latar"]], fg = COL[["hitam"]], primary = COL[["navy"]],
                 secondary = COL[["abu_tua"]], success = COL[["teal"]], danger = COL[["koral"]],
                 warning = COL[["oker"]], info = COL[["violet"]],
                 base_font = font_app, heading_font = font_app, "font-size-base" = "1rem")
css <- tags$style(HTML(paste0(
  ".istilah{border-bottom:1px dotted ", COL[["abu_tua"]], ";cursor:help}",
  ".arti{background:", tint(COL[["navy"]], 0.94), ";border-left:4px solid ", COL[["navy"]],
  ";padding:12px 14px;border-radius:4px;margin-top:8px}",
  ".bab-no{color:", COL[["abu_tua"]], ";font-size:.8rem;letter-spacing:.08em;text-transform:uppercase}",
  ".lead{font-size:1.1rem;font-weight:400}",          # audit 17 Nov: huruf tipis (300) kurang terbaca
  ".leaflet-container{background:#FFFFFF}",
  ".legenda-peta{max-width:420px;margin-top:8px}",
  ".tabel-gulir{max-height:480px;overflow:auto}",
  # audit 17 Nov, layar HP (< 576 px): peta lebih pendek (pulau tetap utuh), kotak angka lebih ringkas,
  # kartu rekomendasi tidak dipaksa sama tinggi, grafik jelajah lebih tinggi karena legendanya berbaris
  "@media (max-width:575.98px){",
  "#peta_cerita,#peta_utama{height:230px!important}",
  ".bslib-value-box .value-box-area{padding:.75rem 1rem!important}",
  ".bslib-value-box{min-height:0!important}",
  ".rekom .card{height:auto!important}",
  "#p_jelajah{height:560px!important}",
  "}")))

KAMUS <- setNames(GLOSARIUM_VDP$arti, sub(" \\(.*", "", GLOSARIUM_VDP$istilah))

keluaran_gg <- function(id) uiOutput(paste0(id, "_ui"))

bab <- function(no, isi_bab, ..., catatan_bawah = NULL) {
  card(class = "mb-4",
       card_header(tags$div(tags$div(class = "bab-no", paste0("Bab ", no)), tags$h4(class = "mb-0", isi_bab$judul))),
       tags$p(class = "lead", isi_bab$lead),
       ...,
       tags$div(class = "arti", tags$strong("Apa artinya? "), isi_bab$arti),
       if (!is.null(catatan_bawah)) card_footer(catatan_bawah))
}

t25 <- D$tangga[D$tangga$tahun == 2025 & D$tangga$var %in% V_APS, ]
vb_tangga <- function(v, judul, warna) {
  value_box(title = judul, value = paste0(t25$dari_100_sekolah[t25$var == v], " dari 100"),
            tags$p("bersekolah pada 2025"),
            theme = value_box_theme(bg = tint(warna, 0.86), fg = COL[["hitam"]]))
}

hal_cerita <- nav_panel(
  "Cerita",
  card(class = "mb-4",
       tags$h3("Sudah sejauh mana anak Indonesia bersekolah?"),
       tags$p(class = "lead", TB$intro),
       layout_column_wrap(width = 1 / 3, fill = FALSE,
                          vb_tangga("APS_07_12", "Anak usia SD (7–12 tahun)", COL[["navy"]]),
                          vb_tangga("APS_16_18", "Remaja usia SMA (16–18 tahun)", COL[["oker"]]),
                          vb_tangga("APS_19_23", "Pemuda usia kuliah (19–23 tahun)", COL[["koral"]])),
       catatan("Angka di atas disebut ", istilah("Angka Partisipasi Sekolah (APS)", KAMUS[["APS"]]),
               " — persentase anak yang bersekolah pada kelompok umurnya.")),
  bab(1, TB$bab1, keluaran_gg("waffle"),
      catatan_bawah = catatan("Satu kotak = 1 dari 100 anak. Arahkan kursor (di HP: ketuk) ke kotak untuk angkanya.")),
  bab(2, TB$bab2, plotlyOutput("p_tren", height = "430px"),
      catatan_bawah = catatan("Garis putus-putus = provinsi pilihanmu. Geser pita di bawah grafik untuk memperbesar ",
                              "rentang tahun. Klik nama di legenda untuk menyembunyikan garis. ",
                              istilah("Poin persen", KAMUS[["Poin persen"]]))),
  bab(3, TB$bab3,
      layout_columns(col_widths = breakpoints(sm = 12, lg = c(8, 4)), fill = FALSE,
                     div(leafletOutput("peta_cerita", height = "380px"), legenda_selisih(D$lim[["APS_16_18"]])),
                     uiOutput("posisi_prov")),
      catatan_bawah = catatan("Remaja usia 16–18 yang bersekolah, 2025, dibanding angka Indonesia. Oranye = di bawah ",
                              "Indonesia, biru = di atas. ", SUMBER_PETA)),
  bab(4, TB$bab4,
      layout_columns(col_widths = breakpoints(sm = 12, lg = c(7, 5)), fill = FALSE,
                     div(tags$h6("Gerak provinsi dari tahun ke tahun (tekan ▶ Putar)"),
                         plotlyOutput("p_gerak", height = "460px")),
                     div(tags$h6("Jarak antarprovinsi (makin rendah, makin merata)"),
                         plotlyOutput("p_jarak", height = "300px"),
                         catatan(istilah("Jarak antarprovinsi", KAMUS[["Jarak antarprovinsi"]]), " · ",
                                 istilah("Mengejar ketertinggalan", KAMUS[["Mengejar ketertinggalan"]])))),
      accordion(open = FALSE, accordion_panel(
        "Lihat versi animasi GIF",
        tags$img(src = "gerak_provinsi.gif", style = "width:100%;max-width:820px;height:auto;",
                 alt = "Animasi titik provinsi bergerak dari 2015 sampai 2025"))),
      catatan_bawah = catatan("Tiap titik = satu provinsi; berlian hitam = Indonesia; titik bergaris tebal = provinsi ",
                              "pilihanmu. Papua & Papua Barat sebelum 2024 = wilayah lama (sebelum pemekaran).")),
  bab(5, TB$bab5,
      layout_columns(col_widths = breakpoints(sm = 12, lg = c(6, 6)), fill = FALSE,
                     div(tags$h6("Memakai internet: kota vs desa, Indonesia"), plotlyOutput("p_internet", height = "400px")),
                     div(tags$h6("Internet dan remaja bersekolah per provinsi, 2024"), keluaran_gg("sebar"))),
      catatan_bawah = catatan(istilah("Akses internet", KAMUS[["Akses internet"]]), " · ",
                              istilah("Perkotaan / perdesaan", KAMUS[["Perkotaan / perdesaan"]]),
                              " · Data internet 2025 belum diterbitkan BPS.")),
  card(class = "mb-4",
       card_header(tags$h4(class = "mb-0", "Apa yang bisa kita lakukan?")),
       layout_column_wrap(width = 1 / 3, fill = FALSE, class = "rekom", !!!lapply(REK, function(r)
         card(class = "h-100", style = paste0("border-top:4px solid ", COL[[r$warna]]),
              card_header(r$judul), tags$ul(lapply(r$butir, tags$li))))),
       card_footer(catatan("Rekomendasi disusun dari pola data agregat provinsi, bukan hasil uji sebab-akibat; ",
                           "perlu dipadukan dengan kajian lapangan.")))
)

PILIH_PETA <- c(setNames(V_APS, lab_awam(V_APS)), setNames(V_APM, lab_awam(V_APM)),
                setNames(c("NET_TOTAL", "NET_DESA"), lab_awam(c("NET_TOTAL", "NET_DESA"))))

hal_peta <- nav_panel(
  "Peta & peringkat",
  card(layout_columns(col_widths = breakpoints(sm = 12, lg = c(7, 5)), fill = FALSE,
                      selectInput("peta_var", "Indikator", PILIH_PETA, selected = "APS_16_18", width = "100%"),
                      sliderInput("peta_th", "Tahun (tekan ▶ untuk animasi)", min = 2015, max = 2025, value = 2025,
                                  step = 1, sep = "", width = "100%",
                                  animate = animationOptions(interval = 1600))),
       uiOutput("peta_info")),
  layout_columns(col_widths = breakpoints(sm = 12, lg = c(7, 5)), fill = FALSE,
                 card(card_header("Peta: selisih dari angka Indonesia"), leafletOutput("peta_utama", height = "520px"),
                      uiOutput("legenda_peta"), card_footer(catatan(SUMBER_PETA))),
                 card(card_header("Peringkat provinsi"), keluaran_gg("peringkat")))
)

PILIH_SEMUA <- list(
  "Bersekolah menurut umur (APS)" = setNames(V_APS, lab_awam(V_APS)),
  "Bersekolah di jenjang yang sesuai umur (APM)" = setNames(V_APM, lab_awam(V_APM)),
  "Jumlah murid dibanding anak seusia jenjang (APK)" = setNames(V_APK, lab_awam(V_APK)),
  "Memakai internet" = setNames(V_NET, lab_awam(V_NET)))

hal_jelajah <- nav_panel(
  "Jelajahi data",
  layout_columns(
    col_widths = breakpoints(sm = 12, lg = c(4, 8)), fill = FALSE,
    card(card_header("Pilihan"),
         selectInput("j_var", "Indikator", PILIH_SEMUA, selected = "APS_16_18", width = "100%"),
         selectizeInput("j_prov", "Bandingkan provinsi (maks. 5)", NAMA_PROV, multiple = TRUE,
                        options = list(maxItems = 5, placeholder = "Ketik nama provinsi"), width = "100%"),
         uiOutput("j_istilah"),
         downloadButton("unduh", "Unduh data (CSV, bisa dibuka di Excel)", class = "btn-sm btn-outline-primary mt-2")),
    card(card_header(textOutput("j_judul", inline = TRUE)), plotlyOutput("p_jelajah", height = "440px"),
         card_footer(catatan("Garis hitam tebal = Indonesia; tiap provinsi punya warna dan jenis garis sendiri. ",
                             "Geser pita di bawah grafik untuk memilih rentang tahun.")))),
  card(card_header("Semua provinsi pada tahun terpilih"),
       sliderInput("j_th", NULL, min = 2015, max = 2025, value = 2025, step = 1, sep = "", width = "320px"),
       div(class = "tabel-gulir", tableOutput("t_jelajah")))
)

hal_tentang <- nav_panel(
  "Tentang",
  card(
    card_header("Tentang cerita data ini"),
    tags$p("Prototipe tugas akhir mata kuliah Visualisasi Data Pendidikan. Tujuannya menyajikan data partisipasi ",
           "pendidikan antarprovinsi 2015–2025 kepada publik dengan bahasa dan grafik yang mudah dipahami."),
    tags$h5("Istilah"),
    tags$dl(lapply(seq_len(nrow(GLOSARIUM_VDP)), function(i)
      tagList(tags$dt(GLOSARIUM_VDP$istilah[i]), tags$dd(GLOSARIUM_VDP$arti[i])))),
    tags$h5("Cara grafik dirancang"), tags$ul(lapply(METODE_VDP, tags$li)),
    tags$h5("Keterbatasan"), tags$ul(lapply(KETERBATASAN_VDP, tags$li)),
    tags$h5("Sumber data"),
    tags$ul(class = "small", lapply(SUMBER_DATA_VDP, function(s)
      tags$li(s[[1]], " ", tags$a(href = s[[2]], target = "_blank", rel = "noopener", "Tautan")))),
    tags$h5("Pustaka"), tags$ul(class = "small", lapply(PUSTAKA_VDP, tags$li)),
    card_footer(catatan("Aplikasi hanya memuat tabel ringkasan per provinsi yang diterbitkan terbuka oleh BPS. ",
                        "Tidak ada data pengguna yang disimpan. Bahan aplikasi dibuat: ", D$dibuat, "."))
  )
)

JUDUL <- "Sudah Sejauh Mana Anak Indonesia Bersekolah?"
ui <- page_navbar(
  # audit 17 Nov: judul panjang mendorong tombol menu keluar layar HP -> judul pendek di layar kecil
  title = tags$span(tags$span(class = "d-none d-lg-inline", JUDUL),
                    tags$span(class = "d-lg-none", "Anak Indonesia Bersekolah?")),
  window_title = JUDUL,
  theme = tema, fillable = FALSE, lang = "id", header = css,
  sidebar = sidebar(
    width = 290, open = list(desktop = "open", mobile = "closed"),
    selectInput("prov", "Pilih provinsimu", c("Tidak memilih (Indonesia saja)" = "", NAMA_PROV)),
    uiOutput("ringkas_prov"),
    catatan("Provinsi pilihan ditandai di semua grafik dan peta.")
  ),
  hal_cerita, hal_peta, hal_jelajah, hal_tentang
)

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  sorot <- reactive(if (is.null(input$prov)) "" else input$prov)

  # Lebar wadah keluaran (px) dari peramban; NA sebelum tampil. sempit = layar HP (< 540 px).
  lebar_px <- function(id) {
    x <- session$clientData[[paste0("output_", id, "_width")]]
    if (is.null(x) || x <= 0) NA_real_ else x
  }
  sempit <- function(id) { x <- lebar_px(id); !is.na(x) && x < 540 }

  # grafik ggplot: interaktif (ggiraph) bila tersedia, statis bila tidak.
  # Audit 17 Nov: ukuran SVG mengikuti lebar wadah (96 px = 1 inci) agar teks tidak mengecil di HP;
  # w_maks = lebar maksimum (inci); h = fungsi(w, sempit) -> tinggi (inci).
  render_gg <- function(id, buat, w_maks, h) {
    id_out <- paste0(id, if (ADA_GIRAFE) "_g" else "_p")
    ukuran <- reactive({
      px <- lebar_px(id_out); s <- !is.na(px) && px < 540
      w <- if (is.na(px)) w_maks else min(w_maks, max(3, px / 96))
      list(w = w, h = h(w, s), s = s)
    })
    output[[paste0(id, "_ui")]] <- renderUI(
      if (ADA_GIRAFE) ggiraph::girafeOutput(id_out)
      else plotOutput(id_out, height = "auto"))
    if (ADA_GIRAFE) output[[id_out]] <- ggiraph::renderGirafe({ u <- ukuran(); girafe_vdp(buat(u$s), u$w, u$h) })
    else output[[id_out]] <- renderPlot(buat(ukuran()$s), res = 96, height = function() round(ukuran()$h * 96))
  }

  output$ringkas_prov <- renderUI({
    p <- sorot(); if (!nzchar(p)) return(NULL)
    v <- nilai_prov(p, "APS_16_18", 2025); ind <- nilai_nas("APS_16_18", 2025)
    r <- D$posisi[D$posisi$var == "APS_16_18" & D$posisi$nama == p, ]
    net <- nilai_prov(p, "NET_TOTAL", 2024)
    tags$div(class = "small mt-2",
             tags$p(tags$strong(p), ": ", round(v), " dari 100 remaja usia 16–18 bersekolah (Indonesia ",
                    round(ind), "). Peringkat ", r$peringkat, " dari ", nrow(D$posisi[D$posisi$var == "APS_16_18", ]), "."),
             tags$p(pct(net), " penduduknya memakai internet (2024)."),
             if (p %in% c("Papua", "Papua Barat"))
               tags$p(class = "text-muted", "Angka sebelum 2024 mencakup wilayah lama, sebelum pemekaran 2022."),
             if (p %in% D$provinsi$nama[D$provinsi$pemekaran_2022])
               tags$p(class = "text-muted", "Provinsi baru (pemekaran 2022); datanya tersedia mulai 2024."))
  })

  # waffle: lebar 10 inci -> tinggi 3,2 (Indonesia) / 5,6 (+ provinsi); di HP 2 kolom panel
  render_gg("waffle", function(s) gg_waffle(sorot(), s), 10, function(w, s) {
    n_wil <- if (nzchar(sorot())) 2 else 1
    if (!s) return(w * if (n_wil == 2) 0.56 else 0.32)
    baris <- 4 * n_wil / 2
    0.5 + baris * ((w - 0.3) / 2 * 1.19 + if (n_wil == 2) 0.45 else 0.28)
  })
  output$p_tren <- renderPlotly(plot_tren(sorot(), sempit("p_tren")))

  output$peta_cerita <- renderLeaflet(peta_selisih("APS_16_18", 2025, sorot()))
  output$posisi_prov <- renderUI({
    r3 <- D$ringkas3[D$ringkas3$var == "APS_16_18", ]
    p <- sorot()
    tagList(
      value_box(title = "Di bawah angka Indonesia", value = paste0(r3$n_di_bawah_nasional, " dari ", r3$n_prov),
                tags$p("provinsi, 2025"), theme = value_box_theme(bg = tint(COL[["koral"]]), fg = COL[["hitam"]])),
      value_box(title = "Terendah", value = pct(r3$nilai_terendah), tags$p(r3$terendah),
                theme = value_box_theme(bg = tint(COL[["koral"]], 0.92), fg = COL[["hitam"]])),
      if (nzchar(p)) {
        r <- D$posisi[D$posisi$var == "APS_16_18" & D$posisi$nama == p, ]
        value_box(title = p, value = pct(r$nilai), tags$p("peringkat ", r$peringkat, " dari ", r3$n_prov),
                  theme = value_box_theme(bg = tint(COL[["navy"]], 0.9), fg = COL[["hitam"]]))
      } else
        value_box(title = "Tertinggi", value = pct(r3$nilai_tertinggi), tags$p(r3$tertinggi),
                  theme = value_box_theme(bg = tint(COL[["navy"]], 0.9), fg = COL[["hitam"]])))
  })

  output$p_gerak  <- renderPlotly(plot_gerak(sorot(), sempit("p_gerak")))
  output$p_jarak  <- renderPlotly(plot_jarak(sempit("p_jarak")))
  output$p_internet <- renderPlotly(plot_internet(sorot(), sempit("p_internet")))
  render_gg("sebar", function(s) gg_internet_sekolah(sorot()), 7, function(w, s) max(3.4, w * 0.72))

  # Peta & peringkat: internet belum ada untuk 2025 -> pakai 2024
  th_peta <- reactive(if (input$peta_var %in% V_NET) min(input$peta_th, 2024) else input$peta_th)
  output$peta_info <- renderUI({
    v <- input$peta_var; th <- th_peta()
    catatan(HTML(paste0(
      "<strong>", lab_awam(v), "</strong>, ", th, ". Indonesia: ", pct(nilai_nas(v, th)), ".",
      if (th != input$peta_th) " Data internet 2025 belum terbit, jadi ditampilkan 2024." else "",
      if (any(PROV[PROV$tahun == th, if (v %in% V_NET) "CAKUPAN_LAMA_NET" else "CAKUPAN_LAMA_PEND"]))
        " Tahun ini empat provinsi baru Papua belum punya data sendiri; wilayahnya diwarnai dengan angka provinsi induk." else "")))
  })
  output$peta_utama <- renderLeaflet(peta_selisih(input$peta_var, th_peta(), sorot()))
  output$legenda_peta <- renderUI(legenda_selisih(D$lim[[input$peta_var]]))
  render_gg("peringkat", function(s) gg_peringkat(input$peta_var, th_peta(), sorot(), s), 6.5, function(w, s) 9.5)

  # Jelajahi data
  observeEvent(sorot(), {
    if (nzchar(sorot()) && !(sorot() %in% input$j_prov))
      updateSelectizeInput(session, "j_prov", selected = c(sorot(), input$j_prov)[seq_len(min(5, length(input$j_prov) + 1))])
  }, ignoreInit = TRUE)
  output$j_istilah <- renderUI({
    v <- input$j_var; ist <- LABEL_VDP$istilah[LABEL_VDP$var == v]
    arti <- if (ist == "Internet") KAMUS[["Akses internet"]] else KAMUS[[ist]]
    tags$div(class = "arti small", tags$strong(lab_awam(v)), tags$br(), arti)
  })
  output$j_judul <- renderText(lab_awam(input$j_var))
  output$p_jelajah <- renderPlotly(plot_jelajah(input$j_var, input$j_prov, sempit("p_jelajah")))
  # audit 17 Nov: 1 desimal (sama dengan peta & tooltip), judul kolom ringkas agar muat di HP
  output$t_jelajah <- renderTable({
    v <- input$j_var; th <- input$j_th
    d <- PROV[PROV$tahun == th, c("nama", v)]
    d <- d[order(-d[[v]], na.last = TRUE), ]
    data.frame(No = seq_len(nrow(d)), Provinsi = d$nama,
               Nilai = ifelse(is.na(d[[v]]), "–", paste0(fmt_angka(d[[v]], 1), "%")),
               `Beda dari Indonesia (poin)` = ifelse(is.na(d[[v]]), "–", paste0(ifelse(d[[v]] >= nilai_nas(v, th), "+", "−"),
                                                                           fmt_angka(abs(d[[v]] - nilai_nas(v, th)), 1))),
               check.names = FALSE)
  }, striped = TRUE, hover = TRUE, spacing = "s", align = "rlrr")
  output$unduh <- downloadHandler(
    filename = function() "partisipasi_pendidikan_provinsi_2015_2025.csv",
    content = function(file) {
      d <- P[, c("kode", "nama", "tahun", LABEL_VDP$var, "CAKUPAN_LAMA_PEND", "CAKUPAN_LAMA_NET")]
      write.csv(d, file, row.names = FALSE, na = "")
    })
}

shinyApp(ui, server)
