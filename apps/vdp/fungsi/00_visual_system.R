# ==============================================================================
# 00 SISTEM VISUAL BERSAMA — TUGAS AKHIR (EDM · ML · VDP)
# Dasar  : Universal_Academic_Data_Visualization_Design_System.md
#          "Satu keputusan desain -> banyak keluaran yang konsisten" (bagian 19)
# Isi    : palet semantik, tema ggplot, format angka Indonesia, penyimpan grafik
# Pakai  : source("D:/R/DATA SAINS (TUGAS AKHIR)/00_visual_system.R")
# Paket  : ggplot2, scales, ragg (PNG), systemfonts (cek font)
# ==============================================================================

library(ggplot2)
library(scales)

# 1. Palet ----------------------------------------------------------------------
# Terbatas & redup: navy (utama), teal (sekunder), koral (aksen/perhatian),
# violet & oker (pendukung), abu-abu (netral). Diuji agar tetap terbedakan
# dalam skala abu-abu karena kecerahannya berbeda (navy gelap -> oker terang).
COL <- c(
  navy     = "#1B3A5C",
  teal     = "#3D8C8C",
  koral    = "#D1693F",
  violet   = "#6E5FA0",
  oker     = "#C2953A",
  hitam    = "#2B2F33",   # teks utama
  abu_tua  = "#5F6670",   # teks sekunder (subjudul, caption)
  abu      = "#9AA1AA",   # elemen pendukung / baseline
  abu_muda = "#D5D9DE",   # garis bantu
  grid     = "#ECEEF1",
  latar    = "#FFFFFF"
)

# Pemetaan semantik (WAJIB dipakai ulang di semua grafik, dashboard, Shiny).
# Satu kategori = satu warna di seluruh proyek (bagian 22).
COL_STATUS <- c("Di bawah L2" = COL[["koral"]], "L2 ke atas" = COL[["teal"]])
# EDM: kab/kota prioritas afirmasi (HLS <= Q1) = koral (perhatian), bukan = teal
COL_PRIORITAS <- c("Prioritas" = COL[["koral"]], "Bukan" = COL[["teal"]])
# EDM: model & baseline (algoritma = warna; prediktor utama = garis penuh, pembanding = putus)
COL_MODEL_EDM <- c("CART" = COL[["navy"]], "C5.0" = COL[["violet"]],
                   "Kebijakan berjalan" = COL[["oker"]], "Baseline" = COL[["abu"]])
# EDM: sisi variabel (kerangka ekologi)
COL_SISI <- c("Kondisi penduduk" = COL[["navy"]], "Wilayah" = COL[["oker"]],
              "Akses wilayah" = COL[["violet"]])

COL_MODEL <- c("XGBoost"       = COL[["navy"]],
               "Random forest" = COL[["teal"]],
               "MLP"           = COL[["violet"]],
               "Elastic net"   = COL[["oker"]],
               "Baseline"      = COL[["abu"]])
LT_MODEL  <- c("XGBoost" = "solid", "Random forest" = "longdash", "MLP" = "dashed",
               "Elastic net" = "dotdash", "Baseline" = "dotted")
NAMA_MODEL <- c(rec_xgboost = "XGBoost", rec_random_forest = "Random forest",
                rec_mlp = "MLP", rec_elastic_net = "Elastic net", baseline = "Baseline")

COL_TINGKAT <- c(siswa = COL[["navy"]], sekolah = COL[["oker"]])

# Klaster berurutan dari kemampuan terendah (K1) ke tertinggi (Kk):
# koral (perhatian) -> ... -> navy. Urutan kecerahan juga bergradasi.
pal_klaster <- function(k) {
  urut <- list(c("koral", "navy"),
               c("koral", "oker", "navy"),
               c("koral", "oker", "teal", "navy"),
               c("koral", "oker", "violet", "teal", "navy"),
               c("koral", "oker", "violet", "teal", "navy", "abu_tua"))
  setNames(unname(COL[urut[[k - 1]]]), paste0("K", seq_len(k)))
}

# Divergen (z-score, selisih): koral (negatif) - putih - navy (positif)
DIV_RENDAH <- COL[["koral"]]; DIV_TENGAH <- "#F7F7F5"; DIV_TINGGI <- COL[["navy"]]

# Warna teks di atas sel berwarna: putih bila latar gelap, hitam bila terang
# (luminans relatif WCAG; ambang 0,18 ~ titik kontras seimbang hitam/putih).
warna_kontras <- function(fill) {
  rgb <- grDevices::col2rgb(fill) / 255
  lin <- ifelse(rgb <= 0.03928, rgb / 12.92, ((rgb + 0.055) / 1.055)^2.4)
  lum <- 0.2126 * lin[1, ] + 0.7152 * lin[2, ] + 0.0722 * lin[3, ]
  ifelse(lum < 0.18, COL[["latar"]], COL[["hitam"]])
}
# Warna sel divergen untuk nilai x dengan batas simetris ±lim (sama dengan skala)
warna_divergen <- function(x, lim) {
  scales::div_gradient_pal(DIV_RENDAH, DIV_TENGAH, DIV_TINGGI)(scales::rescale(x, c(0, 1), c(-lim, lim)))
}

# 2. Font -----------------------------------------------------------------------
# Satu keluarga font, dukungan Unicode baik, aman untuk PDF (cairo) & PNG (ragg).
pilih_font <- function(kandidat = c("Segoe UI", "Calibri", "Arial")) {
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    ada <- unique(systemfonts::system_fonts()$family)
    f <- kandidat[kandidat %in% ada]
    if (length(f)) return(f[1])
  }
  "sans"
}
FONT <- pilih_font()

# 3. Tema -----------------------------------------------------------------------
# Hierarki: judul (tebal, terbesar) > subjudul (abu tua) > judul sumbu >
# label tick > anotasi > caption. Latar putih, grid tipis horizontal saja.
theme_akademik <- function(base_size = 9.5, grid = "y", legend = "top") {
  g <- theme_minimal(base_size = base_size, base_family = FONT) +
    theme(
      text               = element_text(colour = COL[["hitam"]]),
      plot.background    = element_rect(fill = COL[["latar"]], colour = NA),
      panel.background   = element_rect(fill = COL[["latar"]], colour = NA),
      plot.title         = element_text(face = "bold", size = rel(1.25), hjust = 0,
                                        margin = margin(b = 3)),
      plot.subtitle      = element_text(colour = COL[["abu_tua"]], size = rel(0.98),
                                        lineheight = 1.15, margin = margin(b = 10)),
      plot.caption       = element_text(colour = COL[["abu_tua"]], size = rel(0.78),
                                        hjust = 0, lineheight = 1.15, margin = margin(t = 10)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.margin        = margin(12, 14, 10, 12),
      axis.title         = element_text(colour = COL[["abu_tua"]], size = rel(0.92)),
      axis.title.x       = element_text(margin = margin(t = 6)),
      axis.title.y       = element_text(margin = margin(r = 6)),
      axis.text          = element_text(colour = COL[["hitam"]], size = rel(0.88)),
      axis.ticks         = element_line(colour = COL[["abu_muda"]], linewidth = 0.3),
      axis.ticks.length  = unit(2.5, "pt"),
      axis.line          = element_line(colour = COL[["abu"]], linewidth = 0.35),
      panel.grid.minor   = element_blank(),
      panel.grid.major   = element_line(colour = COL[["grid"]], linewidth = 0.35),
      strip.text         = element_text(face = "bold", hjust = 0, size = rel(0.95),
                                        margin = margin(b = 4, t = 2)),
      strip.background   = element_blank(),
      panel.spacing      = unit(14, "pt"),
      legend.position    = legend,
      legend.justification = "left",
      legend.location    = "plot",          # legenda rata kiri dengan judul
      legend.title       = element_text(colour = COL[["abu_tua"]], size = rel(0.88)),
      legend.text        = element_text(size = rel(0.88)),
      legend.key.width   = unit(18, "pt"),
      legend.margin      = margin(0, 0, 0, 0),
      legend.box.spacing = unit(4, "pt")
    )
  if (grid == "y")    g <- g + theme(panel.grid.major.x = element_blank())
  if (grid == "x")    g <- g + theme(panel.grid.major.y = element_blank())
  if (grid == "none") g <- g + theme(panel.grid.major = element_blank())
  g
}

# Ukuran teks anotasi geom_text/annotate (mm) yang selaras dengan tema
TXT_ANOT <- 2.9

# 4. Format angka Indonesia ------------------------------------------------------
# Desimal koma, ribuan titik. Dipakai di sumbu, label, tooltip, dan tabel.
fmt_angka  <- function(x, digit = 2) formatC(x, format = "f", digits = digit,
                                              big.mark = ".", decimal.mark = ",")
fmt_bulat  <- function(x) formatC(round(x), format = "d", big.mark = ".", decimal.mark = ",")
fmt_persen <- function(x, digit = 1, skala = 1) paste0(fmt_angka(x * skala, digit), "%")
fmt_p      <- function(p) ifelse(p < 0.001, "< 0,001", fmt_angka(p, 3))

# Untuk scale_*_continuous(labels = ...). digit = NULL -> presisi otomatis dari
# jarak antar-break (mencegah label menyesatkan, mis. break 0,25 tertulis "0,2").
lab_angka  <- function(digit = NULL) label_number(accuracy = if (is.null(digit)) NULL else 10^-digit,
                                                  big.mark = ".", decimal.mark = ",")
lab_persen <- function(digit = 0, skala = 100) label_percent(accuracy = 10^-digit, scale = skala,
                                                             big.mark = ".", decimal.mark = ",")

# 5. Caption standar --------------------------------------------------------------
caption_sumber <- function(sumber, catatan = NULL) {
  paste0(if (!is.null(catatan)) paste0("Catatan: ", catatan, "\n") else "",
         "Sumber: ", sumber)
}

# 6. Pembungkus teks panjang ---------------------------------------------------------
# Judul/subjudul/caption dibungkus otomatis sesuai lebar gambar akhir agar tidak
# terpotong (Visual QA bagian 28: clipping). Baris "\n" yang disengaja dipertahankan.
bungkus <- function(x, n) {
  if (is.null(x) || !is.character(x) || !length(x)) return(x)
  baris <- strsplit(x, "\n", fixed = TRUE)[[1]]
  paste(vapply(baris, function(b) paste(strwrap(b, n), collapse = "\n"), ""), collapse = "\n")
}
# Perkiraan lebar karakter Segoe UI pada ukuran tema (cm/karakter), diukur dari render
CM_PER_KAR <- c(title = 0.215, subtitle = 0.16, caption = 0.12)
n_kar <- function(lebar_cm, jenis) floor((lebar_cm - 1.2) / CM_PER_KAR[[jenis]])

rapikan_teks <- function(p, lebar_cm) {
  if (!inherits(p, "ggplot") || inherits(p, "patchwork")) return(p)
  lb <- tryCatch(ggplot2::get_labs(p), error = function(e) p$labels)
  baru <- list()
  for (j in names(CM_PER_KAR)) {
    if (!is.null(lb[[j]]) && is.character(lb[[j]]))
      baru[[j]] <- bungkus(lb[[j]], n_kar(lebar_cm, j))
  }
  if (length(baru)) p + do.call(labs, baru) else p
}

# 7. Penyimpan grafik --------------------------------------------------------------
# Ukuran dipilih dari tata letak laporan A4 (lebar teks ±16 cm), bukan default.
# PDF (vektor, cairo) untuk naskah; PNG 300 dpi (ragg) untuk Word/web/Shiny.
UKURAN <- list(penuh = c(16, 10), penuh_tinggi = c(16, 13), lebar = c(16, 7.5),
               setengah = c(8, 7))

simpan_grafik <- function(p, nama, folder, ukuran = "penuh", lebar = NULL, tinggi = NULL,
                          pdf = TRUE) {
  wh <- UKURAN[[ukuran]]
  if (!is.null(lebar))  wh[1] <- lebar
  if (!is.null(tinggi)) wh[2] <- tinggi
  p <- rapikan_teks(p, wh[1])
  dev_png <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
  ggsave(file.path(folder, paste0(nama, ".png")), p, width = wh[1], height = wh[2],
         units = "cm", dpi = 300, device = dev_png, bg = COL[["latar"]])
  if (pdf) ggsave(file.path(folder, paste0(nama, ".pdf")), p, width = wh[1], height = wh[2],
                  units = "cm", device = cairo_pdf, bg = COL[["latar"]])
  cat("  tersimpan:", nama, sprintf("(%.0f x %.0f cm)\n", wh[1], wh[2]))
  invisible(p)
}
