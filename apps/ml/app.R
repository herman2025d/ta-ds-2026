# ==============================================================================
# PROTOTIPE SHINY ML — DETEKSI DINI SISWA DI BAWAH LEVEL 2 SAINS + PROFIL BELAJAR
# Versi kalkulator ringkas: 10 pertanyaan butir asli PISA 2025 diisi siswa (LOG ML-24/25)
# Data   : data/app_ML.rds (dibuat 02_ML/script/06_siapkan_shiny_ML.R)
# Paket  : shiny, bslib, ggplot2, scales — semuanya tersedia di webR, sehingga
#          aplikasi bisa diterbitkan statis dengan shinylive (GitHub Pages/Netlify)
# Visual : fungsi/00_visual_system.R (salinan otomatis dari root, jangan diedit di sini)
# Jalankan lokal: shiny::runApp("D:/R/DATA SAINS (TUGAS AKHIR)/02_ML/shiny/app")
# ==============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(scales)

for (f in c("00_visual_system.R", "label_variabel_ML.R", "fungsi_shiny_ML.R", "teks_ML.R"))
  source(file.path("fungsi", f), encoding = "UTF-8")

D <- readRDS(file.path("data", "app_ML.rds"))
M <- D$model; I <- D$input; E <- D$evaluasi; P <- D$profil
M_SAINS <- P$model_sains; SI <- P$sains_info

BASE_APP <- 12                                  # ukuran teks grafik di layar
SIKAP    <- c("EFFSCIE", "JOYSCIE", "CURIO", "SELFREG")
FITUR_P  <- P$model$fitur
K        <- nrow(P$model$centers)
NAMA_P   <- setNames(P$nama, rownames(P$model$centers))
COL_P    <- pal_klaster(K)
LAB_P    <- setNames(paste(names(NAMA_P), NAMA_P), names(NAMA_P))
TANYA    <- I$prediktor
stopifnot(all(TANYA %in% names(PERTANYAAN)))

# Siswa tipikal = nilai imputasi recipe (median/modus training). Pertanyaan yang
# belum dijawab diperlakukan sama seperti data hilang pada model: nilai tipikal.
DASAR <- as.data.frame(c(as.list(M$ref_num), as.list(M$ref_kat)), stringsAsFactors = FALSE)[, TANYA]
LOGIT_DASAR <- logit_risiko(DASAR, M)

rk     <- E$metrik_ringkas
auc_rk <- rk$roc_auc[1]
auc_en <- E$metrik_tes$roc_auc[E$metrik_tes$model == "rec_elastic_net"]
auc_xg <- E$metrik_tes$roc_auc[E$metrik_tes$model == "rec_xgboost"]
tint <- function(col, t = 0.82) grDevices::colorRampPalette(c(col, "#FFFFFF"))(101)[round(t * 100) + 1]
catatan <- function(...) tags$p(class = "small text-muted mb-0", ...)
id_in <- function(v) paste0("q_", v)

# ------------------------------------------------------------------------------
# Form pertanyaan
# ------------------------------------------------------------------------------
judul_tanya <- function(v) {
  q  <- PERTANYAAN[[v]]
  ks <- q$konstruk
  tagList(
    tags$span(class = "small text-uppercase text-muted", label_ml(ks, kode = FALSE), " ",
              tooltip(tags$span("ⓘ", style = "cursor:help"), ISTILAH_ML[[ks]])),
    tags$br(), tags$span(class = "fw-semibold", q$teks))
}
tanya_ui <- function(v) {
  q <- PERTANYAAN[[v]]
  stopifnot(all(names(q$pilihan) %in% I$nilai_valid[[v]]))
  radioButtons(id_in(v), judul_tanya(v), choiceNames = unname(q$pilihan),
               choiceValues = names(q$pilihan), selected = character(0))
}
form_ui <- function() {
  kel <- vapply(TANYA, function(v) PERTANYAAN[[v]]$kelompok, "")
  tagList(lapply(names(KELOMPOK_TANYA), function(g) {
    v <- TANYA[kel == g]
    if (!length(v)) return(NULL)
    tagList(tags$h6(class = "mt-3 mb-2", KELOMPOK_TANYA[[g]]), lapply(v, tanya_ui))
  }))
}

baca_jawaban <- function(input) {
  b <- DASAR; jawab <- setNames(logical(length(TANYA)), TANYA)
  for (v in TANYA) {
    x <- input[[id_in(v)]]
    if (is.null(x) || !nzchar(x)) next
    b[[v]] <- if (v %in% I$kat) x else as.numeric(x)
    jawab[[v]] <- TRUE
  }
  list(baris = b, dijawab = jawab)
}

# Indeks sikap diperkirakan dari jawaban butir (rata-rata indeks siswa PISA dengan
# jawaban sama; skrip 07). Belum dijawab -> rata-rata umum.
z_siswa <- function(baris, dijawab, sains) {
  sikap <- vapply(SIKAP, function(s) {
    pk <- P$peta_sikap[[s]]; b <- pk$butir
    if (!isTRUE(dijawab[[b]])) return(pk$umum)
    e <- unname(pk$rata[as.character(baris[[b]])]); if (is.na(e)) pk$umum else e
  }, numeric(1))
  x <- c(SAINS = sains, sikap)
  (x[FITUR_P] - P$model$center[FITUR_P]) / P$model$scale[FITUR_P]
}

# ------------------------------------------------------------------------------
# Grafik (fungsi murni agar bisa diuji di luar Shiny)
# ------------------------------------------------------------------------------
# Data uji tersedia hanya sebagai hitungan per interval peluang 0,005 (ketentuan OECD:
# hasil agregat saja; LOG ML-32). Histogram digabung menjadi interval 0,025.
hist_uji <- function(ub, lebar = 0.025) {
  g <- floor(ub$lo / lebar + 1e-9)
  a <- aggregate(cbind(n_bawah, n_atas) ~ g, data = cbind(ub, g = g), FUN = sum)
  rbind(data.frame(x = a$g * lebar + lebar / 2, aktual = "Di bawah L2", n = a$n_bawah),
        data.frame(x = a$g * lebar + lebar / 2, aktual = "L2 ke atas", n = a$n_atas))
}
N_UJI      <- sum(E$uji_bin$n_bawah + E$uji_bin$n_atas)
PROP_BAWAH <- sum(E$uji_bin$n_bawah) / N_UJI

plot_risiko <- function(p, ub) {
  d <- hist_uji(ub)
  d$aktual <- factor(d$aktual, levels = names(COL_STATUS))
  kanan <- p > 0.72
  ggplot(d, aes(x, n, fill = aktual)) +
    geom_col(width = 0.025, colour = COL[["latar"]], linewidth = 0.2) +
    geom_vline(xintercept = 0.5, linetype = "dashed", colour = COL[["abu_tua"]], linewidth = 0.5) +
    annotate("text", x = 0.5, y = Inf, label = "ambang model 0,5", hjust = -0.06, vjust = 3.4,
             size = TXT_ANOT * 1.15, colour = COL[["abu_tua"]], family = FONT) +
    geom_vline(xintercept = p, colour = COL[["hitam"]], linewidth = 1) +
    annotate("label", x = p, y = Inf, label = paste0("Siswa ini: ", fmt_persen(p, 0, 100)),
             hjust = if (kanan) 1.04 else -0.04, vjust = 1.25, size = TXT_ANOT * 1.3,
             fill = COL[["latar"]], colour = COL[["hitam"]], family = FONT, fontface = "bold",
             border.colour = NA) +
    scale_fill_manual(values = COL_STATUS, name = "Kelas aktual siswa testing:") +
    scale_x_continuous(labels = lab_persen(), breaks = seq(0, 1, 0.25), expand = expansion(0.01)) +
    scale_y_continuous(labels = fmt_bulat, expand = expansion(mult = c(0, 0.3))) +
    coord_cartesian(xlim = c(0, 1)) +
    labs(x = "Peluang di bawah Level 2 menurut model", y = "Jumlah siswa") +
    theme_akademik(base_size = BASE_APP)
}

plot_kontribusi <- function(k, logit_dasar) {
  dp <- plogis(logit_dasar + k) - plogis(logit_dasar)
  d  <- data.frame(v = names(k), dp = unname(dp))
  d  <- d[abs(d$dp) >= 0.0005, , drop = FALSE]                   # >= 0,05 poin persen
  validate(need(nrow(d) > 0, paste(
    "Jawaban masih sama dengan siswa tipikal.",
    "Jawab pertanyaan di panel kiri untuk melihat hal yang menaikkan atau menurunkan risiko.")))
  d <- d[order(-abs(d$dp)), ]
  lab <- label_ml(I$konstruk[d$v], kode = FALSE)
  d$lab  <- factor(lab, levels = rev(lab))
  d$arah <- ifelse(d$dp > 0, "Menaikkan risiko", "Menurunkan risiko")
  ggplot(d, aes(100 * dp, lab, fill = arah)) +
    geom_col(width = 0.65) +
    geom_vline(xintercept = 0, colour = COL[["abu"]], linewidth = 0.4) +
    geom_text(aes(label = paste0(ifelse(dp > 0, "+", "-"), fmt_angka(abs(100 * dp), 1)),
                  hjust = ifelse(dp > 0, -0.15, 1.15)),
              size = TXT_ANOT * 1.2, colour = COL[["abu_tua"]], family = FONT) +
    scale_fill_manual(values = c("Menaikkan risiko" = COL[["koral"]], "Menurunkan risiko" = COL[["teal"]]),
                      name = NULL) +
    scale_x_continuous(labels = lab_angka(), expand = expansion(mult = 0.3)) +
    labs(x = "Perubahan peluang (poin persen)", y = NULL) +
    theme_akademik(base_size = BASE_APP, grid = "x")
}

# Nama profil ditulis DI ATAS batang (bukan di sumbu kiri) agar grafik tetap
# terbaca di kartu sempit/layar kecil (perbaikan tampilan 7 Nov, LOG ML-29).
plot_anggota <- function(u) {
  d <- data.frame(kl = names(u), u = unname(u))
  d$y <- rev(seq_len(nrow(d)))                                   # K1 paling atas
  ggplot(d) +
    geom_rect(aes(xmin = 0, xmax = 1, ymin = y - 0.17, ymax = y + 0.17), fill = COL[["grid"]]) +
    geom_rect(aes(xmin = 0, xmax = u, ymin = y - 0.17, ymax = y + 0.17, fill = kl)) +
    # Garis acuan hanya sepanjang batang, agar tidak mencoret nama profil
    geom_segment(aes(x = 1 / K, xend = 1 / K, y = y - 0.22, yend = y + 0.22),
                 linetype = "dotted", colour = COL[["abu_tua"]], linewidth = 0.5) +
    geom_segment(aes(x = 0.6, xend = 0.6, y = y - 0.22, yend = y + 0.22),
                 linetype = "dashed", colour = COL[["abu_tua"]], linewidth = 0.5) +
    geom_text(aes(x = 0, y = y + 0.24, label = LAB_P[kl]), hjust = 0, vjust = 0,
              size = TXT_ANOT * 1.2, colour = COL[["hitam"]], family = FONT) +
    geom_text(aes(x = pmax(u, 0) + 0.015, y = y, label = fmt_persen(u, 0, 100)), hjust = 0,
              size = TXT_ANOT * 1.2, colour = COL[["hitam"]], family = FONT, fontface = "bold") +
    scale_fill_manual(values = COL_P, guide = "none") +
    scale_x_continuous(labels = lab_persen(), breaks = c(0, 0.25, 0.6, 1), limits = c(0, 1.1),
                       expand = expansion(0)) +
    scale_y_continuous(breaks = NULL, limits = c(0.7, nrow(d) + 0.75), expand = expansion(0)) +
    labs(x = "Kedekatan dengan profil", y = NULL) +
    theme_akademik(base_size = BASE_APP, grid = "none") +
    theme(axis.line.y = element_blank())
}

# Grafik profil DIPUTAR: aspek di sumbu kiri (satu baris per label), z di sumbu
# horizontal. Pada kartu sempit, lima label di sumbu bawah selalu bertumpuk;
# susunan tegak tidak dibatasi lebar kartu (perbaikan tampilan, LOG ML-30).
LABEL_PENDEK <- c(SAINS = "Kemampuan sains", EFFSCIE = "Keyakinan diri", JOYSCIE = "Kesenangan belajar",
                  CURIO = "Rasa ingin tahu", SELFREG = "Mengatur diri")

plot_profil <- function(z = NULL) {
  cen <- P$model$centers
  pos <- setNames(rev(seq_along(FITUR_P)), FITUR_P)               # aspek pertama paling atas
  d <- data.frame(kl = rep(rownames(cen), ncol(cen)), fitur = rep(colnames(cen), each = nrow(cen)),
                  z = as.vector(cen))
  d$yy  <- pos[d$fitur]
  d$lab <- factor(LAB_P[d$kl], levels = LAB_P)
  d <- d[order(d$kl, d$yy), ]
  g <- ggplot(d, aes(z, yy)) +
    geom_vline(xintercept = 0, colour = COL[["abu"]], linewidth = 0.4) +
    geom_path(aes(group = lab, colour = lab), linewidth = 0.9) +
    geom_point(aes(colour = lab), size = 2.2)
  if (!is.null(z)) {
    s <- data.frame(yy = pos[names(z)], z = unname(z))
    s <- s[!is.na(s$z), ]; s <- s[order(s$yy), ]
    g <- g +
      geom_path(data = s, aes(linetype = "Siswa ini (perkiraan)"), colour = COL[["hitam"]], linewidth = 0.7) +
      geom_point(data = s, aes(shape = "Siswa ini (perkiraan)"), colour = COL[["hitam"]],
                 fill = COL[["latar"]], size = 2.8, stroke = 1) +
      scale_linetype_manual(values = c("Siswa ini (perkiraan)" = "22"), name = NULL) +
      scale_shape_manual(values = c("Siswa ini (perkiraan)" = 21), name = NULL)
  }
  g +
    scale_colour_manual(values = setNames(COL_P, LAB_P), name = NULL,
                        labels = function(x) sub(", ", ",\n", x, fixed = TRUE)) +   # 2 baris: aman di layar sempit
    scale_y_continuous(breaks = pos, labels = LABEL_PENDEK[names(pos)], expand = expansion(add = 0.4)) +
    scale_x_continuous(labels = lab_angka()) +
    guides(colour = guide_legend(ncol = 1, order = 1),
           linetype = guide_legend(order = 2), shape = guide_legend(order = 2)) +
    labs(x = "z-score (0 = rata-rata)", y = NULL) +
    theme_akademik(base_size = BASE_APP, grid = "x") +
    theme(legend.key.spacing.y = unit(3, "pt"), legend.text = element_text(lineheight = 1),
          legend.box = "vertical", legend.box.just = "left",       # legenda bertumpuk, tidak melebar
          legend.spacing.y = unit(2, "pt"))
}

NAMA_RK <- "Ringkas 10 pertanyaan"
COL_RK  <- c(COL_MODEL, setNames(COL[["hitam"]], NAMA_RK))     # koral dicadangkan untuk "di bawah L2"
LT_RK   <- c(LT_MODEL,  setNames("solid", NAMA_RK))

plot_roc <- function() {
  roc <- E$roc
  roc$model <- ifelse(roc$model == "ringkas", NAMA_RK, NAMA_MODEL[roc$model])
  auc  <- c(setNames(E$metrik_tes$roc_auc, NAMA_MODEL[E$metrik_tes$model]), setNames(auc_rk, NAMA_RK))
  urut <- names(sort(auc[names(auc) != "Baseline"], decreasing = TRUE))
  lab  <- setNames(paste0(urut, " · AUC ", fmt_angka(auc[urut], 3),
                          ifelse(urut == NAMA_RK, " (kalkulator)", "")), urut)
  roc$lab <- factor(lab[roc$model], levels = lab)
  ggplot(roc, aes(fpr, tpr, colour = lab, linetype = lab, linewidth = lab)) +
    geom_abline(slope = 1, intercept = 0, colour = COL[["abu"]], linetype = "dotted", linewidth = 0.5) +
    geom_path() +
    scale_colour_manual(values = setNames(COL_RK[urut], lab), name = NULL) +
    scale_linetype_manual(values = setNames(LT_RK[urut], lab), name = NULL) +
    scale_linewidth_manual(values = setNames(ifelse(urut == NAMA_RK, 1.3, 0.55), lab), name = NULL) +
    scale_x_continuous(labels = lab_angka(1), breaks = seq(0, 1, 0.2), expand = expansion(0.01)) +
    scale_y_continuous(labels = lab_angka(1), breaks = seq(0, 1, 0.2), expand = expansion(0.01)) +
    coord_equal() +
    labs(x = "1 − spesifisitas (siswa ≥ L2 yang keliru ditandai)",
         y = "Sensitivitas (siswa < L2 yang terdeteksi)") +
    theme_akademik(base_size = BASE_APP, grid = "xy", legend = "inside") +
    theme(legend.position.inside = c(0.98, 0.03), legend.justification.inside = c(1, 0),
          legend.background = element_rect(fill = alpha(COL[["latar"]], 0.92), colour = NA),
          legend.key.width = unit(24, "pt"))
}

tabel_metrik <- function() {
  t  <- E$metrik_tes[E$metrik_tes$model != "baseline", ]
  cv <- E$metrik_cv
  ci <- setNames(paste0(fmt_angka(cv$mean_roc_auc, 3), " ± ", fmt_angka(1.96 * cv$std_err_roc_auc, 3)),
                 NAMA_MODEL[cv$wflow_id])
  r1 <- data.frame(Model = paste0(NAMA_MODEL[t$model], " (52 prediktor)"), auc = t$roc_auc,
                   acc = t$accuracy, sens = t$sens, spec = t$spec, cv = unname(ci[NAMA_MODEL[t$model]]))
  r2 <- data.frame(Model = paste0(NAMA_RK, " (kalkulator)"), auc = rk$roc_auc[1], acc = rk$accuracy[1],
                   sens = rk$sens[1], spec = rk$spec[1], cv = fmt_angka(rk$cv_auc[1], 3))
  d <- rbind(r1, r2); d <- d[order(-d$auc), ]
  data.frame(Model = d$Model, `ROC AUC testing` = fmt_angka(d$auc, 3), Akurasi = fmt_angka(d$acc, 3),
             Sensitivitas = fmt_angka(d$sens, 3), Spesifisitas = fmt_angka(d$spec, 3),
             `ROC AUC validasi silang` = ifelse(is.na(d$cv), "–", d$cv), check.names = FALSE)
}

tabel_profil <- function() {
  r <- P$ringkas
  data.frame(Profil = LAB_P[r$klaster],
             Siswa = paste0(fmt_bulat(r$n), " (", fmt_persen(r$persen, 1), ")"),
             `Rata-rata skor sains` = fmt_bulat(r$sains),
             `Di bawah L2` = fmt_persen(r$persen_di_bawah_L2, 0),
             Perbatasan = fmt_persen(r$persen_perbatasan, 0), check.names = FALSE)
}

# ------------------------------------------------------------------------------
# Antarmuka
# ------------------------------------------------------------------------------
font_app <- font_collection("Segoe UI", "Calibri", "Arial", "sans-serif")
tema <- bs_theme(version = 5, bg = COL[["latar"]], fg = COL[["hitam"]], primary = COL[["navy"]],
                 secondary = COL[["abu_tua"]], success = COL[["teal"]], danger = COL[["koral"]],
                 warning = COL[["oker"]], info = COL[["violet"]],
                 base_font = font_app, heading_font = font_app, "font-size-base" = "0.95rem")

sidebar_siswa <- sidebar(
  width = 390, title = "Jawab 10 pertanyaan",
  catatan("Diisi oleh siswa sendiri (boleh didampingi guru). Pertanyaan diambil dari kuesioner ",
          "siswa PISA 2025. Tidak ada jawaban benar atau salah, dan tidak ada data yang disimpan."),
  uiOutput("progres"),
  uiOutput("form"),
  actionButton("reset", "Hapus semua jawaban", class = "btn-outline-primary btn-sm mt-2")
)

hal_kalkulator <- nav_panel(
  "Kalkulator siswa",
  layout_sidebar(
    sidebar = sidebar_siswa, fillable = FALSE,
    layout_column_wrap(width = 1 / 3, fill = FALSE, heights_equal = "row",
                       uiOutput("vb_risiko"), uiOutput("vb_kalibrasi"), uiOutput("vb_profil")),
    card(card_header("Rekomendasi pembelajaran untuk siswa ini"), uiOutput("rekomendasi"),
         card_footer(catatan(KERANGKA_REKOMENDASI, " ", CATATAN_REKOMENDASI))),
    layout_columns(
      col_widths = c(6, 6), fill = FALSE,
      card(card_header("Posisi siswa di antara siswa data testing"),
           plotOutput("g_risiko", height = 300),
           card_footer(catatan(
             "Histogram = peluang menurut model ringkas untuk ", fmt_bulat(N_UJI),
             " siswa testing (dari sekolah yang tidak dipakai melatih model), diwarnai kelas aktualnya. ",
             "ROC AUC model ringkas ", fmt_angka(auc_rk, 3), ". Peluang adalah estimasi, bukan kepastian."))),
      card(card_header("Jawaban yang paling mengubah risiko"),
           plotOutput("g_kontribusi", height = 300),
           card_footer(catatan(
             "Tiap batang = perubahan peluang bila hanya jawaban itu yang berbeda dari jawaban siswa tipikal. ",
             "Model menangkap asosiasi, bukan sebab-akibat: mengubah satu jawaban belum tentu mengubah capaian. ",
             "Perhatian: makin sering siswa melaporkan menyusun argumen ilmiah di kelas, peluangnya justru ",
             "sedikit lebih tinggi; pola ini berlawanan intuisi (lihat tab Tentang).")))
    ),
    layout_columns(
      col_widths = c(5, 7), fill = FALSE,
      card(card_header("Kedekatan dengan profil belajar"),
           plotOutput("g_anggota", height = 440),
           card_footer(uiOutput("catatan_anggota"))),
      card(card_header("Siswa ini dibandingkan pusat keempat profil"),
         plotOutput("g_profil_siswa", height = 440),
         card_footer(catatan(
           "Garis hitam putus-putus = perkiraan dari jawaban siswa. Sikap belajar: satu pertanyaan per aspek ",
           "(R² ", fmt_angka(min(P$r2_sikap), 2), "–", fmt_angka(max(P$r2_sikap), 2),
           " terhadap indeks PISA lengkap). Kemampuan sains: perkiraan dari 10 jawaban (R² ",
           fmt_angka(SI$r2, 2), "; rata-rata galat ± ", fmt_bulat(SI$rmse), " poin), bukan skor tes, dan ",
           "cenderung tertarik ke rata-rata sehingga siswa sangat rendah/tinggi tampak lebih ke tengah."))))
  )
)

hal_profil <- nav_panel(
  "Profil belajar",
  layout_columns(
    col_widths = c(7, 5), fill = FALSE,
    card(card_header(paste0("Pusat ", K, " profil belajar (solusi diagnostik)")),
         plotOutput("g_profil", height = 440),
         card_footer(catatan(
           "Fuzzy c-means (k = ", K, ", m = ", fmt_angka(P$model$m, 1), ") pada kemampuan sains dan empat ",
           "indikator sikap belajar (disposisi), z-score relatif terhadap siswa Indonesia. Nama profil dibuat ",
           "otomatis: kemampuan = tanda z sains; sikap belajar = rata-rata z keempat indikator ",
           "(> 0,3 tinggi, < −0,3 rendah)."))),
    card(card_header("Ringkasan profil (siswa PISA 2025 Indonesia)"),
         tableOutput("t_profil"),
         card_footer(catatan("Perbatasan = kedekatan tertinggi dengan satu profil < 0,6.")))
  ),
  card(card_header("Cara membaca profil dengan jujur"),
       tags$ul(
         tags$li("Struktur klaster lemah (silhouette ", fmt_angka(P$silhouette, 3),
                 "; < 0,25). Siswa membentuk kontinum, jadi profil adalah alat bantu diagnostik, ",
                 "bukan tipe siswa yang tegas."),
         tags$li("Profil stabil lintas 10 plausible values: rata-rata ",
                 fmt_persen(P$kesesuaian_pv, 1, 100), " siswa masuk profil yang sama."),
         tags$li("Sekitar ", fmt_persen(P$persen_perbatasan, 0), " siswa berada di antara profil. ",
                 "Karena itu kalkulator menampilkan kedekatan dengan tiap profil, bukan satu label."),
         tags$li("Skor PISA perorangan tidak pernah diketahui guru atau siswa. Karena itu kalkulator memperkirakan ",
                 "kemampuan sains dan sikap belajar dari 10 jawaban. Pada siswa testing, profil hasil perkiraan sama ",
                 "dengan profil dari data PISA lengkap untuk ", fmt_persen(SI$profil$kesesuaian_profil[2], 0, 100),
                 " siswa (dengan skor tes asli ", fmt_persen(SI$profil$kesesuaian_profil[1], 0, 100),
                 "; tanpa informasi kemampuan ", fmt_persen(SI$profil$kesesuaian_profil[3], 0, 100), "). ",
                 "Label kemampuan profil yang bertentangan dengan angka risiko turun dari ",
                 fmt_persen(SI$profil$inkonsisten_risiko[3], 0, 100), " menjadi ",
                 fmt_persen(SI$profil$inkonsisten_risiko[2], 0, 100), "."),
         tags$li("Sikap belajar tinggi tidak menjamin kemampuan: profil dengan sikap tertinggi masih memuat ",
                 fmt_persen(P$ringkas$persen_di_bawah_L2[which.max(rowMeans(P$model$centers[, SIKAP]))], 0),
                 " siswa di bawah L2."),
         tags$li("Solusi statistik utama adalah k = 2 (tinggi vs rendah menyeluruh). Solusi k = 4 dipakai ",
                 "di aplikasi karena lebih berguna untuk rekomendasi; keputusan ini dicatat sebelum hasil dilihat.")
       ))
)

sl <- E$selisih_auc
hal_model <- nav_panel(
  "Perbandingan model",
  layout_columns(
    col_widths = c(6, 6), fill = FALSE,
    card(card_header("Kurva ROC pada data testing"),
         plotOutput("g_roc", height = 460),
         card_footer(catatan("Garis titik-titik = tebakan acak (AUC 0,50)."))),
    card(card_header("Metrik model"),
         tableOutput("t_metrik"),
         tags$p("Model lengkap memakai 52 prediktor tingkat siswa dan sekolah; XGBoost terbaik (AUC ",
                fmt_angka(auc_xg, 3), ") dan menjadi model analisis utama."),
         tags$p("Kalkulator memakai model ringkas: 10 pertanyaan yang bisa dijawab siswa sendiri, dilatih ",
                "langsung pada jawaban butir PISA dengan split sekolah yang sama. Harga kepraktisannya: AUC turun ",
                fmt_angka(abs(sl$boot_mean[["vs_en"]]), 3), " dibanding Elastic Net lengkap (95% CI ",
                fmt_angka(abs(sl$ci[2, "vs_en"]), 3), "–", fmt_angka(abs(sl$ci[1, "vs_en"]), 3), ") dan ",
                fmt_angka(abs(sl$boot_mean[["vs_xgb"]]), 3), " dibanding XGBoost (95% CI ",
                fmt_angka(abs(sl$ci[2, "vs_xgb"]), 3), "–", fmt_angka(abs(sl$ci[1, "vs_xgb"]), 3),
                "; bootstrap per sekolah)."),
         tags$p("Peluang model ringkas dan Elastic Net lengkap berkorelasi r = ", fmt_angka(E$kesepakatan$r_en, 2),
                "; klasifikasinya sama untuk ", fmt_persen(E$kesepakatan$sama_en, 1, 100), " siswa testing."),
         card_footer(catatan(
           "Model ringkas: Elastic Net (logistik glmnet), tuning 5-fold per sekolah; mixture = ",
           fmt_angka(E$tuning$mixture, 2), ". Ambang klasifikasi 0,5.")))
  )
)

hal_tentang <- nav_panel(
  "Tentang",
  card(
    card_header("Tentang prototipe ini"),
    tags$p("Prototipe tugas akhir mata kuliah Machine Learning. Tujuannya membantu guru mengenali lebih awal ",
           "siswa yang berpeluang berada di bawah Level 2 sains PISA (batas ", fmt_angka(D$ambang_l2, 2),
           ") dan memilih pendekatan belajar sesuai profil."),
    tags$h6("Data dan metode"),
    tags$ul(
      tags$li(D$sumber, " Sumber data: ", tags$em(D$pengakuan),
              " Aplikasi hanya memuat hasil agregat dan parameter model, tanpa data perorangan siswa atau sekolah."),
      tags$li("Model lengkap: 52 prediktor tingkat siswa dan sekolah; data dibagi per sekolah (80% latih, 20% uji), ",
              "validasi silang 5-fold per sekolah, empat algoritma dibandingkan dengan baseline."),
      tags$li("Kalkulator: model ringkas 10 pertanyaan. Empat aspek sikap belajar wajib (untuk profil) ditambah ",
              "enam faktor terkuat model lengkap yang dapat dijawab siswa; tiap indeks diwakili satu butir PISA ",
              "dengan korelasi tertinggi terhadap indeksnya. Aturan ini ditetapkan sebelum hasil dilihat."),
      tags$li("Pertanyaan yang belum dijawab diperlakukan sebagai jawaban siswa tipikal, sama seperti data hilang pada model."),
      tags$li("Profil belajar: fuzzy c-means pada kemampuan sains dan empat aspek sikap belajar. Di kalkulator, ",
              "kemampuan sains diperkirakan dengan regresi linear dari 10 jawaban yang sama (R² ",
              fmt_angka(SI$r2, 2), " pada data testing). Cara ini dipilih karena lolos aturan keputusan yang ",
              "ditetapkan sebelum hasil dilihat: kesesuaian profil naik dan pertentangan dengan risiko turun.")
    ),
    tags$h6("Keterbatasan"),
    tags$ul(
      tags$li("Hasil berupa asosiasi, bukan hubungan sebab-akibat. Arah sebagian faktor bisa tampak berlawanan ",
              "dengan intuisi karena model membaca pola bersama, bukan pengaruh satu faktor."),
      tags$li("Keterlibatan dalam praktik sains berarah berlawanan intuisi: siswa yang melaporkan lebih sering ",
              "menyusun argumen ilmiah di kelas justru sedikit lebih berpeluang di bawah L2, baik di model lengkap ",
              "maupun ringkas. Dugaan penyebabnya (belum diuji): gaya menjawab atau perbedaan pemahaman siswa ",
              "tentang apa itu 'argumen ilmiah'. Faktor ini tetap dipakai karena aturan pemilihan ditetapkan ",
              "sebelum hasil dilihat; jangan dibaca sebagai saran mengurangi praktik sains."),
      tags$li("Pemahaman isu lingkungan adalah prediktor terkuat; kemungkinan ia ikut menangkap pengetahuan sains."),
      tags$li("Jawaban adalah laporan diri siswa; kejujuran dan pemahaman pertanyaan memengaruhi hasil."),
      tags$li("Tidak ada data yang disimpan: semua perhitungan terjadi di peramban/komputer pengguna."),
      tags$li("Bukan untuk keputusan berisiko tinggi tentang individu siswa.")
    ),
    tags$h6("Pustaka rekomendasi pembelajaran"),
    tags$ul(class = "small", lapply(PUSTAKA, tags$li)),
    card_footer(catatan("Bahan aplikasi dibuat: ", D$dibuat, "."))
  )
)

ui <- page_navbar(
  title = "Deteksi Dini Sains · PISA 2025 Indonesia",
  theme = tema, fillable = FALSE, lang = "id",
  hal_kalkulator, hal_profil, hal_model, hal_tentang
)

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  versi_form <- reactiveVal(0)
  output$form <- renderUI({ versi_form(); form_ui() })      # dibuat ulang saat "hapus jawaban"
  observeEvent(input$reset, {
    for (v in TANYA) session$sendInputMessage(id_in(v), list(value = character(0)))
    versi_form(versi_form() + 1)
  })

  jwb     <- reactive(baca_jawaban(input))
  logit   <- reactive(logit_risiko(jwb()$baris, M))
  risiko  <- reactive(plogis(logit()))
  n_jawab <- reactive(sum(jwb()$dijawab))
  # Skor PISA perorangan tidak pernah diketahui -> kemampuan diperkirakan dari
  # jawaban yang sama (skrip 08, opsi B lolos aturan pra-tetap; LOG ML-27/28)
  sains   <- reactive(prediksi_linear(jwb()$baris, M_SAINS))
  z       <- reactive(z_siswa(jwb()$baris, jwb()$dijawab, sains()))
  anggota <- reactive(keanggotaan_fcm(z(), P$model$centers, P$model$m))

  output$progres <- renderUI({
    n <- n_jawab(); tot <- length(TANYA)
    div(class = paste("small py-1 px-2 rounded", if (n == tot) "bg-success-subtle" else "bg-warning-subtle"),
        if (n == tot) "Semua pertanyaan terjawab."
        else paste0(n, " dari ", tot, " terjawab. Yang belum dijawab dianggap jawaban siswa tipikal."))
  })

  output$vb_risiko <- renderUI({
    p <- risiko(); berisiko <- p >= 0.5
    value_box(
      title = "Peluang di bawah Level 2", value = fmt_persen(p, 0, 100),
      tags$p(if (berisiko) "Di atas ambang 0,5: ditandai berisiko" else "Di bawah ambang 0,5: tidak ditandai"),
      tags$p(class = "small mb-0", "Siswa tipikal: ", fmt_persen(plogis(LOGIT_DASAR), 0, 100),
             " (", fmt_persen(PROP_BAWAH, 0, 100),
             " siswa testing memang di bawah L2)"),
      theme = value_box_theme(bg = tint(if (berisiko) COL[["koral"]] else COL[["teal"]]), fg = COL[["hitam"]]))
  })

  output$vb_kalibrasi <- renderUI({
    p <- risiko(); u <- E$uji_bin
    dekat <- abs((u$lo + u$hi) / 2 - p) <= 0.05
    n <- sum(u$n_bawah[dekat] + u$n_atas[dekat])
    cukup <- n >= 30
    value_box(
      title = "Bukti dari data testing",
      value = if (cukup) fmt_persen(sum(u$n_bawah[dekat]) / n, 0, 100) else "–",
      tags$p(if (cukup) paste0("dari ", fmt_bulat(n), " siswa testing dengan peluang serupa (± 5 poin) ",
                               "memang di bawah L2")
             else "Terlalu sedikit siswa testing dengan peluang serupa (< 30)"),
      theme = value_box_theme(bg = tint(COL[["navy"]], 0.88), fg = COL[["hitam"]]))
  })

  output$vb_profil <- renderUI({
    u <- anggota(); i <- which.max(u)
    value_box(
      title = "Profil belajar terdekat", value = paste0(names(u)[i], " · ", fmt_persen(u[[i]], 0, 100)),
      tags$p(NAMA_P[[i]]),
      tags$p(class = "small mb-0", if (u[[i]] >= 0.6) "Kedekatan tegas (≥ 0,6)"
             else "Profil campuran (< 0,6): baca dua profil teratas"),
      theme = value_box_theme(bg = tint(COL_P[[i]]), fg = COL[["hitam"]]))
  })

  output$g_risiko <- renderPlot(plot_risiko(risiko(), E$uji_bin), res = 96,
                                alt = "Histogram peluang siswa testing dengan posisi siswa ini")
  output$g_kontribusi <- renderPlot(
    plot_kontribusi(kontribusi_logit(jwb()$baris, DASAR, M), LOGIT_DASAR), res = 96,
    alt = "Jawaban yang paling menaikkan atau menurunkan peluang di bawah Level 2")
  output$g_anggota <- renderPlot(plot_anggota(anggota()), res = 96,
                                 alt = "Kedekatan siswa dengan empat profil belajar")
  output$g_profil_siswa <- renderPlot(plot_profil(z()), res = 96,
                                      alt = "Perkiraan sikap belajar siswa dibandingkan pusat empat profil")
  output$g_profil <- renderPlot(plot_profil(), res = 96, alt = "Pusat empat profil belajar")
  output$g_roc    <- renderPlot(plot_roc(), res = 96, alt = "Kurva ROC model lengkap dan ringkas")
  output$t_profil <- renderTable(tabel_profil(), striped = TRUE, spacing = "s", align = "lrrrr")
  output$t_metrik <- renderTable(tabel_metrik(), striped = TRUE, spacing = "s", align = "lrrrrr")

  output$catatan_anggota <- renderUI({
    catatan(
      "Garis putus = 0,6 (kedekatan tegas); titik-titik = ", fmt_angka(1 / K, 2),
      " (merata, tanpa informasi). Kemampuan sains untuk profil diperkirakan dari jawaban, ",
      "bukan skor tes; pada siswa testing profilnya sama dengan profil sebenarnya untuk ",
      fmt_persen(SI$profil$kesesuaian_profil[2], 0, 100), " siswa.")
  })

  output$rekomendasi <- renderUI({
    u <- sort(anggota(), decreasing = TRUE)
    tampil <- if (u[[1]] < 0.6) names(u)[1:2] else names(u)[1]
    blok <- lapply(tampil, function(kl) {
      r <- REKOMENDASI[[NAMA_P[[kl]]]]
      if (is.null(r)) return(tags$p("Belum ada rekomendasi untuk profil ", NAMA_P[[kl]], "."))
      tagList(tags$h6(class = "mt-2", paste0(LAB_P[[kl]], " (kedekatan ", fmt_persen(u[[kl]], 0, 100), ")")),
              tags$p(class = "mb-1", tags$strong("Fokus: "), r$fokus),
              tags$ul(lapply(r$langkah, tags$li)))
    })

    # Saran dari jawaban: aspek yang dijawab pada separuh bawah skala (hanya yang dijawab)
    j <- jwb()
    picu <- Filter(function(v) isTRUE(j$dijawab[[v]]) &&
                     as.character(j$baris[[v]]) %in% SARAN_BUTIR[[v]]$pemicu, names(SARAN_BUTIR))
    kartu_saran <- function(v) {
      s <- SARAN_BUTIR[[v]]
      tags$li(class = "mb-2", tags$strong(s$judul, ". "), s$teks,
              if (!is.null(s$teori)) tags$span(class = "small text-muted", " Dasar: ", s$teori, "."))
    }
    saran   <- Filter(function(v) SARAN_BUTIR[[v]]$jenis == "saran", picu)
    konteks <- Filter(function(v) SARAN_BUTIR[[v]]$jenis == "konteks", picu)

    tagList(
      if (n_jawab() < length(TANYA))
        catatan("Jawab semua pertanyaan agar profil dan rekomendasi lebih tepat."),
      if (risiko() >= 0.5)
        div(class = "alert alert-warning py-2 my-2",
            "Peluang di bawah L2 ≥ 0,5: prioritaskan asesmen diagnostik awal dan pemantauan berkala."),
      layout_columns(
        col_widths = breakpoints(sm = 12, lg = c(7, 5)), fill = FALSE,
        div(tags$h6(class = "text-uppercase small text-muted mt-2", "Sesuai profil belajar terdekat"),
            if (length(tampil) > 1)
              catatan("Profil campuran: saran dua profil teratas ditampilkan; pastikan dengan pengamatan guru."),
            blok),
        div(tags$h6(class = "text-uppercase small text-muted mt-2", "Sesuai jawaban siswa"),
            if (!length(saran) && !length(konteks))
              tags$p(class = "small", if (n_jawab() == 0) "Belum ada jawaban."
                     else "Tidak ada aspek yang dijawab rendah. Pertahankan dan gunakan saran sesuai profil."),
            if (length(saran)) tags$ul(class = "ps-3", lapply(saran, kartu_saran)),
            if (length(konteks))
              tagList(tags$p(class = "small fw-semibold mb-1", "Perhatian konteks (bukan saran pembelajaran):"),
                      tags$ul(class = "ps-3 small", lapply(konteks, kartu_saran))))
      )
    )
  })
}

shinyApp(ui, server)
