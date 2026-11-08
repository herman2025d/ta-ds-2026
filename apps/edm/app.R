# ==============================================================================
# PROTOTIPE SHINY EDM — DASHBOARD PENENTUAN PRIORITAS AFIRMASI AKSES PENDIDIKAN
# Kab/kota Indonesia, target HLS 2026 ≤ kuartil-1 (LOG EDM-08), model CART utama (EDM-15/16)
# Data   : data/app_EDM.rds (dibuat 01_EDM/script/04_siapkan_shiny_EDM.R)
# Paket  : shiny, bslib, ggplot2, scales, rpart — semuanya ada di webR (bisa shinylive)
# Visual : fungsi/00_visual_system.R (salinan otomatis dari root, jangan diedit di sini)
# Jalankan lokal: shiny::runApp("D:/R/DATA SAINS (TUGAS AKHIR)/01_EDM/shiny/app")
# ==============================================================================

library(shiny)
library(bslib)
library(ggplot2)
library(scales)
library(rpart)

for (f in c("00_visual_system.R", "label_variabel_EDM.R", "teks_EDM.R"))
  source(file.path("fungsi", f), encoding = "UTF-8")

D <- readRDS(file.path("data", "app_EDM.rds"))
M <- D$model
BASE_APP <- 12
LV <- c("Prioritas", "Bukan")

nama_prov <- function(x) {
  x <- tools::toTitleCase(tolower(x))
  x <- sub("^D i ", "DI ", x); sub("^Dki ", "DKI ", x)
}
K <- D$kab
K$prov_lab <- nama_prov(K$provinsi)
K$label <- paste0(K$nama, " — ", K$prov_lab)
K$aturan_nama <- unname(NAMA_ATURAN[as.character(K$node)])
N_PRIO <- sum(K$PRIORITAS == "Prioritas")
tint <- function(col, t = 0.82) grDevices::colorRampPalette(c(col, "#FFFFFF"))(101)[round(t * 100) + 1]
catatan <- function(...) tags$p(class = "small text-muted mb-0", ...)

DASAR <- c("Model CART (peluang prioritas)" = "model",
           "Kebijakan berjalan (% desa daerah khusus)" = "kebijakan")
COL_DASAR <- c(model = COL_MODEL_EDM[["CART"]], kebijakan = COL_MODEL_EDM[["Kebijakan berjalan"]])
LAB_DASAR <- setNames(names(DASAR), DASAR)

# Urutan peringkat: nilai sama (satu daun pohon / persen DK sama) diurutkan menurut RLS
# terendah, prediktor terpenting di semua model (EDM-16).
urutkan <- function(d, dasar) {
  if (dasar == "model") d[order(-d$prob_prioritas, d$RLS), ] else d[order(-d$DK_PERSEN, d$RLS), ]
}
kurva <- function(d, dasar) {
  u <- urutkan(d, dasar)
  data.frame(dasar = dasar, k = seq_len(nrow(u)), tangkap = cumsum(u$PRIORITAS == "Prioritas"))
}

# ------------------------------------------------------------------------------
# Grafik (fungsi murni)
# ------------------------------------------------------------------------------
plot_gains <- function(d, kuota) {
  g <- rbind(kurva(d, "model"), kurva(d, "kebijakan"))
  n <- nrow(d); P <- sum(d$PRIORITAS == "Prioritas"); kk <- min(kuota, n)
  titik <- g[g$k == kk, ]
  ggplot(g, aes(k, tangkap, colour = dasar)) +
    annotate("segment", x = 0, y = 0, xend = n, yend = P, colour = COL[["abu"]], linetype = "dotted") +
    geom_vline(xintercept = kk, colour = COL[["abu_tua"]], linetype = "dashed", linewidth = 0.4) +
    geom_step(linewidth = 0.9) +
    geom_point(data = titik, size = 3) +
    geom_label(data = titik, aes(label = paste0(tangkap, " Prioritas")), hjust = -0.08, vjust = 1.2,
               size = TXT_ANOT * 1.2, family = FONT, border.colour = NA, fill = alpha(COL[["latar"]], 0.85),
               show.legend = FALSE) +
    scale_colour_manual(values = COL_DASAR, labels = LAB_DASAR, name = NULL) +
    scale_x_continuous(labels = fmt_bulat, expand = expansion(c(0, 0.02))) +
    scale_y_continuous(labels = fmt_bulat, expand = expansion(c(0, 0.05))) +
    labs(x = "Jumlah kab/kota yang dibiayai (dari peringkat teratas)",
         y = "Kab/kota Prioritas 2026 yang terjaring") +
    theme_akademik(base_size = BASE_APP, grid = "xy") +
    theme(legend.direction = "vertical")
}

plot_biaya <- function() {
  nm <- c(cart_utama = "CART utama (+DK)", cart_pembanding = "CART pembanding",
          c50_utama = "C5.0 utama (+DK)", c50_pembanding = "C5.0 pembanding",
          baseline_A_semua_bukan = "Semua Bukan", baseline_B_kebijakan_berjalan = "Kebijakan berjalan")
  al <- c(cart_utama = "CART", cart_pembanding = "CART", c50_utama = "C5.0", c50_pembanding = "C5.0",
          baseline_A_semua_bukan = "Baseline", baseline_B_kebijakan_berjalan = "Kebijakan berjalan")
  d <- D$metrik_uji; d$lab <- factor(nm[d$model], levels = nm[d$model][order(-d$biaya_per_kab)])
  d$alg <- al[d$model]
  ggplot(d, aes(biaya_per_kab, lab, colour = alg)) +
    geom_segment(aes(x = 0, xend = biaya_per_kab, yend = lab), linewidth = 0.6, alpha = 0.5) +
    geom_point(size = 3.4) +
    geom_text(aes(label = fmt_angka(biaya_per_kab, 3)), hjust = -0.35, size = TXT_ANOT * 1.2,
              family = FONT, show.legend = FALSE) +
    scale_colour_manual(values = COL_MODEL_EDM, guide = "none") +
    scale_x_continuous(labels = lab_angka(1), limits = c(0, 0.88), expand = expansion(0)) +
    labs(x = "Biaya salah klasifikasi per kab/kota (3 × terlewat + 1 × salah tandai) / n", y = NULL) +
    theme_akademik(base_size = BASE_APP, grid = "x")
}

tabel_metrik <- function() {
  d <- D$metrik_uji
  nm <- c(cart_utama = "CART utama (+DK) — dashboard", cart_pembanding = "CART pembanding",
          c50_utama = "C5.0 utama (+DK)", c50_pembanding = "C5.0 pembanding",
          baseline_A_semua_bukan = "Baseline A: semua Bukan",
          baseline_B_kebijakan_berjalan = "Baseline B: kebijakan berjalan")
  data.frame(Model = nm[d$model], Terjaring = d$TP, Terlewat = d$FN, `Salah tandai` = d$FP,
             Biaya = fmt_angka(d$biaya_per_kab, 3), Sensitivitas = fmt_angka(d$sensitivitas, 2),
             Spesifisitas = fmt_angka(d$spesifisitas, 2),
             Presisi = ifelse(is.na(d$presisi), "–", fmt_angka(d$presisi, 2)),
             AUC = ifelse(is.na(d$auc), "–", fmt_angka(d$auc, 2)), check.names = FALSE)
}

# ------------------------------------------------------------------------------
# Antarmuka
# ------------------------------------------------------------------------------
font_app <- font_collection("Segoe UI", "Calibri", "Arial", "sans-serif")
tema <- bs_theme(version = 5, bg = COL[["latar"]], fg = COL[["hitam"]], primary = COL[["navy"]],
                 secondary = COL[["abu_tua"]], success = COL[["teal"]], danger = COL[["koral"]],
                 warning = COL[["oker"]], info = COL[["violet"]],
                 base_font = font_app, heading_font = font_app, "font-size-base" = "0.95rem")

sidebar_prioritas <- sidebar(
  width = 360, title = "Skenario anggaran",
  sliderInput("kuota", "Kuota kab/kota yang dapat dibiayai", min = 5, max = 300, value = 130, step = 5),
  radioButtons("dasar", "Dasar peringkat", choices = DASAR, selected = "model"),
  selectizeInput("prov", "Batasi provinsi (kosong = seluruh Indonesia)",
                 choices = sort(unique(K$prov_lab)), multiple = TRUE),
  checkboxInput("tanpa_dk", "Hanya kab/kota tanpa desa daerah khusus", FALSE),
  catatan("Kuota 130 = jumlah kab/kota Prioritas 2026 (HLS ≤ ", fmt_angka(D$batas_q1), " tahun). ",
          "Peringkat model memakai peluang prioritas dari pohon CART; nilai sama diurutkan menurut RLS terendah.")
)

hal_prioritas <- nav_panel(
  "Penentuan prioritas",
  layout_sidebar(
    sidebar = sidebar_prioritas, fillable = FALSE,
    layout_column_wrap(width = 1 / 3, fill = FALSE, heights_equal = "row",
                       uiOutput("vb_terpilih"), uiOutput("vb_terjaring"), uiOutput("vb_tanpa_dk")),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(6, 6)), fill = FALSE,
      card(card_header("Gains: berapa banyak kab/kota Prioritas terjaring untuk tiap kuota?"),
           plotOutput("g_gains", height = 400),
           card_footer(catatan("Garis titik-titik = tanpa peringkat (acak). Dihitung pada semua kab/kota terpilih ",
                               "(termasuk data latih), jadi sedikit optimistis; evaluasi resmi pada data uji ada di tab ",
                               "Evaluasi model (Larose & Larose, 2014, 14.8)."))),
      card(card_header("Sebaran kab/kota terpilih menurut aturan pohon"),
           tableOutput("t_aturan_terpilih"),
           card_footer(catatan("Klik tab Aturan & rekomendasi untuk program yang sesuai tiap aturan.")))
    ),
    card(card_header(class = "d-flex justify-content-between align-items-center",
                     "Daftar kab/kota terpilih",
                     downloadButton("unduh", "Unduh CSV", class = "btn-sm btn-outline-primary")),
         div(style = "max-height: 520px; overflow-y: auto;", tableOutput("t_terpilih")),
         card_footer(catatan("Status 2026 = status sebenarnya (HLS 2026 ≤ kuartil-1); dipakai untuk menilai skenario, ",
                             "bukan masukan model.")))
  )
)

kartu_aturan <- function(nd) {
  a <- D$aturan[D$aturan$node == nd, ]; r <- REKOM[[as.character(nd)]]
  di514 <- K[K$node == nd, ]
  prio <- a$kelas == "Prioritas"
  accordion_panel(
    title = tags$span(tags$span(class = paste("badge me-2", if (prio) "bg-danger" else "bg-success"),
                                if (prio) "Prioritas" else "Bukan"), NAMA_ATURAN[[as.character(nd)]]),
    value = paste0("a", nd),
    tags$p(tags$strong(sub("^JIKA ", "JIKA ", a$aturan)), " → ", tags$strong(a$kelas)),
    tags$p(class = "small", "Data latih: ", a$prioritas_latih, " dari ", a$n_latih, " Prioritas (",
           fmt_persen(a$confidence_latih, 0, 100), "). Data uji: ", a$prioritas_uji, " dari ", a$n_uji,
           " (", fmt_persen(a$confidence_uji, 0, 100), "). Semua 514: ", nrow(di514), " kab/kota, ",
           sum(di514$PRIORITAS == "Prioritas"), " Prioritas."),
    tags$p(r$makna),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(6, 6)), fill = FALSE,
      div(tags$h6(class = "text-uppercase small text-muted", "Rekomendasi kebijakan"),
          tags$ul(lapply(r$kebijakan, tags$li))),
      div(tags$h6(class = "text-uppercase small text-muted", "Implikasi pembelajaran"),
          tags$ul(lapply(r$pembelajaran, tags$li)))),
    catatan("Dasar: ", r$teori)
  )
}

hal_aturan <- nav_panel(
  "Aturan & rekomendasi",
  layout_columns(
    col_widths = breakpoints(sm = 12, lg = c(6, 6)), fill = FALSE,
    card(card_header("Pohon keputusan CART utama"),
         tags$img(src = "pohon_cart_utama.png", style = "width:100%; height:auto;",
                  alt = "Pohon keputusan CART utama dengan lima daun"),
         card_footer(catatan("Dilatih pada 360 kab/kota (70%), dipangkas dengan aturan 1-SE, biaya FN:FP = 3:1. ",
                             "Kotak koral = ditandai Prioritas."))),
    card(card_header("Lima aturan keputusan (Larose & Larose, 2014, 8.5)"),
         accordion(id = "akordeon", open = "a4", multiple = TRUE,
                   !!!lapply(D$aturan$node[order(match(D$aturan$node, c(4, 22, 10, 23, 3)))], kartu_aturan)),
         card_footer(catatan(KERANGKA_EDM)))
  )
)

hal_simulasi <- nav_panel(
  "Simulasi kab/kota",
  layout_sidebar(
    sidebar = sidebar(
      width = 360, title = "Pilih & ubah indikator",
      selectizeInput("kab", "Kab/kota", choices = sort(K$label), selected = K$label[which.max(K$prob_prioritas)]),
      actionButton("asli", "Kembalikan nilai asli", class = "btn-sm btn-outline-secondary mb-2"),
      numericInput("s_RLS", LABEL_PENDEK_EDM[["RLS"]], NA, step = 0.1),
      numericInput("s_TPT", LABEL_PENDEK_EDM[["TPT"]], NA, step = 0.1),
      numericInput("s_DK_PERSEN", LABEL_PENDEK_EDM[["DK_PERSEN"]], NA, min = 0, max = 100, step = 1),
      numericInput("s_PENGELUARAN", LABEL_PENDEK_EDM[["PENGELUARAN"]], NA, step = 100),
      numericInput("s_MISKIN", LABEL_PENDEK_EDM[["MISKIN"]], NA, step = 0.1),
      numericInput("s_UHH", LABEL_PENDEK_EDM[["UHH"]], NA, step = 0.1),
      radioButtons("s_KOTA", "Jenis wilayah", c(Kabupaten = 0, Kota = 1), inline = TRUE),
      catatan("Pohon hanya memakai RLS, TPT, dan desa daerah khusus untuk memutuskan; indikator lain ",
              "tidak mengubah hasil pohon ini tetapi ditampilkan sebagai konteks.")
    ),
    fillable = FALSE,
    layout_column_wrap(width = 1 / 2, fill = FALSE, heights_equal = "row",
                       uiOutput("vb_sim"), uiOutput("vb_sim_asli")),
    card(card_header("Aturan yang berlaku dan rekomendasinya"), uiOutput("sim_aturan"))
  )
)

hal_evaluasi <- nav_panel(
  "Evaluasi model",
  layout_columns(
    col_widths = breakpoints(sm = 12, lg = c(6, 6)), fill = FALSE,
    card(card_header("Biaya salah klasifikasi pada data uji (154 kab/kota, 39 Prioritas)"),
         plotOutput("g_biaya", height = 360),
         card_footer(catatan("Ukuran utama aturan pra-tetap (Larose & Larose, 2014, 14.6–14.7). ",
                             "Lebih kecil lebih baik."))),
    card(card_header("Temuan evaluasi (dilaporkan apa adanya)"),
         uiOutput("temuan_eval"))
  ),
  card(card_header("Metrik pada data uji"), tableOutput("t_metrik"),
       card_footer(catatan("C5.0 dengan matriks biaya tidak menghasilkan peluang kelas, sehingga AUC-nya tidak tersedia.")))
)

hal_brief <- nav_panel(
  "Policy brief",
  card(card_header(class = "d-flex justify-content-between align-items-center", "Ringkasan kebijakan",
                   downloadButton("unduh_brief", "Unduh (.txt)", class = "btn-sm btn-outline-primary")),
       uiOutput("brief"))
)

hal_tentang <- nav_panel(
  "Tentang",
  card(
    card_header("Tentang prototipe ini"),
    tags$p("Prototipe tugas akhir mata kuliah AI for Education Data Mining (CRISP-DM; Larose & Larose, 2014). ",
           "Tujuannya membantu perencana kebijakan menentukan kab/kota prioritas afirmasi akses pendidikan dan ",
           "memahami kondisi yang menyertainya."),
    tags$h6("Data"),
    tags$ul(
      tags$li("514 kab/kota. Target: Prioritas = harapan lama sekolah (HLS) 2026 ≤ kuartil-1 (",
              fmt_angka(D$batas_q1), " tahun), 130 kab/kota."),
      tags$li("Prediktor: RLS, pengeluaran per kapita disesuaikan (BPS 2026); TPT, penduduk miskin, umur harapan hidup ",
              "(BPS 2025); jenis wilayah; persentase desa/kelurahan daerah khusus (Kepmendikbudristek 160/P/2021 ÷ ",
              "Kepmendagri 146.1-4717/2020)."),
      tags$li("Aplikasi memuat tabel agregat kab/kota yang dipublikasikan terbuka; sumber lengkap di bawah.")),
    tags$h6("Sumber data"),
    tags$ul(class = "small", lapply(SUMBER_DATA_EDM, function(s)
      tags$li(s[[1]], " ", tags$a(href = s[[2]], target = "_blank", rel = "noopener", "Tautan")))),
    tags$h6("Metode"),
    tags$ul(
      tags$li("Aturan pemodelan ditetapkan sebelum model dijalankan: holdout 70/30 berstrata, CART (rpart) utama dan ",
              "C5.0 pembanding, pemangkasan 1-SE, biaya Prioritas terlewat : salah tandai = 3 : 1 (Elkan, 2001)."),
      tags$li("Dibandingkan dengan baseline semua Bukan dan kebijakan berjalan (kab/kota dengan ≥ 1 desa daerah khusus)."),
      tags$li("Validasi silang 10-fold × 10 sebagai pelengkap.")),
    tags$h6("Keterbatasan"),
    tags$ul(
      tags$li("Model membaca asosiasi, bukan sebab-akibat. RLS dan HLS sama-sama komponen IPM, jadi model lebih ",
              "berguna untuk memetakan profil kondisi dan menyaring prioritas daripada meramal jauh ke depan."),
      tags$li("Daftar daerah khusus berasal dari 2021 dan belum dievaluasi ulang; pembilang (daftar desa Podes) dan ",
              "penyebut (Kemendagri) tidak sepenuhnya satu semesta (mis. Teluk Bintuni dibatasi 100%)."),
      tags$li("Kab/kota dalam satu provinsi tidak sepenuhnya independen; data uji hanya 154 kab/kota."),
      tags$li("Peta tidak disertakan karena batas wilayah 514 kab/kota memerlukan berkas geospasial besar; sebaran ",
              "ditampilkan dalam tabel dan grafik.")),
    tags$h6("Pustaka"),
    tags$ul(class = "small", lapply(PUSTAKA_EDM, tags$li)),
    card_footer(catatan("Bahan aplikasi dibuat: ", D$dibuat, "."))
  )
)

ui <- page_navbar(
  title = "Prioritas Afirmasi Akses Pendidikan · Kab/Kota 2026",
  theme = tema, fillable = FALSE, lang = "id",
  hal_prioritas, hal_aturan, hal_simulasi, hal_evaluasi, hal_brief, hal_tentang
)

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  layak <- reactive({
    d <- K
    if (length(input$prov)) d <- d[d$prov_lab %in% input$prov, ]
    if (isTRUE(input$tanpa_dk)) d <- d[d$DK_JUMLAH == 0, ]
    d
  })
  terpilih <- reactive({
    d <- urutkan(layak(), input$dasar)
    d$peringkat <- seq_len(nrow(d))
    head(d, input$kuota)
  })

  output$vb_terpilih <- renderUI({
    s <- terpilih(); n <- nrow(layak())
    value_box(title = "Kab/kota dibiayai", value = fmt_bulat(nrow(s)),
              tags$p("dari ", fmt_bulat(n), " kab/kota yang memenuhi filter"),
              theme = value_box_theme(bg = tint(COL[["navy"]], 0.88), fg = COL[["hitam"]]))
  })
  output$vb_terjaring <- renderUI({
    s <- terpilih(); P <- sum(layak()$PRIORITAS == "Prioritas"); t <- sum(s$PRIORITAS == "Prioritas")
    value_box(title = "Prioritas 2026 terjaring", value = paste0(t, " / ", P),
              tags$p(if (P) paste0(fmt_persen(t / P, 0, 100), " Prioritas terjangkau; ",
                                   fmt_persen(if (nrow(s)) t / nrow(s) else 0, 0, 100), " kab/kota terpilih memang Prioritas")
                     else "Tidak ada kab/kota Prioritas pada filter ini"),
              theme = value_box_theme(bg = tint(COL[["koral"]]), fg = COL[["hitam"]]))
  })
  output$vb_tanpa_dk <- renderUI({
    s <- terpilih(); n0 <- sum(s$DK_JUMLAH == 0); p0 <- sum(s$DK_JUMLAH == 0 & s$PRIORITAS == "Prioritas")
    value_box(title = "Terpilih tanpa desa daerah khusus", value = fmt_bulat(n0),
              tags$p(p0, " di antaranya Prioritas: belum tersentuh afirmasi berbasis geografis"),
              theme = value_box_theme(bg = tint(COL[["oker"]]), fg = COL[["hitam"]]))
  })

  output$g_gains <- renderPlot({
    validate(need(nrow(layak()) > 0, "Tidak ada kab/kota yang memenuhi filter."))
    plot_gains(layak(), input$kuota)
  }, res = 96, alt = "Kurva gains: jumlah Prioritas terjaring menurut kuota untuk dua dasar peringkat")

  output$t_aturan_terpilih <- renderTable({
    s <- terpilih(); validate(need(nrow(s) > 0, "Belum ada kab/kota terpilih."))
    a <- aggregate(cbind(n = 1, prioritas = s$PRIORITAS == "Prioritas") ~ aturan_nama, data = s, FUN = sum)
    a <- a[order(-a$n), ]
    data.frame(Aturan = a$aturan_nama, `Kab/kota` = a$n, `Prioritas 2026` = a$prioritas, check.names = FALSE)
  }, striped = TRUE, spacing = "s", align = "lrr")

  tabel_terpilih <- reactive({
    s <- terpilih()
    data.frame(`#` = s$peringkat, Provinsi = s$prov_lab, `Kab/kota` = s$nama,
               `P model` = fmt_persen(s$prob_prioritas, 0, 100), Aturan = s$aturan_nama,
               `Desa DK (%)` = fmt_angka(s$DK_PERSEN, 1), `RLS` = fmt_angka(s$RLS, 2),
               `HLS 2026` = fmt_angka(s$HLS, 2), `Status 2026` = as.character(s$PRIORITAS),
               check.names = FALSE)
  })
  output$t_terpilih <- renderTable(tabel_terpilih(), striped = TRUE, spacing = "s", align = "rlllllrrl")
  output$unduh <- downloadHandler(
    filename = function() paste0("prioritas_afirmasi_", input$dasar, "_", input$kuota, ".csv"),
    content = function(file) {
      s <- terpilih()
      write.csv(data.frame(peringkat = s$peringkat, provinsi = s$prov_lab, kabkota = s$nama,
                           peluang_prioritas = round(s$prob_prioritas, 3), aturan = s$aturan_nama,
                           desa_daerah_khusus_persen = s$DK_PERSEN, RLS = s$RLS, HLS_2026 = s$HLS,
                           status_2026 = s$PRIORITAS), file, row.names = FALSE)
    })

  # Simulasi ---------------------------------------------------------------------
  asli <- reactive(K[K$label == input$kab, ][1, ])
  isi_asli <- function() {
    a <- asli()
    for (v in c("RLS", "TPT", "DK_PERSEN", "PENGELUARAN", "MISKIN", "UHH"))
      updateNumericInput(session, paste0("s_", v), value = a[[v]])
    updateRadioButtons(session, "s_KOTA", selected = a$KOTA)
  }
  observeEvent(input$kab, isi_asli())
  observeEvent(input$asli, isi_asli())
  sim <- reactive({
    a <- asli()
    b <- a[, PRED_EDM]
    for (v in c("RLS", "TPT", "DK_PERSEN", "PENGELUARAN", "MISKIN", "UHH")) {
      x <- input[[paste0("s_", v)]]; if (!is.null(x) && !is.na(x)) b[[v]] <- x
    }
    b$KOTA <- as.integer(input$s_KOTA)
    p <- predict(M, b, type = "prob")[, "Prioritas"]; k <- as.character(predict(M, b, type = "class"))
    nd <- as.integer(rownames(M$frame))[which(M$frame$var == "<leaf>" &
                                               abs(M$frame$yval2[, 4] - p) < 1e-12)][1]
    list(p = p, kelas = k, node = nd)
  })
  vb_kelas <- function(judul, p, kelas, ket) {
    prio <- kelas == "Prioritas"
    value_box(title = judul, value = kelas, tags$p("Peluang prioritas di daun: ", fmt_persen(p, 0, 100)),
              tags$p(class = "small mb-0", ket),
              theme = value_box_theme(bg = tint(if (prio) COL[["koral"]] else COL[["teal"]]), fg = COL[["hitam"]]))
  }
  output$vb_sim <- renderUI({
    s <- sim(); vb_kelas("Hasil pohon dengan nilai di panel kiri", s$p, s$kelas,
                         paste0("Ambang biaya 3:1 = 25%. ", NAMA_ATURAN[[as.character(s$node)]]))
  })
  output$vb_sim_asli <- renderUI({
    a <- asli()
    vb_kelas("Nilai asli kab/kota ini", a$prob_prioritas, a$prediksi,
             paste0("Status 2026 sebenarnya: ", a$PRIORITAS, " (HLS ", fmt_angka(a$HLS), " tahun)"))
  })
  output$sim_aturan <- renderUI({
    s <- sim(); a <- D$aturan[D$aturan$node == s$node, ]; r <- REKOM[[as.character(s$node)]]
    tagList(tags$p(tags$strong(a$aturan), " → ", tags$strong(a$kelas)), tags$p(r$makna),
            layout_columns(col_widths = breakpoints(sm = 12, lg = c(6, 6)), fill = FALSE,
                           div(tags$h6(class = "text-uppercase small text-muted", "Rekomendasi kebijakan"),
                               tags$ul(lapply(r$kebijakan, tags$li))),
                           div(tags$h6(class = "text-uppercase small text-muted", "Implikasi pembelajaran"),
                               tags$ul(lapply(r$pembelajaran, tags$li)))),
            catatan("Dasar: ", r$teori))
  })

  # Evaluasi & brief -------------------------------------------------------------
  output$g_biaya <- renderPlot(plot_biaya(), res = 96, alt = "Biaya salah klasifikasi tiap model pada data uji")
  output$t_metrik <- renderTable(tabel_metrik(), striped = TRUE, spacing = "s", align = "lrrrrrrrr")
  mu <- function(m, k) D$metrik_uji[D$metrik_uji$model == m, k]
  output$temuan_eval <- renderUI({
    cv <- D$cv; sel <- D$cv_selisih
    cvb <- function(m) fmt_angka(cv$biaya_per_kab_rata[cv$model == m], 3)
    tags$ul(
      tags$li("CART utama lebih murah daripada kebijakan berjalan (", fmt_angka(mu("cart_utama", "biaya_per_kab"), 3),
              " vs ", fmt_angka(mu("baseline_B_kebijakan_berjalan", "biaya_per_kab"), 3), ") dan menandai ",
              mu("cart_utama", "FP"), " kab/kota keliru dibanding ", mu("baseline_B_kebijakan_berjalan", "FP"), "."),
      tags$li("C5.0 lebih murah daripada CART (data uji ", fmt_angka(mu("c50_utama", "biaya_per_kab"), 3),
              "; CV ", cvb("c50_utama"), " vs ", cvb("cart_utama"), "), dengan sensitivitas tinggi tetapi banyak salah ",
              "tandai. CART tetap dipakai di dashboard sesuai aturan pra-tetap: aturannya ringkas dan dapat berjalan di peramban."),
      tags$li("Persentase desa daerah khusus tidak menambah daya prediksi: model tanpa DK sama atau sedikit lebih murah ",
              "(selisih CV CART +", fmt_angka(sel$selisih_biaya_rata[sel$algoritma == "cart"], 3), " ± ",
              fmt_angka(sel$selisih_biaya_se[sel$algoritma == "cart"], 3), "). Status daerah khusus sudah tercermin ",
              "pada kondisi sosial-pendidikan penduduk."),
      tags$li("Tanpa biaya asimetris (1:1 atau 2:1), pohon terpangkas menjadi satu simpul dan tidak menandai siapa pun; ",
              "analisis biaya karena itu menjadi bagian inti metode.")
    )
  })

  teks_brief <- reactive({
    sm <- K; P <- N_PRIO
    tanpa <- sum(sm$PRIORITAS == "Prioritas" & sm$DK_JUMLAH == 0)
    tandai_tanpa <- sum(sm$prediksi == "Prioritas" & sm$DK_JUMLAH == 0)
    benar_tanpa <- sum(sm$prediksi == "Prioritas" & sm$DK_JUMLAH == 0 & sm$PRIORITAS == "Prioritas")
    c(
      "RINGKASAN KEBIJAKAN — PRIORITAS AFIRMASI AKSES PENDIDIKAN KAB/KOTA 2026",
      "",
      "Masalah",
      paste0("- ", P, " dari 514 kab/kota (25,3%) memiliki HLS 2026 ≤ ", fmt_angka(D$batas_q1), " tahun. Mereka tersebar di ",
             D$eda[["n_provinsi_ada_prioritas"]], " dari 38 provinsi; ", D$eda[["n_prioritas_papua"]], " di Papua."),
      paste0("- ", tanpa, " kab/kota prioritas tidak punya satu pun desa daerah khusus, sehingga tidak tersentuh afirmasi ",
             "berbasis geografis (Kepmendikbudristek 160/P/2021)."),
      "",
      "Temuan",
      "- Rata-rata lama sekolah penduduk dewasa (RLS) adalah pembeda terkuat di semua model.",
      paste0("- Pohon CART menghasilkan 5 aturan; kab/kota dengan RLS < 8,1 tahun berpeluang prioritas ±56%. ",
             "Hambatan geografis (≥ 28% desa daerah khusus) berperan pada RLS 8,1–9,5 tahun."),
      paste0("- Dibanding sasaran kebijakan berjalan, model lebih murah dalam biaya salah sasaran (",
             fmt_angka(mu("cart_utama", "biaya_per_kab"), 3), " vs ", fmt_angka(mu("baseline_B_kebijakan_berjalan", "biaya_per_kab"), 3),
             " per kab/kota pada data uji) dan separuh lebih sedikit salah tandai."),
      paste0("- Model menandai ", tandai_tanpa, " kab/kota tanpa desa daerah khusus; ", benar_tanpa, " di antaranya memang ",
             "prioritas (dari ", tanpa, ")."),
      "",
      "Rekomendasi",
      "1. Gunakan indikator pendidikan penduduk dewasa (RLS), bukan hanya status geografis, untuk menyaring sasaran afirmasi.",
      "2. Untuk kab/kota RLS < 8,1 tahun: afirmasi berbasis keluarga (PIP/KIP) dan pendidikan kesetaraan orang dewasa.",
      "3. Untuk kab/kota dengan hambatan geografis: tunjangan guru daerah khusus, sekolah satu atap/berasrama, kelas rangkap.",
      "4. Mutakhirkan daftar daerah khusus 2021 sesuai kewajiban evaluasi dua tahunan.",
      paste0("5. Verifikasi lapangan untuk aturan yang kurang stabil (ekonomi perdesaan, TPT < 2,1%)."),
      "",
      "Keterbatasan: asosiasi, bukan sebab-akibat; data uji 154 kab/kota; daftar daerah khusus 2021.",
      paste0("Sumber: ", D$sumber)
    )
  })
  output$brief <- renderUI({
    t <- teks_brief()
    tagList(lapply(t, function(x) {
      if (!nzchar(x)) NULL
      else if (x == toupper(x) && nchar(x) > 20) tags$h5(x)
      else if (x %in% c("Masalah", "Temuan", "Rekomendasi")) tags$h6(class = "mt-3", x)
      else tags$p(class = "mb-1", x)
    }))
  })
  output$unduh_brief <- downloadHandler(
    filename = function() "policy_brief_prioritas_afirmasi_2026.txt",
    content = function(file) writeLines(teks_brief(), file, useBytes = TRUE))
}

shinyApp(ui, server)
