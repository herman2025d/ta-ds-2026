# ==============================================================================
# VDP - Label variabel & glosarium bahasa awam (dipakai skrip grafik dan aplikasi Shiny)
# Keputusan pengguna 16 Nov: istilah tidak awam diganti atau diberi keterangan ringkas.
# ==============================================================================

# Label pendek (sumbu/legenda) dan kalimat awam (judul/tooltip) per variabel
LABEL_VDP <- data.frame(
  var = c("APS_07_12", "APS_13_15", "APS_16_18", "APS_19_23",
          "APM_SD", "APM_SMP", "APM_SM",
          "APK_SD", "APK_SMP", "APK_SM",
          "NET_KOTA", "NET_DESA", "NET_TOTAL"),
  pendek = c("Usia 7–12", "Usia 13–15", "Usia 16–18", "Usia 19–23",
             "SD tepat usia", "SMP tepat usia", "SMA/SMK tepat usia",
             "Murid SD", "Murid SMP", "Murid SMA/SMK",
             "Kota", "Desa", "Kota + desa"),
  awam = c("Anak usia 7–12 tahun yang masih bersekolah",
           "Anak usia 13–15 tahun yang masih bersekolah",
           "Remaja usia 16–18 tahun yang masih bersekolah",
           "Pemuda usia 19–23 tahun yang masih bersekolah/kuliah",
           "Anak usia SD (7–12) yang duduk di SD",
           "Anak usia SMP (13–15) yang duduk di SMP",
           "Remaja usia SMA (16–18) yang duduk di SMA/SMK",
           "Jumlah murid SD dibanding jumlah anak usia SD",
           "Jumlah murid SMP dibanding jumlah anak usia SMP",
           "Jumlah murid SMA/SMK dibanding jumlah remaja usia SMA",
           "Penduduk kota yang memakai internet (3 bulan terakhir)",
           "Penduduk desa yang memakai internet (3 bulan terakhir)",
           "Penduduk yang memakai internet (3 bulan terakhir)"),
  istilah = c(rep("APS", 4), rep("APM", 3), rep("APK", 3), rep("Internet", 3)),
  stringsAsFactors = FALSE
)
lab_pendek <- function(v) LABEL_VDP$pendek[match(v, LABEL_VDP$var)]
lab_awam   <- function(v) LABEL_VDP$awam[match(v, LABEL_VDP$var)]

# Glosarium: istilah -> penjelasan satu-dua kalimat untuk orang awam
GLOSARIUM_VDP <- data.frame(
  istilah = c("APS (Angka Partisipasi Sekolah)",
              "APM (Angka Partisipasi Murni)",
              "APK (Angka Partisipasi Kasar)",
              "Perkotaan / perdesaan",
              "Akses internet",
              "Provinsi pemekaran 2022",
              "Rata-rata provinsi",
              "Jarak antarprovinsi",
              "Mengejar ketertinggalan",
              "Poin persen"),
  arti = c(
    "Dari setiap 100 anak pada kelompok umur tertentu, berapa yang masih bersekolah (di jenjang apa pun).",
    "Dari setiap 100 anak usia sekolah suatu jenjang, berapa yang bersekolah di jenjang yang sesuai umurnya (mis. anak 13–15 tahun di SMP).",
    "Jumlah seluruh murid suatu jenjang dibanding jumlah anak seusia jenjang itu. Bisa lebih dari 100% karena ada murid yang lebih muda atau lebih tua dari usia normal.",
    "Pembagian wilayah oleh BPS menurut ciri desa/kelurahan (kepadatan, fasilitas). DKI Jakarta seluruhnya perkotaan, jadi tidak punya angka perdesaan.",
    "Persentase penduduk usia 5 tahun ke atas yang pernah memakai internet dalam 3 bulan terakhir.",
    "Pada 2022 Papua dimekarkan menjadi Papua, Papua Selatan, Papua Tengah, Papua Pegunungan, dan Papua Barat Daya. Angka provinsi baru tersedia mulai 2024; angka Papua dan Papua Barat sebelum itu mencakup wilayah lama yang lebih luas, jadi tidak bisa langsung dibandingkan.",
    "Setiap provinsi dihitung sama bobotnya, berapa pun jumlah penduduknya. Berbeda dengan angka Indonesia dari BPS yang memperhitungkan jumlah penduduk.",
    "Seberapa jauh angka provinsi-provinsi tersebar dari rata-ratanya (secara teknis: simpangan baku). Makin kecil, makin merata.",
    "Apakah provinsi yang dulu tertinggal naik lebih cepat daripada provinsi yang sudah tinggi (secara teknis: konvergensi beta).",
    "Selisih antara dua persentase. Contoh: dari 70% naik ke 78% = naik 8 poin persen."
  ),
  stringsAsFactors = FALSE
)

# Kontras WCAG (audit desain 17 Nov): rasio kontras dua warna, dan versi warna yang digelapkan
# (dicampur dengan COL hitam) sampai mencapai rasio minimum terhadap latar putih.
# Teks berwarna (label langsung) >= 4,5:1 (WCAG 1.4.3); garis/titik data >= 3:1 (WCAG 1.4.11).
rasio_kontras <- function(a, b = "#FFFFFF") {
  lum <- function(h) { r <- grDevices::col2rgb(h) / 255
    l <- ifelse(r <= 0.03928, r / 12.92, ((r + 0.055) / 1.055)^2.4)
    0.2126 * l[1, ] + 0.7152 * l[2, ] + 0.0722 * l[3, ] }
  la <- lum(a); lb <- lum(b); (pmax(la, lb) + 0.05) / (pmin(la, lb) + 0.05)
}
warna_teks <- function(col, min_rasio = 4.5) {
  vapply(unname(col), function(x) {
    for (t in seq(0, 1, by = 0.02)) {
      y <- grDevices::colorRampPalette(c(x, COL[["hitam"]]))(101)[round(t * 100) + 1]
      if (rasio_kontras(y) >= min_rasio) return(y)
    }
    COL[["hitam"]]
  }, "", USE.NAMES = FALSE)
}

# Warna semantik VDP (butuh COL dari 00_visual_system.R -> source file itu lebih dulu)
# Usia 16-18 = fokus cerita (navy, garis tebal); kota/desa; Papua = sorotan (koral = perhatian)
COL_USIA <- c("Usia 7–12" = COL[["abu_tua"]], "Usia 13–15" = COL[["teal"]],
              "Usia 16–18" = COL[["navy"]], "Usia 19–23" = COL[["violet"]])
# Desa: oker asli hanya 2,7:1 terhadap putih -> digelapkan sampai 3:1 agar garisnya tetap terlihat
COL_DAERAH <- c("Kota" = COL[["navy"]], "Desa" = warna_teks(COL[["oker"]], 3))
BENTUK_PAPUA <- c("6 provinsi Papua" = 17, "Provinsi lain" = 16)   # segitiga vs bulat: tidak hanya warna
COL_PAPUA  <- c("6 provinsi Papua" = COL[["koral"]], "Provinsi lain" = COL[["teal"]])
COL_SEKOLAH <- c("Bersekolah" = COL[["navy"]], "Tidak bersekolah" = COL[["koral"]])
BASE_PUBLIK <- 11          # base_size tema untuk publik awam (laporan akademik: 9,5)

SUMBER_VDP <- paste0("BPS, tabel APS, APK, APM menurut provinsi 2015–2025 dan akses internet menurut ",
                     "provinsi & klasifikasi daerah 2015–2024 (diakses 6 Nov 2026)")
SUMBER_PETA <- "Batas provinsi: denyherianto/indonesia-geojson-topojson-maps-with-38-provinces (CC BY 4.0)"
