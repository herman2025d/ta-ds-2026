# ==============================================================================
# Teks dashboard EDM: rekomendasi kebijakan & implikasi pembelajaran per aturan
# pohon CART utama (nomor daun = node rpart; diverifikasi di 04_siapkan_shiny_EDM.R).
# Ditulis langsung di folder aplikasi (bukan salinan).
# ==============================================================================

NAMA_ATURAN <- c(
  "4"  = "A1 · Pendidikan penduduk dewasa sangat rendah",
  "10" = "A2 · Pendidikan menengah-rendah di ekonomi perdesaan",
  "22" = "A3 · Pendidikan menengah-rendah dengan hambatan geografis",
  "23" = "P1 · Pantau: pendidikan menengah-rendah tanpa hambatan geografis",
  "3"  = "B1 · Pendidikan penduduk dewasa memadai"
)

REKOM <- list(
  "4" = list(
    makna = paste("Rata-rata lama sekolah penduduk dewasa di bawah 8,1 tahun (belum tamat SMP). Lebih dari separuh",
                  "kab/kota dalam kelompok ini memang berstatus prioritas."),
    kebijakan = c(
      "Afirmasi menyeluruh berbasis keluarga: perluas sasaran PIP/KIP untuk anak dari orang tua berpendidikan rendah.",
      "Pendidikan kesetaraan dan keaksaraan orang dewasa (Paket A/B/C di PKBM) untuk memutus pewarisan pendidikan rendah antargenerasi.",
      "Gerakan kembali bersekolah dan pendataan anak tidak sekolah per desa sebagai bagian Wajib Belajar 12 tahun."),
    pembelajaran = c(
      "Program literasi keluarga dan pendampingan belajar di rumah (guru kunjung, relawan) karena dukungan akademik orang tua terbatas.",
      "Pelibatan orang tua dengan cara sederhana (kunjungan rumah, pertemuan kelas) yang tidak menuntut kemampuan akademik orang tua."),
    teori = "Becker (1964); Haveman & Wolfe (1995); Bronfenbrenner (1979): keluarga sebagai mikrosistem; Epstein (2001)."),
  "10" = list(
    makna = paste("RLS 8,1–9,5 tahun dengan pengangguran terbuka sangat rendah (< 2,1%), ciri ekonomi perdesaan dan sektor",
                  "informal tempat anak mudah ikut bekerja. Aturan ini kurang stabil: di data uji hanya 2 dari 8 kab/kota yang prioritas."),
    kebijakan = c(
      "Bantuan pendidikan bersyarat (kehadiran) untuk menekan biaya kesempatan bersekolah pada keluarga petani/nelayan.",
      "SMK dan program vokasi yang sesuai potensi ekonomi lokal agar bersekolah lebih lama dipandang bermanfaat.",
      "Verifikasi lapangan sebelum penetapan karena aturan ini didukung sedikit kab/kota."),
    pembelajaran = c(
      "Pembelajaran kontekstual berbasis potensi lokal (pertanian, perikanan) yang mengaitkan materi dengan pekerjaan keluarga.",
      "Jadwal dan penilaian yang luwes pada musim panen/melaut agar siswa tidak putus sekolah."),
    teori = "Becker (1964): biaya kesempatan; Basu & Van (1998): pekerja anak; Fiszbein & Schady (2009): bantuan tunai bersyarat."),
  "22" = list(
    makna = paste("RLS 8,1–9,5 tahun dan ≥ 28% desa/kelurahan berstatus daerah khusus (terpencil, perbatasan, pulau kecil).",
                  "Hambatan geografis menambah risiko pada pendidikan penduduk yang sudah menengah-rendah."),
    kebijakan = c(
      "Afirmasi geografis: tunjangan khusus guru di daerah khusus, sekolah satu atap atau berasrama, dan bantuan transportasi.",
      "Afirmasi pendidikan menengah (mis. ADEM) dan pemerataan SMA/SMK/MA di kecamatan tanpa sekolah menengah.",
      "Mutakhirkan daftar daerah khusus (Kepmen 160/P/2021 belum dievaluasi lagi walau wajib dievaluasi 2 tahun sekali)."),
    pembelajaran = c(
      "Pembelajaran kelas rangkap (multigrade) dan modul luring untuk sekolah kecil dengan guru terbatas.",
      "Pembelajaran jarak jauh hibrida bila sinyal tersedia, dengan guru kunjung untuk desa tanpa sinyal."),
    teori = "Bronfenbrenner (1979): eksosistem/akses; Little (2001): kelas rangkap; Permendikbudristek 14/2021."),
  "23" = list(
    makna = paste("RLS 8,1–9,5 tahun tanpa hambatan geografis berarti. Model tidak menandainya (peluang prioritas ±16%,",
                  "di bawah ambang 25%), tetapi sebagian kecil tetap prioritas sehingga perlu dipantau."),
    kebijakan = c(
      "Daftar pantau: tinjau tahunan, terutama kab/kota yang HLS-nya dekat batas kuartil-1.",
      "Prioritaskan program reguler (BOS, PIP reguler) dan perbaikan mutu, bukan afirmasi khusus."),
    pembelajaran = c(
      "Penguatan transisi SMP–SMA (bimbingan karier, informasi beasiswa) agar lama sekolah tidak berhenti di jenjang SMP."),
    teori = "Bowers, Sprott & Taff (2013): sistem peringatan dini membutuhkan pemantauan berkala."),
  "3" = list(
    makna = "RLS ≥ 9,5 tahun. Hampir tidak ada kab/kota prioritas di kelompok ini (±2%).",
    kebijakan = c(
      "Tidak memerlukan afirmasi tingkat kab/kota; arahkan sasaran ke kantong kemiskinan di dalam wilayah (desa/kelurahan, keluarga)."),
    pembelajaran = c("Fokus pada mutu dan pemerataan di dalam kab/kota (sekolah dengan capaian rendah)."),
    teori = "Larose & Larose (2014) 6.7: dibandingkan dengan baseline nasional 25,3%.")
)

KERANGKA_EDM <- paste(
  "Rekomendasi disusun per aturan pohon keputusan (Larose & Larose, 2014, 8.5) dengan kerangka modal manusia",
  "(Becker, 1964) dan ekologi (Bronfenbrenner, 1979): kondisi keluarga, ekonomi lokal, dan akses wilayah.")

# Sumber data lengkap (ketentuan BPS: judul, tanggal akses, tautan ke konten asli)
SUMBER_DATA_EDM <- list(
  list("BPS. [Metode Baru] Harapan Lama Sekolah (Tahun), 2026. Diakses 6 November 2026.",
       "https://www.bps.go.id/id/statistics-table/2/NDE3IzI=/-metode-baru--harapan-lama-sekolah--tahun-.html"),
  list("BPS. [Metode Baru] Rata-rata Lama Sekolah (Tahun), 2026. Diakses 6 November 2026.",
       "https://www.bps.go.id/id/statistics-table/2/NDE1IzI=/-metode-baru--rata-rata-lama-sekolah--tahun-.html"),
  list("BPS. [Metode Baru] Pengeluaran per Kapita Disesuaikan (Ribu Rupiah/Orang/Tahun), 2026. Diakses 6 November 2026.",
       "https://www.bps.go.id/id/statistics-table/2/NDE2IzI=/-metode-baru--pengeluaran-per-kapita-disesuaikan--ribu-rupiah-orang-tahun-.html"),
  list("BPS. Tingkat Pengangguran Terbuka menurut Jenis Kelamin dan Kabupaten/Kota, 2025. Diakses 6 November 2026.",
       "https://www.bps.go.id/id/statistics-table/2/MjgyNCMy/tingkat-pengangguran-terbuka-menurut-jenis-kelamin-dan-kabupaten-kota-.html"),
  list("BPS. Persentase Penduduk Miskin (P0) Menurut Kabupaten/Kota (Persen), 2025. Diakses 6 November 2026.",
       "https://www.bps.go.id/id/statistics-table/2/NjIxIzI=/persentase-penduduk-miskin--p0--menurut-kabupaten-kota--persen-.html"),
  list("BPS. Umur Harapan Hidup saat Lahir menurut Jenis Kelamin (UHH hasil SP2020 LF), 2025. Diakses 6 November 2026.",
       "https://www.bps.go.id/id/statistics-table/2/MjI3MCMy/umur-harapan-hidup-saat-lahir-menurut-jenis-kelamin--menggunakan-uhh-hasil-sp2020-lf-.html"),
  list("Kepmendikbudristek Nomor 160/P/2021 tentang Daerah Khusus Berdasarkan Kondisi Geografis (JDIH Kemendikbud). Diakses 7 November 2026.",
       "https://jdih.kemdikbud.go.id/detail_peraturan?main=2413"),
  list("Kode dan data wilayah administrasi 2020 (Permendagri 72/2019 jo. Kepmendagri 146.1-4717/2020), turunan cahyadsn/wilayah, lisensi MIT. Diakses 8 November 2026.",
       "https://github.com/cahyadsn/wilayah")
)

PUSTAKA_EDM <- c(
  "Basu, K., & Van, P. H. (1998). The economics of child labor. American Economic Review, 88(3), 412–427.",
  "Becker, G. S. (1964). Human capital. University of Chicago Press.",
  "Bowers, A. J., Sprott, R., & Taff, S. A. (2013). Do we know who will drop out? The High School Journal, 96(2), 77–100.",
  "Breiman, L., Friedman, J., Olshen, R., & Stone, C. (1984). Classification and regression trees. Wadsworth.",
  "Bronfenbrenner, U. (1979). The ecology of human development. Harvard University Press.",
  "Elkan, C. (2001). The foundations of cost-sensitive learning. Proceedings of IJCAI-01, 973–978.",
  "Epstein, J. L. (2001). School, family, and community partnerships. Westview Press.",
  "Fiszbein, A., & Schady, N. (2009). Conditional cash transfers: Reducing present and future poverty. World Bank.",
  "Haveman, R., & Wolfe, B. (1995). The determinants of children's attainments. Journal of Economic Literature, 33(4), 1829–1878.",
  "Larose, D. T., & Larose, C. D. (2014). Discovering knowledge in data (2nd ed.). Wiley.",
  "Little, A. W. (2001). Multigrade teaching. International Journal of Educational Development, 21(6), 481–497.",
  "Quinlan, J. R. (1993). C4.5: Programs for machine learning. Morgan Kaufmann."
)
