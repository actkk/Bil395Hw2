FROM ubuntu:22.04

# Gerekli paketleri kur
RUN apt-get update && apt-get install -y \
    build-essential \
    gnat \
    perl \
    cpanminus \
    swi-prolog \
    guile-3.0 \
    curl \
    locales \
    && rm -rf /var/lib/apt/lists/*

# Perl Error modülünü kur
RUN cpanm Error

# Karakter kodlaması ayarla
RUN locale-gen tr_TR.UTF-8
ENV LANG=tr_TR.UTF-8
ENV LC_ALL=tr_TR.UTF-8

# Rust kurulumu
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

# Çalışma dizini oluştur
WORKDIR /app

# Tüm kaynak dosyalarını kopyala
COPY . .

# Her dil için klasörleri oluştur (zaten varsa sorun değil)
RUN mkdir -p rust prolog ada scheme perl

# Tüm test scriptlerini çalıştırılabilir yap
RUN chmod +x run_all_tests.sh
RUN chmod +x rust/test_rust.sh
RUN chmod +x prolog/test_prolog.sh
RUN chmod +x ada/test_ada.sh
RUN chmod +x scheme/test_scheme.sh
RUN chmod +x perl/test_perl.sh

# run_all_tests.sh çalıştır
CMD ["./run_all_tests.sh"] 