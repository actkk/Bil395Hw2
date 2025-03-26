#!/bin/bash


declare -a TEST_CASES=(
    "5+5"          # 10
    "10-4"         # 6
    "3*7"          # 21
    "8/2"          # 4
    "5+5*2"        # 15
    "10/2+3"       # 8
    "2*3+4*5"      # 26
    "5+5+5+5"      # 20
    "8/0"          # Hata (sıfıra bölme)
    "5+a"          # Hata (geçersiz karakter)
)

echo "=== Rust Hesap Makinesi Testleri ==="

if ! command -v rustc &> /dev/null; then
    echo "Rust derleyicisi kurulu değil."
    echo "Testler çalıştırılamadı."
    exit 1
fi

echo "Rust hesap makinesi derleniyor..."
rustc -o calculator calculator.rs || { echo "Rust derlemesi başarısız oldu!"; exit 1; }

for i in "${!TEST_CASES[@]}"; do
    test_case="${TEST_CASES[$i]}"
    
    echo "Test $((i+1)): '${test_case}'"
    echo -e "${test_case}\nexit" | ./calculator
    echo ""
done

echo -e "\nDerlenen dosyalar temizleniyor..."
rm -f calculator 