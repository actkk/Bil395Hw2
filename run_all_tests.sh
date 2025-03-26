#!/bin/bash



echo "=== Tüm Diller İçin Hesap Makinesi Testleri ==="
echo "Bu script, her bir programlama dili için ayrı test scriptini çalıştırır."

languages=("rust" "prolog" "ada" "scheme" "perl")

for lang in "${languages[@]}"; do
    echo -e "\n=== $lang için testler çalıştırılıyor ==="
    
    if [ -f "$lang/test_${lang}.sh" ]; then
        cd $lang
        ./test_${lang}.sh
        cd ..
    else
        echo "$lang için test scripti bulunamadı."
    fi
done

echo -e "\n=== Testler tamamlandı ===" 