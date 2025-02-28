# Crear el directorio de fuentes si no existe
mkdir -p ~/.local/share/fonts

# Descargar e instalar Source Code Pro
cd ~/.local/share/fonts
wget https://github.com/adobe-fonts/source-code-pro/releases/download/2.038R-ro%2F1.058R-it%2F1.018R-VAR/TTF-source-code-pro-2.038R-ro-1.058R-it.zip
unzip TTF-source-code-pro-2.038R-ro-1.058R-it.zip
rm TTF-source-code-pro-2.038R-ro-1.058R-it.zip

# Actualizar la caché de fuentes
fc-cache -f -v