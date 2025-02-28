#!/bin/bash

echo "Iniciando limpieza completa de EAF y sus dependencias..."

# 1. Desinstalar paquetes APT
sudo apt remove -y \
    fd-find \
    libtag1-dev \
    python3-backcall \
    python3-dateutil \
    python3-decorator \
    python3-parso \
    python3-jedi \
    python3-pickleshare \
    python3-wcwidth \
    python3-prompt-toolkit \
    python3-ipython-genutils \
    python3-traitlets \
    python3-ipython \
    python3-jupyter-core \
    python3-tornado \
    python3-zmq \
    python3-jupyter-client \
    python3-ipykernel \
    python3-markdown \
    python3-pyqt5.qtsvg \
    python3-qrcode \
    python3-qtconsole \
    python3-webob \
    python3-cssselect \
    python3-pyquery

# 2. Desinstalar paquetes Python
pip3 uninstall -y \
    pysocks packaging pymupdf mutagen pytaglib certifi \
    pycryptodome rsa Pillow requests flask pyquery \
    feedparser retrying markdown pygit2 charset-normalizer \
    pygments giturlparse unidiff pypinyin exif psutil \
    pycurl python-tsp pyte epc sexpdata PyQt5-sip

# 3. Eliminar directorios de EAF
rm -rf ~/.emacs.d/site-lisp/emacs-application-framework
rm -rf ~/.emacs.d/eaf

# 4. Limpiar NPM global
sudo npm uninstall -g album-art

# 5. Limpiar cachés
npm cache clean --force
rm -rf ~/.npm/_logs/*
rm -rf ~/.cache/pip

# 6. Auto-remove para limpiar dependencias huérfanas
sudo apt autoremove -y

echo "Limpieza completa finalizada."