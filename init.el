;;; -*- lexical-binding: t -*-

;; Optimizaciones iniciales para mejor rendimiento y funcionamiento
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
(setq create-lockfiles nil)

;; Configuración básica
(setq inhibit-startup-message t)
(scroll-bar-mode 1)
(tool-bar-mode 1)
(menu-bar-mode 1)
(column-number-mode 1)
;; Line numbers desactivados por defecto
(global-display-line-numbers-mode -1)

;; (setq default-frame-alist '((toolkit . gtk)))

;; Configuración de paquetes
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; Instalar use-package si no está instalado
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Vertico para completado vertical
(use-package vertico
  :init
  (vertico-mode))

;; Marginalia para anotaciones en el minibuffer
(use-package marginalia
  :init
  (marginalia-mode))

;; Corfu para completado en el buffer
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  :init
  (global-corfu-mode))

;; Consult para búsqueda mejorada
(use-package consult
  :bind
  (("C-S" . consult-line) ;; Buscar en la línea actual 
   ("C-R" . consult-ripgrep) ;; Buscar en archivos
   ("M-y" . consult-yank-pop) ;; Historial del portapapeles
   ("C-x b" . consult-buffer)
   :map minibuffer-local-map
   ("C-H m" . consult-history))) ;; Historial del minibuffer

;; Embark para acciones contextuales
(use-package embark
  :bind
  (("C-." . embark-act) ;; Acción principal
   ("C-;" . embark-dwim) ;; Acción Haz lo que yo quiero
   ("C-h B" . embark-bindings))) ;; Mostrar combinaciones de teclas

;; Embark-consult integración
  (use-package embark-consult
    :after (embark consult))

;; Company para autocompletado
(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 1))

;; Hyperbole
(use-package hyperbole
  :ensure t
  :config
  (hyperbole-mode 1)

  ;; Configurar HyWiki dentro de tu directorio hyperb existente
  (setq hywiki-directory (expand-file-name "wiki" hbmap:dir-user))
	;; Asegurar que existe el directorio
	(unless (file-exists-p hywiki-directory)
	  (make-directory hywiki-directory t))
	;; Activar HyWiki globalmente
	(hywiki-mode 1)

  
  (setq hyperbole-contact-notebook-mode 'hyrolo)
  (setq hsys-org-enable-smart-keys t)
  (setq hyrolo-date-format "%Y-%m-%d %H:%M:%S")
  )


;; Soporte para Lisp (SLY)
(use-package sly
  :custom
  (inferior-lisp-program "sbcl")
  :config
  (define-key sly-mode-map (kbd "C-c C-c") 'sly-eval-defun)
  (define-key sly-mode-map (kbd "C-c C-k") 'sly-compile-and-load-file))

;; Soporte para Scheme (Geiser)
(use-package geiser
  :ensure t
  :config
  (setq geiser-active-implementations '(racket)))

(use-package geiser-racket
  :ensure t
  :after geiser)

;; Projectile para gestión de proyectos
(use-package projectile
  :init
  (projectile-mode +1)
  :bind
  (:map projectile-mode-map
        ("C-c p" . projectile-command-map)))

;; Magit para control de versiones
(use-package magit
  :bind
  ("C-x g" . magit-status))

;; vterm para emulación de terminal
(use-package vterm
  :load-path "~/.emacs.d/site-lisp/emacs-libvterm"
  :config
  (setq vterm-shell "/bin/bash")
  (setq vterm-max-scrollback 10000)
  ;; Keybindings actualizados
  :bind (("C-c v" . vterm)
         ("C-c C-v" . vterm-other-window)))

;; Portapapeles xclip
(use-package xclip
  :config
  (xclip-mode 1)
  (setq xclip-program "xclip")
  (setq xclip-select-enable-clipboard t))

;;; Navegador EAF con Configuración para Cinnamon

;(setenv "PYTHONPATH" 
;        (concat 
;         (expand-file-name "~/.emacs.d/site-lisp/emacs-application-framework/python-epc")
;         ":"
;         (getenv "PYTHONPATH")))


    ;; Deshabilitar notificaciones de sistema y configuración específica para Cinnamon
;; Configuración más detallada para EAF
;(setq eaf-browser-enable-adblocker t)
;(setq eaf-browser-enable-autofill t)
;(setq eaf-browser-default-zoom 1.0)
;(setq eaf-browser-dark-mode nil)
;(setq eaf-browser-enable-notify nil)
;(setq eaf-python-command "python3")
;(setq eaf-wm-name "Cinnamon")
;(setq eaf-browser-enable-plugin t)
;(setq eaf-enable-debug t)
;(setq eaf-browser-qwebengine-args '("--disable-web-security"
;                                   "--no-sandbox"
;                                   "--disable-features=VizDisplayCompositor"))

;; Específico para Qt6
;(setq eaf-qt-args '("-platform" "xcb"))

;(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
;(require 'eaf)
;(require 'eaf-browser)

     ;; Opcional: Hacer EAF el navegador por defecto
;(setq browse-url-browser-function 'eaf-open-browser)







;; Tema moderno y ligero
(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil))

;; Fuente
(set-face-attribute 'default nil
                    :family "SauceCodePro Nerd Font Mono"   ;; "JetBrains Mono NL Nerd Font Regular"
                    :height 110
                    :weight 'normal
                    :width 'normal)

;; Rainbow-delimiters para colorear paréntesis
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Smartparens para manejo inteligente de paréntesis
(use-package smartparens
  :ensure t
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  ;; Configuración específica para Lisp
  (sp-use-paredit-bindings)
  ;; Resaltar pareja de paréntesis
  (setq sp-show-pair-from-inside t)
  ;; No crear pares de comillas simples en modo Lisp
  (sp-local-pair '(emacs-lisp-mode lisp-mode scheme-mode) "'" nil :actions nil))

(use-package) indium 			;; Recomendación de DeepSeek-R1
:ensure t

(use-package lispy		;; Recomendación de DeepSeek-R1 (para revisar paréntesis y cosas así)
  :ensure t
  :hook (emacs-lisp-mode . lispy-mode))

;; Hooks personalizados para modos Lisp
(defun my-lisp-mode-setup ()
  (show-paren-mode 1)
  (smartparens-strict-mode 1)
  (company-mode 1)
  (display-line-numbers-mode 1)
  (rainbow-delimiters-mode 1))

(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-setup)
(add-hook 'lisp-mode-hook 'my-lisp-mode-setup)
(add-hook 'scheme-mode-hook 'my-lisp-mode-setup)
(add-hook 'sly-mrepl-mode-hook 'my-lisp-mode-setup)
(add-hook 'geiser-repl-mode-hook 'my-lisp-mode-setup)

;; Configuración de EAT (cargar al final)
(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-eat")
(when (require 'eat nil t)
  (setq eat-enable-mouse t)
  (setq eat-enable-alternative-display t)
  (add-hook 'eshell-mode-hook 'eat-eshell-enable)
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (global-set-key (kbd "C-c t") #'eat))

;;(add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-eat")
;;(require 'eat nil t)

;; (when (fboundp 'global-eat-eshell-mode)
;;  (setq eat-enable-mouse t)
;;  (setq eat-enable-alternative-display t)
;;  (global-eat-eshell-mode 1)
;;  (add-hook 'eshell-mode-hook 'eat-eshell-enable)
;;  (add-hook 'eshell-load-hook #'eat-eshell-mode)
;;  (define-key eshell-mode-map (kbd "C-c C-j") 'eat-semi-char-mode)
;;  (define-key eshell-mode-map (kbd "C-c C-e") 'eat-emacs-mode)
;;  (define-key eshell-mode-map (kbd "C-c M-d") 'eat-char-mode)
;;  (define-key eshell-mode-map (kbd "C-c C-k") 'eat-kill-process)
;;  (global-set-key (kbd "C-c t") #'eat))

;; (use-package eaf
;;  :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
;;  :custom
  ; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;;  (eaf-browser-continue-where-left-off t)
;;  (eaf-browser-enable-adblocker t)
;;  (browse-url-browser-function 'eaf-open-browser)
;;  :config
;;  (defalias 'browse-web #'eaf-open-browser)
;;  (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;  (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;  (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;;  (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki

;;; ** PARA HACER LOS BACKUPS - Configuración de Git y Magit **

;;; 1. Asegúrate de tener Git instalado en tu sistema Linux Mint.
;;;    En la terminal, puedes verificar con:  `git --version`
;;;    Si no está instalado, puedes instalarlo con: `sudo apt update && sudo apt install git`

;;; 2. Instala Magit y Transient en Emacs (si no los tienes ya).
;;;    Emacs 30.0.50 ya debería tenerlos, pero si no, usa M-x package-install RET magit RET y M-x package-install RET transient RET

;;; 3. Configuración básica de Magit (opcional, pero recomendado)
(use-package magit
  :commands (magit-status magit-clone) ; Comandos que queremos cargar al inicio
  :config
  ;;; Atajos de teclado Emacs-style para Magit (recomendado si no usas Evil Mode)
  (global-set-key (kbd "C-c g s") 'magit-status) ; Atajo Emacs-style: Ctrl+c seguido de g, luego s para magit-status
  (global-set-key (kbd "C-c g c") 'magit-clone)  ; Atajo Emacs-style: Ctrl+c seguido de g, luego c para magit-clone

  ;;; Atajos de teclado Vim-style para Magit (opcional, si usas Evil Mode)
  ;; (evil-define-key 'normal 'global "gs" 'magit-status) ; Atajo Vim-style para magit-status (comentado porque no usas Evil Mode ahora)
  ;; (evil-define-key 'normal 'magit-mode-map "q" 'magit-mode-bury-buffer) ; Atajo Vim-style para cerrar buffers de Magit (comentado)

  ;;; Personalizaciones adicionales de Magit (opcional, puedes investigar más adelante)
  )

;;; 4. Configuración básica de Transient (ya debería venir con Magit, pero por si acaso)
(use-package transient
  :ensure nil ; Transient suele venir con Magit, así que no es necesario instalarlo aparte
  ;;; Puedes agregar configuraciones adicionales de Transient aquí si lo deseas
  )

;;; ** Función para Backupear la Configuración a GitHub (my/update-config) - Versión FINAL (DeepSeek-R1)**


(defun my/update-config ()
  "Actualiza la configuración de Emacs en GitHub usando Magit."
  (interactive)
  (let ((emacs-config-dir (or (and (boundp 'user-emacs-directory) user-emacs-directory)
                              (expand-file-name "~/.emacs.d/"))))
    (unless emacs-config-dir
      (error "No se pudo determinar el directorio de configuración de Emacs."))
    (unless (executable-find "git")
      (error "Git no está instalado o no está en el PATH."))
    (save-some-buffers t)
    
    ;; Abrir Magit en el directorio de configuración
    (magit-status emacs-config-dir)
    (message "Usa Magit para stage (s), commit (c c), y push (P p).")

    ;; Automatización opcional (si se confirma)
    (when (y-or-n-p "¿Quieres stagear todos los cambios y hacer commit automáticamente? ")
      (magit-stage-modified)
      (let ((commit-message (read-string "Mensaje de commit: ")))
        (magit-commit-create (list "-m" commit-message)) ; <- Aquí se cerró el let
      (when (y-or-n-p "¿Hacer push a GitHub? ")
        (magit-push-current-to-pushremote)))
    ))) ; Cierre del when y la función

;; Optimizaciones finales
(setq jit-lock-defer-time 0.05)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling t)

;; Provide el módulo
(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-fold-core-style 'overlays)
 '(package-selected-packages
   '(ac-geiser company corfu doom-modeline doom-themes embark-consult
	       geiser-racket hyperbole lispy magit magit-p4 marginalia
	       projectile rainbow-delimiters sly smartparens vertico
	       xclip)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Carga doom-modeline
(require 'doom-modeline)

;; Configura los iconos de doom-modeline
;; (setq doom-modeline-icon (display-graphic-p 'nerd-icons))

;; Activa doom-modeline
(doom-modeline-mode 1)
